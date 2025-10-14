use viper::Expr;

use crate::{
    ir::{self, Arg, FnDec, Type},
    utils::{ToViper, ToViperError, ToViperType, TryToViper, ViperEncodeCtx, ViperUtils},
};

impl<'a, T: TryToViper<'a>> TryToViper<'a> for Vec<T> {
    type Output = Vec<T::Output>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        self.into_iter()
            .map(|a| a.to_viper(ctx))
            .collect::<Result<Vec<_>, _>>()
    }
}

impl<'a, T: ToViper<'a>> ToViper<'a> for Vec<T> {
    type Output = Vec<T::Output>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Self::Output {
        self.into_iter().map(|a| a.to_viper(ctx)).collect()
    }
}

impl Arg {
    /// Generates preconditions for an argument
    ///
    /// If an `Arg` is not of shape `1` it is encoded as a `Seq[Int]`.
    /// We can automatically infer the length of the sequence given we know its shape.
    /// We also assert that all the elements of the sequence or the single Int are bounded
    pub fn precondition<'a>(
        &self,
        is_predicate: bool,
        ctx: &ViperEncodeCtx<'a>,
    ) -> Option<Expr<'a>> {
        let ast = ctx.ast;
        let arg_var = ctx.ast.new_var(&self.name, self.typ.to_viper_type(ctx)).1;

        match &self.typ {
            ir::Type::Struct(_) => {
                let length = ast.int_lit(self.typ.len() as i64);
                let length_pre = ast.eq_cmp(ast.seq_length(arg_var), length);
                let i = ast.new_var("i", ast.int_type());
                let bound_pre = ast.forall(
                    &[i.0],
                    &[],
                    ast.implies(
                        ast.and(ast.le_cmp(ast.zero(), i.1), ast.lt_cmp(i.1, length)),
                        ctx.utils
                            .bounded_f(ast.seq_index(arg_var, i.1), ctx.options.word_size),
                    ),
                );
                Some(ast.and(length_pre, bound_pre))
            }
            ir::Type::Int => {
                if is_predicate {
                    None
                } else {
                    Some(ctx.utils.bounded_f(arg_var, ctx.options.word_size))
                }
            }
            _ => None,
        }
    }
}

impl<'a> ToViperType<'a> for ir::Type {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a> {
        let ast = ctx.ast;
        match self {
            ir::Type::Bool => ast.bool_type(),
            ir::Type::Int => ast.int_type(),
            ir::Type::Array => ctx.heap.get_type(),
            ir::Type::Struct(_) => ast.seq_type(ast.int_type()),
            ir::Type::Ref => ast.ref_type(),
            ir::Type::Map(k, v) => ast.map_type(k.to_viper_type(ctx), v.to_viper_type(ctx)),
            ir::Type::Set(i) => ast.set_type(i.to_viper_type(ctx)),
            ir::Type::Seq(i) => ast.seq_type(i.to_viper_type(ctx)),
            x => panic!("Want type of {:?}", x),
        }
    }
}

impl FnDec {
    pub fn postcondition<'a>(&self, ctx: &ViperEncodeCtx<'a>) -> Expr<'a> {
        let ast = ctx.ast;
        match ctx.get_type(&self.retvar).unwrap() {
            Type::Int => {
                let retval = ast.local_var(&self.retvar, ast.int_type());
                ctx.utils.bounded_f(retval, ctx.options.word_size)
            }
            struc @ Type::Struct(_) => {
                // Length of the returned `IArray`
                let retval = ast.local_var(&self.retvar, struc.to_viper_type(ctx));
                let length = ast.int_lit(struc.len() as i64);
                let length_post = ast.eq_cmp(ast.seq_length(retval), length);
                // Bound of all elements
                let i = ast.new_var("i", ast.int_type());
                let bounded = ast.forall(
                    &[i.0],
                    &[],
                    ast.implies(
                        ast.and(ast.le_cmp(ast.zero(), i.1), ast.lt_cmp(i.1, length)),
                        ctx.utils
                            .bounded_f(ast.seq_index(retval, i.1), ctx.options.word_size),
                    ),
                );

                ast.and(length_post, bounded)
            }
            _ => unreachable!(),
        }
    }
}
