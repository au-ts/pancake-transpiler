use crate::{
    annotation::{
        parse_extern_ffi, parse_extern_field, parse_extern_const, parse_extern_predicate, parse_function, parse_method,
        parse_model_field, parse_model_predicate, parse_predicate, parse_shared,
    },
    ir::{self, Model},
    pancake,
    utils::{ToType, TranslationError, TryToIR},
};

use super::utils::stmt_annotation_push;

impl TryToIR for pancake::Arg {
    type Output = ir::Arg;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            name: self.name,
            typ: self.shape.to_type(false),
        })
    }
}

impl TryToIR for pancake::FnDec {
    type Output = ir::FnDec;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        let args = self.args.to_ir()?;
        let mut body = args.iter().fold(self.body.to_ir()?, |scope, arg| {
            ir::Stmt::Definition(ir::Definition {
                lhs: arg.name.clone(),
                rhs: ir::Expr::Var(ir::Var {name: arg.name.clone(), global: None}),
                scope: Box::new(scope),
            })
        });
        let ((pres, posts), trusted) = stmt_annotation_push(&mut body);
        Ok(Self::Output {
            fname: self.fname,
            args,
            body,
            pres,
            posts,
            retvar: "retval".into(),
            trusted,
        })
    }
}

impl TryToIR for pancake::Predicate {
    type Output = ir::Predicate;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        parse_predicate(&self.text).map_err(|err| TranslationError::ParsingError(err.to_string()))
    }
}

impl TryToIR for pancake::Function {
    type Output = ir::Function;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        parse_function(&self.text).map_err(|err| TranslationError::ParsingError(err.to_string()))
    }
}

impl TryToIR for pancake::Method {
    type Output = ir::AbstractMethod;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        parse_method(&self.text).map_err(|err| TranslationError::ParsingError(err.to_string()))
    }
}

impl TryToIR for pancake::Shared {
    type Output = ir::Shared;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        parse_shared(&self.text).map_err(|err| TranslationError::ParsingError(err.to_string()))
    }
}

impl TryToIR for pancake::GlobalVar {
    type Output = ir::GlobalVar;

    fn to_ir(self) -> Result<Self::Output, TranslationError> {
        Ok(Self::Output {
            name: self.name,
            typ: self.shape.to_type(false),
            value: self.value.to_ir()?,
        })
    }
}

impl TryFrom<pancake::Program> for ir::Program {
    type Error = TranslationError;

    fn try_from(value: pancake::Program) -> Result<Self, Self::Error> {
        let viper_functions = value.viper_functions.to_ir()?;
        let predicates = value.predicates.to_ir()?;
        let functions = value.functions.to_ir()?;
        let global_vars = value.global_vars.to_ir()?;
        let methods = value.methods.to_ir()?;
        let shared = value.shared.to_ir()?;

        let model_predicates = value
            .model_predicates
            .iter()
            .map(|s| {
                parse_model_predicate(s)
                    .map_err(|err| TranslationError::ParsingError(err.to_string()))
            })
            .collect::<Result<_, _>>()?;
        let model_fields = value
            .model_fields
            .iter()
            .map(|s| {
                parse_model_field(s).map_err(|err| TranslationError::ParsingError(err.to_string()))
            })
            .collect::<Result<_, _>>()?;

        let model = Model {
            predicates: model_predicates,
            fields: model_fields,
        };

        let extern_predicates = value
            .extern_predicates
            .iter()
            .map(|s| {
                parse_extern_predicate(s)
                    .map_err(|err| TranslationError::ParsingError(err.to_string()))
            })
            .collect::<Result<_, _>>()?;
        let extern_fields = value
            .extern_fields
            .iter()
            .map(|s| {
                parse_extern_field(s)
                    .map(|decl| (decl.name, decl.typ))
                    .map_err(|err| TranslationError::ParsingError(err.to_string()))
            })
            .collect::<Result<_, _>>()?;
        let extern_consts = value
            .extern_consts
            .iter()
            .map(|s| {
                parse_extern_const(s)
                    .map(|decl| (decl.name, decl.typ))
                    .map_err(|err| TranslationError::ParsingError(err.to_string()))
            })
            .collect::<Result<_, _>>()?;
        let extern_methods = value
            .extern_methods
            .iter()
            .map(|s| {
                parse_extern_ffi(s).map_err(|err| TranslationError::ParsingError(err.to_string()))
            })
            .collect::<Result<_, _>>()?;

        Ok(ir::Program {
            functions,
            global_vars,
            predicates,
            viper_functions,
            methods,
            shared,
            extern_predicates,
            extern_fields,
            extern_consts,
            extern_methods,
            model,
        })
    }
}
