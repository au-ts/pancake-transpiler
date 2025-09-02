use crate::{
    ir,
    utils::{
        ForceToBool, Mangler, ToViperError, ToViperType, TranslationMode, TryToShape, TryToViper,
        ViperEncodeCtx, ViperUtils,
    },
};

impl<'a> TryToViper<'a> for ir::Stmt {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        use ir::Stmt::*;
        if ctx.options.debug_comments {
            ctx.stack.push(ast.comment(&format!("Stmt: {}", self)));
        }
        let stmt = match self {
            Skip => ast.comment("skip"),
            Break => ast.goto(&ctx.outer_break_label()),
            Continue => ast.goto(&ctx.outer_continue_label()),
            Return => ast.goto(ctx.return_label()),
            x => match x {
                Annotation(annot) => annot.to_viper(ctx),
                Definition(def) => def.to_viper(ctx),
                Assign(ass) => ass.to_viper(ctx),
                If(ifs) => ifs.to_viper(ctx),
                While(whiles) => whiles.to_viper(ctx),
                Seq(seq) => seq.to_viper(ctx),
                Call(call) => call.to_viper(ctx),
                ExtCall(ext) => ext.to_viper(ctx),
                Store(store) => store.to_viper(ctx),
                StoreBits(store) => store.to_viper(ctx),
                SharedStore(store) => store.to_viper(ctx),
                SharedStoreBits(store) => store.to_viper(ctx),
                SharedLoad(load) => load.to_viper(ctx),
                SharedLoadBits(load) => load.to_viper(ctx),
                _ => unreachable!(),
            }?,
        };
        if ctx.consume_stack {
            ctx.stack.push(stmt);
            let decls = ctx.pop_decls();

            let seq = ast.seqn(&ctx.stack, &decls);
            ctx.stack.clear();
            Ok(seq)
        } else {
            Ok(stmt)
        }
    }
}

impl<'a> TryToViper<'a> for ir::If {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        let cond = self.cond.force_to_bool(ctx)?;
        let mut then_ctx = ctx.child();
        let then_body = self.if_branch.to_viper(&mut then_ctx)?;
        let mut else_ctx = then_ctx.child();
        let else_body = self.else_branch.to_viper(&mut else_ctx)?;

        let decls = ctx.pop_decls();

        ctx.stack.push(ast.if_stmt(cond, then_body, else_body));
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::While {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        ctx.set_mode(TranslationMode::WhileCond);
        let cond = self.cond.force_to_bool(ctx)?;
        ctx.set_mode(TranslationMode::Normal);
        let mut body_ctx = ctx.child();
        body_ctx.enter_new_loop();
        let body = self.body.to_viper(&mut body_ctx)?;

        let decls = ctx.pop_decls();

        let mut body_seq = ctx.while_stack.clone();
        body_seq.push(body);
        body_seq.push(ast.label(&ctx.current_continue_label(), &[]));
        body_seq.extend(ctx.stack.clone());

        let body = ast.seqn(&body_seq, &[]);

        ctx.stack.push(ast.while_stmt(
            cond,
            &body_ctx.invariants.drain(..).collect::<Vec<_>>(),
            body,
        ));
        ctx.stack.push(ast.label(&ctx.current_break_label(), &[]));
        ctx.stack.append(&mut ctx.while_stack);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::Seq {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let stmts = self.stmts.to_viper(ctx)?;
        Ok(ast.seqn(&stmts, &[]))
    }
}

impl<'a> TryToViper<'a> for ir::Definition {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        let shape = self.rhs.to_shape(ctx.type_ctx())?;
        let var = ast.new_var(&self.lhs, shape.to_viper_type(ctx));
        ctx.declarations.push(var.0);

        let ass = ast.local_var_assign(var.1, self.rhs.to_viper(ctx)?);
        let mut scope_ctx = ctx.child();
        let scope = self.scope.to_viper(&mut scope_ctx)?;
        // Push not consumed invariants up
        ctx.invariants.append(&mut scope_ctx.invariants);

        let decls = ctx.pop_decls();

        ctx.stack.push(ass);
        ctx.stack.push(scope);
        let seq = ast.seqn(&ctx.stack, &decls);
        ctx.stack.clear();
        Ok(seq)
    }
}

impl<'a> TryToViper<'a> for ir::Assign {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;
        if self.global {
            let ast = ctx.ast;
            let var = ctx.gv_access(&self.lhs);
            let ass = ast.field_assign(var, self.rhs.to_viper(ctx)?);
            let decls = ctx.pop_decls();

            ctx.stack.push(ass);
            let seq = ast.seqn(&ctx.stack, &decls);
            ctx.stack.clear();
            Ok(seq)
        } else {
            let lhs_shape = ctx.get_type(&self.lhs)?;
            // let rhs_shape = self.rhs.to_shape(ctx.typectx_get())?;
            // FIXME: add type checking
            // if lhs_shape != rhs_shape {
            //     return Err(ToViperError::MismatchedShapes(lhs_shape, rhs_shape));
            // }
            let var = ast.new_var(&self.lhs, lhs_shape.to_viper_type(ctx));

            let ass = ast.local_var_assign(var.1, self.rhs.to_viper(ctx)?);
            let decls = ctx.pop_decls();

            ctx.stack.push(ass);
            let seq = ast.seqn(&ctx.stack, &decls);
            ctx.stack.clear();
            Ok(seq)
        }
    }
}

impl<'a> TryToViper<'a> for ir::Call {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        ir::Definition {
            lhs: Mangler::fresh_varname(),
            rhs: self.call,
            scope: Box::new(ir::Stmt::Skip),
        }
        .to_viper(ctx)
    }
}

impl<'a> TryToViper<'a> for ir::ExtCall {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let trimmed = self.fname.trim_start_matches("f_ffi");
        let name = if ctx.extern_methods.contains(trimmed) {
            trimmed
        } else {
            &self.fname
        };
        let ast = ctx.ast;
        let args = self.args.to_viper(ctx)?;
        let mut base_args = ctx.get_default_args().1;
        base_args.extend(args);
        Ok(ast.method_call(name, &base_args, &[]))
    }
}

impl<'a> TryToViper<'a> for ir::Annotation {
    type Output = viper::Stmt<'a>;
    fn to_viper(self, ctx: &mut ViperEncodeCtx<'a>) -> Result<Self::Output, ToViperError> {
        let ast = ctx.ast;

        use crate::ir::AnnotationType::*;
        match self.typ {
            Use => {
                if let ir::Expr::Var(name) = self.expr {
                    ctx.shared_override = Some(name.clone());
                    return Ok(ast.comment(&format!("use {}", name)));
                }
                Err(ToViperError::InvalidAnnotation)
            }
            fold @ (Fold | Unfold) => match self.expr {
                ir::Expr::FunctionCall(access) => {
                    let ast_node = |e| match fold {
                        Unfold => ast.unfold(e),
                        Fold => ast.fold(e),
                        _ => unreachable!(),
                    };
                    Ok(ast_node(access.to_viper(ctx)?))
                }
                _ => Err(ToViperError::InvalidFold(self.expr)),
            },
            x => {
                let no_pos = ast.no_position();

                ctx.set_mode(self.typ.into());
                let body = self.expr.force_to_bool(ctx)?;
                ctx.set_mode(TranslationMode::Normal);
                ctx.mangler.clear_annot_var();
                match x {
                    Assertion => Ok(ast.assert(body, no_pos)),
                    Assumption | Inhale => Ok(ast.inhale(body, no_pos)),
                    Exhale => Ok(ast.exhale(body, no_pos)),
                    Refutation => Ok(ast.refute(body, no_pos)),
                    Invariant => {
                        ctx.invariants.push(body);
                        Ok(ast.comment("invariant pushed"))
                    }
                    _ => Err(ToViperError::InvalidAnnotation),
                }
            }
        }
    }
}
