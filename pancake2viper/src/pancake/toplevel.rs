use crate::pancake::Expr;
use crate::utils::Shape;

use super::Stmt;

#[derive(Debug, Clone)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<Arg>,
    pub body: Stmt,
    pub rettyp: Option<Shape>,
}

// [Int(1), Symbol("global"), Symbol("x"), Symbol(":="), List([Symbol("Const"), Symbol("0x1")])]
#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub name: String,
    pub shape: Shape,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub shape: Shape,
}

#[derive(Debug, Clone)]
pub struct Predicate {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct Shared {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FnDec>,
    pub global_vars: Vec<GlobalVar>,
    pub predicates: Vec<Predicate>,
    pub viper_functions: Vec<Function>,
    pub methods: Vec<Method>,
    pub shared: Vec<Shared>,
    pub model_predicates: Vec<String>,
    pub model_fields: Vec<String>,
    pub extern_predicates: Vec<String>,
    pub extern_fields: Vec<String>,
    pub extern_consts: Vec<String>,
    pub extern_methods: Vec<String>,
}
