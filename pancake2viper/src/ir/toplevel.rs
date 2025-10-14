use std::collections::{HashMap, HashSet};

use super::{Decl, Expr, Stmt, Type};

#[derive(Debug, Clone)]
pub struct FnDec {
    pub fname: String,
    pub args: Vec<Arg>,
    pub pres: Vec<Expr>,
    pub posts: Vec<Expr>,
    pub body: Stmt,
    pub retvar: String,
    pub trusted: bool,
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub name: String,
    pub typ: Type,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arg {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Predicate {
    pub name: String,
    pub args: Vec<Arg>,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<Arg>,
    pub typ: Type,
    pub pres: Vec<Expr>,
    pub posts: Vec<Expr>,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct AbstractMethod {
    pub name: String,
    pub args: Vec<Arg>,
    pub pres: Vec<Expr>,
    pub posts: Vec<Expr>,
    pub rettyps: Vec<Decl>,
}

#[derive(Debug, Clone, Copy)]
pub enum SharedPerm {
    ReadWrite,
    WriteOnly,
    ReadOnly,
}

#[derive(Debug, Clone)]
pub struct Shared {
    pub name: String,
    pub typ: SharedPerm,
    pub bits: u64,
    pub lower: Expr,
    pub upper: Expr,
    pub stride: Expr,
}

#[derive(Debug, Clone)]
pub struct Model {
    pub fields: Vec<String>,
    pub predicates: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FnDec>,
    pub global_vars: Vec<GlobalVar>,
    pub predicates: Vec<Predicate>,
    pub viper_functions: Vec<Function>,
    pub methods: Vec<AbstractMethod>,
    pub shared: Vec<Shared>,
    pub model: Model,
    pub extern_predicates: Vec<String>,
    pub extern_fields: HashMap<String, Type>,
    pub extern_methods: HashSet<String>,
    pub extern_consts: HashMap<String, Type>,
}
