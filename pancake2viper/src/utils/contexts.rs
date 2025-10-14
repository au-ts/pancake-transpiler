use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use viper::{AstFactory, Declaration, LocalVarDecl};

use crate::{
    ir::{self, shared::SharedContext, types::Type, AnnotationType, FnDec, Model},
    viper_prelude::{utils::Utils, HeapHelper},
};

use super::{mangler::Mangler, TranslationError, RESERVED};

#[derive(Debug, Clone, Copy, Default)]
pub enum TranslationMode {
    #[default]
    Normal,
    PrePost,
    Assertion,
    WhileCond,
}

impl TranslationMode {
    pub fn is_annot(&self) -> bool {
        match self {
            Self::Normal | Self::WhileCond => false,
            Self::Assertion | Self::PrePost => true,
        }
    }
}

impl From<AnnotationType> for TranslationMode {
    fn from(value: AnnotationType) -> Self {
        use AnnotationType::*;
        match value {
            Postcondition | Precondition => Self::PrePost,
            _ => Self::Assertion,
        }
    }
}

// todo: might not need this type map anymore if all variables have type upon declaration
#[derive(Debug, Clone)]
pub struct TypeContext {
    type_map: HashMap<String, Type>,
    fields: HashMap<String, Type>,
}

type Exprs = Vec<ir::Expr>;
type Args = Vec<ir::Arg>;

#[derive(Debug, Clone)]
pub struct MethodContext(pub HashMap<String, (Exprs, Exprs, Args)>);

impl MethodContext {
    pub fn new(functions: &[FnDec]) -> Self {
        let mut annot_ctx = HashMap::new();
        for f in functions {
            annot_ctx.insert(
                f.fname.clone(),
                (f.pres.clone(), f.posts.clone(), f.args.clone()),
            );
        }
        Self(annot_ctx)
    }

    pub fn get_pre(&self, name: &str) -> &[ir::Expr] {
        &self.0.get(name).unwrap().0
    }

    pub fn get_post(&self, name: &str) -> &[ir::Expr] {
        &self.0.get(name).unwrap().1
    }

    pub fn get_args(&self, name: &str) -> &[ir::Arg] {
        &self.0.get(name).unwrap().2
    }
}

impl TypeContext {
    pub fn new(fields: HashMap<String, Type>) -> Self {
        let type_map = RESERVED
            .iter()
            .map(|(&s, t)| (s.to_owned(), t.clone()))
            .collect();
        Self { type_map, fields }
    }

    pub fn child(&self) -> Self {
        self.clone()
    }

    pub fn get_type_no_mangle(&self, var: &str) -> Result<Type, TranslationError> {
        self.type_map
            .get(var)
            .cloned()
            .ok_or_else(|| {
                // println!("Error: variable '{}' not found", var);
                // println!("Available types in type_map: {:?}", self.type_map);
                TranslationError::UnknownShape(var.to_owned())
            })
    }

    pub fn get_function_type(&self, fname: &str) -> Result<Type, TranslationError> {
        self.type_map
            .get(fname.strip_prefix("f_").unwrap())
            .or_else(|| self.type_map.get(fname))
            .ok_or(TranslationError::UnknownReturnType(fname.to_owned()))
            .cloned()
    }

    pub fn insert_field(&mut self, var: String, typ: Type) {
        self.fields.insert(var.clone(), typ.clone());
        self.type_map.insert(var, typ);
    }

    pub fn set_type(&mut self, var: String, typ: Type) {
        self.type_map.insert(var, typ);
    }

    pub fn size(&self) -> usize {
        self.type_map.len()
    }

    pub fn get_field_type(&self, field: &str) -> Result<Type, TranslationError> {
        self.fields
            .get(field)
            .map(|t| t.to_owned())
            .ok_or_else( || {
                // println!("Error: Unknown Field '{}'", field);
                // println!("Available types in field_map: {:?}", self.fields);
                // println!("Available types in type_map: {:?}", self.type_map);
                TranslationError::UnknownField(field.to_owned())
            })
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new(HashMap::new())
    }
}

pub struct ViperEncodeCtx<'a> {
    mode: TranslationMode,
    pub ast: AstFactory<'a>,
    pub stack: Vec<viper::Stmt<'a>>,
    pub while_stack: Vec<viper::Stmt<'a>>,
    pub declarations: Vec<viper::LocalVarDecl<'a>>,
    while_counter: u64,
    types: TypeContext,
    pub heap: HeapHelper<'a>,
    pub utils: Utils<'a>,
    pub options: EncodeOptions,
    pub consume_stack: bool,

    pub invariants: Vec<viper::Expr<'a>>,
    predicates: HashSet<String>,
    pub mangler: Mangler,
    pub shared: Rc<SharedContext>,
    pub method: Rc<MethodContext>,
    pub model: Model,
    pub extern_methods: HashSet<String>,
    pub shared_override: Option<String>,
    pub extern_consts: HashMap<String, Type>,
}

#[derive(Clone, Copy)]
pub struct EncodeOptions {
    pub assert_aligned_accesses: bool,
    pub word_size: u64,
    pub heap_base: u64,
    pub heap_top: u64,
    pub check_overflows: bool,
    pub bounded_arithmetic: bool,
    pub debug_comments: bool,
    pub include_prelude: bool,
    pub allow_undefined_shared: bool,
    pub ignore_warnings: bool,
}

impl Default for EncodeOptions {
    fn default() -> Self {
        Self {
            assert_aligned_accesses: true,
            word_size: 64,
            heap_base: 0x20000000,
            heap_top: 0x40000000,
            check_overflows: true,
            bounded_arithmetic: false,
            debug_comments: false,
            include_prelude: true,
            allow_undefined_shared: false,
            ignore_warnings: false,
        }
    }
}

impl<'a> ViperEncodeCtx<'a> {
    pub fn new(
        types: TypeContext,
        predicates: HashSet<String>,
        ast: AstFactory<'a>,
        options: EncodeOptions,
        shared: Rc<SharedContext>,
        annot: Rc<MethodContext>,
        model: Model,
        extern_methods: HashSet<String>,
        extern_consts: HashMap<String, Type>,
    ) -> Self {
        let heap = HeapHelper::new(ast);
        let fields_set: HashSet<String> = model.fields.clone().into_iter().collect();
        let consts_set: HashSet<String> = extern_consts.keys().cloned().collect();
        let mangler_set: HashSet<String> = fields_set.union(&consts_set).cloned().collect::<HashSet<String>>();
        Self {
            mode: TranslationMode::Normal,
            ast,
            stack: vec![],
            while_stack: vec![],
            declarations: vec![],
            types,
            while_counter: 0,
            heap,
            utils: Utils::new(ast, heap.get_type(), model.clone()),
            options,
            consume_stack: true,
            invariants: vec![],
            predicates,
            mangler: Mangler::new(mangler_set),
            shared,
            method: annot,
            model,
            extern_methods,
            shared_override: None,
            extern_consts,
        }
    }

    pub fn child(&self) -> Self {
        Self {
            mode: self.mode,
            ast: self.ast,
            stack: vec![],
            while_stack: vec![],
            declarations: vec![],
            types: self.types.child(),
            while_counter: self.while_counter,
            heap: self.heap,
            utils: self.utils.clone(),
            options: self.options,
            consume_stack: self.consume_stack,
            invariants: vec![],
            predicates: self.predicates.clone(),
            mangler: self.mangler.clone(),
            shared: self.shared.clone(),
            method: self.method.clone(),
            model: self.model.clone(),
            extern_methods: self.extern_methods.clone(),
            shared_override: self.shared_override.clone(),
            extern_consts: self.extern_consts.clone(),
        }
    }

    pub fn enter_new_loop(&mut self) {
        self.while_counter += 1;
    }

    pub fn current_break_label(&self) -> String {
        format!("break_label_{}", self.while_counter)
    }

    pub fn current_continue_label(&self) -> String {
        format!("continue_label_{}", self.while_counter)
    }

    pub fn outer_break_label(&self) -> String {
        format!("break_label_{}", self.while_counter - 1)
    }

    pub fn outer_continue_label(&self) -> String {
        format!("continue_label_{}", self.while_counter - 1)
    }

    pub fn return_label(&self) -> &'static str {
        "return_label"
    }

    pub fn return_var_name(&self) -> &'static str {
        "retval"
    }

    pub fn new_while_ctx(&mut self) {
        self.while_counter += 1;
    }

    pub fn pop_decls(&mut self) -> Vec<Declaration<'a>> {
        self.declarations
            .drain(..)
            .map(LocalVarDecl::into)
            .collect()
    }

    pub fn heap_type(&self) -> viper::Type {
        self.heap.get_type()
    }

    pub fn heap_vars(&self) -> Vec<(viper::LocalVarDecl, viper::Expr)> {
        self.utils.heap_vars() 
    }

    pub fn gv_ref(&self) -> (viper::LocalVarDecl, viper::Expr) {
        self.utils.gv_ref()
    }

    pub fn gv_access(&self, name: &String) -> viper::Expr<'a> {
        let ast = self.ast;
        let var= self.utils.gv_ref();
        ast.field_access(var.1, ast.field(name, ast.int_type()))
    }

    pub fn set_mode(&mut self, mode: TranslationMode) {
        self.mode = mode;
    }

    pub fn get_mode(&self) -> TranslationMode {
        self.mode
    }

    pub fn is_predicate(&self, ident: &str) -> bool {
        self.predicates.contains(ident)
    }

    pub fn get_type(&self, var: &str) -> Result<Type, TranslationError> {
        self.types.get_type_no_mangle(var)
    }

    pub fn get_function_type(&self, fname: &str) -> Result<Type, TranslationError> {
        self.types.get_function_type(fname)
    }

    pub fn set_type(&mut self, var: String, typ: Type) {
        self.types.set_type(var, typ);
    }

    pub fn type_ctx(&self) -> &TypeContext {
        &self.types
    }

    pub fn typectx_get(&self) -> &TypeContext {
        &self.types
    }

    pub fn typectx_get_mut(&mut self) -> &mut TypeContext {
        &mut self.types
    }

    pub fn word_values(&self) -> viper::Expr<'a> {
        let ast = self.ast;
        ast.mul(
            ast.int_lit(4),
            ast.int_lit(2i64.pow(self.options.word_size as u32 - 2)),
        )
    }

    pub fn get_default_args(&self) -> (Vec<viper::LocalVarDecl>, Vec<viper::Expr>) {
        self.model.get_default_args(self.ast, self.heap_vars())
    }
}
