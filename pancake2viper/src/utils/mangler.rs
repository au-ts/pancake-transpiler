use std::{
    collections::{HashMap, HashSet},
    sync::{LazyLock, Mutex},
};

use super::{MangleError, TranslationMode, RESERVED};

static COUNTER: LazyLock<Mutex<u64>> = LazyLock::new(|| Mutex::new(0));

fn get_inc_counter() -> u64 {
    let mut c = COUNTER.lock().unwrap();
    let ret = *c;
    *c += 1;
    ret
}

#[derive(Debug, Clone, Default)]
pub struct Mangler {
    mode: TranslationMode,
    fname: Option<String>,
    annot_map: HashMap<String, String>,
    var_map: HashMap<String, String>,
    arg_map: HashMap<String, String>,
    ref_set: HashSet<String>,
}

pub enum VariableType {
    Variable,
    Argument,
}

impl Mangler {
    pub fn new(ref_set: HashSet<String>) -> Self {
        Self {
            ref_set,
            ..Default::default()
        }
    }

    pub fn child(&self) -> Self {
        Self {
            mode: self.mode,
            fname: self.fname.clone(),
            annot_map: self.annot_map.clone(),
            var_map: self.var_map.clone(),
            arg_map: self.arg_map.clone(),
            ref_set: self.ref_set.clone(),
        }
    }

    fn clean_local(&mut self) {
        self.annot_map.clear();
        self.var_map.clear();
        self.arg_map.clear();
    }

    pub fn global_var_mangle(
        &mut self,
        name: String
    ) -> Result<(), MangleError> {
        if RESERVED.contains_key(name.as_str()) {
            return Err(MangleError::ReservedKeyword(name));
        }
        Ok(())
    }

    pub fn new_mangled_var(
        &mut self,
        name: String,
        typ: VariableType,
    ) -> Result<String, MangleError> {
        if RESERVED.contains_key(name.as_str()) {
            return Err(MangleError::ReservedKeyword(name));
        }
        if self.ref_set.contains(name.as_str()) {
            return Err(MangleError::DoubleDeclaration(name));
        }
        let mangled = format!("{}_{}", &name, get_inc_counter());
        let map = match (&typ, self.mode) {
            (VariableType::Variable, TranslationMode::Normal) => &mut self.var_map,
            (VariableType::Variable, _) => &mut self.annot_map,
            (VariableType::Argument, TranslationMode::Normal) => &mut self.arg_map,
            _ => unreachable!(),
        };
        // Check if variable has already been declared. For normal variables
        // this is allowed due to shadowing.
        if map.contains_key(&name)
            && !matches!(
                (typ, self.mode),
                (VariableType::Variable, TranslationMode::Normal)
            )
        {
            return Err(MangleError::DoubleDeclaration(name));
        }
        map.insert(name.clone(), mangled.clone());
        Ok(mangled)
    }

    pub fn clear_annot_var(&mut self) {
        self.annot_map.clear();
    }

    pub fn fresh_varname() -> String {
        let fresh = format!("fr_{}", get_inc_counter());
        fresh
    }

    pub fn mangle_mode(&mut self, mode: TranslationMode) {
        self.mode = mode;
    }

    pub fn mangle_var<'a>(&'a self, var: &'a str) -> Result<&'a str, MangleError> {
        if RESERVED.contains_key(var) {
            return Ok(var);
        }
        if self.ref_set.contains(var) {
            return Ok(var);
        }
        let maybe_arg = self.arg_map.get(var);
        let maybe_annot = self.annot_map.get(var);
        let maybe_var = self.var_map.get(var);
        match self.mode {
            TranslationMode::Normal | TranslationMode::WhileCond => maybe_var.or(maybe_arg),
            TranslationMode::Assertion => maybe_annot.or(maybe_var),
            TranslationMode::PrePost => maybe_annot.or(maybe_arg),
        }
        .map(String::as_str)
        .ok_or(MangleError::UndeclaredVar(var.to_owned()))
    }

    pub fn switch_ctx(&mut self, fname: String) {
        self.fname = Some(fname);
        self.clean_local();
    }

    pub fn mangle_fn(fname: &str) -> String {
        format!("f_{}", fname)
    }
}
