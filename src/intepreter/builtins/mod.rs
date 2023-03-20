pub mod collections;
pub mod console_io;
pub mod typing;

use crate::parser::ast::Identifier;

use super::obj::{BuiltinFunction, Object};

pub struct BuiltinsFunctions;

impl Default for BuiltinsFunctions {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinsFunctions {
    pub fn new() -> Self {
        BuiltinsFunctions {}
    }

    pub fn get_builtins(&self) -> Vec<(Identifier, Object)> {
        vec![
            add_builtin("andika", 1, console_io::bprint_fn),
            add_builtin("soma", 1, console_io::bread_fn),
            add_builtin("soma_namba", 1, console_io::bread_num_fn),
            add_builtin("bulin", 1, typing::b2bool_fn),
            add_builtin("namba", 1, typing::b2num_fn),
            add_builtin("urefu", 1, collections::blen_fn),
        ]
    }
}

fn add_builtin(name: &str, param_num: usize, func: BuiltinFunction) -> (Identifier, Object) {
    let name = name.to_owned();
    (
        Identifier(name.clone()),
        Object::Builtin(name, param_num, func),
    )
}
