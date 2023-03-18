use text_io::read;

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
            add_builtin("andika", 1, bprint_fn),
            add_builtin("soma", 1, bread_fn),
            add_builtin("urefu", 1, blen_fn),
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

fn bprint_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(Object::String(t)) => {
            println!("{}", t);
            Ok(Object::Null)
        }
        Some(o) => {
            println!("{}", o);
            Ok(Object::Null)
        }
        _ => Err(String::from("")), // TODO error message
    }
}

fn bread_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(Object::String(t)) => {
            println!("{}", t);
            let input: String = read!("{}\n");
            Ok(Object::String(input))
        }
        Some(o) => {
            println!("{}", o);
            let input: String = read!("{}\n");
            Ok(Object::String(input))
        }
        _ => Err(String::from("")), // TODO error message
    }
}

fn blen_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(Object::String(s)) => Ok(Object::Number(s.len() as f64)),
        Some(Object::Array(arr)) => Ok(Object::Number(arr.len() as f64)),
        _ => Err(String::from("")), //TODO error message
    }
}
