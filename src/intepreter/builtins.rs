use text_io::read;

use crate::{intepreter::Evaluator, parser::ast::Identifier};

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
            add_builtin("soma_namba", 1, bread_num_fn),
            add_builtin("bulin", 1, b2bool_fn),
            add_builtin("namba", 1, b2num_fn),
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
        Some(o) => {
            println!("{}", Evaluator::ots(o.clone()));
            Ok(Object::Null)
        }
        _ => Err(String::from("")), // TODO error message
    }
}

fn bread_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => {
            println!("{}", Evaluator::ots(o.clone()));
            let input: String = read!("{}\n");
            Ok(Object::String(input))
        }
        _ => Err(String::from("")), // TODO error message
    }
}

fn bread_num_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => {
            println!("{}", Evaluator::ots(o.clone()));
            let input: f64 = read!("{}\n");
            Ok(Object::Number(input))
        }
        _ => Err(String::from("")), // TODO error message
    }
}

fn b2bool_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => Evaluator::otb(o.clone())
            .map(Object::Boolean)
            .map_err(|_| String::from("Conversion to boolean failed")),
        _ => Err(String::from("")), // TODO error message
    }
}

fn b2num_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => Evaluator::otn(o.clone())
            .map(Object::Number)
            .map_err(|_| String::from("Conversion to number failed")),
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
