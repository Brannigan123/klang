use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::parser::ast::{Expression, Identifier, Statements};

use super::env::Environment;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Number(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Tuple(Vec<Object>),
    Dictionary(HashMap<Object, Object>),
    Function(
        Vec<Identifier>,
        Option<Expression>,
        Statements,
        Rc<RefCell<Environment>>,
    ),
    Builtin(String, usize, BuiltinFunction),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

pub type BuiltinFunction = fn(Vec<Object>) -> Result<Object, String>;

impl Object {
    pub fn is_returned(&self) -> bool {
        matches!(*self, Object::ReturnValue(_))
    }

    pub fn returned(self) -> Self {
        match self {
            Object::ReturnValue(o) => *o,
            o => o,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Number(ref n) => write!(f, "{}", n),
            Object::Boolean(ref b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Object::String(ref s) => write!(f, "{}", s),
            Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push('[');
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push(']');
                write!(f, "{}", fmt_string)
            }
            Object::Tuple(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push('(');
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push(')');
                write!(f, "{}", fmt_string)
            }
            Object::Dictionary(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push('{');
                for (i, (k, v)) in hashmap.iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < hashmap.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push('}');
                write!(f, "{}", fmt_string)
            }
            Object::Function(_, _, _, _) => write!(f, "[function]"),
            Object::Builtin(ref name, _, _) => write!(f, "[built-in function: {}]", *name),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(ref o) => write!(f, "{}", *o),
            Object::Error(ref s) => write!(f, "Error: {}", s),
        }
    }
}

impl Eq for Object {}

#[allow(clippy::all)]
impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Boolean(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
