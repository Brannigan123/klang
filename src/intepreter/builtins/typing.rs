use crate::intepreter::{obj::Object, Evaluator};

pub fn b2bool_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => Evaluator::otb(o.clone())
            .map(Object::Boolean)
            .map_err(|_| String::from("Conversion to boolean failed")),
        _ => Err(String::from("")), // TODO error message
    }
}

pub fn b2num_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => Evaluator::otn(o.clone())
            .map(Object::Number)
            .map_err(|_| String::from("Conversion to number failed")),
        _ => Err(String::from("")), // TODO error message
    }
}
