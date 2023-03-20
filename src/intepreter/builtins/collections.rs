use crate::intepreter::obj::Object;


pub fn blen_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(Object::String(s)) => Ok(Object::Number(s.len() as f64)),
        Some(Object::Array(arr)) => Ok(Object::Number(arr.len() as f64)),
        _ => Err(String::from("")), //TODO error message
    }
}