use text_io::read;

use crate::intepreter::{obj::Object, Evaluator};

pub fn bprint_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => {
            println!("{}", Evaluator::ots(o.clone()));
            Ok(Object::Null)
        }
        _ => Err(String::from("")), // TODO error message
    }
}

pub fn bread_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => {
            println!("{}", Evaluator::ots(o.clone()));
            let input: String = read!("{}\n");
            Ok(Object::String(input))
        }
        _ => Err(String::from("")), // TODO error message
    }
}

pub fn bread_num_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(o) => {
            println!("{}", Evaluator::ots(o.clone()));
            let input: String = read!("{}\n");
            input
                .parse()
                .map(Object::Number)
                .map_err(|_| format!("Expected input to be a number. Found: {}", input))
        }
        _ => Err(String::from("")), // TODO error message
    }
}
