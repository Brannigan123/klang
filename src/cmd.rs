use pest::Parser;
use std::{fs, path::PathBuf};

use crate::{
    intepreter::Evaluator,
    parser::{gen_ast, KlangParser, Rule},
};

pub fn run_sample_files() {
    for entry in fs::read_dir("./samples").unwrap() {
        if let Ok(path) = entry.map(|e| e.path()) && path.is_file() {
            println!("Sample: {}", path.display());
            match run_file(path.clone()) {
                Ok(_) => println!("Success: {}\n", path.display()),
                Err(e) => println!("{}\n", e),
            };
        }
    }
}

pub fn run_file(path: PathBuf) -> Result<(), String> {
    match fs::read_to_string(&path) {
        Ok(source) => run(source),
        Err(_) => Err(format!("Failed to load file {}", path.display())),
    }
}

pub fn run(source: String) -> Result<(), String> {
    match KlangParser::parse(Rule::program_file, &source) {
        Ok(mut res) => {
            let mut evaluator = Evaluator::new();
            let ast = gen_ast(res.next().unwrap());
            let ret = evaluator.eval_program(ast);
            if ret.is_error() {
                Err(ret.to_string())
            } else {
                Ok(())
            }
        }
        Err(err) => Err(format!("{:#?}", err)),
    }
}
