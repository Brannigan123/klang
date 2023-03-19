#![feature(let_chains)]

pub mod cmd;
pub mod intepreter;
pub mod parser;

use std::{env, path::PathBuf};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        args[1..]
            .into_iter()
            .map(|p| cmd::run_file(PathBuf::from(p)))
            .for_each(|res| match res {
                Ok(_) => println!("Code executed successfully"),
                Err(e) => println!("{}", e),
            });
    } else {
        cmd::run_sample_files();
    }
}
