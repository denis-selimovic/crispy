use crispy::{exec, Scope};

use clap::Parser;
use std::cell::RefCell;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::rc::Rc;

/// Simple program to greet a person
#[derive(Parser, Debug)]
struct Args {
    path: PathBuf,
}

pub fn main() {
    let args = Args::parse();
    let scope = Rc::new(RefCell::new(Scope::new()));

    match read_to_string(args.path.as_path()) {
        Err(e) => println!("Error while reading the file: {}", e.to_string()),
        Ok(input) => match exec(&input, scope.clone()) {
            Err(e) => println!("Interpreter error: {}", e.to_string()),
            Ok(ast) => println!("{}", ast),
        },
    }
}
