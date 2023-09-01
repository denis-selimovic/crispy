mod ast;
mod errors;
mod exec;
mod lexer;
mod parser;
mod scope;

use std::cell::RefCell;
use std::io::BufRead;
use std::rc::Rc;

pub use ast::ASTNode;
pub use errors::CompileError;
pub use exec::exec;
pub use scope::Scope;

pub fn interpreter<B: BufRead>(reader: &mut B) {
    let scope = Rc::new(RefCell::new(Scope::new()));

    for line in reader.lines() {
        match line {
            Ok(l) => {
                if l.eq("exit") {
                    println!("Exiting Lisp interpreter");
                    break;
                }

                match exec(&l, scope.clone()) {
                    Err(e) => println!("err: {}", e.to_string()),
                    Ok(ast) => println!("lisp-scheme>> {}", ast),
                }
            }
            Err(e) => {
                println!("Error while reading input {}", e.to_string());
                break;
            }
        }
    }
}
