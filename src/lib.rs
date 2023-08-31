mod ast;
mod errors;
mod exec;
mod lexer;
mod parser;
mod scope;

pub use ast::ASTNode;
pub use errors::CompileError;
pub use exec::exec;
pub use scope::Scope;
