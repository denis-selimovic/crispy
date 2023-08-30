use std::{error, fmt::Debug};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("string not terminated")]
    NotTerminated,
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Error during lexical analysis: {0}")]
    Lex(String),
    #[error("Unexpected token. Expected {0} got {1}")]
    UnexpectedToken(String, String),
    #[error("Expected token, but reached end of file")]
    EndOfFile,
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Error during compilation: {0}")]
    Parser(String),
    #[error("Unexpected AST node")]
    UnexpectedNode,
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),
    #[error("Unbound lambda function: {0}")]
    UnboundLambda(String),
    #[error("Syntax error: {0}")]
    Syntax(String),
    #[error("Wrong type for {0} expression")]
    TypeMismatch(String),
    #[error("Wrong number of params for: {0}")]
    WrongParamNumber(String),
    #[error("Unsupported operator: {0}")]
    UnsupportedOperator(String),
    #[error("Unsupported operand types for operator: {0}")]
    UnsupportedOperands(String),
}
