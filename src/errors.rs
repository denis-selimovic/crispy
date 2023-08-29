use std::fmt::Debug;
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
