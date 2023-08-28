use std::fmt::Debug;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("string not terminated")]
    NotTerminated,
}
