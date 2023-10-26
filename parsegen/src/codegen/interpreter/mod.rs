use std::fmt::Display;

use super::bytecode::{ByteCode, Ident};

pub mod c;
pub mod js;
pub mod runtime;
pub mod rust;

#[derive(Debug)]
pub struct Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for Error {}

pub trait Interpreter {
    fn exec(&mut self, entry: Ident, bc: &[ByteCode]) -> Result<String, Error>;
}
