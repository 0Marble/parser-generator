use std::{
    fmt::Display,
    io::{Cursor, Write},
};

use super::bytecode::ByteCode;

pub mod c;
pub mod js;
pub mod runtime;
pub mod rust;

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Bytecode { code: usize, expl: String },
}

impl From<std::io::Error> for Error {
    fn from(v: std::io::Error) -> Self {
        Self::Io(v)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for Error {}

pub trait Translator {
    fn translate_program(&mut self, bc: &[ByteCode]) -> Result<String, Error> {
        let mut res = vec![];
        let mut w = Cursor::new(&mut res);
        let w = &mut w;

        for c in bc {
            self.translate_step(&c, w)?;
        }

        Ok(String::from_utf8(res).unwrap())
    }
    fn translate_step(&mut self, bc: &ByteCode, w: &mut dyn Write) -> Result<(), Error>;
}
