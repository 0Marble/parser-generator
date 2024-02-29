use std::{fmt::Display, rc::Rc};

// pub mod codegen;
pub mod lexer;
pub mod parser;
// pub mod tokenizer;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    name: Rc<str>,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenError {
    NotAnIdentifier(String),
}

impl Token {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    // ('_' | letter) ('_' | letter | number)^* and not all undersocres
    pub fn new(name: impl ToString) -> Result<Self, TokenError> {
        let name = name.to_string();
        if !name.starts_with(|c: char| c == '_' || c.is_ascii_alphabetic()) {
            return Err(TokenError::NotAnIdentifier(name));
        }
        for c in name.chars() {
            if c != '_' && !c.is_ascii_alphabetic() && !c.is_ascii_digit() {
                return Err(TokenError::NotAnIdentifier(name));
            }
        }
        Ok(Self { name: name.into() })
    }
}
