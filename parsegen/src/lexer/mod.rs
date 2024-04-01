use crate::Token;

pub mod charset;
// pub mod regular_expression;
pub mod set_automata;
// pub mod state_machine;

pub struct Lexer {}

impl Lexer {
    pub fn new<'a>(regexes: impl IntoIterator<Item = (&'a str, Token)>) -> Self {
        todo!()
    }
}
