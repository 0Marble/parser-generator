use crate::{
    lexer::Lexer,
    parser::{grammar::Grammar, lgraph::Lgraph},
};

pub trait Codegen {
    fn gen_code(&self, lexer: &Lexer, parser: &Lgraph, grammar: &Grammar, path: &str);
}

pub mod rs;
