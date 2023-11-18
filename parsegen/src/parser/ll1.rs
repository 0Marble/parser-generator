use super::{grammar::Grammar, lgraph::Lgraph};

impl Lgraph {
    pub fn ll1(grammar: Grammar) -> Self {
        let first: Vec<_> = grammar.first().collect();
        todo!()
    }
}
