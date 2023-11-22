use crate::{parser::grammar::Production, regex::state_machine::Nfa, tokenizer::Token};

use super::{
    grammar::{Follow, Grammar},
    lgraph::Lgraph,
};

impl Lgraph {
    pub fn slr(grammar: &Grammar) -> Self {
        let new_start = Token::new("Start");
        let grammar = Grammar::new(
            grammar
                .productions()
                .cloned()
                .chain(std::iter::once(Production::new(
                    new_start.clone(),
                    vec![grammar.start()],
                ))),
            new_start.clone(),
        );
        let first = grammar.first();
        let follow = grammar.follow(&first);
        let (start_index, _) = grammar
            .productions()
            .enumerate()
            .find(|(_, s)| s.lhs() == new_start)
            .unwrap();

        let mut stack = vec![vec![(0, 0)]];
        let mut lr0_states: Vec<Vec<(i32, i32)>> = vec![];
        let mut lr0_goto = vec![];
        while let Some(state) = stack.pop() {
            let state_index = if let Some((i, _)) = lr0_states
                .iter()
                .enumerate()
                .find(|(_, s)| s.as_slice() == state.as_slice())
            {
                i
            } else {
                lr0_states.push(state.clone());
                lr0_states.len() - 1
            };
        }

        todo!()
    }
}
