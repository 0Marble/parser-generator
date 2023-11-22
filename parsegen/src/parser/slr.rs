use crate::{parser::grammar::Production, tokenizer::Token};

use super::{grammar::Grammar, lgraph::Lgraph};

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
        let (lr0_states, lr0_goto) = Self::lr0_automata(&grammar);

        todo!()
    }

    fn lr0_automata(grammar: &Grammar) -> (Vec<Vec<(usize, usize)>>, Vec<(usize, Token, usize)>) {
        let (start_index, _) = grammar
            .productions()
            .enumerate()
            .find(|(_, s)| s.lhs() == grammar.start())
            .unwrap();

        let mut stack = vec![vec![(start_index, 0)]];
        let mut lr0_states: Vec<Vec<(usize, usize)>> = vec![];
        let mut lr0_goto = vec![];
        while let Some(state) = stack.pop() {
            let state_index = lr0_states.len();
            lr0_states.push(state);
            let state = lr0_states[state_index].as_slice();

            let mut full_state = state.to_vec();
            for (prod_index, spot) in state {
                let symbol = grammar
                    .productions()
                    .nth(*prod_index)
                    .unwrap()
                    .rhs()
                    .get(*spot)
                    .unwrap();
                if grammar.is_terminal(symbol.clone()) {
                    continue;
                }

                for (other_idx, prod) in grammar.productions().enumerate() {
                    if &prod.lhs() != symbol || full_state.contains(&(other_idx, 0)) {
                        continue;
                    }
                    full_state.push((other_idx, 0));
                }
            }

            let mut goto: Vec<(&Token, Vec<(usize, usize)>)> = vec![];
            for (prod_index, spot) in full_state {
                let prod = grammar.productions().nth(prod_index).unwrap();
                if prod.rhs().len() <= spot + 1 {
                    continue;
                }
                let symbol = prod.rhs().get(spot).unwrap();

                let goto_on_symbol =
                    if let Some((_, g)) = goto.iter_mut().find(|(s, _)| s == &symbol) {
                        g
                    } else {
                        goto.push((symbol, vec![]));
                        &mut goto.last_mut().unwrap().1
                    };
                if !goto_on_symbol.contains(&(prod_index, spot + 1)) {
                    goto_on_symbol.push((prod_index, spot + 1));
                }
            }

            for (symbol, next_state) in goto {
                let next_state_index = if let Some((i, _)) = lr0_states
                    .iter()
                    .enumerate()
                    .find(|(_, s)| s.as_slice() == next_state.as_slice())
                {
                    i
                } else {
                    stack.push(next_state);
                    lr0_states.len()
                };

                let edge = (state_index, symbol.clone(), next_state_index);
                if !lr0_goto.contains(&edge) {
                    lr0_goto.push(edge);
                }
            }
        }

        (lr0_states, lr0_goto)
    }
}
