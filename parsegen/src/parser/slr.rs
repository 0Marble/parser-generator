use std::io::{Cursor, Write};

use crate::{parser::grammar::Production, tokenizer::Token};

use super::{
    grammar::Grammar,
    lgraph::{Bracket, Item, Lgraph},
};

type LR0Automata1 = (
    Vec<(Token, Vec<(usize, usize)>)>,
    Vec<(usize, Token, usize)>,
);

impl Lgraph {
    fn print_lr0(lr0: &LR0Automata1, grammar: &Grammar) -> std::io::Result<()> {
        let mut buf = vec![];
        let mut w = Cursor::new(&mut buf);
        let w = &mut w;

        writeln!(w, "digraph {{")?;
        for (state_index, (_, kernel)) in lr0.0.iter().enumerate() {
            write!(w, "{state_index} [shape=record, label=\"{{{state_index}|")?;
            let closure = Self::closure(grammar, kernel);
            for (prod_index, spot) in closure {
                let prod = grammar.productions().nth(prod_index).unwrap();
                write!(w, "{} \\-\\> ", prod.lhs())?;
                for i in 0..spot {
                    write!(w, "{} ", prod.rhs()[i])?;
                }
                write!(w, ".")?;
                for i in spot..prod.rhs().len() {
                    write!(w, "{} ", prod.rhs()[i])?;
                }
                write!(w, "\\n")?;
            }
            writeln!(w, "}}\"];")?;
        }

        for (from, sym, to) in lr0.1.iter() {
            writeln!(w, "{from} -> {to} [label={sym}];")?;
        }
        writeln!(w, "}}")?;

        std::fs::write("tests/lr0.dot", buf)
    }

    pub fn slr(grammar: &Grammar) -> Self {
        todo!()
    }

    fn lr0_automata(grammar: &Grammar) -> LR0Automata1 {
        let (start_index, _) = grammar
            .productions()
            .enumerate()
            .find(|(_, s)| s.lhs() == grammar.start())
            .unwrap();

        let mut stack = vec![(grammar.start(), vec![(start_index, 0)])];
        let mut lr0_states = vec![];
        let mut lr0_goto = vec![];
        while let Some((state_symbol, state)) = stack.pop() {
            let state_index = lr0_states.len();
            lr0_states.push((state_symbol, state));
            let state = lr0_states[state_index].1.as_slice();

            let full_state = Self::closure(&grammar, state);

            let mut goto = vec![];
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
                    .find(|(_, (_, s))| s.as_slice() == next_state.as_slice())
                {
                    i
                } else {
                    stack.push((symbol.clone(), next_state));
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

    fn closure(grammar: &Grammar, kernel: &[(usize, usize)]) -> Vec<(usize, usize)> {
        let mut full_state = kernel.to_vec();
        let mut changed = true;
        while changed {
            changed = false;
            let old_state = full_state.clone();
            for (prod_index, spot) in old_state {
                let prod = grammar.productions().nth(prod_index).unwrap();
                let symbol = if let Some(s) = prod.rhs().get(spot) {
                    s
                } else {
                    continue;
                };

                if grammar.is_terminal(symbol.clone()) {
                    continue;
                }

                for (other_idx, prod) in grammar.productions().enumerate() {
                    if &prod.lhs() != symbol || full_state.contains(&(other_idx, 0)) {
                        continue;
                    }
                    full_state.push((other_idx, 0));
                    changed = true;
                }
            }
        }
        full_state
    }

    fn goto(state: usize, sym: Token, g: &[(usize, Token, usize)]) -> Option<usize> {
        for (from, s, to) in g {
            if &state == from && &sym == s {
                return Some(*to);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, str::FromStr};

    use super::*;

    #[test]
    fn closure() {
        let g = Grammar::from_str("Start -> S; S -> a S | T c; T -> a T b | ;").unwrap();
        let kernel = [(0, 0)];
        let full_state: HashSet<_> = Lgraph::closure(&g, &kernel).into_iter().collect();
        let actual_state = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]
            .into_iter()
            .collect();
        assert_eq!(full_state, actual_state)
    }

    #[test]
    fn goto() {
        let g = Grammar::from_str("Start -> S; S -> a S | T c; T -> a T b | ;").unwrap();
    }
}
