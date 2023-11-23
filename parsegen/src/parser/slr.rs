use std::fmt::Display;

use crate::{parser::grammar::Production, tokenizer::Token};

use super::{grammar::Grammar, lgraph::Lgraph};

struct LR0Automata {
    states: Vec<Vec<(usize, usize)>>,
    goto: Vec<(usize, Token, usize)>,
    grammar: Grammar,
}

impl LR0Automata {
    pub fn new(grammar: &Grammar) -> Self {
        let start = grammar.get_unique_token("Start");
        let grammar = Grammar::new(
            grammar
                .productions()
                .cloned()
                .chain(std::iter::once(Production::new(
                    start.clone(),
                    vec![grammar.start()],
                ))),
            start.clone(),
        );
        let (start_prod, _) = grammar
            .productions()
            .enumerate()
            .find(|(_, p)| p.lhs() == start)
            .unwrap();
        let mut stack = vec![0];
        let mut states = vec![vec![(start_prod, 0)]];
        let mut goto = vec![];

        while let Some(kernel_idx) = stack.pop() {
            let mut goto_symbols = vec![];
            for (prod_idx, spot) in Self::closure(&grammar, &states[kernel_idx]) {
                let prod = grammar.productions().nth(prod_idx).unwrap();
                let sym = if let Some(sym) = prod.rhs().get(spot) {
                    sym
                } else {
                    continue;
                };

                if !goto_symbols.contains(&sym) {
                    goto_symbols.push(sym);
                }
            }

            for sym in goto_symbols {
                let next_kernel = Self::goto(&grammar, &states[kernel_idx], sym.clone());
                let next_kernel_idx = if let Some((i, _)) = states
                    .iter()
                    .enumerate()
                    .find(|(_, k)| k.as_slice() == next_kernel.as_slice())
                {
                    i
                } else {
                    states.push(next_kernel);
                    stack.push(states.len() - 1);
                    states.len() - 1
                };
                goto.push((kernel_idx, sym.clone(), next_kernel_idx));
            }
        }

        Self {
            states,
            goto,
            grammar,
        }
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

    fn goto(grammar: &Grammar, kernel: &[(usize, usize)], symbol: Token) -> Vec<(usize, usize)> {
        let mut next_kernel = vec![];

        for (prod_idx, spot) in Self::closure(grammar, kernel) {
            let prod = grammar.productions().nth(prod_idx).unwrap();
            let sym = if let Some(sym) = prod.rhs().get(spot) {
                sym
            } else {
                continue;
            };
            let item = (prod_idx, spot + 1);
            if sym != &symbol || next_kernel.contains(&item) {
                continue;
            }
            next_kernel.push(item);
        }

        next_kernel
    }
}

impl Display for LR0Automata {
    fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(w, "digraph {{")?;
        for (state_index, kernel) in self.states.iter().enumerate() {
            write!(w, "  {state_index} [shape=record, label=\"{{{state_index}|")?;
            let closure = Self::closure(&self.grammar, kernel);
            for (prod_index, spot) in closure {
                let prod = self.grammar.productions().nth(prod_index).unwrap();
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

        for (from, sym, to) in self.goto.iter() {
            writeln!(w, "  {from} -> {to} [label={sym}];")?;
        }
        writeln!(w, "}}")
    }
}

impl Lgraph {
    pub fn slr(grammar: &Grammar) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, str::FromStr};

    use super::*;
    fn expr_grammar() -> Grammar {
        Grammar::from_str("E -> T add E | T; T -> F mul T | F; F -> id | lp E rp").unwrap()
    }

    #[test]
    fn closure() {
        let grammar = expr_grammar();
        assert_eq!(
            LR0Automata::closure(&grammar, &[(0, 0)])
                .into_iter()
                .collect::<HashSet<_>>(),
            [(0, 0), (2, 0), (3, 0), (4, 0), (5, 0)]
                .into_iter()
                .collect()
        )
    }

    #[test]
    fn lr0() {
        let grammar = expr_grammar();
        let lr0 = LR0Automata::new(&grammar);
        std::fs::write("tests/lr0.dot", lr0.to_string()).unwrap();
        assert_eq!(lr0.states.len(), 12);
    }
}
