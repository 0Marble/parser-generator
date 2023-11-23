use std::{fmt::Display, io::Cursor};

use crate::{parser::grammar::Production, tokenizer::Token};

use super::{
    grammar::{Grammar, TokenOrEnd},
    lgraph::{Bracket, Item, Lgraph},
};

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

    fn goto_idx(&self, from: usize, sym: Token) -> Option<usize> {
        self.goto
            .iter()
            .find(|(n, t, _)| n == &from && &sym == t)
            .map(|a| a.2.clone())
    }

    fn state_descirption(
        w: &mut dyn std::io::Write,
        closure: &[(usize, usize)],
        state_index: usize,
        grammar: &Grammar,
    ) -> std::io::Result<()> {
        write!(w, "{{{state_index} |")?;
        for (prod_index, spot) in closure {
            let prod = grammar.productions().nth(*prod_index).unwrap();
            write!(w, "{} \\-\\> ", prod.lhs())?;
            for i in 0..*spot {
                write!(w, "{} ", prod.rhs()[i])?;
            }
            write!(w, ".")?;
            for i in *spot..prod.rhs().len() {
                write!(w, "{} ", prod.rhs()[i])?;
            }
            write!(w, "\\n")?;
        }
        write!(w, "}}")
    }
}

impl Display for LR0Automata {
    fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(w, "digraph {{")?;
        for (state_index, kernel) in self.states.iter().enumerate() {
            write!(w, "  {state_index} [shape=record, label=\"")?;
            let closure = Self::closure(&self.grammar, kernel);
            let mut buf = vec![];
            Self::state_descirption(
                &mut Cursor::new(&mut buf),
                &closure,
                state_index,
                &self.grammar,
            )
            .unwrap();
            writeln!(w, "{}\"];", String::from_utf8(buf).unwrap())?;
        }

        for (from, sym, to) in self.goto.iter() {
            writeln!(w, "  {from} -> {to} [label={sym}];")?;
        }
        writeln!(w, "}}")
    }
}

impl Lgraph {
    pub fn slr(grammar: &Grammar) -> Self {
        let lr0 = LR0Automata::new(grammar);
        let first = lr0.grammar.first();
        let follow = lr0.grammar.follow(&first);

        let mut slr = Lgraph::default();
        let start_node = lr0.states.len();
        let end_node = lr0.states.len() + 1;
        let dispatch_node_offset = lr0.states.len() + 2;
        let mut path_node = dispatch_node_offset + lr0.grammar.terminals().count() + 1;
        let bracket_offset = lr0.grammar.non_terminals().count();

        for (from, sym, to) in lr0.goto.iter().cloned() {
            if !lr0.grammar.is_terminal(sym.clone()) {
                continue;
            }

            slr = slr.add_edge(
                from,
                Item::new(
                    Some(TokenOrEnd::Token(sym)),
                    None,
                    Some(Bracket::new(to + bracket_offset, true)),
                ),
                to,
            );
        }

        for (state_idx, kernel) in lr0.states.iter().enumerate() {
            let state = LR0Automata::closure(&lr0.grammar, kernel);
            let mut buf = vec![];
            LR0Automata::state_descirption(
                &mut Cursor::new(&mut buf),
                &state,
                state_idx,
                &lr0.grammar,
            )
            .unwrap();
            slr = slr.set_node_label(state_idx, String::from_utf8(buf).unwrap());

            for (prod_idx, spot) in state {
                let prod = lr0.grammar.productions().nth(prod_idx).unwrap();
                if spot != prod.rhs().len() {
                    continue;
                }

                let follow_a = follow.follow(prod.lhs()).unwrap();
                for sym in follow_a {
                    let dispatch_node = match sym.as_token() {
                        Some(tok) => {
                            lr0.grammar
                                .terminals()
                                .enumerate()
                                .find(|(_, t)| t == tok)
                                .unwrap()
                                .0
                        }
                        None => lr0.grammar.terminals().count(),
                    } + dispatch_node_offset;
                    slr = slr.set_node_label(
                        dispatch_node,
                        format!("{{{dispatch_node}|{sym} dispatch node}}"),
                    );

                    // to dispatch
                    if prod.rhs().len() == 0 {
                        slr = slr.add_edge(
                            state_idx,
                            Item::new(Some(sym.clone()), None, Some(Bracket::new(prod_idx, true))),
                            dispatch_node,
                        );
                    } else {
                        slr = slr.add_edge(
                            state_idx,
                            Item::new(Some(sym.clone()), None, Some(Bracket::Wildcard)),
                            path_node,
                        );
                        path_node += 1;

                        for _ in 1..prod.rhs().len() {
                            slr = slr.add_edge(
                                path_node - 1,
                                Item::new(None, None, Some(Bracket::Wildcard)),
                                path_node,
                            );
                            path_node += 1;
                        }

                        slr = slr.add_edge(
                            path_node - 1,
                            Item::new(None, None, Some(Bracket::new(prod_idx, true))),
                            dispatch_node,
                        );
                    }

                    // from dispatch
                    for (from, nt, to) in lr0.goto.iter().cloned() {
                        if nt != prod.lhs() {
                            continue;
                        }

                        let final_node = match sym.as_token() {
                            Some(sym) => {
                                if let Some(n) = lr0.goto_idx(to, sym.clone()) {
                                    n
                                } else {
                                    println!(
                                        "Sus {} -{}-> {} -{}[{}]-> {} -{}",
                                        dispatch_node,
                                        prod_idx,
                                        from,
                                        prod.lhs(),
                                        prod_idx,
                                        to,
                                        sym
                                    );
                                    // We want the LR0 automata to actually have a connection
                                    // from -A-> to -sym-> final
                                    // But we actually have to look at the slr at that point
                                    // because there might be a situation where
                                    // to -sym-> some other dispatch node....
                                    continue;
                                }
                            }
                            None => end_node,
                        };
                        let brackets = [
                            Bracket::new(prod_idx, false),
                            Bracket::new(from + bracket_offset, false),
                            Bracket::new(from + bracket_offset, true),
                            Bracket::new(to + bracket_offset, true),
                            Bracket::new(final_node + bracket_offset, true),
                        ];

                        let mut prev = dispatch_node;
                        for i in 0..brackets.len() {
                            let item = Item::new(None, None, Some(brackets[i]));
                            let next = if let Some((_, _, old)) =
                                slr.edges().find(|(from, i, _)| from == &prev && i == &item)
                            {
                                prev = old;
                                continue;
                            } else if i + 1 == brackets.len() {
                                final_node
                            } else {
                                path_node += 1;
                                path_node - 1
                            };

                            slr = slr.add_edge(prev, item, next);
                            prev = next;
                        }
                    }
                }
            }
        }

        let start_prod_idx = lr0
            .grammar
            .productions()
            .enumerate()
            .find(|(_, p)| p.lhs() == lr0.grammar.start())
            .unwrap()
            .0;
        let start_state = lr0
            .states
            .iter()
            .enumerate()
            .find(|(_, kernel)| kernel.as_slice() == &[(start_prod_idx, 0)])
            .unwrap()
            .0;
        slr = slr.add_edge(
            start_node,
            Item::new(
                None,
                None,
                Some(Bracket::new(start_state + bracket_offset, true)),
            ),
            start_state,
        );

        slr.add_start_node(start_node)
            .add_end_node(end_node)
            .add_edge(
                end_node,
                Item::new(None, None, Some(Bracket::Wildcard)),
                end_node,
            )
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
