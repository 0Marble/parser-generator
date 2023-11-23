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
        let lr0 = Self::lr0_automata(&grammar);
        Self::print_lr0(&lr0, &grammar).unwrap();
        let (lr0_states, lr0_goto) = lr0;

        // We create a LR(0) automaton first
        // From it, we create an L-graph in the following way:
        //
        // for all edges (i, terminal, j), add an edge (i, terminal, (j, j)
        //
        // if a node i contains LR0 item A -> beta. we should
        // for all c in FOLLOW(A) add a series of edges
        //    (i, c, wildcard x n (k, j)
        // where n is len(beta), j is the c-dispatcher node and A -> beta
        // is the k'th production
        //
        // when we are in a c-dispatcher i, we have just eaten a terminal c, and our
        // stack looks like (j (k, where k is an index of A -> beta, and j is some node
        // index. We must simulate than the movement from node j of LR0 automata
        // on input A c. We will end up at a state
        // l = goto(l1, c), where l1 = goto(j, A)
        // Therefore, for all edges (_, (k, i) we add a series of edges
        //      (i, None, )k )j (j (l1 (l, l)
        // Note that l1 is a node of symbol A, so we get j by looking at all LR0 edges
        //      (j, A, l1)

        let mut slr = Lgraph::default();
        let start_node = lr0_states.len();
        let end_node = lr0_states.len() + 1;
        let dispatch_node_offset = lr0_states.len() + 2;
        let path_node_offset = dispatch_node_offset + grammar.terminals().count() + 1;
        let mut path_nodes_count = 0;
        let state_bracket_offsets = grammar.non_terminals().count();

        for (from, symbol, to) in lr0_goto.iter().cloned() {
            if !grammar.is_terminal(symbol.clone()) {
                continue;
            }

            slr = slr.add_edge(
                from,
                Item::new(
                    Some(symbol),
                    None,
                    Some(Bracket::new(state_bracket_offsets + to, true)),
                ),
                to,
            );
        }

        for (state_index, (_, state)) in lr0_states.iter().enumerate() {
            for (prod_index, spot) in state {
                let prod = grammar.productions().nth(*prod_index).unwrap();
                let prod_len = prod.rhs().len();
                if *spot != prod_len {
                    continue;
                }

                let follow_a = follow.follow(prod.lhs()).unwrap();
                for c in follow_a {
                    let c_dispatch_node = if let Some((i, _)) = grammar
                        .terminals()
                        .enumerate()
                        .find(|(_, t)| Some(t) == c.as_ref())
                    {
                        i
                    } else {
                        grammar.terminals().count()
                    };
                    let look_ahead = if c.is_none() { Some(vec![None]) } else { None };

                    if prod_len == 0 {
                        slr = slr.add_edge(
                            state_index,
                            Item::new(c.clone(), look_ahead, Some(Bracket::new(*prod_index, true))),
                            c_dispatch_node + dispatch_node_offset,
                        );
                    } else {
                        slr = slr.add_edge(
                            state_index,
                            Item::new(c.clone(), look_ahead, None),
                            path_nodes_count + path_node_offset,
                        );
                        path_nodes_count += 1;
                        for _ in 0..prod_len {
                            slr = slr.add_edge(
                                path_nodes_count + path_node_offset,
                                Item::new(None, None, Some(Bracket::Wildcard)),
                                path_node_offset + path_nodes_count + 1,
                            );
                            path_nodes_count += 1;
                        }
                        slr = slr.add_edge(
                            path_nodes_count + path_node_offset,
                            Item::new(None, None, Some(Bracket::new(*prod_index, true))),
                            c_dispatch_node + dispatch_node_offset,
                        );
                    }
                    // from dispatch
                    for (from, sym, to) in &lr0_goto {
                        if sym != &prod.lhs() {
                            continue;
                        }

                        let after_to = if let Some(c) = c {
                            Self::goto(*to, c.clone(), &lr0_goto).unwrap()
                        } else {
                            end_node
                        };

                        let mut prev = c_dispatch_node + dispatch_node_offset;
                        let brackets = [
                            Bracket::new(*prod_index, false),
                            Bracket::new(from + state_bracket_offsets, false),
                            Bracket::new(from + state_bracket_offsets, true),
                            Bracket::new(*to + state_bracket_offsets, true),
                            Bracket::new(after_to + state_bracket_offsets, true),
                        ];

                        for (i, bracket) in brackets.into_iter().enumerate() {
                            let item = Item::new(None, None, Some(bracket));
                            let to = if i + 1 == brackets.len() {
                                after_to
                            } else if let Some((_, _, to)) =
                                slr.edges_from(prev).find(|(_, it, _)| it == &item)
                            {
                                prev = to;
                                continue;
                            } else {
                                path_nodes_count += 1;
                                path_node_offset + path_nodes_count - 1
                            };

                            slr = slr.add_edge(prev, item, to);
                            prev = to;
                        }
                    }
                }
            }
        }

        slr.add_start_node(start_node).add_end_node(end_node)
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
