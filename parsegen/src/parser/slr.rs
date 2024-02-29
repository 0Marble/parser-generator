use std::{fmt::Display, io::Cursor};

use crate::{parser::grammar::Production, Token};

use super::{
    grammar::{Grammar, Node, TokenOrEnd},
    lgraph::{Bracket, Item, Lgraph},
};

struct LR0Automata {
    states: Vec<(Token, Vec<(usize, usize)>)>,
    goto: Vec<(usize, Token, usize)>,
    grammar: Grammar,
}

impl LR0Automata {
    pub fn new(grammar: &Grammar) -> Self {
        let start = grammar.get_unique_token("Start").unwrap();
        let grammar = Grammar::new(
            grammar.productions().cloned().chain(std::iter::once(
                Production::new(start.clone(), vec![grammar.start()])
                    .try_into()
                    .unwrap(),
            )),
            start.clone(),
        );
        let (start_prod, _) = grammar
            .productions()
            .enumerate()
            .find(|(_, p)| p.lhs() == start)
            .unwrap();
        let mut stack = vec![0];
        let mut states = vec![(start.clone(), vec![(start_prod, 0)])];
        let mut goto = vec![];

        while let Some(kernel_idx) = stack.pop() {
            let mut goto_symbols = vec![];
            for (prod_idx, spot) in Self::closure(&grammar, &states[kernel_idx].1) {
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
                let next_kernel = Self::goto(&grammar, &states[kernel_idx].1, sym.clone());
                let next_kernel_idx = if let Some((i, _)) = states
                    .iter()
                    .enumerate()
                    .find(|(_, (_, k))| k.as_slice() == next_kernel.as_slice())
                {
                    i
                } else {
                    states.push((sym.clone(), next_kernel));
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

    fn is_slr(&self) {
        let first = self.grammar.first();
        let follow = self.grammar.follow(&first);

        for (state_idx, (_, kernel)) in self.states.iter().enumerate() {
            let state = Self::closure(&self.grammar, kernel);

            let mut shift_outs = vec![];
            let mut reduce_outs = vec![];
            for (prod_idx, spot) in state {
                let prod = self.grammar.productions().nth(prod_idx).unwrap();
                match prod.rhs().get(spot) {
                    Some(sym) => {
                        if shift_outs.contains(sym) {
                            continue;
                        }
                        shift_outs.push(sym.clone());
                    }
                    None => {
                        for sym in follow.follow(prod.lhs()).unwrap() {
                            assert!(
                                !reduce_outs.contains(sym),
                                "Grammar not SLR: REDUCE/REDUCE conflict on {state_idx}, {sym}"
                            );
                            reduce_outs.push(sym.clone());
                        }
                    }
                }
            }

            for reduce in reduce_outs {
                assert!(
                    !reduce.as_token().map_or(false, |t| shift_outs.contains(t)),
                    "Grammar not SLR: SHIFT/REDUCE conflict on {state_idx}, {reduce}"
                );
            }
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
        &self,
        w: &mut dyn std::io::Write,
        state_index: usize,
    ) -> std::io::Result<()> {
        let (kernel_symbol, kernel) = &self.states[state_index];
        let closure = LR0Automata::closure(&self.grammar, kernel);
        write!(w, "{{{state_index} {kernel_symbol}|")?;
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
        write!(w, "}}")
    }
}

impl Display for LR0Automata {
    fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(w, "digraph {{")?;
        for (state_index, (_, _)) in self.states.iter().enumerate() {
            write!(w, "  {state_index} [shape=record, label=\"")?;
            let mut buf = vec![];
            self.state_descirption(&mut Cursor::new(&mut buf), state_index)
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
    //  step 0: construct a normal lr0 automaton.
    //
    //  step 1: for all edges (p, x, q) in lr0, where p is associated with a terminal or with S',
    //  and x is a terminal, add the edge directly.
    //
    //  step 2: connecting reductions. Let state P of lr0 have an item (i, len of prod[i]), with
    //  prod[i] = A -> alpha. for all a in FOLLOW(A), connect P to (A, a) with a series of edges
    //   (P, a, pre(i, A, a)) (pre(i, A, a), { )*... ^End(i, A) } (A, a). Note that we skip
    //   outputing the start symbol, as it is the start sybmol of the augmented grammar.
    //
    //  step 3: resolve dispatch. for all edges (p, A, q), where A is a terminal, we look
    //  at node q. There are two possibilities:
    //      1. If there is an lr0 edge (q, a, r), we must add a series of edges from (A, a) to r:
    //         ((A,a) { )p (p (q ^a (r} r).
    //      2. There is an item (i, len of prod[i]) in q, where prod[i] = B -> beta. We must add a
    //         series of edges from (A, a) to (B, a): ((A, a), { )p (p (q } pre(i, B, a))
    //
    //  step 4: starting. At the start, we should put (P on the stack, where P is the lr0 state
    //  associated with the S' start symbol.
    //
    //  step 5: ending. Let P be an lr0 state that is associated with token S' (there is only one
    //  such state). Add an edge ((S', $), )P, End), where End is marked as an end node.
    //
    // NOTE: For constuction, we do not use the input grammar G directly. We instead use an
    // augmented grammar G', containing an extra rule S' -> S. We must make sure production indices
    // from G' are compatible with the ones from G.
    pub fn slr(grammar: &Grammar) -> Self {
        let lr0 = LR0Automata::new(grammar);
        lr0.is_slr();

        let grammar = &lr0.grammar;
        let first = grammar.first();
        let follow = grammar.follow(&first);

        let mut slr = Self::default();

        let terminal_count = grammar.terminals().count() + 1;
        let prod_count = grammar.productions().count();
        let nonterm_count = grammar.non_terminals().count();
        let start_node = lr0.states.len();
        let end_node = start_node + 1;
        let dispatch_node_offset = end_node + 1;
        let pre_dispatch_node_offset = dispatch_node_offset + terminal_count * nonterm_count;
        let mut path_node = pre_dispatch_node_offset + terminal_count * prod_count;

        // step 1
        for (from, tok, to) in &lr0.goto {
            if !grammar.is_terminal(tok.clone()) {
                continue;
            }

            let (from_assoc, _) = &lr0.states[*from];
            if !grammar.is_terminal(from_assoc.clone()) && from_assoc != &grammar.start() {
                continue;
            }
            slr = slr
                .add_edge(
                    *from,
                    Item::default()
                        .with_token(Some(tok.clone().into()))
                        .with_bracket(Some(Bracket::Open(*to)))
                        .with_output(Some(Node::Leaf(tok.clone()))),
                    *to,
                )
                .label_lr0_state(*from, &lr0)
                .label_lr0_state(*to, &lr0);
        }

        // step 2
        for (node_idx, (_, kernel)) in lr0.states.iter().enumerate() {
            for (prod_idx, spot) in LR0Automata::closure(grammar, kernel) {
                let prod = grammar.productions().nth(prod_idx).unwrap();
                if prod.rhs().len() != spot {
                    continue;
                }

                for after_prod in follow.follow(prod.lhs()).unwrap() {
                    let tok_idx = match after_prod {
                        TokenOrEnd::Token(t) => grammar.terminal_index(t.clone()).unwrap(),
                        TokenOrEnd::End => terminal_count - 1,
                    };

                    let pre_dispatch =
                        prod_idx * terminal_count + tok_idx + pre_dispatch_node_offset;
                    let nt_idx = grammar.non_terminal_index(prod.lhs()).unwrap();
                    let dispatch = nt_idx * terminal_count + tok_idx + dispatch_node_offset;

                    slr = slr
                        .add_edge(
                            node_idx,
                            Item::default().with_token(Some(after_prod.clone())),
                            pre_dispatch,
                        )
                        .label_lr0_state(node_idx, &lr0)
                        .label_pre_dispatch(pre_dispatch, prod.lhs(), prod_idx, after_prod.clone())
                        .label_dispatch(dispatch, prod.lhs(), after_prod.clone());

                    let output = if prod.lhs() == grammar.start() {
                        None
                    } else {
                        Some(Node::RuleEnd(prod_idx, prod.lhs()))
                    };
                    let items = if prod.rhs().is_empty() {
                        vec![Item::default().with_output(output)]
                    } else {
                        let mut items = vec![
                            Item::default().with_bracket(Some(Bracket::Wildcard));
                            prod.rhs().len() - 1
                        ];
                        items.push(
                            Item::default()
                                .with_output(output)
                                .with_bracket(Some(Bracket::Wildcard)),
                        );

                        items
                    };

                    slr = slr.add_path(pre_dispatch, dispatch, &items, &mut path_node);
                }
            }
        }

        // step 3
        for (from, a_nt, to) in &lr0.goto {
            if grammar.is_terminal(a_nt.clone()) {
                continue;
            }
            let nt_idx = grammar.non_terminal_index(a_nt.clone()).unwrap();

            for tok in follow.follow(a_nt.clone()).unwrap() {
                let tok_idx = match tok {
                    TokenOrEnd::Token(t) => grammar.terminal_index(t.clone()).unwrap(),
                    TokenOrEnd::End => terminal_count - 1,
                };
                let dispatch = nt_idx * terminal_count + tok_idx + dispatch_node_offset;
                slr = slr.label_dispatch(dispatch, a_nt.clone(), tok.clone().into());

                if let Some((after, tok)) = tok
                    .as_token()
                    .and_then(|t| lr0.goto_idx(*to, t.clone()).zip(Some(t)))
                {
                    slr = slr
                        .add_path(
                            dispatch,
                            after,
                            &[
                                Item::default().with_bracket(Some(Bracket::Closed(*from))),
                                Item::default().with_bracket(Some(Bracket::Open(*from))),
                                Item::default().with_bracket(Some(Bracket::Open(*to))),
                                Item::default()
                                    .with_bracket(Some(Bracket::Open(after)))
                                    .with_output(Some(Node::Leaf(tok.clone().into()))),
                            ],
                            &mut path_node,
                        )
                        .label_lr0_state(after, &lr0)
                } else {
                    let (_, kernel) = &lr0.states[*to];
                    for (prod_idx, spot) in LR0Automata::closure(grammar, kernel) {
                        let prod = grammar.productions().nth(prod_idx).unwrap();
                        if prod.rhs().len() != spot
                            || !follow.follow(prod.lhs()).unwrap().contains(tok)
                        {
                            continue;
                        }

                        let pre_dispatch =
                            prod_idx * terminal_count + tok_idx + pre_dispatch_node_offset;

                        slr = slr
                            .add_path(
                                dispatch,
                                pre_dispatch,
                                &[
                                    Item::default().with_bracket(Some(Bracket::Closed(*from))),
                                    Item::default().with_bracket(Some(Bracket::Open(*from))),
                                    Item::default().with_bracket(Some(Bracket::Open(*to))),
                                ],
                                &mut path_node,
                            )
                            .label_pre_dispatch(pre_dispatch, a_nt.clone(), prod_idx, tok.clone());
                    }
                }
            }
        }

        // step 4
        slr = slr.add_start_node(start_node);
        let (lr0_start, _) = lr0
            .states
            .iter()
            .enumerate()
            .find(|(_, (s, _))| s == &grammar.start())
            .unwrap();
        slr = slr.add_edge(
            start_node,
            Item::default().with_bracket(Some(Bracket::Open(lr0_start))),
            lr0_start,
        );

        // step 5
        let start_sym_idx = grammar.non_terminal_index(grammar.start()).unwrap();
        let start_end_dispatch =
            start_sym_idx * terminal_count + terminal_count - 1 + dispatch_node_offset;
        slr = slr.add_end_node(end_node);
        slr = slr
            .add_edge(
                start_end_dispatch,
                Item::default().with_bracket(Some(Bracket::Closed(lr0_start))),
                end_node,
            )
            .label_dispatch(start_end_dispatch, grammar.start(), TokenOrEnd::End);

        slr
    }

    pub fn add_path(
        mut self,
        from: usize,
        to: usize,
        items: &[Item],
        path_node: &mut usize,
    ) -> Self {
        let mut prev = from;
        self = self.add_node(from);
        for (i, item) in items.iter().enumerate() {
            let next =
                if let Some((_, _, old)) = self.edges_from(prev).find(|(_, it, _)| it == item) {
                    if i + 1 == items.len() {
                        assert_eq!(old, to);
                    }
                    prev = old;
                    continue;
                } else if i + 1 == items.len() {
                    to
                } else {
                    *path_node += 1;
                    *path_node - 1
                };

            self = self.add_edge(prev, item.clone(), next);
            prev = next;
        }

        self
    }

    fn label_lr0_state(self, state_idx: usize, lr0: &LR0Automata) -> Self {
        let mut buf = vec![];
        lr0.state_descirption(&mut Cursor::new(&mut buf), state_idx)
            .unwrap();
        self.set_node_label(state_idx, String::from_utf8(buf).unwrap())
    }
    fn label_dispatch(self, node: usize, nonterminal: Token, terminal: TokenOrEnd) -> Self {
        self.set_node_label(node, format!("{{{node}|({nonterminal}, {terminal})}}"))
    }
    fn label_pre_dispatch(
        self,
        node: usize,
        nonterminal: Token,
        prod_id: usize,
        terminal: TokenOrEnd,
    ) -> Self {
        self.set_node_label(
            node,
            format!("{{{node}|({prod_id}, {nonterminal}, {terminal})}}"),
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
