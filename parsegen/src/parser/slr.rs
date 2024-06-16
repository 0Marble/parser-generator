use std::{collections::HashMap, fmt::Display};

use crate::{parser::grammar::Production, Token};

use super::{
    grammar::{self, Grammar, Node, TokenOrEnd},
    lgraph::{Bracket, Item, Lgraph, Lookahead},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LR0Item {
    prod_idx: usize,
    point: usize,
}

impl LR0Item {
    fn new(prod_idx: usize, point: usize) -> Self {
        Self { prod_idx, point }
    }
}

struct LR0 {
    states: Vec<Vec<LR0Item>>,
    goto: Vec<Vec<(Token, usize)>>,
    start: Token,
    end: Token,
    start_state: usize,
    end_state: usize,
    grammar: Grammar,
}

impl LR0 {
    pub fn new(grammar: &Grammar) -> Self {
        let start = grammar.get_unique_token("Start").unwrap();
        let end = grammar.get_unique_token("End").unwrap();
        let grammar = Grammar::new(
            grammar
                .productions()
                .cloned()
                .chain(std::iter::once(Production::new(
                    start.clone(),
                    vec![grammar.start(), end.clone()],
                ))),
            start.clone(),
        );

        let (start_prod, _) = grammar
            .productions()
            .enumerate()
            .find(|(_, p)| p.lhs() == start.clone())
            .unwrap();
        let first_state = Self::closure(&grammar, &[LR0Item::new(start_prod, 0)]);
        let mut states = vec![first_state];
        let mut stack = vec![0];
        let mut goto = vec![vec![]];
        let mut end_state = 0;

        while let Some(state_idx) = stack.pop() {
            let mut edges = vec![];

            let mut goto_symbols = vec![];
            for item in &states[state_idx] {
                let prod = grammar.productions().nth(item.prod_idx).unwrap();
                let next_sym = prod.rhs().get(item.point);
                if let Some(sym) = next_sym {
                    goto_symbols.push(sym.clone());
                }
            }
            goto_symbols.sort();
            goto_symbols.dedup();

            for sym in goto_symbols {
                let next = Self::goto(&grammar, &states[state_idx], sym.clone());
                let idx = if let Some((idx, _)) =
                    states.iter().enumerate().find(|(_, state)| *state == &next)
                {
                    idx
                } else {
                    let idx = states.len();
                    states.push(next);
                    stack.push(idx);
                    goto.push(vec![]);
                    idx
                };
                if sym == end {
                    end_state = idx;
                }
                edges.push((sym, idx));
            }

            goto[state_idx] = edges;
        }

        Self {
            states,
            goto,
            start,
            end,
            grammar,
            start_state: 0,
            end_state,
        }
    }
    fn closure(grammar: &Grammar, set: &[LR0Item]) -> Vec<LR0Item> {
        let mut items = set.to_vec();
        let mut from = 0;
        let mut new_items = vec![];

        loop {
            for item in &items[from..] {
                let prod = grammar.productions().nth(item.prod_idx).unwrap();
                if item.point >= prod.rhs().len() {
                    continue;
                }
                let sym = prod.rhs()[item.point].clone();
                if grammar.is_terminal(sym.clone()) {
                    continue;
                }

                for (i, _) in grammar
                    .productions()
                    .enumerate()
                    .filter(|(_, p)| p.lhs() == sym)
                {
                    let item = LR0Item::new(i, 0);
                    if items.contains(&item) || new_items.contains(&item) {
                        continue;
                    }
                    new_items.push(item)
                }
            }

            if new_items.is_empty() {
                break;
            }
            from = items.len();
            items.append(&mut new_items);
        }
        items.sort();
        items.dedup();

        items
    }

    fn goto(grammar: &Grammar, items: &[LR0Item], sym: Token) -> Vec<LR0Item> {
        let mut kernel = vec![];
        for item in items {
            let prod = grammar.productions().nth(item.prod_idx).unwrap();
            if item.point >= prod.rhs().len() {
                continue;
            }
            let sym_before = prod.rhs().get(item.point).unwrap();
            if sym_before != &sym {
                continue;
            }

            kernel.push(LR0Item::new(item.prod_idx, item.point + 1));
        }

        Self::closure(grammar, &kernel)
    }

    pub fn edges(&self) -> impl Iterator<Item = (usize, Token, usize)> + '_ {
        self.nodes().flat_map(move |from| self.edges_from(from))
    }

    pub fn edges_from(&self, from: usize) -> impl Iterator<Item = (usize, Token, usize)> + '_ {
        self.goto[from]
            .iter()
            .map(move |(tok, to)| (from, tok.clone(), *to))
    }

    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.states.iter().enumerate().map(|(i, _)| i)
    }

    pub fn items_in_state(&self, state: usize) -> impl Iterator<Item = LR0Item> + '_ {
        self.states[state].iter().cloned()
    }
}

impl Display for LR0 {
    fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(w, "digraph {{")?;
        for node_idx in self.nodes() {
            write!(w, "{node_idx} [shape=record, label=\"{{{node_idx}|")?;
            for item in self.items_in_state(node_idx) {
                let prod = self.grammar.productions().nth(item.prod_idx).unwrap();
                write!(w, "{} \\-\\> ", prod.lhs())?;
                for t in &prod.rhs()[..item.point] {
                    write!(w, "{t} ")?;
                }
                write!(w, ".")?;
                if item.point == prod.rhs().len() {
                    write!(w, "\\n")?;
                    continue;
                }
                for t in &prod.rhs()[item.point..] {
                    write!(w, "{t} ")?;
                }
                write!(w, "\\n")?;
            }
            writeln!(w, "}}\"]")?;
        }

        for (from, tok, to) in self.edges() {
            writeln!(w, "{from} -> {to} [label=\"{tok}\"]")?;
        }
        writeln!(w, "}}")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LR0Node {
    LR0(usize),
    Nonterminal(Token),
    Reduce(Token, usize, usize),
    ReduceCheckBracket1(Token, usize, usize),
    ReduceCheckBracket2(Token, usize, usize),
    Start,
    End,
}

impl Lgraph {
    pub fn slr(grammar: &Grammar) -> Self {
        let lr0 = LR0::new(grammar);
        std::fs::write("tests/lr0.dot", lr0.to_string()).unwrap();

        let grammar = &lr0.grammar;
        let first = grammar.first();
        let follow = grammar.follow(&first);
        let mut node_ids = HashMap::new();
        let start_node = Self::get_idx(LR0Node::Start, &mut node_ids);
        let end_node = Self::get_idx(LR0Node::End, &mut node_ids);
        let lr0_start_node = Self::get_idx(LR0Node::LR0(lr0.start_state), &mut node_ids);
        let lr0_end_node = Self::get_idx(LR0Node::LR0(lr0.end_state), &mut node_ids);
        let mut lg = Lgraph::new(start_node);

        for state in lr0.nodes() {
            let node = Self::get_idx(LR0Node::LR0(state), &mut node_ids);
            for (_, tok, next_state) in lr0.edges_from(state) {
                let next_node = Self::get_idx(LR0Node::LR0(next_state), &mut node_ids);
                if !grammar.is_terminal(tok.clone()) {
                    continue;
                }

                let item = if tok == lr0.end {
                    Item::default()
                        .with_token(Some(TokenOrEnd::End))
                        .with_bracket(Some(Bracket::Closed(node)))
                } else {
                    Item::default()
                        .with_token(Some(TokenOrEnd::Token(tok.clone())))
                        .with_output(Some(Node::Leaf(tok.clone())))
                        .with_bracket(Some(Bracket::Open(next_node)))
                };
                lg = lg.add_edge(node, item, next_node);
            }

            for item in lr0.items_in_state(state) {
                let prod = grammar.productions().nth(item.prod_idx).unwrap();
                if item.point != prod.rhs().len() {
                    continue;
                }

                let rule_end_node =
                    Self::get_idx(LR0Node::Reduce(prod.lhs(), item.prod_idx, 0), &mut node_ids);

                for tok in follow.follow(prod.lhs()).unwrap() {
                    let tok = match tok {
                        TokenOrEnd::Token(t) if t == &lr0.end => TokenOrEnd::End,
                        TokenOrEnd::Token(t) => TokenOrEnd::Token(t.clone()),
                        TokenOrEnd::End => continue,
                    };
                    lg = lg.add_edge(
                        node,
                        Item::default()
                            .with_look_ahead(Some(Lookahead::new(vec![tok])))
                            .with_output(Some(Node::RuleEnd(item.prod_idx, prod.lhs()))),
                        rule_end_node,
                    );
                }
            }
        }

        for (prod_idx, prod) in grammar.productions().enumerate() {
            let mut prev_node =
                Self::get_idx(LR0Node::Reduce(prod.lhs(), prod_idx, 0), &mut node_ids);

            for (i, _) in prod.rhs().iter().enumerate() {
                let next_node =
                    Self::get_idx(LR0Node::Reduce(prod.lhs(), prod_idx, i + 1), &mut node_ids);
                lg = lg.add_edge(
                    prev_node,
                    Item::default().with_bracket(Some(Bracket::Wildcard)),
                    next_node,
                );
                prev_node = next_node;
            }

            let nt_node = Self::get_idx(LR0Node::Nonterminal(prod.lhs()), &mut node_ids);
            lg = lg.add_edge(prev_node, Item::default(), nt_node);
        }

        for (before_reduce, tok, after_reduce) in lr0.edges() {
            if grammar.is_terminal(tok.clone()) {
                continue;
            }
            let nt_node = Self::get_idx(LR0Node::Nonterminal(tok.clone()), &mut node_ids);

            let before_reduce_node = Self::get_idx(LR0Node::LR0(before_reduce), &mut node_ids);
            let after_reduce_node = Self::get_idx(LR0Node::LR0(after_reduce), &mut node_ids);
            let br1_node = Self::get_idx(
                LR0Node::ReduceCheckBracket1(tok.clone(), before_reduce_node, after_reduce_node),
                &mut node_ids,
            );
            let br2_node = Self::get_idx(
                LR0Node::ReduceCheckBracket2(tok.clone(), before_reduce_node, after_reduce_node),
                &mut node_ids,
            );

            lg = lg
                .add_edge(
                    nt_node,
                    Item::default().with_bracket(Some(Bracket::Closed(before_reduce_node))),
                    br1_node,
                )
                .add_edge(
                    br1_node,
                    Item::default().with_bracket(Some(Bracket::Open(before_reduce_node))),
                    br2_node,
                )
                .add_edge(
                    br2_node,
                    Item::default().with_bracket(Some(Bracket::Open(after_reduce_node))),
                    after_reduce_node,
                );
        }

        lg.add_edge(
            start_node,
            Item::default().with_bracket(Some(Bracket::Open(lr0_start_node))),
            lr0_start_node,
        )
        .add_edge(
            lr0_end_node,
            Item::default().with_bracket(Some(Bracket::Closed(lr0_start_node))),
            end_node,
        )
        .add_end_node(end_node)
    }
}

