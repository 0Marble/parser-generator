use std::{collections::HashMap, hash::Hash};

use super::{
    grammar::Grammar,
    lgraph::{Lgraph, Lookahead},
};
use crate::{
    parser::{
        grammar::{Node, TokenOrEnd, TokenOrEps},
        lgraph::{Bracket, Item},
    },
    Token,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum LL1Node {
    TermStart(Token),
    TermEnd(Token),
    Rule(Token, usize, usize),
    Start,
    End,
}

impl Lgraph {
    pub fn ll1(grammar: &Grammar) -> Self {
        let first = grammar.first();
        let follow = grammar.follow(&first);
        let mut node_inds = HashMap::new();
        let mut bracket_inds = HashMap::new();

        let start = Self::get_idx(LL1Node::Start, &mut node_inds);
        let mut lg = Lgraph::new(start);

        for (i, prod) in grammar.productions().enumerate() {
            let start = Self::get_idx(LL1Node::TermStart(prod.lhs()), &mut node_inds);
            let end = Self::get_idx(LL1Node::TermEnd(prod.lhs()), &mut node_inds);

            let mut has_eps = false;
            let mut lk_toks = vec![];
            for tok in first.first_of_sent(prod.rhs()).unwrap() {
                match tok {
                    TokenOrEps::Token(t) => lk_toks.push(TokenOrEnd::Token(t)),
                    TokenOrEps::Eps => has_eps = true,
                }
            }
            if has_eps {
                lk_toks.extend(follow.follow(prod.lhs()).unwrap().iter().cloned());
            }

            let mut prev_node = Self::get_idx(LL1Node::Rule(prod.lhs(), i, 0), &mut node_inds);
            for tok in lk_toks {
                lg = lg.add_edge(
                    start,
                    Item::default()
                        .with_look_ahead(Some(Lookahead::new(vec![tok])))
                        .with_output(Some(Node::RuleStart(i, prod.lhs()))),
                    prev_node,
                );
            }

            for (j, tok) in prod.rhs().iter().enumerate() {
                let next_node = Self::get_idx(LL1Node::Rule(prod.lhs(), i, j + 1), &mut node_inds);
                if grammar.is_terminal(tok.clone()) {
                    lg = lg.add_edge(
                        prev_node,
                        Item::default()
                            .with_token(Some(tok.clone().into()))
                            .with_output(Some(Node::Leaf(tok.clone().into()))),
                        next_node,
                    );
                } else {
                    let term_start = Self::get_idx(LL1Node::TermStart(tok.clone()), &mut node_inds);
                    let term_end = Self::get_idx(LL1Node::TermEnd(tok.clone()), &mut node_inds);
                    let bracket = Self::get_idx((prod.lhs(), i, j), &mut bracket_inds);
                    lg = lg
                        .add_edge(
                            prev_node,
                            Item::default().with_bracket(Some(Bracket::Open(bracket))),
                            term_start,
                        )
                        .add_edge(
                            term_end,
                            Item::default().with_bracket(Some(Bracket::Closed(bracket))),
                            next_node,
                        );
                }

                prev_node = next_node;
            }

            lg = lg.add_edge(
                prev_node,
                Item::default().with_output(Some(Node::RuleEnd(i, prod.lhs()))),
                end,
            );
        }

        let s_end = Self::get_idx(LL1Node::TermEnd(grammar.start()), &mut node_inds);
        let s_start = Self::get_idx(LL1Node::TermStart(grammar.start()), &mut node_inds);
        let end = Self::get_idx(LL1Node::End, &mut node_inds);
        let bracket = bracket_inds.len();

        lg.add_edge(
            s_end,
            Item::default()
                .with_token(Some(TokenOrEnd::End))
                .with_bracket(Some(Bracket::Closed(bracket))),
            end,
        )
        .add_end_node(end)
        .add_edge(
            start,
            Item::default().with_bracket(Some(Bracket::Open(bracket))),
            s_start,
        )
    }

    pub(crate) fn get_idx<T>(node: T, indices: &mut HashMap<T, usize>) -> usize
    where
        T: PartialEq + Hash + Eq,
    {
        let next = indices.len();
        *indices.entry(node).or_insert(next)
    }
}
