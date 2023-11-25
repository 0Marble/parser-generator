use super::{grammar::Grammar, lgraph::Lgraph};
use crate::parser::{
    grammar::{TokenOrEnd, TokenOrEps},
    lgraph::{Bracket, Item},
};

impl Lgraph {
    pub fn ll1(grammar: &Grammar) -> Self {
        let first = grammar.first();
        let follow = grammar.follow(&first);
        for nt in grammar.non_terminals() {
            // checking ll1-ness.
            // for all pairs of productions A -> alpha, A -> beta:
            // 1. FIRST(alpha) intersect FIRST(beta) = 0
            // 2. if None in FIRST(alpha), than FIRST(beta) intersect FOLLOW(A) = 0
            //    and wise-wersa
            for (i, alpha_prod) in grammar.productions_for(nt.clone()).enumerate() {
                for beta_prod in grammar.productions_for(nt.clone()).skip(i + 1) {
                    let alpha_first = first.first_of_sent(alpha_prod.rhs()).unwrap();
                    let beta_first = first.first_of_sent(beta_prod.rhs()).unwrap();

                    assert!(
                        alpha_first
                            .iter()
                            .all(|a| !beta_first.contains(a) || a.is_eps()),
                        "Grammar not LL1: FIRST set conflict on {alpha_prod} and {beta_prod}"
                    );

                    let a_follow = follow.follow(nt.clone()).unwrap();
                    if alpha_first.contains(&TokenOrEps::Eps) {
                        assert!(
                            beta_first
                                .iter()
                                .all(|a| a.as_token().map_or(false, |a| !a_follow
                                    .contains(&TokenOrEnd::Token(a.clone())))),
                            "Grammar not LL1: FIRST and FOLLOW conflict on {alpha_prod} and {beta_prod}"
                        );
                    } else if beta_first.contains(&TokenOrEps::Eps) {
                        assert!(
                            alpha_first
                                .iter()
                                .all(|a| a.as_token().map_or(false, |a| !a_follow
                                    .contains(&TokenOrEnd::Token(a.clone())))),
                            "Grammar not LL1: FIRST and FOLLOW conflict on {alpha_prod} and {beta_prod}"
                        );
                    }
                }
            }
        }

        let mut ll1 = Self::default();
        // generate starts and ends of each section responsible for a production
        // for prod[i] section should look like
        // (4*i) -(i-> (4*i+1) -production-> (4*i+2) -)i-> (4*i+3)
        // and connections to it should be:
        // ... -(b, look_ahead[i]-> (4*i) -.....->(4*i+3) -)b -> j....
        // where j is some node, and b is a uinique bracket
        // all productions of the start symbol should be connected to
        // the start node, for example:
        // -> P -look_ahead[i]-> (4*i) -....->(4*i+3) -look_ahead=None-> Q
        // where P - start node, Q - the end node, ith production is of the
        // start symbol

        let mut node_count = 0;
        let terminal_count = grammar.terminals().count() + 1;
        let prod_count = grammar.productions().count();
        let terminal_bracket_offset = 0;
        let prod_bracket_offset = terminal_count;
        let mut bracket_count = prod_bracket_offset + prod_count;
        let mut look_aheads = vec![];

        // step 1: create starting and ending nodes for productions
        // and calculate look-aheads.
        for (prod_idx, prod) in grammar.productions().enumerate() {
            let mut look_ahead = vec![];
            let mut had_eps = false;
            let first_alpha = first.first_of_sent(prod.rhs()).unwrap();
            for t in first_alpha {
                if let TokenOrEps::Token(t) = t {
                    look_ahead.push(TokenOrEnd::Token(t.clone()));
                } else {
                    had_eps = true;
                }
            }
            if had_eps {
                for t in follow.follow(prod.lhs()).unwrap() {
                    if look_ahead.contains(t) {
                        continue;
                    }
                    look_ahead.push(t.clone());
                }
            }
            look_aheads.push(look_ahead.clone());

            ll1 = ll1.add_edge(
                4 * prod_idx,
                Item::new(
                    None,
                    None,
                    Some(Bracket::new(prod_idx + prod_bracket_offset, true)),
                ),
                4 * prod_idx + 1,
            );
            ll1 = ll1.add_edge(
                4 * prod_idx + 2,
                Item::new(
                    None,
                    None,
                    Some(Bracket::new(prod_idx + prod_bracket_offset, false)),
                ),
                4 * prod_idx + 3,
            );
            ll1 = ll1
                .set_node_label(
                    4 * prod_idx,
                    format!("{{{}|Start of {}[{}]}}", 4 * prod_idx, prod.lhs(), prod_idx),
                )
                .set_node_label(
                    4 * prod_idx + 3,
                    format!(
                        "{{{}|End of {}[{}]}}",
                        4 * prod_idx + 3,
                        prod.lhs(),
                        prod_idx
                    ),
                );
            node_count += 4;
        }

        // step 2: fill out the productions, connecting them when needed
        for (prod_idx, prod) in grammar.productions().enumerate() {
            if prod.rhs().is_empty() {
                ll1 = ll1.add_edge(
                    4 * prod_idx + 1,
                    Item::new(None, None, None),
                    4 * prod_idx + 2,
                );
                continue;
            }
            let mut source = 4 * prod_idx + 1;

            for (i, t) in prod.rhs().iter().enumerate() {
                let target = if i + 1 == prod.rhs().len() {
                    4 * prod_idx + 2
                } else {
                    node_count += 1;
                    node_count - 1
                };
                if grammar.is_terminal(t.clone()) {
                    let sym_id = grammar.terminal_index(t.clone()).unwrap();
                    let item = Item::new(
                        Some(TokenOrEnd::Token(t.clone())),
                        None,
                        Some(Bracket::new(sym_id + terminal_bracket_offset, true)),
                    );
                    ll1 = ll1.add_edge(source, item, node_count).add_edge(
                        node_count,
                        Item::new(
                            None,
                            None,
                            Some(Bracket::new(sym_id + terminal_bracket_offset, false)),
                        ),
                        target,
                    );
                    node_count += 1;
                } else {
                    for (prod2_idx, prod2) in grammar.productions().enumerate() {
                        if &prod2.lhs() != t {
                            continue;
                        }

                        let a_item = Item::new(
                            None,
                            Some(look_aheads[prod2_idx].clone()),
                            Some(Bracket::new(bracket_count, true)), // we will return to target
                        );

                        let first_beta = first.first_of_sent(&prod.rhs()[i + 1..]).unwrap();
                        let mut following = vec![];
                        let mut had_eps = false;
                        for t in first_beta {
                            if let Some(t) = t.as_token() {
                                following.push(TokenOrEnd::Token(t.clone()));
                            } else {
                                had_eps = true;
                            }
                        }
                        if had_eps {
                            let a_follow = follow.follow(prod.lhs()).unwrap();
                            for t in a_follow {
                                if !following.contains(t) {
                                    following.push(t.clone());
                                }
                            }
                        }
                        let b_item = Item::new(
                            None,
                            Some(following),
                            Some(Bracket::new(bracket_count, false)),
                        );
                        ll1 = ll1.add_edge(source, a_item, 4 * prod2_idx);
                        ll1 = ll1.add_edge(4 * prod2_idx + 3, b_item, target);
                        bracket_count += 1;
                    }
                }
                source = target;
            }
        }

        // step 3: connect productions of the start symbol to the start and end
        let start_node = node_count;
        let end_node = node_count + 1;
        node_count += 2;
        ll1 = ll1.add_start_node(start_node);
        ll1 = ll1.add_end_node(end_node);
        let start_symbol = grammar.start();
        for (i, prod) in grammar.productions().enumerate() {
            if prod.lhs() != start_symbol {
                continue;
            }
            let start_item = Item::new(
                None,
                Some(look_aheads[i].clone()),
                Some(Bracket::new(bracket_count, true)),
            );
            let end_item = Item::new(
                Some(TokenOrEnd::End),
                None,
                Some(Bracket::new(bracket_count, false)),
            );
            ll1 = ll1.add_edge(start_node, start_item, 4 * i);
            ll1 = ll1
                .add_edge(4 * i + 3, end_item, node_count)
                .add_edge(
                    node_count,
                    Item::new(
                        None,
                        None,
                        Some(Bracket::new(
                            terminal_count - 1 + terminal_bracket_offset,
                            true,
                        )),
                    ),
                    node_count + 1,
                )
                .add_edge(
                    node_count + 1,
                    Item::new(
                        None,
                        None,
                        Some(Bracket::new(
                            terminal_count - 1 + terminal_bracket_offset,
                            false,
                        )),
                    ),
                    end_node,
                );
            node_count += 2;
        }
        ll1
    }
}
