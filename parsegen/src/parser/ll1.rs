use crate::parser::lgraph::{Bracket, Item};

use super::{grammar::Grammar, lgraph::Lgraph};

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
                    let alpha_first = match alpha_prod.rhs().first() {
                        None => &[None],
                        Some(t) => first.first(t.clone()).unwrap(),
                    };
                    let beta_first = match beta_prod.rhs().first() {
                        None => &[None],
                        Some(t) => first.first(t.clone()).unwrap(),
                    };

                    assert!(
                        alpha_first.iter().all(|a| !beta_first.contains(a)),
                        "Grammar not LL1: FIRST set conflict on {alpha_prod} and {beta_prod}"
                    );

                    let a_follow = follow.follow(nt.clone()).unwrap();
                    if alpha_first.contains(&None) {
                        assert!(
                        beta_first.iter().all(|a| !a_follow.contains(a)),
                        "Grammar not LL1: FIRST and FOLLOW conflict on {alpha_prod} and {beta_prod}"
                    );
                    } else if beta_first.contains(&None) {
                        assert!(
                            alpha_first.iter().all(|a| !a_follow.contains(a)),
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
        let mut bracket_count = grammar.non_terminals().count();
        let mut look_aheads = vec![];

        // step 1: create starting and ending nodes for productions
        // and calculate look-aheads.
        for (i, prod) in grammar.productions().enumerate() {
            let mut look_ahead = vec![];
            let mut had_eps = false;
            let first_alpha = first.first_of_sent(prod.rhs()).unwrap();
            for t in first_alpha {
                if t.is_some() {
                    look_ahead.push(t.clone());
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
                4 * i,
                Item::new(None, Some(look_ahead.clone()), Some(Bracket::new(i, true))),
                4 * i + 1,
            );
            ll1 = ll1.add_edge(
                4 * i + 2,
                Item::new(None, None, Some(Bracket::new(i, false))),
                4 * i + 3,
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
                    let item = Item::new(Some(t.clone()), None, None);
                    ll1 = ll1.add_edge(source, item, target);
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
                        let b_item =
                            Item::new(None, None, Some(Bracket::new(bracket_count, false)));
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
        ll1 = ll1.add_start_node(start_node);
        ll1 = ll1.add_end_node(end_node);
        let start_symbol = grammar.start();
        for (i, prod) in grammar.productions().enumerate() {
            if prod.lhs() != start_symbol {
                continue;
            }
            let start_item = Item::new(None, Some(look_aheads[i].clone()), None);
            let end_item = Item::new(None, Some(vec![None]), None);
            ll1 = ll1.add_edge(start_node, start_item, 4 * i);
            ll1 = ll1.add_edge(4 * i + 3, end_item, end_node);
        }
        ll1
    }
}
