use std::collections::HashSet;

use crate::{
    parser::grammar::{Grammar, ParseTree, Production},
    Token,
};

use super::Rng;

impl Grammar {
    pub fn fuzz(&self, rng: &mut dyn Rng, stop: usize) -> ParseTree {
        'OUTER: loop {
            let mut tree = ParseTree::new(self.start());
            let mut cur_node = 0;
            let mut nt = self.start();

            for _ in 0..stop {
                let prod_count = self.productions_for(nt.clone()).count();
                let pick = rng.gen() % prod_count;

                let (prod_idx, prod) = self
                    .productions()
                    .enumerate()
                    .filter(|(_, p)| p.lhs() == nt)
                    .nth(pick)
                    .unwrap();
                tree.replace_with_production(cur_node, prod, prod_idx);

                let Some((next_node, next_nt)) = tree
                    .nodes()
                    .skip(cur_node)
                    .find(|(_, x)| x.is_leaf() && !self.is_terminal(x.as_leaf().unwrap().clone()))
                else {
                    return tree;
                };

                cur_node = next_node;
                nt = next_nt.as_leaf().unwrap().clone();
            }

            loop {
                let mut min = NumOrInf::Inf;
                let mut min_idx = 0;
                let mut min_prod = None;
                for (prod_idx, prod) in self
                    .productions()
                    .enumerate()
                    .filter(|(_, p)| p.lhs() == nt)
                {
                    let mut visited = HashSet::new();
                    match (self.prod_cost(prod, &mut visited), &mut min) {
                        (NumOrInf::Num(a), NumOrInf::Num(b)) => {
                            if a < *b {
                                *b = a;
                                min_idx = prod_idx;
                                min_prod = Some(prod);
                            }
                        }
                        (NumOrInf::Num(a), NumOrInf::Inf) => {
                            min = NumOrInf::Num(a);
                            min_idx = prod_idx;
                            min_prod = Some(prod);
                        }
                        _ => (),
                    }
                }

                if min.is_inf() {
                    continue 'OUTER;
                }

                tree.replace_with_production(cur_node, min_prod.unwrap(), min_idx);

                let Some((next_node, next_nt)) = tree
                    .nodes()
                    .skip(cur_node)
                    .find(|(_, x)| x.is_leaf() && !self.is_terminal(x.as_leaf().unwrap().clone()))
                else {
                    return tree;
                };

                cur_node = next_node;
                nt = next_nt.as_leaf().unwrap().clone();
            }
        }
    }

    fn cost(&self, sym: Token, visited: &mut HashSet<Token>) -> NumOrInf {
        if self.is_terminal(sym.clone()) {
            return NumOrInf::Num(1);
        }
        if !visited.contains(&sym) {
            return NumOrInf::Inf;
        }

        let mut max = NumOrInf::Num(0);
        for prod in self.productions_for(sym) {
            match (self.prod_cost(prod, visited), &mut max) {
                (NumOrInf::Num(i), NumOrInf::Num(m)) => *m = (*m).max(i),
                (NumOrInf::Inf, _) => max = NumOrInf::Inf,
                _ => (),
            }

            if max.is_inf() {
                break;
            }
        }

        max
    }

    fn prod_cost(&self, prod: &Production, visited: &mut HashSet<Token>) -> NumOrInf {
        let mut total = NumOrInf::Num(0);
        for t in prod.rhs() {
            match (self.cost(t.clone(), visited), &mut total) {
                (NumOrInf::Num(i), NumOrInf::Num(t)) => *t += i,
                (NumOrInf::Inf, _) => total = NumOrInf::Inf,
                _ => (),
            }
        }
        total
    }
}

enum NumOrInf {
    Num(usize),
    Inf,
}

impl NumOrInf {
    /// Returns `true` if the num or inf is [`Inf`].
    ///
    /// [`Inf`]: NumOrInf::Inf
    #[must_use]
    fn is_inf(&self) -> bool {
        matches!(self, Self::Inf)
    }
}
