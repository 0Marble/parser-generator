use crate::parser::grammar::{Grammar, ParseTree};

use super::Rng;

impl Grammar {
    pub fn fuzz(&self, rng: &mut dyn Rng, max_derivations: usize) -> ParseTree {
        loop {
            let mut tree = ParseTree::new(self.start());
            for _ in 0..max_derivations {
                let Some((node_idx, nt)) = tree
                    .nodes()
                    .find(|(_, n)| n.is_leaf() && !self.is_terminal(n.as_leaf().unwrap().clone()))
                else {
                    break;
                };
                let nt = nt.as_leaf().cloned().unwrap();

                let prod_count = self.productions_for(nt.clone()).count();
                let mut pick = rng.gen() % prod_count;

                for (prod_idx, prod) in self.productions().enumerate() {
                    if prod.lhs() == nt {
                        if pick == 0 {
                            tree.replace_with_production(node_idx, prod, prod_idx);
                            break;
                        }
                        pick -= 1;
                    }
                }
            }
            if tree.leafs().all(|t| self.is_terminal(t)) {
                return tree;
            }
        }
    }
}
