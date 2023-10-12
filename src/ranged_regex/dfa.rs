use std::ops::RangeBounds;

use super::charset::CharSet;

struct Nfa {
    edges: Vec<(usize, Option<CharSet>, usize)>,
    start: Vec<usize>,
    end: Vec<usize>,
}

impl Nfa {
    fn to_dfa(&self) -> (Dfa, Vec<(usize, Vec<usize>)>) {
        let mut translation: Vec<_> = self
            .start
            .iter()
            .enumerate()
            .map(|(i, n)| (i, vec![*n]))
            .collect();
        let mut next_nodes: Vec<_> = translation.iter().map(|(i, _)| *i).collect();
        let mut edges = vec![];

        while let Some(node) = next_nodes.pop() {
            let (_, cur_set) = &translation[node];
            let mut reachable_by_token: Vec<(CharSet, Vec<usize>)> = vec![];
            let mut inputs = cur_set.clone();
            let mut i = 0;

            // first find all empty transitions
            while i < inputs.len() {
                let n = inputs[i];
                for (from, tok, to) in &self.edges {
                    if *from == n && tok.is_none() {
                        inputs.push(*to);
                    }
                }
                i += 1;
            }

            for (from, tok, to) in &self.edges {
                if !inputs.contains(from) {
                    continue;
                }

                if let Some(tok) = tok {
                    for (t, s) in &mut reachable_by_token {
                        if t == tok {
                            s.push(*to);
                        }
                    }
                }
            }

            for (atok, aq) in &reachable_by_token {
                for (btok, bq) in &reachable_by_token {
                    let c = atok.intersection(btok);
                    let notc = c.complement();
                    let a = atok.intersection(&notc);
                    let b = btok.intersection(&notc);
                }
            }
        }

        (todo!(), translation)
    }
}

pub struct Dfa {
    edges: Vec<(usize, CharSet, usize)>,
    start: usize,
    end: Vec<usize>,
}
