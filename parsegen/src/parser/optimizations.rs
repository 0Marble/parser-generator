use super::lgraph::{Item, Lgraph};
use std::io::Write;

impl Lgraph {
    pub fn optimize(mut self) -> Self {
        let mut last_chance = true;
        let mut count = 0;
        loop {
            loop {
                let old_node_count = self.nodes().count();
                let old_edge_count = self.edges().count();
                println!("=====================");
                let (next, step0) = self.deadends();
                let (next, step1) = next.back_merge();
                let (next, step2) = next.empty_edges();

                if count == 1 {
                    return next;
                }
                self = next;
                count += 1;
                assert!(
                    old_node_count >= self.nodes().count()
                        && old_edge_count >= self.edges().count()
                );
                if step0 || step1 || step2 {
                    last_chance = true;
                    continue;
                }

                break;
            }
            if !last_chance {
                break;
            }
            last_chance = false;
        }
        self
    }

    // remove nodes with no inputs or no outputs (ignoring starts and ends)
    fn deadends(self) -> (Self, bool) {
        let mut res = Self::default();
        let mut changed = false;
        let mut no_inputs = vec![];
        let mut no_outputs = vec![];

        for node in self.nodes() {
            if self.edges_to(node).count() == 0 && self.start_nodes().all(|n| n != node) {
                no_inputs.push(node);
                changed = true;
            }
            if self.edges_from(node).count() == 0 && self.end_nodes().all(|n| n != node) {
                no_outputs.push(node);
                changed = true;
            }
        }
        if !changed {
            return (self, false);
        }

        for (from, item, to) in self.edges() {
            if no_inputs.contains(&from) || no_outputs.contains(&to) {
                continue;
            }
            res = res.add_edge(from, item, to);
        }
        for start in self.start_nodes() {
            res = res.add_start_node(start);
        }
        for end in self.end_nodes() {
            res = res.add_end_node(end);
        }

        (res, true)
    }

    // if all edges from nodes q1 .. qn are of the kind (qi, xij, p), where p is some node and the
    // sets of items { xij } are equal, than we can merge qi nodes.
    fn back_merge(self) -> (Self, bool) {
        let mut res = Lgraph::default();
        let mut next_node = self.nodes().max().unwrap_or_default() + 1;
        let mut pairs = vec![];

        for p in self.nodes() {
            'OUTER: for (i, (q1, _, _)) in self.edges_to(p).enumerate() {
                if self.edges_from(q1).any(|(_, _, n)| n != p)
                    || pairs.iter().any(|(n1, n2, _)| *n1 == q1 || *n2 == q1)
                {
                    continue;
                }

                for (q2, _, _) in self.edges_to(p).skip(i + 1) {
                    if self.edges_from(q2).any(|(_, item2, n)| {
                        n != p || self.edges_from(q1).all(|(_, item1, _)| item1 != item2)
                    }) || pairs.iter().any(|(n1, n2, _)| *n1 == q2 || *n2 == q2)
                    {
                        continue;
                    }

                    pairs.push((q1, q2, next_node));
                    next_node += 1;
                    continue 'OUTER;
                }
            }
        }

        if pairs.is_empty() {
            return (self, false);
        }

        for (from, item, to) in self.edges() {
            let from = Self::merged_name(from, &pairs);
            let to = Self::merged_name(to, &pairs);
            if res.edges_from(from).any(|(_, it, t)| it == item && t == to) {
                continue;
            }
            res = res.add_edge(from, item, to);
        }

        for node in self.start_nodes() {
            let node = Self::merged_name(node, &pairs);
            if res.start_nodes().any(|n| n == node) {
                continue;
            }
            res = res.add_start_node(node);
        }
        for node in self.end_nodes() {
            let node = Self::merged_name(node, &pairs);
            if res.end_nodes().any(|n| n == node) {
                continue;
            }
            res = res.add_end_node(node);
        }

        (res, false)
    }

    fn merged_name(node: usize, pairs: &[(usize, usize, usize)]) -> usize {
        if let Some((_, _, merge)) = pairs.iter().find(|(a, b, _)| *a == node || *b == node) {
            *merge
        } else {
            node
        }
    }

    pub fn empty_edges(self) -> (Self, bool) {
        let mut res = Lgraph::default();
        let mut next_node = self.nodes().max().unwrap_or_default() + 1;
        let mut pairs = vec![];
        'OUTER: for (from, item, to) in self.edges() {
            if !item.is_empty()
                || pairs
                    .iter()
                    .any(|(a, b, _)| *a == from || *a == to || *b == from || *b == to)
            {
                continue;
            }

            for (_, item1, _) in self.edges_from(from) {
                for (_, item2, _) in self.edges_from(to) {
                    if !item1.is_distinguishable(&item2) {
                        continue 'OUTER;
                    }
                }
            }
            pairs.push((from, to, next_node));
            next_node += 1;
        }
        if pairs.is_empty() {
            return (self, false);
        }

        for (from, item, to) in self.edges() {
            if item.is_empty() && pairs.iter().any(|(a, b, _)| *a == from && *b == to) {
                continue;
            }
            let from = Self::merged_name(from, &pairs);
            let to = Self::merged_name(to, &pairs);
            if res.edges_from(from).any(|(_, it, t)| it == item && t == to) {
                continue;
            }
            res = res.add_edge(from, item, to);
        }

        for node in self.start_nodes() {
            let node = Self::merged_name(node, &pairs);
            if res.start_nodes().any(|n| n == node) {
                continue;
            }
            res = res.add_start_node(node);
        }
        for node in self.end_nodes() {
            let node = Self::merged_name(node, &pairs);
            if res.end_nodes().any(|n| n == node) {
                continue;
            }
            res = res.add_end_node(node);
        }

        (res, false)
    }

    // if there is a section a -x-> b -y-> c, where there are no other edges coming from or into b, we can
    // try merge x and y. open-closed bracket pair can be removed, lookahead may be merged if it
    // does not influence determinism on a.
    // pub fn linear_merge(self) -> (Self, bool) {
    //     let mut res = Lgraph::default();
    //
    //     for b in self.nodes() {
    //         let mut edges_from = self.edges_from(b);
    //         let mut edges_to = self.edges_to(b);
    //
    //         let (a, x) = if let Some((a, x, _)) = edges_to.next() {
    //             (a, x)
    //         } else {
    //             continue;
    //         };
    //         let (c, y) = if let Some((_, y, c)) = edges_from.next() {
    //             (c, y)
    //         } else {
    //             continue;
    //         };
    //
    //         if edges_from.next().is_some() || edges_to.next().is_some() {
    //             continue;
    //         }
    //     }
    //
    //     (res, true)
    // }
}
