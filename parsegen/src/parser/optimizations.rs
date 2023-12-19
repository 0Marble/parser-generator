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
                let (next, step3) = next.merge_diamonds();

                self = next;
                count += 1;
                assert!(
                    old_node_count >= self.nodes().count()
                        && old_edge_count >= self.edges().count()
                );
                if step0 || step1 || step2 || step3 {
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

    // merge two nodes if they have an empty edge between them, and all the other output edges
    // respect determinism
    fn empty_edges(self) -> (Self, bool) {
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

    // if there are edges (p, )ji, qj), (qj, (ji, q), we can replace them with an edge (p, _, q)
    fn merge_diamonds(self) -> (Self, bool) {
        let mut res = Lgraph::default();
        let mut pqs = vec![];

        'OUTER: for p in self.nodes() {
            let mut q = None;

            for (_, i1, q1) in self.edges_from(p) {
                if i1.tok().is_some()
                    || i1.look_ahead().is_some()
                    || i1.output().is_some()
                    || self.is_end_node(q1)
                    || self.is_start_node(q1)
                {
                    continue 'OUTER;
                }

                let b1 = if let Some(b) = i1.bracket() {
                    if b.is_open() {
                        continue 'OUTER;
                    }
                    b
                } else {
                    continue 'OUTER;
                };

                if self.edges_to(q1).any(|(from, _, _)| from != p) {
                    continue 'OUTER;
                }

                for (_, i2, to) in self.edges_from(q1) {
                    if i2.tok().is_some() || i2.look_ahead().is_some() || i2.output().is_some() {
                        continue 'OUTER;
                    }
                    let b2 = if let Some(b) = i2.bracket() {
                        if !b.is_open() {
                            continue 'OUTER;
                        }
                        b
                    } else {
                        continue 'OUTER;
                    };

                    if b2.index() != b1.index() && !b1.is_wildcard() {
                        continue 'OUTER;
                    }

                    if q.is_none() {
                        q = Some(to);
                        continue;
                    }

                    if q != Some(to) {
                        continue 'OUTER;
                    }
                }
            }
            if let Some(q) = q {
                pqs.push((p, q));
                res = res.add_edge(p, Item::default(), q);
            }
        }

        if pqs.is_empty() {
            return (self, false);
        }

        for (from, item, to) in self.edges() {
            if pqs.iter().any(|(p, _)| *p == from) {
                continue;
            }
            res = res.add_edge(from, item, to);
        }
        for node in self.start_nodes() {
            res = res.add_start_node(node);
        }
        for node in self.end_nodes() {
            res = res.add_end_node(node);
        }

        (res, true)
    }
}
