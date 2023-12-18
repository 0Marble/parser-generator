use super::lgraph::Lgraph;

impl Lgraph {
    pub fn optimize(mut self) -> Self {
        loop {
            let (next, step0) = self.deadends();
            let (next, step1) = next.back_merge();

            self = next;
            if step0 || step1 {
                continue;
            }

            break;
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

        (res, changed)
    }

    // if two nodes q1 and q2 have only one target p, we can merge these nodes into q3
    fn back_merge(self) -> (Self, bool) {
        let mut res = Lgraph::default();
        let mut changed = false;
        let mut next_node = self.nodes().max().unwrap_or_default();
        let mut merged_nodes = vec![];

        for p in self.nodes() {
            let mut merged_qs = vec![];
            'QS: for (q, x, _) in self.edges_to(p) {
                for (_, _, to) in self.edges_from(q) {
                    if to != p {
                        continue 'QS;
                    }
                }
                merged_qs.push((q, x));
            }

            if merged_qs.len() > 1 {
                changed = true;
            } else {
                continue;
            }

            let new_q = next_node;
            res = res.add_node(new_q).add_node(p);
            next_node += 1;
            for (q, x) in merged_qs {
                merged_nodes.push((q, new_q));
                if res.edges_from(new_q).any(|(_, y, r)| y == x && p == r) {
                    continue;
                }
                res = res.add_edge(new_q, x, p);
            }
        }

        for (from, item, to) in self.edges() {
            if merged_nodes.iter().any(|(q, _)| *q == from) {
                continue;
            }

            if let Some((_, new_q)) = merged_nodes.iter().find(|(q, _)| *q == to).cloned() {
                if res.edges_to(new_q).any(|(f, x, _)| f == from && item == x) {
                    continue;
                }
                res = res.add_edge(from, item, new_q);
                continue;
            }

            res = res.add_edge(from, item, to);
        }

        for start in self.start_nodes() {
            if let Some((_, new_q)) = merged_nodes.iter().find(|(q, _)| *q == start).cloned() {
                if res.start_nodes().any(|s| s == new_q) {
                    continue;
                }
                res = res.add_start_node(new_q);
            }
        }

        for end in self.end_nodes() {
            if let Some((_, new_q)) = merged_nodes.iter().find(|(q, _)| *q == end).cloned() {
                if res.end_nodes().any(|s| s == new_q) {
                    continue;
                }
                res = res.add_end_node(new_q);
            }
        }

        (res, changed)
    }
}
