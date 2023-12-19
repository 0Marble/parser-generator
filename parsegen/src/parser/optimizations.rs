use super::lgraph::Lgraph;
use std::io::Write;

impl Lgraph {
    pub fn optimize(mut self) -> Self {
        let mut last_chance = true;
        loop {
            loop {
                println!("=========================");
                let (next, step0) = self.deadends();
                let (next, step1) = next.back_merge();

                self = next;
                if step0 || step1 {
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
                    println!("({},{}) -> {}", q1, q2, next_node);
                    next_node += 1;
                    continue 'OUTER;
                }
            }
        }

        if pairs.is_empty() {
            return (self, false);
        }

        for (from, item, to) in self.edges() {
            let from = if let Some((_, _, merge)) =
                pairs.iter().find(|(a, b, _)| *a == from || *b == from)
            {
                *merge
            } else {
                from
            };
            let to =
                if let Some((_, _, merge)) = pairs.iter().find(|(a, b, _)| *a == to || *b == to) {
                    *merge
                } else {
                    to
                };
            if res.edges_from(from).any(|(_, it, t)| it == item && t == to) {
                continue;
            }
            res = res.add_edge(from, item, to);
        }

        for node in self.start_nodes() {
            let node = if let Some((_, _, merge)) =
                pairs.iter().find(|(a, b, _)| *a == node || *b == node)
            {
                *merge
            } else {
                node
            };
            if res.start_nodes().any(|n| n == node) {
                continue;
            }
            res = res.add_start_node(node);
        }
        for node in self.end_nodes() {
            let node = if let Some((_, _, merge)) =
                pairs.iter().find(|(a, b, _)| *a == node || *b == node)
            {
                *merge
            } else {
                node
            };
            if res.end_nodes().any(|n| n == node) {
                continue;
            }
            res = res.add_end_node(node);
        }

        (res, false)
    }
}
