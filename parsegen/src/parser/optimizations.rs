use crate::TransitionScheme;
use std::io::Write;

use super::lgraph::{Bracket, Item, Lgraph};
use std::collections::{HashSet, VecDeque};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Optimization {
    Deadend,
    EmptyEdge,
    BackMerge,
    Diamonds,
    UselessBrackets,
    LinearBrackets,
    UselessLookahead,
}

impl Optimization {
    pub fn all() -> impl IntoIterator<Item = Optimization> {
        [
            Self::Deadend,
            Self::EmptyEdge,
            Self::BackMerge,
            Self::Diamonds,
            Self::UselessLookahead,
            Self::LinearBrackets,
        ]
    }
}

impl Lgraph {
    pub fn optimize(mut self, optimizations: impl IntoIterator<Item = Optimization>) -> Self {
        let mut last_chance = true;
        let mut count = 0;
        let opts: HashSet<_> = optimizations.into_iter().collect();
        use Optimization as O;
        loop {
            loop {
                let old_node_count = self.nodes().count();
                let old_edge_count = self.edges().count();

                let mut res = false;
                if opts.contains(&O::UselessLookahead) {
                    let (next, step) = self.useless_lookahead();
                    self = next;
                    // std::fs::write(
                    //     format!("tests/optimize_useless_lookahead_{count}.dot"),
                    //     self.to_string(),
                    // )
                    // .unwrap();
                    res |= step;
                }
                if opts.contains(&O::BackMerge) {
                    let (next, step) = self.back_merge();
                    self = next;
                    // std::fs::write(
                    //     format!("tests/optimize_backmerge_{count}.dot"),
                    //     self.to_string(),
                    // )
                    // .unwrap();
                    res |= step;
                }
                if opts.contains(&O::LinearBrackets) {
                    let (next, step) = self.linear_brackets();
                    self = next;
                    // std::fs::write(
                    //     format!("tests/optimize_backmerge_{count}.dot"),
                    //     self.to_string(),
                    // )
                    // .unwrap();
                    res |= step;
                }
                if opts.contains(&O::EmptyEdge) {
                    let (next, step) = self.empty_edges();
                    self = next;
                    // std::fs::write(
                    //     format!("tests/optimize_empty_edge_{count}.dot"),
                    //     self.to_string(),
                    // )
                    // .unwrap();
                    res |= step;
                }
                if opts.contains(&O::Diamonds) {
                    let (next, step) = self.merge_diamonds();
                    self = next;
                    // std::fs::write(
                    //     format!("tests/optimize_diamonds_{count}.dot"),
                    //     self.to_string(),
                    // )
                    // .unwrap();
                    res |= step;
                }
                if opts.contains(&O::UselessBrackets) {
                    let (next, step) = self.useless_brackets();
                    self = next;
                    // std::fs::write(
                    //     format!("tests/optimize_useless_brackets_{count}.dot"),
                    //     self.to_string(),
                    // )
                    // .unwrap();
                    res |= step;
                }
                if opts.contains(&O::Deadend) {
                    let (next, step) = self.deadends();
                    self = next;
                    // std::fs::write(
                    //     format!("tests/optimize_deadend_{count}.dot"),
                    //     self.to_string(),
                    // )
                    // .unwrap();
                    res |= step;
                }

                count += 1;
                assert!(
                    old_node_count >= self.nodes().count()
                        && old_edge_count >= self.edges().count()
                );
                if res {
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
        let mut res = Self::new(self.start());
        let mut changed = false;
        let mut no_inputs = vec![];
        let mut no_outputs = vec![];

        for node in self.nodes() {
            if self.edges_to(node).count() == 0 && self.start() != node {
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
        for end in self.end_nodes() {
            res = res.add_end_node(end);
        }

        (res, true)
    }

    // if all edges from nodes q1 .. qn are of the kind (qi, xij, p), where p is some node and the
    // sets of items { xij } are equal, than we can merge qi nodes.
    fn back_merge(self) -> (Self, bool) {
        let mut res = Self::new(0);
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

        res = res.set_start_node(Self::merged_name(self.start(), &pairs));
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
        let mut next_node = self.nodes().max().unwrap_or_default() + 1;
        let mut res = Self::new(next_node);
        next_node += 1;
        let mut pairs = vec![];
        'OUTER: for (from, item, to) in self.edges() {
            if !item.is_empty()
                || pairs
                    .iter()
                    .any(|(a, b, _)| *a == from || *a == to || *b == from || *b == to)
            {
                continue;
            }

            for (_, item1, q) in self.edges_from(from) {
                for (_, item2, _) in self.edges_from(to) {
                    if !item1.is_distinguishable(&item2) && !(q == to && item1 == item) {
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
            if res.has_node(from) && res.edges_from(from).any(|(_, it, t)| it == item && t == to) {
                continue;
            }
            res = res.add_edge(from, item, to);
        }

        res = res.set_start_node(Self::merged_name(self.start(), &pairs));
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
        let mut res = Self::new(self.start());
        let mut pqs = vec![];

        'OUTER: for p in self.nodes() {
            let mut q = None;

            for (_, i1, q1) in self.edges_from(p) {
                if i1.tok().is_some()
                    || i1.look_ahead().is_some()
                    || i1.output().is_some()
                    || self.is_end_node(q1)
                    || self.start() == q1
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

                    if b2.index() != b1.index() || b1.is_wildcard() {
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
        for node in self.end_nodes() {
            res = res.add_end_node(node);
        }

        (res, true)
    }

    // An edge (p, (i, q1) has a useless open bracket if for all paths p-(i->q1-a1->...->)i->qn, ai
    // do not have a bracket. Similarily for a closed bracket, but backwards.
    // We can produce a list l1 of pairs (e, c), where e is an edge with a useless open bracket, and c
    // is a list of corresponding edges with closing brackets (qn-1 -)i->qn). Similarily for
    // useless closed brackets, create a list l2.
    // An edge is useless if its open bracket (i is useless and all corresponding edges in c are
    // also useless
    // Basically we should construct a "uselessness" dependency graph. It splits into multiple
    // connected components, and if a component has no deadend nodes, all edges in it are useless.
    fn useless_brackets(self) -> (Self, bool) {
        let mut res = Self::new(self.start());

        let mut dep = TransitionScheme::new(0);
        let mut nodes = vec![];

        for (from, item, to) in self.edges() {
            if let Some(b) = item.bracket() {
                if let Some(closing) = self.is_useless_open(b, to) {
                    assert!(!closing.is_empty());
                    let open_node = Self::get_or_add_node(&(from, item, to), &mut nodes);
                    for (a, i, b) in closing {
                        let close_node = Self::get_or_add_node(&(a, i, b), &mut nodes);
                        dep = dep.add_edge(open_node, (), close_node);
                    }
                } else if let Some(opening) = self.is_useless_closed(b, from) {
                    assert!(!opening.is_empty(), "{from}, {b:?}");
                    let close_node = Self::get_or_add_node(&(from, item, to), &mut nodes);
                    for (a, i, b) in opening {
                        let open_node = Self::get_or_add_node(&(a, i, b), &mut nodes);
                        dep = dep.add_edge(close_node, (), open_node);
                    }
                }
            }
        }

        if nodes.is_empty() {
            return (self, false);
        }
        assert!(dep.nodes().count() == nodes.len());

        let mut is_useless = vec![None; nodes.len()];
        for node in dep.nodes() {
            if is_useless[node].is_some() {
                continue;
            }

            let mut res = true;
            let mut visited = HashSet::new();
            let mut stack = vec![node];

            'OUTER: while let Some(from) = stack.pop() {
                if !visited.insert(from) {
                    continue;
                }
                let mut had_next = false;
                for (_, _, next) in dep.edges_from(from) {
                    had_next = true;
                    stack.push(next);

                    if let Some(old_res) = is_useless[next] {
                        res = old_res;
                        break 'OUTER;
                    }
                }

                if !had_next {
                    res = false;
                    break;
                }
            }

            for node in visited.into_iter().chain(stack.into_iter()).chain([node]) {
                is_useless[node] = Some(res);
            }
        }

        let mut changed = false;
        for (from, item, to) in self.edges() {
            if let Some(node) = Self::get_node(&(from, item.clone(), to), &nodes) {
                if is_useless[node].unwrap() {
                    changed = true;
                    res = res.add_edge(
                        from,
                        Item::default()
                            .with_token(item.tok())
                            .with_output(item.output())
                            .with_look_ahead(item.look_ahead()),
                        to,
                    );
                    continue;
                }
            }
            res = res.add_edge(from, item, to);
        }
        for node in self.end_nodes() {
            res = res.add_end_node(node);
        }

        (res, changed)
    }

    fn get_node<N: Clone + PartialEq>(node: &N, nodes: &[N]) -> Option<usize> {
        nodes
            .iter()
            .enumerate()
            .find(|(_, n)| *n == node)
            .map(|(i, _)| i)
    }
    fn get_or_add_node<N>(node: &N, nodes: &mut Vec<N>) -> usize
    where
        N: Clone + PartialEq,
    {
        if let Some(i) = Self::get_node(node, nodes) {
            i
        } else {
            nodes.push(node.clone());
            nodes.len() - 1
        }
    }

    fn is_useless_closed(&self, b: Bracket, from: usize) -> Option<Vec<(usize, Item, usize)>> {
        if b.is_open() {
            return None;
        }

        let mut corresponding = vec![];
        let mut queue = VecDeque::from([(from, HashSet::new())]);
        while let Some((q1, mut visited)) = queue.pop_front() {
            for (prev, item2, _) in self.edges_to(q1) {
                let b1 = if let Some(b) = item2.bracket() {
                    b
                } else if visited.insert((prev, item2.clone(), q1)) {
                    queue.push_back((prev, visited.clone()));
                    continue;
                } else {
                    return None;
                };

                if !b1.is_open() || !(b.is_wildcard() || b.index() == b1.index()) {
                    return None;
                }
                corresponding.push((prev, item2, q1));
            }
        }

        if corresponding.is_empty() {
            return None;
        }
        Some(corresponding)
    }
    fn is_useless_open(&self, b: Bracket, to: usize) -> Option<Vec<(usize, Item, usize)>> {
        if !b.is_open() {
            return None;
        }

        let mut corresponding = vec![];
        let mut queue = VecDeque::from([(to, HashSet::new())]);
        while let Some((q1, mut visited)) = queue.pop_front() {
            for (_, item2, next) in self.edges_from(q1) {
                let b1 = if let Some(b) = item2.bracket() {
                    b
                } else if visited.insert((q1, item2.clone(), next)) {
                    queue.push_back((next, visited.clone()));
                    continue;
                } else {
                    return None;
                };

                if b1.is_open() || !(b1.is_wildcard() || b1.index() == b.index()) {
                    return None;
                }
                corresponding.push((q1, item2, next));
            }
        }
        if corresponding.is_empty() {
            return None;
        }

        Some(corresponding)
    }

    // Remove lookahead on edge (p, i, q), if it doesnt impact determinism
    fn useless_lookahead(self) -> (Self, bool) {
        let mut res = Self::new(self.start());
        let mut changed = false;

        'OUTER: for (from, item, to) in self.edges() {
            if item.look_ahead().is_none() {
                res = res.add_edge(from, item, to);
                continue;
            }

            let item1 = Item::default()
                .with_output(item.output())
                .with_token(item.tok())
                .with_bracket(item.bracket());

            for (_, item2, next) in self.edges_from(from) {
                if item2 == item && next == to {
                    continue;
                }

                if !item1.is_distinguishable(&item2) {
                    res = res.add_edge(from, item, to);
                    continue 'OUTER;
                }
            }

            changed = true;
            res = res.add_edge(from, item1, to);
        }
        for node in self.end_nodes() {
            res = res.add_end_node(node);
        }

        (res, changed)
    }

    fn linear_brackets(self) -> (Self, bool) {
        let mut res = Self::new(self.start());
        let mut changed = false;

        for (from, item, to) in self.edges() {
            let b = if let Some(b) = item.bracket() {
                b
            } else {
                res = res.add_edge(from, item, to);
                continue;
            };

            let to_remove = if let Some((from1, b1, to1)) = self.forward_linear(from, b, to) {
                if let Some((from2, b2, to2)) = self.backward_linear(from1, b1, to1) {
                    assert_eq!((from2, b2, to2), (from, b, to));
                    println!("{:?} and {:?}", (from, b, to), (from1, b1, to1));
                    true
                } else {
                    false
                }
            } else if let Some((from1, b1, to1)) = self.backward_linear(from, b, to) {
                if let Some((from2, b2, to2)) = self.forward_linear(from1, b1, to1) {
                    assert_eq!((from2, b2, to2), (from, b, to));
                    println!("{:?} and {:?}", (from, b, to), (from1, b1, to1));
                    true
                } else {
                    false
                }
            } else {
                false
            };

            if to_remove {
                res = res.add_edge(from, item.with_bracket(None), to);
                changed = true;
            } else {
                res = res.add_edge(from, item, to);
            }
        }

        for node in self.end_nodes() {
            res = res.add_end_node(node)
        }
        (res, changed)
    }

    fn forward_linear(
        &self,
        _from: usize,
        b: Bracket,
        to: usize,
    ) -> Option<(usize, Bracket, usize)> {
        if !b.is_open() {
            return None;
        }

        let mut next = Some(to);
        let mut visited = HashSet::new();
        while let Some(n) = next.take() {
            if !visited.insert(n) {
                return None;
            }
            let mut res = None;

            for (from1, item1, to1) in self.edges_from(n) {
                if next.is_some() {
                    return None;
                }
                next = Some(to1);
                if let Some(b1) = item1.bracket() {
                    res = Some((from1, b1, to1));
                }
            }
            if let Some((from1, b1, to1)) = res {
                if b1.is_wildcard() || !b1.is_open() && b1.index() == b.index() {
                    return Some((from1, b1, to1));
                } else {
                    return None;
                }
            }
        }

        None
    }
    fn backward_linear(
        &self,
        from: usize,
        b: Bracket,
        _to: usize,
    ) -> Option<(usize, Bracket, usize)> {
        if b.is_open() {
            return None;
        }

        let mut next = Some(from);
        let mut visited = HashSet::new();
        while let Some(n) = next.take() {
            if !visited.insert(n) {
                return None;
            }
            let mut res = None;
            for (from1, item1, to1) in self.edges_to(n) {
                if next.is_some() {
                    return None;
                }
                next = Some(from1);

                if let Some(b1) = item1.bracket() {
                    res = Some((from1, b1, to1));
                }
            }
            if let Some((from1, b1, to1)) = res {
                if b1.is_wildcard() || !b1.is_open() && b1.index() == b.index() {
                    return Some((from1, b1, to1));
                } else {
                    return None;
                }
            }
        }

        None
    }
}
