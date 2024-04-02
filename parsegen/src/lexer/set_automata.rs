use std::{collections::HashSet, fmt::Display};

use crate::TransitionScheme;

use super::charset::CharSet;

pub type SetAutomata = TransitionScheme<Option<CharSet>>;

impl SetAutomata {
    pub fn remove_nones(&self) -> Self {
        let mut res = Self::new(self.start());

        for node in self.nodes() {
            if self.is_end_node(node) {
                res = res.add_end(node);
            }

            for (set, to, end) in self.lambda_closure(node) {
                res = res.add_edge(node, Some(set.clone()), to);
                if end {
                    res = res.add_end(node);
                }
            }
        }

        res.remove_unreachable()
    }

    fn lambda_closure(&self, node: usize) -> impl Iterator<Item = (&CharSet, usize, bool)> {
        let mut stack = vec![(node, false)];

        let mut res = vec![];
        let mut visited = HashSet::new();
        while let Some((from, end)) = stack.pop() {
            for (_, set, to) in self.edges_from(from) {
                if !visited.insert(to) {
                    continue;
                }

                match set.as_ref() {
                    Some(set) => {
                        res.push((set, to, end || self.is_end_node(from)));
                    }
                    None => stack.push((to, end || self.is_end_node(from))),
                }
            }
        }

        res.into_iter()
    }

    pub fn determine(&self) -> (Self, Vec<Vec<usize>>) {
        let mut res = Self::new(0);
        let mut subsets = vec![vec![self.start()]];
        let mut stack = vec![0];

        while let Some(node) = stack.pop() {
            let mut joined_outs = OwnedSubsets::empty();
            for (set, to) in subsets[node].iter().flat_map(|n| {
                self.edges_from(*n)
                    .map(|(_, set, to)| (set.as_ref().unwrap(), to))
            }) {
                joined_outs.add(to, &set);
            }

            'OUTER: for (to_subset, set) in joined_outs.into_subsets() {
                for (i, existing_subset) in subsets.iter().enumerate() {
                    if existing_subset == &to_subset {
                        res = res.add_edge(node, Some(set), i);
                        continue 'OUTER;
                    }
                }

                let new_node = subsets.len();
                subsets.push(to_subset);
                res = res.add_edge(node, Some(set), new_node);
                stack.push(new_node);
            }
        }

        for (i, set) in subsets.iter().enumerate() {
            if set.iter().any(|n| self.is_end_node(*n)) {
                res = res.add_end(i);
            }
        }

        (res, subsets)
    }

    pub fn is_dfa(&self) -> bool {
        for node in self.nodes() {
            let mut seen = CharSet::empty();
            for (_, items, _) in self.edges_from(node) {
                let items = if let Some(items) = items {
                    items
                } else {
                    return false;
                };
                if !seen.intersect(items).is_empty() {
                    return false;
                }
                seen = seen.union(items);
            }
        }
        true
    }

    pub fn union(&self, other: &Self) -> Self {
        let a_max = self.nodes().max().unwrap();
        let mut res = Self::new(0);
        for (from, set, to) in self.edges() {
            res = res.add_edge(from + 1, set.clone(), to + 1);
        }
        for node in self.ends() {
            res = res.add_end(node + 1);
        }
        for (from, set, to) in other.edges() {
            res = res.add_edge(from + 2 + a_max, set.clone(), to + 2 + a_max);
        }
        for node in other.ends() {
            res = res.add_end(node + 2 + a_max);
        }
        res = res
            .add_edge(0, None, self.start() + 1)
            .add_edge(0, None, other.start() + 2 + a_max);

        res
    }

    pub fn star(&self) -> Self {
        let mut res = self.clone();
        for end in self.ends() {
            res = res.add_edge(end, None, self.start());
        }

        res.add_end(self.start())
    }

    pub fn concat(&self, other: &Self) -> Self {
        let a_max = self.nodes().max().unwrap();
        let mut res = Self::new(self.start());
        for (from, set, to) in self.edges() {
            res = res.add_edge(from, set.clone(), to);
        }
        for (from, set, to) in other.edges() {
            res = res.add_edge(from + 1 + a_max, set.clone(), to + 1 + a_max);
        }
        for end in self.ends() {
            res = res.add_edge(end, None, other.start() + 1 + a_max);
        }
        for end in other.ends() {
            res = res.add_end(end + 1 + a_max);
        }

        res
    }

    pub fn complement(&self) -> Self {
        let s = self.add_dead_state();
        let mut res = Self::new(s.start());
        for (from, item, to) in s.edges() {
            res = res.add_edge(from, item.clone(), to);
        }
        for end in s.dead_states() {
            res = res.add_end(end);
        }
        res.remove_dead_states()
    }

    pub fn minimize(&self) -> (Self, Vec<(usize, usize)>) {
        assert!(self.is_dfa());
        let s = self.add_dead_state();
        let s = s.remove_unreachable();
        assert!(s.is_dfa());

        let (node_nums, distinguishable) = s.table_filling();
        let mut block_num = vec![0; node_nums.len()];
        let mut nodes_per_block = vec![];
        let mut next_block = 1;

        for i in 0..node_nums.len() {
            if block_num[i] != 0 {
                continue;
            }

            block_num[i] = next_block;
            next_block += 1;
            let mut cur = vec![i];

            for j in i + 1..node_nums.len() {
                if !distinguishable[i * node_nums.len() + j] {
                    assert!(block_num[j] == 0,);
                    block_num[j] = block_num[i];
                    cur.push(j);
                }
            }
            nodes_per_block.push(cur);
        }

        let mut res = Self::new(1);

        for (i, block) in nodes_per_block.iter().enumerate() {
            let mut to_block = vec![CharSet::empty(); nodes_per_block.len()];
            for (_, item, to) in block
                .iter()
                .cloned()
                .map(|i| node_nums[i])
                .flat_map(|a| s.edges_from(a))
            {
                let i = node_nums.binary_search(&to).unwrap();
                to_block[block_num[i] - 1] =
                    to_block[block_num[i] - 1].union(item.as_ref().unwrap());
            }

            for (j, set) in to_block.into_iter().enumerate() {
                if set.is_empty() {
                    continue;
                }
                res = res.add_edge(i + 1, Some(set), j + 1);
            }

            if block.iter().cloned().any(|i| s.is_end_node(node_nums[i])) {
                res = res.add_end(i + 1);
            }
        }

        let mut translation = vec![];
        for i in 0..node_nums.len() {
            translation.push((node_nums[i], block_num[i]));
        }
        (res, translation)
    }

    fn table_filling(&self) -> (Vec<usize>, Vec<bool>) {
        let mut names: Vec<_> = self.nodes().collect();
        names.sort();
        let mut dist = vec![false; names.len() * names.len()];

        let mut next = vec![];
        for (i, a) in names.iter().cloned().enumerate() {
            for (j, b) in names.iter().cloned().enumerate().skip(i + 1) {
                if self.is_end_node(a) != self.is_end_node(b) {
                    assert!(!dist[i * names.len() + j], "{i}, {j}");
                    dist[i * names.len() + j] = true;
                    dist[j * names.len() + i] = true;
                    next.push((i, j));
                }
            }
        }

        while let Some((i, j)) = next.pop() {
            for (to_a, a_item, _) in self.edges_to(names[i]) {
                for (to_b, b_item, _) in self.edges_to(names[j]) {
                    let k = names.binary_search(&to_a).unwrap();
                    let l = names.binary_search(&to_b).unwrap();
                    if dist[k * names.len() + l] {
                        continue;
                    }

                    if a_item
                        .as_ref()
                        .unwrap()
                        .intersect(b_item.as_ref().unwrap())
                        .is_empty()
                    {
                        continue;
                    }
                    dist[k * names.len() + l] = true;
                    dist[l * names.len() + k] = true;
                    next.push((k, l));
                }
            }
        }

        (names, dist)
    }

    pub fn remove_unreachable(&self) -> Self {
        let mut res = Self::new(self.start());
        let mut stack = vec![self.start()];
        let mut visited = HashSet::new();

        while let Some(node) = stack.pop() {
            if !visited.insert(node) {
                continue;
            }

            if self.is_end_node(node) {
                res = res.add_end(node);
            }

            for (_, item, to) in self.edges_from(node) {
                stack.push(to);
                res = res.add_edge(node, item.clone(), to);
            }
        }

        res
    }

    pub fn remove_dead_states(&self) -> Self {
        let dead: HashSet<_> = self.dead_states().collect();
        if dead.contains(&self.start()) {
            return Self::new(0);
        }

        let mut res = Self::new(self.start());
        for (from, item, to) in self.edges() {
            if dead.contains(&from) || dead.contains(&to) {
                continue;
            }

            res = res.add_edge(from, item.clone(), to);
        }

        for end in self.ends() {
            res = res.add_end(end);
        }

        res
    }

    pub fn add_dead_state(&self) -> Self {
        let dead = self.nodes().max().unwrap() + 1;
        let mut res = Self::new(self.start());
        res = res.add_edge(dead, Some(CharSet::full()), dead);

        for n in self.nodes() {
            let mut out = CharSet::empty();
            for (_, item, to) in self.edges_from(n) {
                res = res.add_edge(n, item.clone(), to);
                out = out.union(item.as_ref().unwrap());
            }
            let to_dead = out.complement();
            if !to_dead.is_empty() {
                res = res.add_edge(n, Some(to_dead), dead);
            }
        }

        for n in self.ends() {
            res = res.add_end(n);
        }

        res
    }

    pub fn dead_states(&self) -> impl Iterator<Item = usize> + '_ {
        let mut visited: HashSet<_> = self.ends().collect();
        let mut stack: Vec<_> = self.ends().collect();

        while let Some(node) = stack.pop() {
            for (prev, _, _) in self.edges_to(node) {
                if !visited.insert(prev) {
                    continue;
                }
                stack.push(prev);
            }
        }

        self.nodes().filter(move |n| !visited.contains(n))
    }
}

struct OwnedSubsets {
    ranges: Vec<(usize, u32, u32)>,
    owners: Vec<Vec<usize>>,
}

impl OwnedSubsets {
    pub fn empty() -> Self {
        Self {
            ranges: vec![],
            owners: vec![],
        }
    }
    pub fn add(&mut self, owner: usize, set: &CharSet) {
        for (a, b) in set.ranges() {
            self.add_range(owner, a, b);
        }
    }
    fn add_range(&mut self, owner: usize, mut min: u32, max: u32) {
        let ranges = std::mem::take(&mut self.ranges);

        let mut put = false;
        let new = self.add_owner(vec![owner]);
        for (old, a, b) in ranges {
            // 1. min a max b: [min,a):new, [a,max]:old+new, (max,b]:old
            // 2. a min max b: [a,min):old, [min,max]:old+new, (max,b]:old
            // 3. a min b max: [a,min):old, [min,b]:old+new, (b,max]:new
            // 4. [a, b] does not intersect [min, max]: [a, b]:old
            // 5. min a b max: [min,a):new, [a,b]:old+new, (b,max]:new
            if a > max && !put {
                self.add_closed(new, min, max);
                put = true;
            }

            if a > max || b < min {
                self.add_closed(old, a, b);
                continue;
            }

            let new_old = match self.owners[old].binary_search(&owner) {
                Ok(_) => old,
                Err(i) => {
                    let mut new_old = self.owners[old].clone();
                    new_old.insert(i, owner);
                    self.add_owner(new_old)
                }
            };
            if min <= a && a <= max && max <= b {
                put = true;
                self.add_cl_op(new, min, a);
                self.add_closed(new_old, a, max);
                self.add_op_cl(old, max, b);
            } else if a <= min && min <= max && max <= b {
                put = true;
                self.add_cl_op(old, a, min);
                self.add_closed(new_old, min, max);
                self.add_op_cl(old, max, b);
            } else if a <= min && min <= b && b <= max {
                self.add_cl_op(old, a, min);
                self.add_closed(new_old, min, b);
                if b < max {
                    min = b + 1;
                } else {
                    put = true;
                }
            } else if a >= min && b <= max {
                self.add_cl_op(new, min, a);
                self.add_closed(new_old, a, b);
                if max == b {
                    put = true;
                } else {
                    min = b + 1;
                }
            } else {
                panic!("Should be unreachable: ({min},{max}),({a},{b})");
            }
        }

        if !put {
            self.add_closed(new, min, max);
        }
    }

    fn validate_edge(&self, owner: usize, min: u32, max: u32) {
        assert!(min <= max);
        assert!(self.ranges.last().map_or(true, |(o, _, b)| *b < min
            && if *o == owner { b + 1 < min } else { true }));
    }
    fn add_closed(&mut self, owner: usize, min: u32, max: u32) {
        self.validate_edge(owner, min, max);
        self.ranges.push((owner, min, max));
    }
    fn add_cl_op(&mut self, owner: usize, min: u32, max: u32) {
        if min == max {
        } else {
            self.validate_edge(owner, min, max - 1);
            self.ranges.push((owner, min, max - 1));
        }
    }
    fn add_op_cl(&mut self, owner: usize, min: u32, max: u32) {
        if min == max {
        } else {
            self.validate_edge(owner, min + 1, max);
            self.ranges.push((owner, min + 1, max));
        }
    }
    fn add_owner(&mut self, owners: Vec<usize>) -> usize {
        for (i, old) in self.owners.iter().enumerate() {
            if old == &owners {
                return i;
            }
        }
        self.owners.push(owners);
        self.owners.len() - 1
    }
    pub fn into_subsets(mut self) -> impl Iterator<Item = (Vec<usize>, CharSet)> {
        let owners = std::mem::take(&mut self.owners);
        let mut subsets: Vec<_> = owners.into_iter().map(|o| (o, CharSet::empty())).collect();
        for (o, a, b) in self.ranges {
            subsets[o].1.add_range(a, b);
        }
        subsets.into_iter()
    }
}

impl Display for SetAutomata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{\nnode [shape=circle];\nQ1 [style=invisible, height=0, width=0, fixedsize=true];")?;
        writeln!(f, "Q1 -> {};", self.start())?;
        for (from, set, to) in self.edges() {
            if let Some(set) = set {
                writeln!(f, "{} -> {} [label=\"{}\"];", from, to, set)?;
            } else {
                writeln!(f, "{} -> {}; ", from, to)?;
            }
        }

        for n in self.ends() {
            writeln!(f, "{n} [shape=\"doublecircle\"]; ")?;
        }

        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn traverse(dfa: &SetAutomata, word: impl IntoIterator<Item = char>) -> bool {
        assert!(dfa.is_dfa());
        let mut cur = dfa.start();
        'OUTER: for c in word {
            for (_, set, next) in dfa.edges_from(cur) {
                if set.as_ref().unwrap().contains(c) {
                    cur = next;
                    continue 'OUTER;
                }
            }
        }

        dfa.is_end_node(cur)
    }

    fn one_set(c: char) -> Option<CharSet> {
        Some(CharSet::range(c, c))
    }

    fn example_dfa() -> SetAutomata {
        SetAutomata::new(0)
            .add_edge(0, one_set('0'), 1)
            .add_edge(0, one_set('1'), 5)
            .add_edge(1, one_set('1'), 2)
            .add_edge(1, one_set('0'), 6)
            .add_edge(2, one_set('1'), 2)
            .add_edge(2, one_set('0'), 0)
            .add_edge(3, one_set('0'), 2)
            .add_edge(3, one_set('1'), 6)
            .add_edge(4, one_set('1'), 5)
            .add_edge(4, one_set('0'), 7)
            .add_edge(5, one_set('1'), 6)
            .add_edge(5, one_set('0'), 2)
            .add_edge(6, one_set('0'), 6)
            .add_edge(6, one_set('1'), 4)
            .add_edge(7, one_set('0'), 6)
            .add_edge(7, one_set('1'), 2)
            .add_end(2)
    }

    #[test]
    fn table_filling() {
        let dfa = example_dfa();
        assert!(dfa.is_dfa());
        let (names, distinguishable) = dfa.table_filling();
        assert!(distinguishable.len() == names.len() * names.len());
        assert!(names == [0, 1, 2, 3, 4, 5, 6, 7], "{:?}", names);

        let mut real = vec![true; names.len() * names.len()];
        for (a, b) in [(0, 4), (1, 7), (3, 5)] {
            real[a * names.len() + b] = false;
            real[b * names.len() + a] = false;
        }
        for i in &names {
            real[*i * names.len() + *i] = false;
        }

        assert_eq!(distinguishable, real);
    }

    #[test]
    fn minimize() {
        let dfa = example_dfa();
        let (mdfa, _names) = dfa.minimize();
        assert_eq!(mdfa.nodes().count(), 6);
    }

    #[test]
    fn regular_ops() {
        let a = SetAutomata::new(0).add_edge(0, one_set('a'), 1).add_end(1);
        let b = SetAutomata::new(0).add_edge(0, one_set('b'), 1).add_end(1);

        let c = SetAutomata::new(0);
        let c = c.remove_nones().determine().0.minimize().0;
        for (w, res) in [("", false), ("a", false)] {
            assert_eq!(traverse(&c, w.chars()), res, "{w}");
        }

        let c = SetAutomata::new(0).add_end(0);
        let c = c.remove_nones();
        let c = c.determine().0;
        let c = c.minimize().0;
        for (w, res) in [("", true), ("a", false)] {
            assert_eq!(traverse(&c, w.chars()), res, "{w}");
        }

        let c = a.union(&b);
        let c = c.remove_nones().determine().0.minimize().0;
        for (w, res) in [("a", true), ("b", true), ("c", false), ("aa", false)] {
            assert_eq!(traverse(&c, w.chars()), res, "{w}");
        }

        let c = a.concat(&b);
        let c = c.remove_nones();
        let c = c.determine().0;
        let c = c.minimize().0;
        for (w, res) in [
            ("a", false),
            ("b", false),
            ("c", false),
            ("ab", true),
            ("abb", false),
        ] {
            assert_eq!(traverse(&c, w.chars()), res, "{w}");
        }

        let c = a.star();
        let c = c.remove_nones();
        let c = c.determine().0;
        let c = c.minimize().0;
        for (w, res) in [
            ("", true),
            ("a", true),
            ("b", false),
            ("c", false),
            ("aa", true),
            ("aaa", true),
            ("aaaaaa", true),
            ("aaaaab", false),
        ] {
            assert_eq!(traverse(&c, w.chars()), res, "{w}");
        }

        let c = a.star().union(&b.concat(&b).concat(&b)).star();
        let c = c.remove_nones();
        let c = c.determine().0;
        let c = c.minimize().0;
        for (w, res) in [
            ("a", true),
            ("b", false),
            ("c", false),
            ("aa", true),
            ("aaa", true),
            ("bbbaabbb", true),
            ("bb", false),
        ] {
            assert_eq!(traverse(&c, w.chars()), res, "{w}");
        }
    }
}
