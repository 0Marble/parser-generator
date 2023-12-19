use std::{any::type_name, collections::HashSet, fmt::Display, io::Cursor, io::Write};

use crate::parser::lgraph::Either;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct StateMachine<T> {
    edges: Vec<(Vec<(T, usize)>, Vec<(usize, T)>)>,
    start: Vec<usize>,
    end: Vec<usize>,
    nodes: Vec<(usize, usize)>,
}

impl<T> StateMachine<T>
where
    T: Clone + PartialEq,
{
    pub fn new() -> Self {
        Self {
            edges: vec![],
            start: vec![],
            end: vec![],
            nodes: vec![],
        }
    }
    pub fn add_start_node(mut self, n: usize) -> Self {
        if self.start.contains(&n) {
            return self;
        }
        self.start.push(n);
        if self.nodes.iter().all(|(node, _)| node != &n) {
            self.nodes.push((n, self.edges.len()));
            self.edges.push((vec![], vec![]));
        }
        self
    }
    pub fn add_end_node(mut self, n: usize) -> Self {
        if self.end.contains(&n) {
            return self;
        }
        self.end.push(n);
        if self.nodes.iter().all(|(node, _)| node != &n) {
            self.nodes.push((n, self.edges.len()));
            self.edges.push((vec![], vec![]));
        }
        self
    }
    fn edges_loc(&self, node: usize) -> Option<usize> {
        for (n, loc) in &self.nodes {
            if *n == node {
                return Some(*loc);
            }
        }
        None
    }
    fn from_node(&self, edge_loc: usize) -> Option<usize> {
        for (n, loc) in &self.nodes {
            if *loc == edge_loc {
                return Some(*n);
            }
        }
        None
    }
    pub fn add_edge(mut self, from: usize, letter: T, to: usize) -> Self {
        self = self.add_node(from);
        self = self.add_node(to);
        let from_loc = self.edges_loc(from).unwrap();
        let to_loc = self.edges_loc(to).unwrap();
        self.edges[from_loc].0.push((letter.clone(), to));
        self.edges[to_loc].1.push((from, letter));
        self
    }
    pub fn edges(&self) -> impl Iterator<Item = (usize, T, usize)> + '_ {
        self.edges.iter().enumerate().flat_map(|(loc, edges)| {
            let from = self.from_node(loc).unwrap();
            edges
                .0
                .iter()
                .map(move |(item, to)| (from, item.clone(), *to))
        })
    }
    pub fn edges_from(&self, n: usize) -> impl Iterator<Item = (usize, T, usize)> + '_ {
        if let Some(loc) = self.edges_loc(n) {
            Either::B(
                self.edges[loc]
                    .0
                    .iter()
                    .map(move |(item, to)| (n, item.clone(), *to)),
            )
        } else {
            Either::A(std::iter::empty())
        }
    }
    pub fn edges_to(&self, n: usize) -> impl Iterator<Item = (usize, T, usize)> + '_ {
        if let Some(loc) = self.edges_loc(n) {
            Either::B(
                self.edges[loc]
                    .1
                    .iter()
                    .map(move |(to, item)| (n, item.clone(), *to)),
            )
        } else {
            Either::A(std::iter::empty())
        }
    }
    pub fn is_end_node(&self, n: usize) -> bool {
        self.end.contains(&n)
    }
    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.nodes.iter().map(|(node, _)| *node)
    }
    pub fn start_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.start.iter().cloned()
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.end.iter().cloned()
    }

    pub fn add_node(mut self, n: usize) -> Self {
        if self.nodes.iter().all(|(node, _)| node != &n) {
            self.nodes.push((n, self.edges.len()));
            self.edges.push((vec![], vec![]));
        }

        self
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Dfa<T> {
    edges: Vec<(usize, T, usize)>,
    start: usize,
    end: Vec<usize>,
    nodes: Vec<usize>,
}

impl<T> Dfa<T>
where
    T: Display + PartialEq + Clone,
{
    pub fn to_dot(&self) -> String {
        let mut s = vec![];
        let mut f = Cursor::new(&mut s);
        writeln!(f, "digraph {{\n  node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];").unwrap();
        writeln!(
            f,
            "  node_type = \"{}\";\n  item_type = \"{}\";\n  kind = dfa;",
            type_name::<usize>(),
            type_name::<T>()
        )
        .unwrap();

        writeln!(f, "  Q1 -> \"{}\";", self.start_node()).unwrap();

        for (a, l, b) in self.edges() {
            writeln!(f, "  \"{}\" -> \"{}\" [label=\"{}\"];", a, b, l,).unwrap();
        }
        for node in self.end_nodes() {
            writeln!(f, "  \"{}\" [shape=doublecircle];", node).unwrap();
        }

        writeln!(f, "}}").unwrap();

        String::from_utf8(s).unwrap()
    }
}

impl<T> Dfa<T>
where
    T: PartialEq + Clone,
{
    pub fn traverse<V: Into<T>>(&self, s: impl IntoIterator<Item = V>) -> bool {
        let mut state = self.start;
        'outer: for c in s.into_iter() {
            let c = c.into();
            for (_, l, to) in self.edges_from(state) {
                if l == c {
                    state = to;
                    continue 'outer;
                }
            }

            return false;
        }
        self.is_end_node(state)
    }
    pub fn to_nfa(&self) -> Nfa<T> {
        Nfa {
            edges: self.edges().map(|(a, l, b)| (a, Some(l), b)).collect(),
            start: vec![self.start],
            end: self.end.clone(),
            nodes: self.nodes.clone(),
        }
    }

    pub fn minimize(&self) -> Self {
        let mut eq_sets = vec![vec![], vec![]];
        for n in &self.nodes {
            if self.is_end_node(*n) {
                eq_sets[0].push(*n);
            } else {
                eq_sets[1].push(*n);
            }
        }

        for _ in 1.. {
            let mut next = vec![];
            for set in &eq_sets {
                let mut split: Vec<Vec<_>> = vec![];
                for i in 0..set.len() {
                    let a = &set[i];
                    if split.iter().any(|set| set.iter().any(|n| n == a)) {
                        continue;
                    } else {
                        split.push(vec![*a]);
                    }

                    let mut a_target_sets: Vec<(_, Vec<_>)> = vec![];
                    for (_, letter, to) in self.edges_from(*a) {
                        let cur_set = if let Some((_, set)) =
                            a_target_sets.iter_mut().find(|(l, _)| *l == letter)
                        {
                            set
                        } else {
                            a_target_sets.push((letter, vec![]));
                            let (_, set) = a_target_sets.last_mut().unwrap();
                            set
                        };
                        let edge_set = eq_sets
                            .iter()
                            .enumerate()
                            .find(|(_, set)| set.iter().any(|n| *n == to))
                            .map(|(i, _)| i)
                            .unwrap();
                        if cur_set.iter().all(|i| *i != edge_set) {
                            cur_set.push(edge_set);
                        }
                    }
                    for (_, set) in &mut a_target_sets {
                        set.sort();
                    }

                    'b_loop: for b in set.iter().skip(i) {
                        if split.iter().any(|set| set.iter().any(|n| n == b)) {
                            continue;
                        }

                        // if self.is_end_node(*a) && self.is_end_node(*b) {
                        //     continue 'b_loop;
                        // }

                        let mut b_target_sets: Vec<(_, Vec<_>)> = vec![];
                        for (_, letter, to) in self.edges_from(*b) {
                            let cur_set = if let Some((_, set)) =
                                b_target_sets.iter_mut().find(|(l, _)| *l == letter)
                            {
                                set
                            } else {
                                b_target_sets.push((letter, vec![]));
                                let (_, set) = b_target_sets.last_mut().unwrap();
                                set
                            };
                            let edge_set = eq_sets
                                .iter()
                                .enumerate()
                                .find(|(_, set)| set.iter().any(|n| *n == to))
                                .map(|(i, _)| i)
                                .unwrap();
                            if cur_set.iter().all(|i| *i != edge_set) {
                                cur_set.push(edge_set);
                            }
                        }
                        if a_target_sets.len() != b_target_sets.len() {
                            continue 'b_loop;
                        }
                        for (_, set) in &mut b_target_sets {
                            set.sort();
                        }

                        for (letter, a_set) in &a_target_sets {
                            let b_set = b_target_sets
                                .iter()
                                .find(|(l, _)| l == letter)
                                .map(|(_, set)| set);
                            let eq_for_letter = match b_set {
                                Some(b_set) => b_set == a_set,
                                None => continue 'b_loop,
                            };
                            if !eq_for_letter {
                                continue 'b_loop;
                            }
                        }
                        split.last_mut().unwrap().push(*b);
                    }
                }
                for set in split {
                    next.push(set);
                }
            }

            if next.len() == eq_sets.len() {
                break;
            }
            eq_sets = next;
        }

        // dbg!(&eq_sets);

        let mut edges = vec![];
        let mut start = None;
        let mut end = vec![];

        for (a_idx, a_set) in eq_sets.iter().enumerate() {
            if a_set.is_empty() {
                continue;
            }
            let a = a_set[0];
            if start.is_none() && a_set.contains(&self.start) {
                start = Some(a_idx);
            }
            for e in &self.end {
                if a_set.contains(e) && !end.contains(&a_idx) {
                    end.push(a_idx);
                }
            }

            for (_, letter, to) in self.edges_from(a) {
                let (b_idx, _) = eq_sets
                    .iter()
                    .enumerate()
                    .find(|(_, b)| b.contains(&to))
                    .unwrap();
                edges.push((a_idx, letter, b_idx));
            }
        }

        Dfa {
            edges,
            start: start.unwrap(),
            end,
            nodes: (0..eq_sets.len()).collect(),
        }
    }
    pub fn edges(&self) -> impl Iterator<Item = (usize, T, usize)> + '_ {
        self.edges.iter().cloned()
    }
    pub fn edges_from(&self, n: usize) -> impl Iterator<Item = (usize, T, usize)> + '_ {
        self.edges().filter(move |(from, _, _)| *from == n)
    }
    pub fn is_end_node(&self, n: usize) -> bool {
        self.end.contains(&n)
    }
    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.nodes.iter().cloned()
    }
    pub fn start_node(&self) -> usize {
        self.start
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.end.iter().cloned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Nfa<T> {
    edges: Vec<(usize, Option<T>, usize)>,
    start: Vec<usize>,
    end: Vec<usize>,
    nodes: Vec<usize>,
}

impl<T> Nfa<T>
where
    T: Display + PartialEq + Clone,
{
    pub fn to_dot(&self) -> String {
        let mut s = vec![];
        let mut f = Cursor::new(&mut s);
        writeln!(f, "digraph {{\n  node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];").unwrap();
        writeln!(
            f,
            "  node_type = \"{}\";\n  item_type = \"{}\";\n  kind = nfa;",
            type_name::<usize>(),
            type_name::<T>()
        )
        .unwrap();

        for n in &self.start {
            writeln!(f, "  Q1 -> \"{}\";", n).unwrap();
        }
        for (a, l, b) in self.edges() {
            writeln!(
                f,
                "  \"{}\" -> \"{}\" [label=\"{}\"];",
                a,
                b,
                l.map(|l| l.to_string()).unwrap_or_default()
            )
            .unwrap();
        }
        for node in self.end_nodes() {
            writeln!(f, "  \"{}\" [shape=doublecircle];", node).unwrap();
        }

        writeln!(f, "}}").unwrap();

        String::from_utf8(s).unwrap()
    }
}

impl<T> Nfa<T>
where
    T: PartialEq + Clone,
{
    pub fn new() -> Self {
        Self {
            edges: vec![],
            start: vec![],
            end: vec![],
            nodes: vec![],
        }
    }
    pub fn add_start_node(mut self, n: usize) -> Self {
        self.start.push(n);
        if !self.nodes.contains(&n) {
            self.nodes.push(n);
        }
        self
    }
    pub fn add_end_node(mut self, n: usize) -> Self {
        self.end.push(n);
        if !self.nodes.contains(&n) {
            self.nodes.push(n);
        }
        self
    }
    pub fn add_edge(mut self, from: usize, letter: Option<T>, to: usize) -> Self {
        self.edges.push((from, letter, to));
        if !self.nodes.contains(&from) {
            self.nodes.push(from);
        }
        if !self.nodes.contains(&to) {
            self.nodes.push(to);
        }
        self
    }
    pub fn clear(mut self) -> Self {
        Self::new()
    }

    pub fn determine(&self) -> Dfa<T> {
        let mut mapping: Vec<Vec<usize>> = vec![self.start.clone()];
        let mut stack = vec![0];
        let mut edges = vec![];
        let start = 0;
        let mut end = vec![];

        while let Some(cur_set_idx) = stack.pop() {
            let cur_set = &mapping[cur_set_idx];
            let mut targets: Vec<(_, Vec<_>)> = vec![];
            let mut sources = cur_set.clone();
            let mut visited: HashSet<_> = HashSet::from_iter(cur_set.iter().cloned());
            let mut is_end = false;

            while let Some(source) = sources.pop() {
                if !is_end && self.end.contains(&source) {
                    is_end = true;
                }

                for (_, letter, to) in self.edges_from(source) {
                    if let Some(letter) = letter {
                        if let Some((_, set)) = targets.iter_mut().find(|(l, _)| *l == letter) {
                            if !set.contains(&to) {
                                set.push(to);
                            }
                        } else {
                            targets.push((letter, vec![to]));
                        }
                    } else if visited.insert(to) {
                        sources.push(to);
                    }
                }
            }

            if is_end {
                end.push(cur_set_idx);
            }

            for (letter, set) in targets {
                let target_subset_idx =
                    if let Some((idx, _)) = mapping.iter().enumerate().find(|(_, s)| *s == &set) {
                        idx
                    } else {
                        mapping.push(set);
                        stack.push(mapping.len() - 1);
                        mapping.len() - 1
                    };
                edges.push((cur_set_idx, letter, target_subset_idx));
            }
        }

        Dfa {
            edges,
            start,
            end,
            nodes: (0..mapping.len()).collect(),
        }
    }

    pub fn edges(&self) -> impl Iterator<Item = (usize, Option<T>, usize)> + '_ {
        self.edges.iter().cloned()
    }
    pub fn edges_from(&self, n: usize) -> impl Iterator<Item = (usize, Option<T>, usize)> + '_ {
        self.edges().filter(move |(from, _, _)| *from == n)
    }
    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.nodes.iter().cloned()
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.end.iter().cloned()
    }

    pub fn star(&self) -> Self {
        let mut s = self.clone();

        let new_node = s.nodes().max().map(|n| n + 1).unwrap_or_default();
        for b in s.start {
            s.edges.push((new_node, None, b));
        }
        s.start = vec![new_node];

        s.nodes.push(new_node);

        for a in &s.end {
            for b in &s.start {
                s.edges.push((*a, None, *b));
            }
        }

        s.end = vec![new_node];
        s
    }

    pub fn concat(&self, other: &Self) -> Self {
        let mut s = self.clone();
        let d = self.nodes().max().map(|n| n + 1).unwrap_or_default();
        for (a, l, b) in other.edges() {
            s.edges.push((a + d, l, b + d));
        }
        for a in &self.end {
            for b in &other.start {
                s.edges.push((*a, None, *b + d));
            }
        }
        s.end = other.end.iter().map(|b| *b + d).collect();
        for b in other.nodes() {
            s.nodes.push(b + d);
        }

        s
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut s = self.clone();
        let d = self.nodes().max().map(|n| n + 1).unwrap_or_default();

        for a in &self.start {
            s.edges.push((d, None, *a));
        }
        for b in &other.start {
            s.edges.push((d, None, b + d + 1));
        }
        for b in &other.end {
            s.end.push(b + d + 1);
        }
        for (a, l, b) in other.edges() {
            s.edges.push((a + d + 1, l, b + d + 1));
        }
        for n in other.nodes() {
            s.nodes.push(n + d + 1);
        }
        s.start = vec![d];

        s
    }

    pub fn is_determined(&self) -> bool {
        if self.start.len() > 1 {
            return false;
        }

        for a in self.nodes() {
            let mut seen = vec![];
            for (_, l, _) in self.edges_from(a) {
                if seen.contains(&l) {
                    return false;
                }
                seen.push(l);
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn nfa1() -> Nfa<usize> {
        let edges = vec![
            (0, Some(1), 1),
            (1, Some(1), 1),
            (2, Some(2), 3),
            (3, Some(2), 3),
        ];
        Nfa {
            edges,
            start: vec![0, 2],
            end: vec![1, 3],
            nodes: vec![0, 1, 2, 3],
        }
    }

    fn nfa2() -> Nfa<usize> {
        let edges = vec![
            (0, Some(1), 1),
            (0, Some(1), 0),
            (0, Some(2), 0),
            (1, Some(1), 1),
            (1, Some(2), 1),
            (1, Some(1), 2),
            (2, Some(1), 2),
            (2, Some(2), 2),
            (2, Some(2), 3),
            (2, None, 0),
        ];
        Nfa {
            edges,
            start: vec![0],
            end: vec![3],
            nodes: vec![0, 1, 2, 3],
        }
    }

    #[test]
    fn mindfa() {
        for (nfa, exaples, size) in [
            (
                nfa1(),
                vec![
                    (vec![], false),
                    (vec![1usize], true),
                    (vec![1, 1], true),
                    (vec![1, 1, 1, 1, 1, 1, 1], true),
                    (vec![1, 2], false),
                    (vec![2], true),
                    (vec![2, 2, 2, 2, 2, 2], true),
                    (vec![2, 1], false),
                ],
                3,
            ),
            (
                nfa2(),
                vec![
                    (vec![], false),
                    (vec![1, 1, 2], true),
                    (vec![1, 2, 2, 1, 2], true),
                    (vec![1, 2, 2, 1, 2, 2, 2], true),
                    (vec![2, 2, 2, 2, 1, 2, 1, 1, 1, 2, 1, 1, 2], true),
                    (vec![2, 2, 2, 1, 2], false),
                    (vec![2, 1, 1, 1], false),
                ],
                4,
            ),
        ] {
            let mdfa = nfa.determine().minimize();
            println!("{:?}", mdfa);
            assert_eq!(mdfa.nodes().count(), size);
            assert!(mdfa.to_nfa().is_determined());

            for (s, res) in exaples {
                assert_eq!(mdfa.traverse(s.iter().cloned()), res, "failed on s={:?}", s);
            }
        }
    }

    #[test]
    fn nfa_ops() {
        for (m, tests) in [
            (
                nfa1().union(&nfa2()),
                vec![
                    (vec![], false),
                    (vec![1usize, 2, 2, 1, 2], true),
                    (vec![1, 1, 1], true),
                    (vec![2, 2, 2], true),
                    (vec![1, 2, 2, 1], false),
                ],
            ),
            (
                nfa1().concat(&nfa2()),
                vec![
                    (vec![], false),
                    (vec![1, 1, 1, 2], true),
                    (vec![2, 1, 1, 2], true),
                    (vec![1, 1, 1], false),
                    (vec![2, 2, 2, 2, 1, 1, 2, 1, 2, 2, 1], false),
                    (vec![1, 2, 2, 1, 1, 1, 1, 2], true),
                ],
            ),
            (
                nfa1().star(),
                vec![
                    (vec![], true),
                    (vec![1, 2], true),
                    (vec![1, 1, 1, 2, 2, 1, 2, 1, 2, 1], true),
                ],
            ),
        ] {
            let m = m.determine().minimize();
            for (s, res) in tests {
                assert_eq!(m.traverse(s.iter().cloned()), res, "failed on s={:?}", s);
            }
        }
    }
}
