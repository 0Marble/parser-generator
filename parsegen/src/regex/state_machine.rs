use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Dfa {
    edges: Vec<(usize, usize, usize)>,
    start: usize,
    end: Vec<usize>,
    nodes: Vec<usize>,
}

impl Dfa {
    pub fn to_nfa(&self) -> Nfa {
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

                    let mut a_target_sets: Vec<(usize, Vec<_>)> = vec![];
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

                        if self.is_end_node(*a) && self.is_end_node(*b) {
                            continue 'b_loop;
                        }

                        let mut b_target_sets: Vec<(usize, Vec<_>)> = vec![];
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

        // dbg!(&eq_sets, &self);

        let mut edges = vec![];
        let mut start = None;
        let mut end = vec![];

        for (a_idx, a_set) in eq_sets.iter().enumerate() {
            let a = a_set[0];
            if start.is_none() && a_set.contains(&self.start) {
                start = Some(a_idx);
            }
            for e in &self.end {
                if a_set.contains(e) {
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
    pub fn edges(&self) -> impl Iterator<Item = (usize, usize, usize)> + '_ {
        self.edges.iter().cloned()
    }
    pub fn edges_from(&self, n: usize) -> impl Iterator<Item = (usize, usize, usize)> + '_ {
        self.edges().filter(move |(from, _, _)| *from == n)
    }
    pub fn is_end_node(&self, n: usize) -> bool {
        self.end.contains(&n)
    }
    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.nodes.iter().cloned()
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Nfa {
    edges: Vec<(usize, Option<usize>, usize)>,
    start: Vec<usize>,
    end: Vec<usize>,
    nodes: Vec<usize>,
}

impl Nfa {
    pub fn determine(&self) -> Dfa {
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

    fn edges(&self) -> impl Iterator<Item = (usize, Option<usize>, usize)> + '_ {
        self.edges.iter().cloned()
    }
    fn edges_from(&self, n: usize) -> impl Iterator<Item = (usize, Option<usize>, usize)> + '_ {
        self.edges().filter(move |(from, _, _)| *from == n)
    }
    fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.nodes.iter().cloned()
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

        s.end.push(new_node);
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
        s.start = vec![d];

        s
    }

    pub fn is_determined(&self) -> bool {
        if self.start.len() > 1 {
            return false;
        }

        for a in self.nodes() {
            let mut seen = HashSet::new();
            for (_, l, _) in self.edges_from(a) {
                if !seen.insert(l) {
                    return false;
                }
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn nfa1() -> Nfa {
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

    fn nfa2() -> Nfa {
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

    fn traverse_dfa<V: Into<usize>>(d: &Dfa, s: impl IntoIterator<Item = V>) -> bool {
        let mut state = d.start;
        'outer: for c in s.into_iter() {
            let c = c.into();
            for (_, l, to) in d.edges_from(state) {
                if l == c {
                    state = to;
                    continue 'outer;
                }
            }

            return false;
        }
        d.is_end_node(state)
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
                12,
            ),
        ] {
            let mdfa = nfa.determine().minimize();
            println!("{:?}", mdfa);
            assert_eq!(mdfa.nodes().count(), size);
            assert!(mdfa.to_nfa().is_determined());

            for (s, res) in exaples {
                assert_eq!(
                    traverse_dfa(&mdfa, s.iter().cloned()),
                    res,
                    "failed on s={:?}",
                    s
                );
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
                assert_eq!(
                    traverse_dfa(&m, s.iter().cloned()),
                    res,
                    "failed on s={:?}",
                    s
                );
            }
        }
    }
}
