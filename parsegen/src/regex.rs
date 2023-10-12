use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Edge<N, I> {
    beg: N,
    item: I,
    end: N,
}

impl<N, I> Edge<N, I> {
    pub fn new(beg: N, item: I, end: N) -> Self {
        Self { beg, item, end }
    }

    pub fn beg(&self) -> &N {
        &self.beg
    }
    pub fn item(&self) -> &I {
        &self.item
    }
    pub fn end(&self) -> &N {
        &self.end
    }
}

#[derive(Debug, Clone)]
struct StateMachine<N, I> {
    nodes: HashSet<N>,
    edges: Vec<Edge<N, I>>,
    end_nodes: HashSet<N>,
    start_node: N,
}

impl<N, I> StateMachine<N, I> {
    pub fn nodes(&self) -> impl Iterator<Item = &N> + '_ {
        self.nodes.iter()
    }
    pub fn edges(&self) -> impl Iterator<Item = &Edge<N, I>> + '_ {
        self.edges.iter()
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = &N> + '_ {
        self.end_nodes.iter()
    }
    pub fn edges_from<'a, 'b>(&'a self, node: &'b N) -> impl Iterator<Item = &'a Edge<N, I>> + 'b
    where
        'a: 'b,
        N: PartialEq,
    {
        self.edges().filter(move |e| e.beg() == node)
    }
    pub fn start_node(&self) -> &N {
        &self.start_node
    }
    pub fn is_end_node(&self, node: &N) -> bool
    where
        N: PartialEq + Eq + Hash,
    {
        self.end_nodes.contains(node)
    }
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }
}

impl<N, I> StateMachine<N, Option<I>>
where
    N: Eq + Hash + Clone,
    I: PartialEq + Clone,
{
    fn remove_nones(&self) -> StateMachine<N, I> {
        let mut res = StateMachine::new(self.start_node().clone());

        for node in self.nodes() {
            for edge in self.lambda_closure(node).flat_map(|n| self.edges_from(n)) {
                if let Some(it) = edge.item() {
                    res = res.add_edge(Edge::new(node.clone(), it.clone(), edge.end().clone()));
                }
            }
        }

        'outer: for node in self.nodes() {
            for next_node in self.lambda_closure(node) {
                if self.is_end_node(next_node) {
                    res = res.add_end_node(node.clone());
                    continue 'outer;
                }
            }
        }

        res
    }
    fn lambda_closure<'a, 'b>(&'a self, node: &'b N) -> impl Iterator<Item = &N>
    where
        'b: 'a,
    {
        let mut res = HashSet::from([node]);
        let mut stack = vec![node];
        while let Some(node) = stack.pop() {
            for edge in self.edges_from(node) {
                if edge.item().is_none() && res.insert(edge.end()) {
                    stack.push(edge.end());
                }
            }
        }
        res.into_iter()
    }
}

impl<N, I> StateMachine<N, I>
where
    N: Eq + Hash + Clone + Debug,
    I: PartialEq + Clone + Debug,
{
    pub fn determine(&self) -> StateMachine<Vec<N>, I> {
        let mut reached_states: Vec<Vec<&N>> = vec![vec![self.start_node()]];
        let mut stack = vec![vec![self.start_node()]];
        let mut res = StateMachine::new(vec![self.start_node().clone()]);

        while let Some(state) = stack.pop() {
            let mut next_states: Vec<(&I, Vec<&N>, bool)> = vec![];
            for node in state.iter().cloned() {
                'edges_from: for edge in self.edges_from(node) {
                    for (l, s, is_end_node) in &mut next_states {
                        if *l == edge.item() {
                            if s.iter().all(|n| *n != edge.end()) {
                                s.push(edge.end());
                                if !*is_end_node {
                                    *is_end_node = self.is_end_node(edge.end());
                                }
                            }
                            continue 'edges_from;
                        }
                    }
                    next_states.push((edge.item(), vec![edge.end()], self.is_end_node(edge.end())));
                }
            }

            let beg: Vec<_> = state.into_iter().cloned().collect();

            for (letter, next_state, is_end_node) in next_states {
                let was_reached = reached_states.iter().any(|s| s == &next_state);
                if !was_reached {
                    reached_states.push(next_state.clone());
                    stack.push(next_state.clone());
                }

                let end: Vec<_> = next_state.into_iter().cloned().collect();
                if is_end_node {
                    res = res.add_end_node(end.clone());
                }

                res = res.add_edge(Edge::new(beg.clone(), letter.clone(), end));
            }
        }

        res
    }

    pub fn minimize(&self, are_end_nodes_equivalent: bool) -> StateMachine<Vec<N>, I> {
        let mut eq_sets = vec![vec![], vec![]];
        for n in self.nodes() {
            if self.is_end_node(n) {
                eq_sets[0].push(n);
            } else {
                eq_sets[1].push(n);
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

                    let mut a_target_sets: Vec<(&I, Vec<_>)> = vec![];
                    for edge in self.edges_from(a) {
                        let cur_set = if let Some((_, set)) =
                            a_target_sets.iter_mut().find(|(l, _)| *l == edge.item())
                        {
                            set
                        } else {
                            a_target_sets.push((edge.item(), vec![]));
                            let (_, set) = a_target_sets.last_mut().unwrap();
                            set
                        };
                        let edge_set = eq_sets
                            .iter()
                            .enumerate()
                            .find(|(_, set)| set.iter().any(|n| *n == edge.end()))
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

                        if !are_end_nodes_equivalent && self.is_end_node(a) && self.is_end_node(b) {
                            continue 'b_loop;
                        }

                        let mut b_target_sets: Vec<(&I, Vec<_>)> = vec![];
                        for edge in self.edges_from(b) {
                            let cur_set = if let Some((_, set)) =
                                b_target_sets.iter_mut().find(|(l, _)| *l == edge.item())
                            {
                                set
                            } else {
                                b_target_sets.push((edge.item(), vec![]));
                                let (_, set) = b_target_sets.last_mut().unwrap();
                                set
                            };
                            let edge_set = eq_sets
                                .iter()
                                .enumerate()
                                .find(|(_, set)| set.iter().any(|n| *n == edge.end()))
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
                        split.last_mut().unwrap().push(b);
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

        let start_node = eq_sets
            .iter()
            .find(|set| set.iter().any(|n| self.start_node() == *n))
            .expect("All nodes are separated into some set");

        let mut res = StateMachine::new(start_node.iter().map(|n| (*n).clone()).collect());

        for set in &eq_sets {
            let set: Vec<N> = set.iter().map(|n| (*n).clone()).collect();
            let a = &set[0];
            for edge in self.edges_from(a) {
                let target = eq_sets
                    .iter()
                    .find(|set| set.iter().any(|n| *n == edge.end()))
                    .expect("All nodes are separated into some set");
                let target = target.iter().map(|n| (*n).clone()).collect();

                res = res.add_edge(Edge::new(set.clone(), edge.item().clone(), target));
            }
            if set.iter().any(|node| self.is_end_node(node)) {
                res = res.add_end_node(set)
            }
        }

        res
    }
}

impl<N, I> StateMachine<N, I>
where
    N: Eq + Hash + Clone,
{
    pub fn new(start_node: N) -> Self {
        Self {
            nodes: HashSet::from([start_node.clone()]),
            edges: vec![],
            end_nodes: HashSet::new(),
            start_node,
        }
    }
    pub fn add_node(mut self, node: N) -> Self {
        self.nodes.insert(node);
        self
    }
    pub fn add_edge(mut self, edge: Edge<N, I>) -> Self {
        self.nodes.insert(edge.beg.clone());
        self.nodes.insert(edge.end.clone());
        self.edges.push(edge);
        self
    }
    pub fn add_end_node(mut self, node: N) -> Self {
        self.nodes.insert(node.clone());
        self.end_nodes.insert(node);
        self
    }
    pub fn rename(self) -> (StateMachine<usize, I>, HashMap<usize, N>)
    where
        N: Eq + Hash + Clone,
    {
        let map = HashMap::from_iter(self.nodes().cloned().enumerate());
        let reverse_map: HashMap<N, usize> =
            HashMap::from_iter(self.nodes().cloned().enumerate().map(|(a, b)| (b, a)));

        let mut res = StateMachine::new(*reverse_map.get(self.start_node()).unwrap());

        for node in self.nodes {
            res = res.add_node(*reverse_map.get(&node).unwrap());
        }
        for node in self.end_nodes {
            res = res.add_end_node(*reverse_map.get(&node).unwrap());
        }
        for edge in self.edges {
            res = res.add_edge(Edge::new(
                *reverse_map.get(&edge.beg).unwrap(),
                edge.item,
                *reverse_map.get(&edge.end).unwrap(),
            ));
        }

        (res, map)
    }
}

pub struct Nfa<N, I> {
    inner: StateMachine<N, Option<I>>,
}

impl<N, I> Nfa<N, I>
where
    N: Eq + Clone + Hash,
{
    pub fn new(start_node: N) -> Self {
        Self {
            inner: StateMachine::new(start_node),
        }
    }
    pub fn add_edge(self, edge: Edge<N, Option<I>>) -> Self {
        Self {
            inner: self.inner.add_edge(edge),
        }
    }
    pub fn add_node(self, node: N) -> Self {
        Self {
            inner: self.inner.add_node(node),
        }
    }
    pub fn add_end_node(self, node: N) -> Self {
        Self {
            inner: self.inner.add_end_node(node),
        }
    }
}

impl<N, I> Nfa<N, I>
where
    N: Eq + Hash + Clone + Debug,
    I: PartialEq + Clone + Debug,
{
    pub fn into_min_dfa(
        self,
        are_end_nodes_equivalent: bool,
    ) -> (Dfa<usize, I>, HashMap<usize, Vec<N>>) {
        let (dfa, dfa_to_old) = self.into_dfa();

        let (min, min_to_dfa) = dfa.inner.minimize(are_end_nodes_equivalent).rename();

        let mut map = HashMap::new();
        for (new_node, dfa_node_set) in min_to_dfa {
            let mut old_nodes = vec![];
            for dfa_node in dfa_node_set {
                old_nodes.append(&mut dfa_to_old.get(&dfa_node).unwrap().clone());
            }
            map.insert(new_node, old_nodes);
        }

        (
            Dfa {
                cur_node: *min.start_node(),
                inner: min,
            },
            map,
        )
    }
    pub fn into_dfa(self) -> (Dfa<usize, I>, HashMap<usize, Vec<N>>) {
        {
            if self.is_dfa() {
                let mut res = StateMachine::new(self.start_node().clone());
                for edge in self.inner.edges {
                    res = res.add_edge(Edge::new(edge.beg, edge.item.unwrap(), edge.end));
                }
                for node in self.inner.end_nodes {
                    res = res.add_end_node(node);
                }

                let (dfa, map) = Dfa {
                    cur_node: res.start_node.clone(),
                    inner: res,
                }
                .rename();
                (dfa, map.into_iter().map(|(k, v)| (k, vec![v])).collect())
            } else {
                let (dfa, map) = self.inner.remove_nones().determine().rename();
                (
                    Dfa {
                        cur_node: dfa.start_node,
                        inner: dfa,
                    },
                    map,
                )
            }
        }
    }
    pub fn is_dfa(&self) -> bool {
        self.nodes().all(|node| {
            let mut letters = vec![];
            for edge in self.edges_from(node) {
                if edge.item().is_none() {
                    return false;
                }
                for l in &letters {
                    if *l == edge.item().as_ref().unwrap() {
                        return false;
                    }
                }
                letters.push(edge.item().as_ref().unwrap());
            }
            true
        })
    }
}

impl<N, I> Nfa<N, I> {
    pub fn edges(&self) -> impl Iterator<Item = &Edge<N, Option<I>>> + '_ {
        self.inner.edges()
    }
    pub fn edges_from<'a, 'b>(
        &'a self,
        node: &'b N,
    ) -> impl Iterator<Item = &'a Edge<N, Option<I>>> + 'b
    where
        'a: 'b,
        N: PartialEq,
    {
        self.inner.edges_from(node)
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = &N> + '_ {
        self.inner.end_nodes()
    }
    pub fn nodes(&self) -> impl Iterator<Item = &N> + '_ {
        self.inner.nodes()
    }
    pub fn start_node(&self) -> &N {
        self.inner.start_node()
    }
    pub fn is_end_node(&self, node: &N) -> bool
    where
        N: Eq + Hash,
    {
        self.inner.is_end_node(node)
    }
    pub fn rename(self) -> (Nfa<usize, I>, HashMap<usize, N>)
    where
        N: Eq + Hash + Clone,
    {
        let (a, b) = self.inner.rename();
        (Nfa { inner: a }, b)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnionNode<N1, N2> {
    Node1(N1),
    Node2(N2),
    Start,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConcatNode<N1, N2> {
    Node1(N1),
    Node2(N2),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StarNode<N1> {
    Node(N1),
    Start,
}

impl<N1, I> Nfa<N1, I> {
    pub fn union<N2>(self, other: Nfa<N2, I>) -> Nfa<UnionNode<N1, N2>, I>
    where
        N1: Eq + Hash + Clone,
        N2: Eq + Hash + Clone,
    {
        let mut res = StateMachine::new(UnionNode::Start)
            .add_edge(Edge::new(
                UnionNode::Start,
                None,
                UnionNode::Node1(self.inner.start_node),
            ))
            .add_edge(Edge::new(
                UnionNode::Start,
                None,
                UnionNode::Node2(other.inner.start_node),
            ));

        for edge in self.inner.edges {
            res = res.add_edge(Edge::new(
                UnionNode::Node1(edge.beg),
                edge.item,
                UnionNode::Node1(edge.end),
            ));
        }
        for edge in other.inner.edges {
            res = res.add_edge(Edge::new(
                UnionNode::Node2(edge.beg),
                edge.item,
                UnionNode::Node2(edge.end),
            ));
        }
        for node in self.inner.nodes {
            res = res.add_node(UnionNode::Node1(node));
        }
        for node in other.inner.nodes {
            res = res.add_node(UnionNode::Node2(node));
        }

        for node in self.inner.end_nodes {
            res = res.add_end_node(UnionNode::Node1(node));
        }
        for node in other.inner.end_nodes {
            res = res.add_end_node(UnionNode::Node2(node));
        }

        Nfa { inner: res }
    }

    pub fn concat<N2>(self, other: Nfa<N2, I>) -> Nfa<ConcatNode<N1, N2>, I>
    where
        N1: Eq + Hash + Clone,
        N2: Eq + Hash + Clone,
    {
        let mut res = StateMachine::new(ConcatNode::Node1(self.inner.start_node));

        for node in self.inner.nodes {
            res = res.add_node(ConcatNode::Node1(node));
        }
        for node in self.inner.end_nodes {
            res = res.add_edge(Edge::new(
                ConcatNode::Node1(node.clone()),
                None,
                ConcatNode::Node2(other.start_node().clone()),
            ));
        }
        for edge in self.inner.edges {
            res = res.add_edge(Edge::new(
                ConcatNode::Node1(edge.beg),
                edge.item,
                ConcatNode::Node1(edge.end),
            ));
        }
        for node in other.inner.nodes {
            res = res.add_node(ConcatNode::Node2(node));
        }
        for node in other.inner.end_nodes {
            res = res.add_end_node(ConcatNode::Node2(node));
        }
        for edge in other.inner.edges {
            res = res.add_edge(Edge::new(
                ConcatNode::Node2(edge.beg),
                edge.item,
                ConcatNode::Node2(edge.end),
            ));
        }

        Nfa { inner: res }
    }

    pub fn star(self) -> Nfa<StarNode<N1>, I>
    where
        N1: Eq + Hash + Clone,
    {
        let mut res = StateMachine::new(StarNode::Start)
            .add_edge(Edge::new(
                StarNode::Start,
                None,
                StarNode::Node(self.start_node().clone()),
            ))
            .add_end_node(StarNode::Start);
        for node in self.inner.nodes {
            res = res.add_node(StarNode::Node(node));
        }
        for node in self.inner.end_nodes {
            res = res.add_edge(Edge::new(
                StarNode::Node(node.clone()),
                None,
                StarNode::Node(self.inner.start_node.clone()),
            ));
            res = res.add_end_node(StarNode::Node(node));
        }
        for edge in self.inner.edges {
            res = res.add_edge(Edge::new(
                StarNode::Node(edge.beg),
                edge.item,
                StarNode::Node(edge.end),
            ));
        }
        Nfa { inner: res }
    }
}

#[derive(Debug, Clone)]
pub struct Dfa<N, I> {
    inner: StateMachine<N, I>,
    cur_node: N,
}

impl<N, I> Dfa<N, I>
where
    I: PartialEq,
    N: Clone,
{
    pub fn traverse_step(&mut self, c: &I) -> bool
    where
        N: Eq + Hash,
    {
        let next_node = self
            .edges_from(&self.cur_node)
            .find(|e| e.item() == c)
            .map(|e| e.end().clone());

        match next_node {
            Some(next_node) => self.cur_node = next_node,
            None => return false,
        }

        true
    }
    pub fn cur_node(&self) -> &N {
        &self.cur_node
    }
    pub fn reset(&mut self) {
        self.cur_node = self.start_node().clone();
    }
}

impl<N, I> Dfa<N, I> {
    pub fn into_nfa(self) -> Nfa<N, I>
    where
        N: Eq + Hash + Clone,
    {
        let inner = self.inner;
        let mut res = StateMachine::new(inner.start_node);
        for node in inner.nodes {
            res = res.add_node(node);
        }
        for node in inner.end_nodes {
            res = res.add_end_node(node);
        }
        for edge in inner.edges {
            res = res.add_edge(Edge::new(edge.beg, Some(edge.item), edge.end));
        }
        Nfa { inner: res }
    }

    pub fn edges(&self) -> impl Iterator<Item = &Edge<N, I>> + '_ {
        self.inner.edges()
    }
    pub fn edges_from<'a, 'b>(&'a self, node: &'b N) -> impl Iterator<Item = &'a Edge<N, I>> + 'b
    where
        'a: 'b,
        N: PartialEq,
    {
        self.inner.edges_from(node)
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = &N> + '_ {
        self.inner.end_nodes()
    }
    pub fn nodes(&self) -> impl Iterator<Item = &N> + '_ {
        self.inner.nodes()
    }
    pub fn start_node(&self) -> &N {
        self.inner.start_node()
    }
    pub fn is_end_node(&self, node: &N) -> bool
    where
        N: Eq + Hash,
    {
        self.inner.is_end_node(node)
    }
    pub fn node_count(&self) -> usize {
        self.inner.node_count()
    }
    pub fn edge_count(&self) -> usize {
        self.inner.edge_count()
    }
    pub fn rename(self) -> (Dfa<usize, I>, HashMap<usize, N>)
    where
        N: Eq + Hash + Clone,
    {
        let cur_state = self.cur_node;
        let (a, b) = self.inner.rename();
        (
            Dfa {
                inner: a,
                cur_node: *b
                    .iter()
                    .find(|(_, v)| *v == &cur_state)
                    .map(|(k, _)| k)
                    .unwrap(),
            },
            b,
        )
    }
}

pub trait Print {
    fn print(&self) -> String;
    fn get_type() -> String;
}

impl<T> Print for T
where
    T: ToString,
{
    fn print(&self) -> String {
        self.to_string()
    }
    fn get_type() -> String {
        std::any::type_name::<T>().to_string()
    }
}

impl<N, I> Display for Nfa<N, I>
where
    N: Print,
    I: Print,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{\n  node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];")?;
        writeln!(
            f,
            "  node_type = \"{}\";\n  item_type = \"{}\";\n  kind = nfa;",
            N::get_type(),
            I::get_type()
        )?;

        for node in self.nodes() {
            writeln!(f, "  \"{}\";", node.print())?;
        }
        writeln!(f, "  Q1 -> \"{}\";", self.start_node().print())?;

        for edge in self.edges() {
            write!(
                f,
                "  \"{}\" -> \"{}\"",
                edge.beg().print(),
                edge.end().print(),
            )?;
            if let Some(l) = edge.item() {
                write!(f, " [label=\"{}\"]", l.print())?;
            }
            writeln!(f, ";")?;
        }
        for node in self.end_nodes() {
            writeln!(f, "  \"{}\" [shape=doublecircle];", node.print())?;
        }

        writeln!(f, "}}")
    }
}

impl<N, I> Display for Dfa<N, I>
where
    N: Print,
    I: Print,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{\n  node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];")?;
        writeln!(
            f,
            "  node_type = \"{}\";\n  item_type = \"{}\";\n  kind = dfa;",
            N::get_type(),
            I::get_type()
        )?;

        writeln!(f, "  Q1 -> \"{}\";", self.start_node().print())?;

        for edge in self.edges() {
            writeln!(
                f,
                "  \"{}\" -> \"{}\" [label=\"{}\"];",
                edge.beg().print(),
                edge.end().print(),
                edge.item().print(),
            )?;
        }
        for node in self.end_nodes() {
            writeln!(f, "  \"{}\" [shape=doublecircle];", node.print())?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Regex {
    Char(char),
    Range(char, char),
    Maybe(Box<Regex>),
    Seq(Vec<Regex>),
    Or(Vec<Regex>),
    Star(Box<Regex>),
}

impl Regex {
    pub fn into_nfa(self) -> Nfa<usize, char> {
        match self {
            Regex::Char(c) => Nfa::new(0)
                .add_edge(Edge::new(0, Some(c), 1))
                .add_end_node(1),
            Self::Range(min, max) => {
                assert!(min.is_ascii());
                assert!(max.is_ascii());
                let min = min as u8;
                let max = max as u8;
                let mut nfa = Nfa::new(0);

                for (i, c) in (min..=max).enumerate() {
                    nfa = nfa
                        .add_edge(Edge::new(0, Some(c as char), i))
                        .add_end_node(i);
                }

                nfa
            }
            Regex::Seq(seq) => {
                let mut nfa = Nfa::new(0).add_end_node(0);
                for r in seq {
                    (nfa, _) = nfa.concat(r.into_nfa()).rename();
                }

                nfa
            }
            Regex::Or(mut options) => {
                let mut nfa = options.pop().unwrap().into_nfa();
                while let Some(r) = options.pop() {
                    (nfa, _) = nfa.union(r.into_nfa()).rename();
                }

                nfa
            }
            Regex::Star(r) => r.into_nfa().star().rename().0,
            Regex::Maybe(r) => {
                let mut nfa = r.into_nfa();
                let end_nodes: Vec<_> = nfa.end_nodes().cloned().collect();
                let start = *nfa.start_node();
                for end in end_nodes {
                    nfa = nfa.add_edge(Edge::new(start, None, end));
                }
                nfa
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn regex() {
        let r = Regex::Seq(vec![
            Regex::Star(Box::new(Regex::Or(vec![
                Regex::Char('a'),
                Regex::Char('b'),
            ]))),
            Regex::Char('a'),
        ]);
        let (mut g, _) = r.into_nfa().into_min_dfa(true);
        assert!(test_dfa(&mut g, "a"));
        assert!(test_dfa(&mut g, "aa"));
        assert!(test_dfa(&mut g, "ba"));
        assert!(test_dfa(&mut g, "aaa"));
        assert!(test_dfa(&mut g, "baa"));
        assert!(test_dfa(&mut g, "aba"));
        assert!(test_dfa(&mut g, "abababababababaabababababaa"));
        assert!(!test_dfa(&mut g, "b"));
        assert!(!test_dfa(&mut g, "c"));
        assert!(!test_dfa(&mut g, ""));

        let g = Regex::Or(vec![
            Regex::Seq(vec![Regex::Char('i'), Regex::Char('f')]),
            Regex::Seq(vec![
                Regex::Char('i'),
                Regex::Char('f'),
                Regex::Char('f'),
                Regex::Char('y'),
            ]),
        ])
        .into_nfa();

        let (mut g, _) = g.into_min_dfa(false);

        assert!(!test_dfa(&mut g, ""));
        assert!(!test_dfa(&mut g, "1"));
        assert!(test_dfa(&mut g, "if"));
        assert!(test_dfa(&mut g, "iffy"));
        assert!(!test_dfa(&mut g, "iff"));
    }

    fn test_dfa(g: &mut Dfa<usize, char>, s: &str) -> bool {
        for c in s.chars() {
            if !g.traverse_step(&c) {
                g.reset();
                return false;
            }
        }
        let res = g.is_end_node(g.cur_node());
        g.reset();
        res
    }

    #[test]
    fn min_dfa() {
        let g = Nfa::new(0)
            .add_edge(Edge::new(0, Some(0), 3))
            .add_edge(Edge::new(3, Some(0), 0))
            .add_edge(Edge::new(0, Some(1), 1))
            .add_edge(Edge::new(3, Some(1), 4))
            .add_edge(Edge::new(4, Some(1), 5))
            .add_edge(Edge::new(5, Some(0), 5))
            .add_edge(Edge::new(5, Some(1), 5))
            .add_edge(Edge::new(4, Some(0), 2))
            .add_edge(Edge::new(2, Some(1), 5))
            .add_edge(Edge::new(2, Some(0), 2))
            .add_edge(Edge::new(1, Some(0), 2))
            .add_edge(Edge::new(1, Some(1), 5))
            .add_end_node(4)
            .add_end_node(2)
            .add_end_node(1);

        let (g, map) = g.into_min_dfa(true);

        assert!(g.clone().into_nfa().is_dfa());
        assert_eq!(g.node_count(), 3);
        assert_eq!(
            map.get(g.start_node())
                .unwrap()
                .iter()
                .cloned()
                .collect::<HashSet<_>>(),
            HashSet::from([0, 3])
        );
    }
}
