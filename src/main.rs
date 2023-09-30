mod regex;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use regex::{Nfa, Regex};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Letter {
    idx: u32,
}

impl Letter {
    fn new(idx: u32) -> Self {
        Self { idx }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Bracket {
    Open(u32),
    Closed(u32),
}

impl Bracket {
    fn is_closing(&self) -> bool {
        match self {
            Bracket::Open(_) => false,
            Bracket::Closed(_) => true,
        }
    }

    fn idx(&self) -> u32 {
        match self {
            Bracket::Open(i) => *i,
            Bracket::Closed(i) => *i,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Node {
    idx: usize,
}

impl Node {
    fn new(idx: usize) -> Self {
        Self { idx }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Edge {
    idx: usize,
    from: Node,
    to: Node,
    letter: Option<Letter>,
    bracket: Option<Bracket>,
}

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]{}-", self.idx, self.from.idx)?;
        if let Some(l) = self.letter {
            write!(f, "{}", l.idx)?;
        }
        if let Some(b) = self.bracket {
            match b {
                Bracket::Open(i) => write!(f, "({}", i)?,
                Bracket::Closed(i) => write!(f, "){}", i)?,
            }
        }

        write!(f, "->{}", self.to.idx)
    }
}

impl Edge {
    fn new(
        idx: usize,
        from: Node,
        to: Node,
        letter: Option<Letter>,
        bracket: Option<Bracket>,
    ) -> Self {
        Self {
            idx,
            from,
            to,
            letter,
            bracket,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Lgraph {
    edges: Vec<Edge>,
    node_count: usize,
    start: usize,
    end_nodes: Vec<usize>,
}

impl Lgraph {
    fn nodes(&self) -> impl Iterator<Item = Node> + '_ {
        (0..self.node_count).map(Node::new)
    }
    fn edges(&self) -> impl Iterator<Item = Edge> + '_ {
        self.edges.iter().cloned()
    }
    fn edges_from(&self, node: Node) -> impl Iterator<Item = Edge> + '_ {
        self.edges().filter(move |e| e.from == node)
    }
    fn start(&self) -> Node {
        Node::new(self.start)
    }
    fn end_nodes(&self) -> impl Iterator<Item = Node> + '_ {
        self.end_nodes.iter().cloned().map(Node::new)
    }
    fn is_end_node(&self, node: Node) -> bool {
        self.end_nodes.contains(&node.idx)
    }
    fn nth_edge(&self, n: usize) -> Option<Edge> {
        self.edges().find(|e| e.idx == n)
    }
    fn nth_node(&self, n: usize) -> Option<Node> {
        if self.node_count > n {
            return Some(Node::new(n));
        }
        None
    }
    fn from_bnf(rules: &[(char, Regex)]) -> (Self, Vec<(Node, Node, char)>) {
        // for each rule, create a DFA (use regex rules?),
        // with each edge marked with the letter from rule.
        // Say we have DFAs d1,d2,..dn, associated with rules r1,r2,...rn.
        // Connect the DFAs in the following way:
        //    for all edges in all dfas:
        //      if edge=(p, x, q), add it directly.
        //      if edge=(p, ri, q), add edges (p, -, (, beg(ri)), (end(ri), -, ), q)

        let mut between_edges = vec![];
        let mut edges = vec![];
        let mut node_count = 0;
        let mut skip_count = 0;
        let mut start_node = None;
        let mut end_nodes = vec![];
        let mut starts_and_ends = vec![];

        for (_, reg) in rules {
            let nfa = reg.clone().into_nfa();
            let (state_machine, _) = nfa.into_min_dfa(true);
            let mut state_machine = state_machine.into_nfa();

            if start_node.is_none() {
                start_node = Some(state_machine.start_node().clone() + node_count);
                end_nodes = state_machine.end_nodes().map(|n| n + node_count).collect();
            } else {
                (state_machine, _) = state_machine.concat(Nfa::new(0).add_end_node(0)).rename();
            }

            starts_and_ends.push((
                state_machine.start_node() + node_count,
                state_machine
                    .end_nodes()
                    .next()
                    .expect("Always has at least one node")
                    + node_count,
            ));

            for (_, edge) in state_machine.edges().enumerate() {
                let letter = edge.item().clone();

                if let Some(letter) = edge.item() {
                    if letter.is_uppercase() {
                        let (rule_idx, _) = rules
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| name == letter)
                            .expect(&format!("No rule '{}' given", letter));
                        between_edges.push((
                            edge.beg() + node_count,
                            edge.end() + node_count,
                            skip_count,
                            rule_idx,
                        ));
                        skip_count += 1;
                        continue;
                    }
                }
                edges.push((edge.beg() + node_count, letter, edge.end() + node_count));
            }

            node_count += state_machine.nodes().count();
        }

        let mut g = Lgraph {
            edges: vec![],
            node_count,
            start: start_node.expect("No rules given"),
            end_nodes,
        };

        for (i, edge) in edges.into_iter().enumerate() {
            let (from, letter, to) = edge;
            g.edges.push(Edge::new(
                i,
                Node::new(from),
                Node::new(to),
                letter.map(|c| Letter::new(c as u32)),
                None,
            ));
        }
        for (_, connection) in between_edges.into_iter().enumerate() {
            let (start, end, bracket_idx, rule) = connection;
            let (from, to) = &starts_and_ends[rule];
            g.edges.push(Edge::new(
                g.edges.len(),
                Node::new(start),
                Node::new(*from),
                None,
                Some(Bracket::Open(bracket_idx)),
            ));

            g.edges.push(Edge::new(
                g.edges.len(),
                Node::new(*to),
                Node::new(end),
                None,
                Some(Bracket::Closed(bracket_idx)),
            ));
        }

        (
            g,
            starts_and_ends
                .into_iter()
                .enumerate()
                .map(|(i, (start, end))| (Node::new(start), Node::new(end), rules[i].0))
                .collect(),
        )
    }
}

impl Display for Lgraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{")?;
        writeln!(
            f,
            "node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];"
        )?;
        writeln!(f, "Q1->{};", self.start().idx)?;
        for n in self.end_nodes() {
            writeln!(f, "{} [shape=doublecircle];", n.idx)?;
        }

        for e in self.edges() {
            write!(f, "{} -> {} [label=\"", e.from.idx, e.to.idx)?;
            if let Some(letter) = e.letter {
                write!(f, "{}", char::from_u32(letter.idx).unwrap())?;
            }
            if let Some(bracket) = e.bracket {
                match bracket {
                    Bracket::Open(i) => write!(f, "({}", i)?,
                    Bracket::Closed(i) => write!(f, "){}", i)?,
                }
            }
            writeln!(f, "\", idx={}];", e.idx)?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
struct Stack {
    s: Vec<u32>,
}

impl Stack {
    fn new() -> Self {
        Self { s: vec![] }
    }
    fn push(&mut self, b: Bracket) -> bool {
        match b {
            Bracket::Open(idx) => self.s.push(idx),
            Bracket::Closed(idx) => {
                if let Some(cur_idx) = self.s.pop() {
                    if cur_idx != idx {
                        self.s.push(cur_idx);
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        true
    }
    fn top(&self) -> Option<u32> {
        self.s.last().cloned()
    }
    fn is_balanced(&self) -> bool {
        self.s.is_empty()
    }
}

struct Parser {
    g: Lgraph,
    edge_direct: HashMap<usize, HashSet<(Letter, Option<u32>)>>,
}

impl Parser {
    fn new(g: Lgraph) -> Self {
        let mut edge_direct: HashMap<usize, HashSet<(Letter, Option<u32>)>> = HashMap::new();

        for node in g.nodes() {
            let mut local_direct: HashMap<_, HashSet<_>> = HashMap::new();

            for start_edge in g.edges_from(node) {
                let mut next = vec![(
                    start_edge,
                    None,
                    Stack::new(),
                    0,
                    HashSet::from([start_edge.idx]),
                )];

                while let Some((edge, mut bracket, mut stack, depth, mut visited)) = next.pop() {
                    println!("{}{}", " ".repeat(depth), edge);

                    if let Some(b) = edge.bracket {
                        if !stack.push(b) && bracket.is_none() {
                            assert!(b.is_closing(), "just in case");
                            bracket = Some(b.idx());
                        }
                    }

                    if let Some(l) = edge.letter {
                        local_direct
                            .entry(start_edge.idx)
                            .or_default()
                            .insert((l, bracket));
                    } else {
                        for next_edge in g.edges_from(edge.to) {
                            let mut visited_clone = visited.clone();
                            if visited_clone.insert(next_edge.idx) {
                                next.push((
                                    next_edge,
                                    bracket,
                                    stack.clone(),
                                    depth + 1,
                                    visited_clone,
                                ));
                            }
                        }
                    }
                }
            }

            // check for conflicts
            for (e1, dir1) in &local_direct {
                for (e2, dir2) in &local_direct {
                    if e1 == e2 {
                        continue;
                    }

                    for (l1, b1) in dir1 {
                        for (l2, b2) in dir2 {
                            assert!(
                                !(l1 == l2 && (b1 == b2 || b1.is_none() || b2.is_none())),
                                "Conflict for node={:?} between edges {} and {}",
                                node,
                                e1,
                                e2
                            );
                        }
                    }
                }
            }

            edge_direct.extend(local_direct);
        }

        dbg!(&edge_direct);
        Self { g, edge_direct }
    }

    fn parse(&self, s: &[Letter]) -> Vec<usize> {
        let mut cur_node = self.g.start();
        let mut res = vec![];
        let mut stack = Stack::new();

        'LETTERS: for letter in s {
            'EDGES: loop {
                for e in self.g.edges_from(cur_node) {
                    let direct = self
                        .edge_direct
                        .get(&e.idx)
                        .expect(&format!("No direct computed for {:?}", e));

                    for (l, b) in direct {
                        if l == letter && (b.is_none() || *b == stack.top()) {
                            res.push(e.idx);
                            cur_node = e.to;

                            if let Some(b) = e.bracket {
                                assert!(
                                    stack.push(b),
                                    "Unbalanced brackets, letter={:?}, path={:?}",
                                    letter,
                                    res
                                );
                            }

                            if let Some(l) = e.letter {
                                assert_eq!(l, *letter);
                                continue 'LETTERS;
                            } else {
                                continue 'EDGES;
                            }
                        }
                    }
                }

                unreachable!(
                    "Can not continue: letter={:?}, path={:?}, node={:?}, stack={:?}",
                    letter, res, cur_node, stack
                );
            }
        }

        assert!(
            self.g.is_end_node(cur_node),
            "Finished in not an end node {:?}, path={:?}, input={:?}",
            cur_node,
            res,
            s
        );
        assert!(stack.is_balanced());
        res
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bnf() {
        let bnf = [
            ('P', Regex::Seq(vec![Regex::Char('S'), Regex::Char('@')])),
            (
                'S',
                Regex::Or(vec![
                    Regex::Seq(vec![Regex::Char('T'), Regex::Char('+'), Regex::Char('S')]),
                    Regex::Char('T'),
                ]),
            ),
            (
                'T',
                Regex::Or(vec![
                    Regex::Seq(vec![Regex::Char('F'), Regex::Char('*'), Regex::Char('T')]),
                    Regex::Char('F'),
                ]),
            ),
            (
                'F',
                Regex::Or(vec![
                    Regex::Char('a'),
                    Regex::Char('b'),
                    Regex::Seq(vec![Regex::Char('('), Regex::Char('S'), Regex::Char(')')]),
                ]),
            ),
        ];

        let (g, starts_and_ends) = Lgraph::from_bnf(&bnf);
        println!("{}", g);

        let p = Parser::new(g);
        println!("{:?}", parse_tree(&p, "a*(b+a)@".chars(), &starts_and_ends));

        panic!()
    }

    fn parse_tree<I>(
        p: &Parser,
        s: impl IntoIterator<Item = I>,
        starts_and_ends: &[(Node, Node, char)],
    ) -> Vec<String>
    where
        I: Into<u32>,
    {
        let l = letters(s);
        let path = p.parse(&l);
        let mut nodes = vec![];

        let mut i = 0;
        for edge in path {
            let edge = p.g.nth_edge(edge).unwrap();

            let mut terminal = None;
            if edge.letter == l.get(i).cloned() {
                if let Some(l) = l.get(i) {
                    terminal = Some(format!("{}", char::from_u32(l.idx).unwrap()));
                }
                i += 1;
            }

            let mut start_node = None;
            let mut end_node = None;
            for (from, to, rule) in starts_and_ends {
                if to.idx == edge.to.idx {
                    end_node = Some(format!("{}]", rule));
                }
                if from.idx == edge.from.idx {
                    start_node = Some(format!("{}[", rule));
                }
            }

            if let Some(n) = start_node {
                nodes.push(n);
            }
            if let Some(t) = terminal {
                nodes.push(t);
            }
            if let Some(n) = end_node {
                nodes.push(n);
            }
        }

        nodes
    }

    fn letters<I>(nums: impl IntoIterator<Item = I>) -> Vec<Letter>
    where
        I: Into<u32>,
    {
        nums.into_iter().map(|l| Letter::new(l.into())).collect()
    }
}
