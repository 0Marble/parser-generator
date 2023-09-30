use std::collections::{HashMap, HashSet};

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
}

struct FirstLetterParser {
    g: Lgraph,
    edge_direct: HashMap<usize, HashSet<Letter>>,
}

impl FirstLetterParser {
    fn new(g: Lgraph) -> Self {
        let mut edge_direct: HashMap<usize, HashSet<Letter>> = HashMap::new();

        for cur_edge in g.edges() {
            let mut next_edges = vec![cur_edge];
            let mut encountered_edges = HashSet::from([cur_edge.idx]);

            while let Some(edge) = next_edges.pop() {
                if let Some(letter) = edge.letter {
                    assert!(
                        edge_direct.entry(cur_edge.idx).or_default().insert(letter),
                        "conflict for node{:?}",
                        cur_edge.from
                    );
                } else {
                    for next_edge in g.edges_from(edge.to) {
                        if encountered_edges.insert(next_edge.idx) {
                            next_edges.push(next_edge);
                        }
                    }
                }
            }
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
                    if self
                        .edge_direct
                        .get(&e.idx)
                        .expect(&format!("No direct for edge {:?}", e))
                        .contains(letter)
                    {
                        if let Some(b) = e.bracket {
                            assert!(stack.push(b), "Inbalanced brackets: {:?}, {:?}", stack, e);
                        }
                        cur_node = e.to;
                        res.push(e.idx);
                        if e.letter == Some(*letter) {
                            continue 'LETTERS;
                        } else {
                            continue 'EDGES;
                        }
                    }
                }
                unreachable!(
                    "Can not continue from node {:?} on letter {:?}",
                    cur_node, letter
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
    fn basic_parser() {
        let g = Lgraph {
            edges: vec![
                Edge::new(0, Node::new(0), Node::new(1), Some(Letter::new(1)), None),
                Edge::new(4, Node::new(1), Node::new(3), Some(Letter::new(3)), None),
                Edge::new(3, Node::new(2), Node::new(1), None, None),
                Edge::new(
                    2,
                    Node::new(0),
                    Node::new(2),
                    None,
                    Some(Bracket::Closed(0)),
                ),
                Edge::new(
                    5,
                    Node::new(2),
                    Node::new(2),
                    Some(Letter::new(1)),
                    Some(Bracket::Closed(0)),
                ),
                Edge::new(
                    1,
                    Node::new(0),
                    Node::new(0),
                    Some(Letter::new(2)),
                    Some(Bracket::Open(0)),
                ),
            ],
            node_count: 4,
            start: 0,
            end_nodes: [3].into(),
        };

        let p = FirstLetterParser::new(g);
        assert_eq!(p.parse(&letters([1, 3])), vec![0, 4]);
        assert_eq!(p.parse(&letters([2, 3])), vec![1, 2, 3, 4]);
        assert_eq!(p.parse(&letters([2, 2, 1, 3])), vec![1, 1, 2, 5, 3, 4]);
    }
    fn letters(nums: impl IntoIterator<Item = u32>) -> Vec<Letter> {
        nums.into_iter().map(Letter::new).collect()
    }
}
