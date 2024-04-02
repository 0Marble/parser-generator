#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Idx(usize);

#[derive(Debug, PartialEq, Eq)]
pub struct Automata<N, I> {
    edges: Vec<Vec<(Idx, Idx)>>,
    back_edges: Vec<Vec<(Idx, Idx)>>,
    items: Vec<I>,
    nodes: Vec<N>,
    start: Idx,
    ends: Vec<Idx>,
}

impl<N, I> Automata<N, I> {
    pub fn new(start: N) -> Self {
        Self {
            edges: vec![vec![]],
            back_edges: vec![vec![]],
            items: vec![],
            nodes: vec![start],
            start: Idx(0),
            ends: vec![],
        }
    }

    pub fn start(&self) -> Idx {
        self.start
    }
    pub fn nodes(&self) -> impl Iterator<Item = Idx> + '_ {
        self.nodes.iter().enumerate().map(|(i, _)| Idx(i))
    }
    pub fn edges(&self) -> impl Iterator<Item = (Idx, Idx, Idx)> + '_ {
        self.edges.iter().enumerate().flat_map(|(from, edges)| {
            edges
                .iter()
                .cloned()
                .map(move |(item, to)| (Idx(from), item, to))
        })
    }
    pub fn edges_from(&self, from: Idx) -> impl Iterator<Item = (Idx, Idx, Idx)> + '_ {
        self.edges.get(from.0).into_iter().flat_map(move |edges| {
            edges
                .iter()
                .cloned()
                .map(move |(item, to)| (from, item, to))
        })
    }
    pub fn edges_to(&self, to: Idx) -> impl Iterator<Item = (Idx, Idx, Idx)> + '_ {
        self.back_edges
            .get(to.0)
            .into_iter()
            .flat_map(move |edges| {
                edges
                    .iter()
                    .cloned()
                    .map(move |(item, from)| (from, item, to))
            })
    }
    pub fn ends(&self) -> impl Iterator<Item = Idx> + '_ {
        self.ends.iter().cloned()
    }
    pub fn is_end(&self, node: Idx) -> bool {
        self.ends.contains(&node)
    }
    pub fn node(&self, index: Idx) -> Option<&N> {
        self.nodes.get(index.0)
    }
    pub fn item(&self, index: Idx) -> Option<&I> {
        self.items.get(index.0)
    }

    pub fn add_node(&mut self, node: N) -> Idx {
        self.nodes.push(node);
        self.edges.push(vec![]);
        self.back_edges.push(vec![]);
        Idx(self.nodes.len() - 1)
    }
    pub fn add_edge(&mut self, from: Idx, item: I, to: Idx) -> Idx {
        assert!(self.nodes.len() > from.0);
        assert!(self.nodes.len() > to.0);
        self.items.push(item);
        let item_idx = Idx(self.items.len() - 1);
        self.edges[from.0].push((item_idx, to));
        self.back_edges[to.0].push((from, item_idx));
        item_idx
    }
    pub fn set_end(&mut self, node: Idx) {
        assert!(self.nodes.len() > node.0);
        if self.ends.contains(&node) {
            return;
        }
        self.ends.push(node)
    }
    pub fn set_start(&mut self, node: Idx) {
        assert!(self.nodes.len() > node.0);
        self.start = node;
    }
    pub fn rename_nodes<M>(self, f: impl Fn(N, Idx) -> M) -> Automata<M, I> {
        Automata {
            edges: self.edges,
            back_edges: self.back_edges,
            items: self.items,
            nodes: self
                .nodes
                .into_iter()
                .enumerate()
                .map(|(i, n)| f(n, Idx(i)))
                .collect(),
            start: self.start,
            ends: self.ends,
        }
    }
    pub fn rename_edges<J>(self, f: impl Fn(I, Idx) -> J) -> Automata<N, J> {
        Automata {
            edges: self.edges,
            back_edges: self.back_edges,
            items: self
                .items
                .into_iter()
                .enumerate()
                .map(|(j, i)| f(i, Idx(j)))
                .collect(),
            nodes: self.nodes,
            start: self.start,
            ends: self.ends,
        }
    }
}

impl<N, I> Automata<N, I>
where
    I: Eq,
{
    pub fn find_edge(&self, edge: &I) -> Option<Idx> {
        self.items
            .iter()
            .enumerate()
            .find(|(_, item)| *item == edge)
            .map(|(i, _)| Idx(i))
    }
}

impl<N, I> Automata<N, I>
where
    N: Eq,
{
    pub fn find_node(&self, node: &N) -> Option<Idx> {
        self.nodes
            .iter()
            .enumerate()
            .find(|(_, n)| *n == node)
            .map(|(i, _)| Idx(i))
    }
}
