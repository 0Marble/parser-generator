use std::{fmt::Display, rc::Rc};

pub mod codegen;
pub mod lexer;
pub mod parser;
// pub mod tokenizer;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    name: Rc<str>,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenError {
    NotAnIdentifier(String),
}

impl Token {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    // ('_' | letter) ('_' | letter | number)^* and not all undersocres
    pub fn new(name: impl ToString) -> Result<Self, TokenError> {
        let name = name.to_string();
        if !name.starts_with(|c: char| c == '_' || c.is_ascii_alphabetic()) {
            return Err(TokenError::NotAnIdentifier(name));
        }
        for c in name.chars() {
            if c != '_' && !c.is_ascii_alphabetic() && !c.is_ascii_digit() {
                return Err(TokenError::NotAnIdentifier(name));
            }
        }
        Ok(Self { name: name.into() })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TransitionScheme<I> {
    edges: Vec<Vec<(usize, usize)>>,
    back: Vec<Vec<(usize, usize)>>,
    nodes: Vec<usize>,
    items: Vec<I>,
    start: usize,
    ends: Vec<usize>,
}

impl<I> TransitionScheme<I> {
    pub fn new(start: usize) -> Self {
        Self {
            edges: vec![vec![]],
            back: vec![vec![]],
            nodes: vec![start],
            items: vec![],
            start: 0,
            ends: vec![],
        }
    }

    pub fn edges(&self) -> impl Iterator<Item = (usize, &I, usize)> {
        self.edges
            .iter()
            .enumerate()
            .flat_map(move |(from, edges)| {
                edges
                    .iter()
                    .cloned()
                    .map(move |(item, to)| (self.nodes[from], &self.items[item], self.nodes[to]))
            })
    }

    pub fn edges_from(&self, node: usize) -> impl Iterator<Item = (usize, &I, usize)> {
        self.node_idx(node)
            .map(|from| {
                self.edges[from]
                    .iter()
                    .cloned()
                    .map(move |(item, to)| (self.nodes[from], &self.items[item], self.nodes[to]))
            })
            .into_iter()
            .flatten()
    }

    pub fn edges_to(&self, node: usize) -> impl Iterator<Item = (usize, &I, usize)> {
        self.node_idx(node)
            .map(|to| {
                self.back[to]
                    .iter()
                    .cloned()
                    .map(move |(from, item)| (self.nodes[from], &self.items[item], self.nodes[to]))
            })
            .into_iter()
            .flatten()
    }

    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.nodes.iter().cloned()
    }
    pub fn has_node(&self, node: usize) -> bool {
        self.node_idx(node).is_some()
    }

    pub fn start(&self) -> usize {
        self.nodes[self.start]
    }

    pub fn ends(&self) -> impl Iterator<Item = usize> + '_ {
        self.ends.iter().cloned().map(|i| self.nodes[i])
    }
    pub fn is_end_node(&self, node: usize) -> bool {
        let node = if let Some(node) = self.node_idx(node) {
            node
        } else {
            return false;
        };
        self.ends.contains(&node)
    }

    fn node_idx(&self, node: usize) -> Option<usize> {
        let (i, _) = self.nodes.iter().enumerate().find(|(_, n)| **n == node)?;
        Some(i)
    }
    fn add_node_get_idx(&mut self, node: usize) -> usize {
        assert!(self.nodes.len() == self.edges.len());
        match self.node_idx(node) {
            Some(i) => i,
            None => {
                self.nodes.push(node);
                self.edges.push(vec![]);
                self.back.push(vec![]);
                self.nodes.len() - 1
            }
        }
    }

    pub fn add_edge(mut self, from: usize, item: I, to: usize) -> Self {
        let from = self.add_node_get_idx(from);
        let to = self.add_node_get_idx(to);
        self.edges[from].push((self.items.len(), to));
        self.back[to].push((from, self.items.len()));
        self.items.push(item);

        self
    }

    pub fn add_node(mut self, node: usize) -> Self {
        self.add_node_get_idx(node);
        self
    }

    pub fn add_end(mut self, node: usize) -> Self {
        let node = self.add_node_get_idx(node);
        self.ends.push(node);

        self
    }

    pub fn set_start(mut self, node: usize) -> Self {
        self.start = self.add_node_get_idx(node);
        self
    }

    pub fn rename(mut self, f: impl Fn(usize) -> usize) -> Self {
        for node in &mut self.nodes {
            *node = f(*node);
        }

        self
    }
}
