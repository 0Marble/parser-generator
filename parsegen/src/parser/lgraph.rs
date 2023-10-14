use crate::tokenizer::Token;

//
pub struct Lgraph {
    edges: Vec<(usize, Option<(usize, bool)>, Option<Token>, usize)>,
    start: usize,
    ends: Vec<usize>,
    nodes: Vec<usize>,
}

impl Lgraph {
    pub fn new() -> Self {
        Self {
            edges: vec![],
            start: 0,
            ends: vec![],
            nodes: vec![],
        }
    }
    pub fn set_start_node(mut self, node: usize) -> Self {
        if !self.nodes.contains(&node) {
            self.nodes.push(node);
        }
        self.start = node;
        self
    }
}

