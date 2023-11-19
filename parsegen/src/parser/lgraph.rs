use crate::{regex::state_machine::StateMachine, tokenizer::Token};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bracket {
    index: usize,
    is_open: bool,
}

impl Bracket {
    pub fn new(index: usize, is_open: bool) -> Self {
        Self { index, is_open }
    }
    pub fn index(&self) -> usize {
        self.index
    }
    pub fn is_open(&self) -> bool {
        self.is_open
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Item {
    tok: Option<Token>,
    look_ahead: Vec<Option<Token>>,
    bracket: Option<Bracket>,
}

impl Item {
    pub fn new(
        tok: Option<Token>,
        look_ahead: Vec<Option<Token>>,
        bracket: Option<Bracket>,
    ) -> Self {
        Self {
            tok,
            look_ahead,
            bracket,
        }
    }
    pub fn tok(&self) -> Option<Token> {
        self.tok.clone()
    }
    pub fn bracket(&self) -> Option<Bracket> {
        self.bracket.clone()
    }
    pub fn look_ahead(&self) -> impl Iterator<Item = Option<Token>> + '_ {
        self.look_ahead.iter().cloned()
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct Stack {
    stack: Vec<usize>,
}

impl Stack {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn try_accept(mut self, b: Bracket) -> (Self, bool) {
        if b.is_open() {
            self.stack.push(b.index);
            return (self, true);
        } else if let Some(top) = self.stack.last() {
            if *top != b.index {
                return (self, false);
            }
            self.stack.pop();
            return (self, true);
        } else {
            return (self, false);
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Lgraph(StateMachine<Item>);

impl Lgraph {
    pub fn add_edge(self, from: usize, letter: Item, to: usize) -> Self {
        Self(self.0.add_edge(from, letter, to))
    }

    pub fn add_end_node(self, n: usize) -> Self {
        Self(self.0.add_end_node(n))
    }

    pub fn add_node(self, n: usize) -> Self {
        Self(self.0.add_node(n))
    }

    pub fn add_start_node(self, n: usize) -> Self {
        Self(self.0.add_start_node(n))
    }

    pub fn edges(&self) -> impl Iterator<Item = (usize, Item, usize)> + '_ {
        self.0.edges()
    }

    pub fn edges_from(&self, n: usize) -> impl Iterator<Item = (usize, Item, usize)> + '_ {
        self.0.edges_from(n)
    }

    pub fn end_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.end_nodes()
    }

    pub fn is_end_node(&self, n: usize) -> bool {
        self.0.is_end_node(n)
    }

    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.nodes()
    }

    pub fn start_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.start_nodes()
    }
}
