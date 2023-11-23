use std::{fmt::Display, io::Cursor, io::Write};

use crate::{regex::state_machine::StateMachine, tokenizer::Token};

use super::grammar::TokenOrEnd;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bracket {
    Open(usize),
    Closed(usize),
    Wildcard,
}

impl Bracket {
    pub fn new(index: usize, is_open: bool) -> Self {
        if is_open {
            Self::Open(index)
        } else {
            Self::Closed(index)
        }
    }
    pub fn is_open(&self) -> bool {
        matches!(self, Self::Open(_))
    }
    pub fn index(&self) -> Option<usize> {
        match self {
            Bracket::Open(i) => Some(*i),
            Bracket::Closed(i) => Some(*i),
            Bracket::Wildcard => None,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Item {
    tok: Option<TokenOrEnd>,
    look_ahead: Option<Vec<TokenOrEnd>>,
    bracket: Option<Bracket>,
}

impl Item {
    pub fn new(
        tok: Option<TokenOrEnd>,
        look_ahead: Option<Vec<TokenOrEnd>>,
        bracket: Option<Bracket>,
    ) -> Self {
        Self {
            tok,
            look_ahead,
            bracket,
        }
    }

    pub fn tok(&self) -> Option<TokenOrEnd> {
        self.tok.clone()
    }
    pub fn bracket(&self) -> Option<Bracket> {
        self.bracket.clone()
    }
    pub fn look_ahead(&self) -> Option<&[TokenOrEnd]> {
        self.look_ahead.as_ref().map(|t| t.as_slice())
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
    pub fn top(&self) -> Option<usize> {
        self.stack.last().cloned()
    }

    pub fn try_accept(mut self, b: Bracket) -> (Self, bool) {
        if b.is_open() {
            self.stack.push(b.index().unwrap());
            return (self, true);
        } else if let Some(top) = self.stack.last() {
            if b.index().map_or(false, |i| i != *top) {
                return (self, false);
            }
            self.stack.pop();
            return (self, true);
        } else {
            return (self, false);
        }
    }
    pub fn try_accept_mut(&mut self, b: Bracket) -> bool {
        if b.is_open() {
            self.stack.push(b.index().unwrap());
            return true;
        } else if let Some(top) = self.stack.last() {
            if b.index().map_or(false, |i| i != *top) {
                return false;
            }
            self.stack.pop();
            return true;
        } else {
            return false;
        }
    }
    pub fn can_accept(&self, b: Bracket) -> bool {
        if b.is_open() {
            return true;
        } else if let Some(top) = self.stack.last() {
            if b.index().map_or(false, |i| i != *top) {
                return false;
            }
            return true;
        } else {
            return false;
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

impl Display for Lgraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{\n  node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];")?;

        writeln!(f, "  Q1 -> \"{}\";", self.start_nodes().next().unwrap())?;

        for (a, l, b) in self.edges() {
            write!(f, "{a} -> {b}")?;
            let mut label: Vec<u8> = vec![];
            let mut w1 = Cursor::new(&mut label);
            let w1 = &mut w1;
            let mut attribs: Vec<u8> = vec![];
            let mut w2 = Cursor::new(&mut attribs);
            let w2 = &mut w2;
            if let Some(tok) = l.tok() {
                write!(w2, "token=\"{}\", ", tok).unwrap();
                write!(w1, "{}\\n", tok).unwrap();
            }
            if let Some(bracket) = l.bracket() {
                let idx = bracket.index().map_or("*".to_string(), |i| i.to_string());
                write!(
                    w2,
                    "bracket_idx={}, bracket_open={}, ",
                    idx,
                    bracket.is_open()
                )
                .unwrap();
                if bracket.is_open() {
                    write!(w1, "({}\\n", idx).unwrap();
                } else {
                    write!(w1, "){}\\n", idx).unwrap();
                }
            }

            if let Some(look_ahead) = l.look_ahead() {
                write!(w2, "look_ahead=\"[").unwrap();
                write!(w1, "[").unwrap();
                let look_ahead_len = look_ahead.iter().count();
                for (i, tok) in look_ahead.iter().enumerate() {
                    write!(w1, "{tok}").unwrap();
                    write!(w2, "{tok}").unwrap();
                    if i + 1 < look_ahead_len {
                        write!(w2, ",").unwrap();
                        write!(w1, ",").unwrap();
                    }
                }
                write!(w2, "]\"").unwrap();
                write!(w1, "]").unwrap();
            }
            writeln!(
                f,
                " [label=\"{}\", {}];",
                String::from_utf8(label).unwrap(),
                String::from_utf8(attribs).unwrap()
            )?;
        }
        for node in self.end_nodes() {
            writeln!(f, "  \"{}\" [shape=doublecircle];", node).unwrap();
        }

        writeln!(f, "}}").unwrap();

        Ok(())
    }
}
