use std::{fmt::Display, io::Cursor, io::Write};

use crate::{regex::state_machine::StateMachine, tokenizer::Token};

use super::grammar::{Grammar, Node, ParseTree, TokenOrEnd};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bracket {
    Open(usize),
    Closed(usize),
    Wildcard,
}

impl Display for Bracket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bracket::Open(i) => write!(f, "({i}"),
            Bracket::Closed(i) => write!(f, "){i}"),
            Bracket::Wildcard => write!(f, ")*"),
        }
    }
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

    /// Returns `true` if the bracket is [`Wildcard`].
    ///
    /// [`Wildcard`]: Bracket::Wildcard
    #[must_use]
    pub fn is_wildcard(&self) -> bool {
        matches!(self, Self::Wildcard)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Item {
    tok: Option<TokenOrEnd>,
    look_ahead: Option<Vec<TokenOrEnd>>,
    bracket: Option<Bracket>,
    output: Option<Node>,
}

impl Item {
    pub fn new(
        tok: Option<TokenOrEnd>,
        look_ahead: Option<Vec<TokenOrEnd>>,
        bracket: Option<Bracket>,
        output: Option<Node>,
    ) -> Self {
        Self {
            tok,
            look_ahead,
            bracket,
            output,
        }
    }
    pub fn with_token(mut self, tok: Option<TokenOrEnd>) -> Self {
        self.tok = tok;
        self
    }
    pub fn with_look_ahead(mut self, look_ahead: Option<Vec<TokenOrEnd>>) -> Self {
        self.look_ahead = look_ahead;
        self
    }
    pub fn with_bracket(mut self, bracket: Option<Bracket>) -> Self {
        self.bracket = bracket;
        self
    }
    pub fn with_output(mut self, output: Option<Node>) -> Self {
        self.output = output;
        self
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

    pub fn is_distinguishable(&self, other: &Self) -> bool {
        let by_brackets = match (self.bracket(), other.bracket()) {
            (None, None) => false,
            (None, Some(b)) => !b.is_open(),
            (Some(b), None) => !b.is_open(),
            (Some(a), Some(b)) => {
                !(a.is_open()
                    || b.is_open()
                    || a.index() == b.index()
                    || a.is_wildcard()
                    || b.is_wildcard())
            }
        };

        let by_letters = match (
            self.tok(),
            self.look_ahead(),
            other.tok(),
            other.look_ahead(),
        ) {
            (Some(a), None, Some(b), None) => a != b,
            (None, Some(la), None, Some(lb)) => la.iter().all(|la| !lb.contains(la)),
            (None, Some(la), Some(b), None) => !la.contains(&b),
            (None, Some(la), Some(b), Some(_)) => !la.contains(&b),
            (Some(a), None, None, Some(lb)) => !lb.contains(&a),
            (Some(a), None, Some(b), Some(_)) => a != b,
            (Some(a), Some(_), None, Some(lb)) => !lb.contains(&a),
            (Some(a), Some(_), Some(b), None) => a != b,
            (Some(a), Some(la), Some(b), Some(lb)) => {
                a != b && la.iter().all(|la| !lb.contains(la))
            }
            _ => false,
        };

        by_brackets || by_letters
    }

    fn output(&self) -> Option<Node> {
        self.output.clone()
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
pub struct Lgraph {
    inner: StateMachine<Item>,
    node_labels: Vec<(usize, String)>,
}

impl Lgraph {
    pub fn add_edge(mut self, from: usize, letter: Item, to: usize) -> Self {
        self.inner = self.inner.add_edge(from, letter, to);
        self
    }

    pub fn add_end_node(mut self, n: usize) -> Self {
        self.inner = self.inner.add_end_node(n);
        self
    }

    pub fn add_node(mut self, n: usize) -> Self {
        self.inner = self.inner.add_node(n);
        self
    }

    pub fn add_start_node(mut self, n: usize) -> Self {
        self.inner = self.inner.add_start_node(n);
        self
    }

    pub fn edges(&self) -> impl Iterator<Item = (usize, Item, usize)> + '_ {
        self.inner.edges()
    }

    pub fn edges_from(&self, n: usize) -> impl Iterator<Item = (usize, Item, usize)> + '_ {
        self.inner.edges_from(n)
    }

    pub fn end_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.inner.end_nodes()
    }

    pub fn is_end_node(&self, n: usize) -> bool {
        self.inner.is_end_node(n)
    }

    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.inner.nodes()
    }

    pub fn start_nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.inner.start_nodes()
    }

    pub fn set_node_label(mut self, node: usize, label: impl ToString) -> Self {
        if let Some((_, old)) = self.node_labels.iter_mut().find(|(n, _)| *n == node) {
            *old = label.to_string();
        } else {
            self.node_labels.push((node, label.to_string()));
        }
        self
    }

    pub fn is_deterministic(&self) -> Option<usize> {
        for node in self.nodes() {
            let mut items: Vec<Item> = vec![];
            for (_, item, _) in self.edges_from(node) {
                for other_item in &items {
                    if !other_item.is_distinguishable(&item) {
                        return Some(node);
                    }
                }
                items.push(item);
            }
        }

        None
    }
}

impl Display for Lgraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{\n  node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];")?;

        writeln!(f, "  Q1 -> \"{}\";", self.start_nodes().next().unwrap())?;

        for (node, label) in self.node_labels.iter() {
            writeln!(f, "  {node} [label=\"{label}\", shape=record];")?;
        }

        for (a, l, b) in self.edges() {
            write!(f, "  {a} -> {b}")?;
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
            if let Some(out) = l.output() {
                write!(w2, "output=\"{}\", ", out).unwrap();
                write!(w1, "<{}>\\n", out).unwrap();
            }

            if let Some(bracket) = l.bracket() {
                let idx = bracket
                    .index()
                    .map_or("\"\\*\"".to_string(), |i| i.to_string());
                write!(
                    w2,
                    "bracket_idx={}, bracket_open={}, ",
                    idx,
                    bracket.is_open()
                )
                .unwrap();

                let idx = bracket.index().map_or("\\*".to_string(), |i| i.to_string());

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Path {
    Empty(usize),
    NonEmpty(Vec<(usize, Item, usize)>),
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::Empty(n) => write!(f, "->{n}->"),
            Path::NonEmpty(e) => {
                write!(f, "->{}-", e[0].0)?;

                for (_, item, to) in e {
                    if let Some(tok) = item.tok() {
                        write!(f, "{tok}")?;
                    }
                    if let Some(lk) = item.look_ahead() {
                        write!(f, "[")?;
                        for tok in lk {
                            write!(f, "{tok},")?;
                        }
                        write!(f, "]")?;
                    }
                    if let Some(b) = item.bracket() {
                        write!(f, "{b}")?;
                    }

                    write!(f, "->{to}-")?;
                }

                write!(f, ">")
            }
        }
    }
}

impl Path {
    pub fn first(&self) -> usize {
        match self {
            Path::Empty(n) => *n,
            Path::NonEmpty(e) => e.first().unwrap().0,
        }
    }
    pub fn last(&self) -> usize {
        match self {
            Path::Empty(n) => *n,
            Path::NonEmpty(e) => e.last().unwrap().2,
        }
    }

    pub fn append(self, edge: (usize, Item, usize)) -> Self {
        assert_eq!(self.last(), edge.0);

        match self {
            Path::Empty(_) => Self::NonEmpty(vec![edge]),
            Path::NonEmpty(mut edges) => {
                edges.push(edge);
                Self::NonEmpty(edges)
            }
        }
    }

    pub fn output(&self) -> impl Iterator<Item = Node> + '_ {
        match self {
            Path::Empty(_) => Either::A(std::iter::empty()),
            Path::NonEmpty(edges) => Either::B(edges.iter().flat_map(|(_, item, _)| item.output())),
        }
    }
    pub fn postfix_output(&self) -> impl Iterator<Item = Node> + '_ {
        self.output().filter(|n| !n.is_rule_start())
    }

    pub fn to_parse_tree(&self, g: &Grammar) -> ParseTree {
        let mut stack = vec![];
        for n in self.postfix_output() {
            match n {
                Node::Leaf(t) => stack.push(ParseTree::new(t)),
                Node::RuleEnd(prod_idx, _) => {
                    let prod = g.productions().nth(prod_idx).unwrap();
                    let mut parent = ParseTree::new(prod.lhs());
                    parent.replace_with_production(0, prod, prod_idx);

                    let body = stack.split_off(stack.len() - prod.rhs().len());
                    assert_eq!(body.len(), prod.rhs().len());
                    for (i, child) in body.into_iter().enumerate().rev() {
                        parent.replace_with_subtree(i + 1, child);
                    }
                    stack.push(parent);
                }
                _ => unreachable!(),
            }
        }

        assert_eq!(stack.len(), 1);
        stack.pop().unwrap()
    }

    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        match self {
            Path::Empty(n) => Either::A(std::iter::once(*n)),
            Path::NonEmpty(edges) => Either::B(
                edges
                    .first()
                    .map(|(from, _, _)| *from)
                    .into_iter()
                    .chain(edges.iter().map(|(_, _, to)| *to)),
            ),
        }
    }
    pub fn edges(&self) -> impl Iterator<Item = (usize, Item, usize)> + '_ {
        match self {
            Path::Empty(_) => Either::A(std::iter::empty()),
            Path::NonEmpty(edges) => Either::B(edges.iter().cloned()),
        }
    }
}

enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B, C> Iterator for Either<A, B>
where
    A: Iterator<Item = C>,
    B: Iterator<Item = C>,
{
    type Item = C;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Either::A(it) => it.next(),
            Either::B(it) => it.next(),
        }
    }
}

impl Lgraph {
    pub fn possible_words<'a, 'b>(&'a self, grammar: &'b Grammar) -> PossibleWords
    where
        'b: 'a,
    {
        PossibleWords {
            lg: self,
            stack: Default::default(),
            next_stack: Vec::from([(
                Path::Empty(self.start_nodes().next().unwrap()),
                Stack::default(),
                None,
            )]),
            grammar,
        }
    }
}

pub struct PossibleWords<'a> {
    lg: &'a Lgraph,
    stack: Vec<(Path, Stack, Option<TokenOrEnd>)>,
    next_stack: Vec<(Path, Stack, Option<TokenOrEnd>)>,
    grammar: &'a Grammar,
}

impl<'a> Iterator for PossibleWords<'a> {
    type Item = (Vec<Token>, String);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.stack.is_empty() {
                if self.next_stack.is_empty() {
                    return None;
                }
                std::mem::swap(&mut self.stack, &mut self.next_stack);
            }

            while let Some((path, stack, lookahead)) = self.stack.pop() {
                for (from, item, to) in self.lg.edges_from(path.last()) {
                    let top = stack.top();
                    let next_stack = if let Some(b) = item.bracket() {
                        let (s, ok) = stack.clone().try_accept(b);
                        if !ok {
                            continue;
                        }
                        s
                    } else {
                        stack.clone()
                    };

                    let next_lookaheads = if let Some(lookahead) = lookahead.as_ref() {
                        match (item.tok(), item.look_ahead()) {
                            (None, None) => Some(vec![lookahead.clone()]),
                            (None, Some(lk)) => {
                                if !lk.contains(lookahead) {
                                    continue;
                                } else {
                                    Some(lk.to_vec())
                                }
                            }
                            (Some(tok), None) => {
                                if &tok != lookahead {
                                    continue;
                                } else {
                                    None
                                }
                            }
                            (Some(tok), Some(lk)) => {
                                if &tok != lookahead {
                                    continue;
                                } else {
                                    Some(lk.to_vec())
                                }
                            }
                        }
                    } else {
                        item.look_ahead().map(|lk| lk.to_vec())
                    };

                    let item = if let Some(Bracket::Wildcard) = item.bracket() {
                        Item::new(
                            item.tok(),
                            item.look_ahead().map(|lk| lk.to_vec()),
                            Some(Bracket::Closed(top.unwrap())),
                            None,
                        )
                    } else {
                        item.clone()
                    };
                    if let Some(lk) = next_lookaheads {
                        for lk in lk {
                            self.next_stack.push((
                                path.clone().append((from, item.clone(), to)),
                                next_stack.clone(),
                                Some(lk.clone()),
                            ));
                        }
                    } else {
                        self.next_stack.push((
                            path.clone().append((from, item.clone(), to)),
                            next_stack.clone(),
                            None,
                        ));
                    }
                }

                if stack.top().is_some() || !self.lg.is_end_node(path.last()) {
                    continue;
                }
                let edges = if let Path::NonEmpty(edges) = path {
                    edges
                } else {
                    continue;
                };

                let terminal_count = self.grammar.terminals().count() + 1;
                let mut toks = vec![];
                let mut buf = vec![];
                let mut w = Cursor::new(&mut buf);
                let w = &mut w;

                for (_, item, _) in edges {
                    if let Some(b) = item.bracket() {
                        if b.is_open() {
                            continue;
                        }
                        let idx = b.index().unwrap();
                        if idx < terminal_count {
                            let t = self
                                .grammar
                                .terminals()
                                .nth(idx)
                                .map(TokenOrEnd::Token)
                                .unwrap_or(TokenOrEnd::End);
                            write!(w, "{t}, ").unwrap();
                            if let TokenOrEnd::Token(t) = t {
                                toks.push(t);
                            }
                        } else {
                            self.grammar
                                .productions()
                                .nth(idx - terminal_count)
                                .map(|p| write!(w, "{}[{}], ", p.lhs(), idx - terminal_count));
                        }
                    }
                }

                return Some((toks, String::from_utf8(buf).unwrap()));
            }
        }
    }
}
