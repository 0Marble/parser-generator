use std::{collections::VecDeque, fmt::Display, io::Cursor, io::Write, str::FromStr};

use crate::tokenizer::Token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Production {
    lhs: Token,
    rhs: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProductionFromStrError {
    NoHead,
    NoArrow,
}
impl Display for ProductionFromStrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
impl std::error::Error for ProductionFromStrError {}

impl FromStr for Production {
    type Err = ProductionFromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let mut split = s.split_whitespace();
        let lhs = Token::new(split.next().ok_or_else(|| ProductionFromStrError::NoHead)?);
        let _ = split
            .next()
            .and_then(|a| if a == "->" { Some(()) } else { None })
            .ok_or_else(|| ProductionFromStrError::NoArrow)?;
        let mut rhs = vec![];
        for tok in split {
            rhs.push(Token::new(tok));
        }
        Ok(Self::new(lhs, rhs))
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.lhs())?;
        for tok in self.rhs() {
            write!(f, "{tok} ")?;
        }
        Ok(())
    }
}

impl Production {
    pub fn new(lhs: Token, rhs: Vec<Token>) -> Self {
        Self { lhs, rhs }
    }
    pub fn lhs(&self) -> Token {
        self.lhs.clone()
    }
    pub fn rhs(&self) -> &[Token] {
        &self.rhs
    }
}

impl<T1: ToString, T2: ToString, I: IntoIterator<Item = T2>> From<(T1, I)> for Production {
    fn from(value: (T1, I)) -> Self {
        Self::new(
            Token::new(value.0),
            value.1.into_iter().map(Token::new).collect(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Grammar {
    start: Token,
    productions: Vec<Production>,
    terminals: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GrammarFromStrError {
    ProdError(ProductionFromStrError),
    NoProductions,
}

impl From<ProductionFromStrError> for GrammarFromStrError {
    fn from(v: ProductionFromStrError) -> Self {
        Self::ProdError(v)
    }
}
impl FromStr for Grammar {
    type Err = GrammarFromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut prods = vec![];
        for prod_variants in s.split(';') {
            if prod_variants.trim().is_empty() {
                continue;
            }
            let mut prod_variants = prod_variants.split('|');

            let first_prod = if let Some(p) = prod_variants.next() {
                Production::from_str(p)?
            } else {
                continue;
            };
            let start = first_prod.lhs();
            prods.push(first_prod);

            for p in prod_variants {
                let mut toks = vec![];
                for t in p.split_whitespace() {
                    toks.push(Token::new(t));
                }
                prods.push(Production::new(start.clone(), toks));
            }
        }
        if prods.is_empty() {
            return Err(GrammarFromStrError::NoProductions);
        }

        let start = prods[0].lhs();
        Ok(Self::new(prods, start))
    }
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for prod in self.productions() {
            write!(f, "{prod}; ")?;
        }
        Ok(())
    }
}

impl Grammar {
    pub fn new<T: Into<Production>>(prods: impl IntoIterator<Item = T>, start: Token) -> Self {
        let productions: Vec<_> = prods.into_iter().map(Into::into).collect();
        assert!(productions.len() > 0, "No productions given!");

        let mut terminals = vec![];
        for prod in productions.iter() {
            for rhs_tok in prod.rhs() {
                if productions.iter().all(|p| &p.lhs() != rhs_tok) {
                    terminals.push(rhs_tok.clone());
                }
            }
        }

        let res = Self {
            start: start.clone(),
            terminals,
            productions,
        };
        assert!(
            !res.is_terminal(start.clone()),
            "{start} is not a non-terminal for grammar {res}"
        );
        res
    }

    pub fn terminal_index(&self, t: Token) -> Option<usize> {
        self.terminals()
            .enumerate()
            .find(|(_, s)| s == &t)
            .map(|(i, _)| i)
    }
    pub fn non_terminal_index(&self, t: Token) -> Option<usize> {
        self.non_terminals()
            .enumerate()
            .find(|(_, s)| s == &t)
            .map(|(i, _)| i)
    }

    pub fn productions(&self) -> impl Iterator<Item = &Production> {
        self.productions.iter()
    }
    pub fn terminals(&self) -> impl Iterator<Item = Token> + '_ {
        self.terminals.iter().cloned()
    }
    pub fn non_terminals(&self) -> impl Iterator<Item = Token> + '_ {
        let mut lhs: Vec<_> = self.productions().map(|p| p.lhs()).collect();
        lhs.dedup();
        lhs.into_iter()
    }
    pub fn productions_for(&self, nonterm: Token) -> impl Iterator<Item = &Production> {
        self.productions().filter(move |p| p.lhs() == nonterm)
    }
    pub fn is_terminal(&self, t: Token) -> bool {
        self.terminals().any(move |term| term == t)
    }
    pub fn start(&self) -> Token {
        self.start.clone()
    }
    pub fn get_unique_token(&self, base_name: &str) -> Token {
        for old_name in self.terminals().chain(self.non_terminals()) {
            if old_name.name() == base_name {
                return self.get_unique_token(&(base_name.to_string() + "a"));
            }
        }
        Token::new(base_name)
    }

    pub fn first(&self) -> First {
        let mut first: Vec<_> = self
            .terminals()
            .map(|t| (t.clone(), vec![TokenOrEps::Token(t)]))
            .chain(self.non_terminals().map(|nt| (nt.clone(), vec![])))
            .collect();
        let mut changed = true;
        while changed {
            changed = false;

            for prod in self.productions() {
                let (a_idx, _) = first
                    .iter()
                    .enumerate()
                    .find(|(_, (t, _))| t == &prod.lhs())
                    .unwrap();

                let mut a_first = first[a_idx].1.clone();

                let mut had_eps = true;
                for y in prod.rhs() {
                    let (_, y_first) = first.iter().find(|(t, _)| t == y).unwrap();
                    had_eps = false;

                    for y_first in y_first {
                        if y_first.is_eps() {
                            had_eps = true;
                            continue;
                        }
                        if a_first.contains(y_first) {
                            continue;
                        }
                        a_first.push(y_first.clone());
                        changed = true;
                    }

                    if !had_eps {
                        break;
                    }
                }

                if had_eps && !a_first.contains(&TokenOrEps::Eps) {
                    a_first.push(TokenOrEps::Eps);
                    changed = true;
                }

                first[a_idx].1 = a_first;
            }
        }

        First { first }
    }

    pub fn follow(&self, first: &First) -> Follow {
        let mut map = vec![(self.start(), vec![TokenOrEnd::End])];
        let mut changed = true;
        while changed {
            changed = false;

            for prod in self.productions() {
                for i in 0..prod.rhs().len() {
                    let b = prod.rhs()[i].clone();

                    let b_follow_idx = if let Some((i, (_, _))) =
                        map.iter().enumerate().find(|(_, (x, _))| x == &b)
                    {
                        i
                    } else {
                        map.push((b.clone(), vec![]));
                        map.len() - 1
                    };
                    let mut b_follow = std::mem::take(&mut map[b_follow_idx].1);

                    let beta = &prod.rhs()[i + 1..];
                    // beta might be a terminal, in that case FIRST(beta) = beta
                    // if B is the last symbol in production, FOLLOW(B) contains everything from
                    // FOLLOW(A), or if FOLLOW(beta[0]) contains eps.
                    let beta_first = first.first_of_sent(beta).unwrap();
                    for t in beta_first {
                        if let Some(tok) = t.as_token() {
                            let t = TokenOrEnd::Token(tok.clone());
                            if b_follow.contains(&t) {
                                continue;
                            }

                            b_follow.push(t.clone());
                            changed = true;
                        } else {
                            // everything from A
                            if prod.lhs() == b {
                                continue;
                            }
                            let follow_a = if let Some((_, follow_a)) =
                                map.iter().find(|(t, _)| t == &prod.lhs())
                            {
                                follow_a
                            } else {
                                continue;
                            };

                            for t in follow_a {
                                if b_follow.contains(t) {
                                    continue;
                                }
                                b_follow.push(t.clone());
                                changed = true;
                            }
                        }
                    }

                    assert!(std::mem::replace(&mut map[b_follow_idx].1, b_follow).is_empty());
                }
            }
        }

        Follow { map }
    }

    pub fn possible_words(&self) -> PossibleWords<'_> {
        PossibleWords::new(self)
    }
}

pub struct PossibleWords<'a> {
    grammar: &'a Grammar,
    depth: usize,
    max_depth: Option<usize>,
    stack: Vec<ParseTree>,
    next_stack: Vec<ParseTree>,
}

impl<'a> PossibleWords<'a> {
    fn new(grammar: &'a Grammar) -> Self {
        Self {
            grammar,
            depth: 0,
            max_depth: None,
            next_stack: vec![ParseTree::new(grammar.start())],
            stack: vec![],
        }
    }

    pub fn with_derivation_count(mut self, derivation_count: usize) -> Self {
        self.max_depth = Some(derivation_count);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Leaf(Token),
    RuleStart(usize, Token),
    RuleEnd(usize, Token),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Leaf(l) => write!(f, "{l}"),
            Node::RuleStart(i, l) => write!(f, "S{l}[{i}]"),
            Node::RuleEnd(i, l) => write!(f, "E{l}[{i}]"),
        }
    }
}

impl Node {
    /// Returns `true` if the node is [`Leaf`].
    ///
    /// [`Leaf`]: Node::Leaf
    #[must_use]
    pub fn is_leaf(&self) -> bool {
        matches!(self, Self::Leaf(..))
    }

    pub fn as_leaf(&self) -> Option<&Token> {
        if let Self::Leaf(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the node is [`RuleStart`].
    ///
    /// [`RuleStart`]: Node::RuleStart
    #[must_use]
    pub fn is_rule_start(&self) -> bool {
        matches!(self, Self::RuleStart(..))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseTree {
    nodes: VecDeque<Node>,
}

impl Display for ParseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut depth = 0;
        for (_, node) in self.nodes() {
            match node {
                Node::Leaf(l) => writeln!(f, "{}-{}", " |".repeat(depth), l)?,
                Node::RuleStart(i, n) => {
                    writeln!(f, "{}-{n}[{i}]", " |".repeat(depth))?;
                    depth += 1;
                }
                Node::RuleEnd(_, _) => {
                    depth -= 1;
                }
            }
        }

        Ok(())
    }
}

impl ParseTree {
    pub fn new(start: Token) -> Self {
        Self {
            nodes: [Node::Leaf(start)].into(),
        }
    }

    pub fn root(&self) -> Token {
        match &self.nodes[0] {
            Node::Leaf(n) => n.clone(),
            Node::RuleStart(_, n) => n.clone(),
            _ => unreachable!(),
        }
    }
    pub fn nodes(&self) -> impl Iterator<Item = (usize, Node)> + '_ {
        self.nodes.iter().cloned().enumerate()
    }

    pub fn leafs(&self) -> impl Iterator<Item = Token> + '_ {
        self.nodes().filter_map(|(_, n)| n.as_leaf().cloned())
    }

    pub fn replace_with_subtree(&mut self, node_idx: usize, mut sub_tree: Self) {
        let mut right = self.nodes.split_off(node_idx);

        assert_eq!(
            right.pop_front().unwrap(),
            Node::Leaf(sub_tree.root()),
            "replacing node {node_idx}"
        );

        self.nodes.append(&mut sub_tree.nodes);
        self.nodes.append(&mut right);
    }

    pub fn replace_with_production(&mut self, node_idx: usize, prod: &Production, prod_idx: usize) {
        let mut right = self.nodes.split_off(node_idx);
        assert!(right
            .pop_front()
            .and_then(|n| n.as_leaf().cloned())
            .map_or(false, |t| prod.lhs() == t));
        self.nodes.push_back(Node::RuleStart(prod_idx, prod.lhs()));
        for t in prod.rhs() {
            self.nodes.push_back(Node::Leaf(t.clone()));
        }
        self.nodes.push_back(Node::RuleEnd(prod_idx, prod.lhs()));
        self.nodes.append(&mut right);
    }
}

impl<'a> Iterator for PossibleWords<'a> {
    type Item = (Vec<Token>, ParseTree);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.stack.is_empty() {
                if self.max_depth.map_or(false, |d| d <= self.depth) || self.next_stack.is_empty() {
                    return None;
                }
                self.depth += 1;
                std::mem::swap(&mut self.stack, &mut self.next_stack);
            }

            while let Some(tree) = self.stack.pop() {
                let mut had_nonterms = false;
                for (i, node) in tree.nodes() {
                    if let Node::Leaf(leaf) = node {
                        if self.grammar.is_terminal(leaf.clone()) {
                            continue;
                        }

                        had_nonterms = true;
                        for (prod_idx, prod) in self.grammar.productions().into_iter().enumerate() {
                            if prod.lhs() != leaf {
                                continue;
                            }
                            let mut new_tree = tree.clone();
                            new_tree.replace_with_production(i, prod, prod_idx);
                            self.next_stack.push(new_tree);
                        }
                        break;
                    }
                }

                if had_nonterms {
                    continue;
                }

                return Some((tree.leafs().collect(), tree));
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenOrEnd {
    Token(Token),
    End,
}

impl Display for TokenOrEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenOrEnd::Token(t) => write!(f, "{t}"),
            TokenOrEnd::End => write!(f, "$"),
        }
    }
}

impl From<Token> for TokenOrEnd {
    fn from(v: Token) -> Self {
        Self::Token(v)
    }
}

impl TokenOrEnd {
    /// Returns `true` if the token or end is [`Token`].
    ///
    /// [`Token`]: TokenOrEnd::Token
    #[must_use]
    pub fn is_token(&self) -> bool {
        matches!(self, Self::Token(..))
    }

    pub fn as_token(&self) -> Option<&Token> {
        if let Self::Token(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the token or end is [`End`].
    ///
    /// [`End`]: TokenOrEnd::End
    #[must_use]
    pub fn is_end(&self) -> bool {
        matches!(self, Self::End)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Follow {
    map: Vec<(Token, Vec<TokenOrEnd>)>,
}

impl Follow {
    pub fn follow(&self, nonterm: Token) -> Option<&[TokenOrEnd]> {
        self.map
            .iter()
            .find(|(t, _)| t == &nonterm)
            .map(|(_, follow)| follow.as_slice())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TokenOrEps {
    Token(Token),
    Eps,
}

impl From<Token> for TokenOrEps {
    fn from(v: Token) -> Self {
        Self::Token(v)
    }
}

impl TokenOrEps {
    /// Returns `true` if the token or eps is [`Token`].
    ///
    /// [`Token`]: TokenOrEps::Token
    #[must_use]
    pub fn is_token(&self) -> bool {
        matches!(self, Self::Token(..))
    }

    pub fn as_token(&self) -> Option<&Token> {
        if let Self::Token(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the token or eps is [`Eps`].
    ///
    /// [`Eps`]: TokenOrEps::Eps
    #[must_use]
    pub fn is_eps(&self) -> bool {
        matches!(self, Self::Eps)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct First {
    first: Vec<(Token, Vec<TokenOrEps>)>,
}

impl First {
    fn new(it: impl IntoIterator<Item = (Token, TokenOrEps)>) -> Self {
        let mut s = Self { first: vec![] };
        for (nt, t) in it {
            if let Some((_, a)) = s.first.iter_mut().find(|(t, _)| *t == nt) {
                a.push(t);
            } else {
                s.first.push((nt, vec![t]));
            }
        }

        s
    }

    pub fn first(&self, nonterm: Token) -> Option<&[TokenOrEps]> {
        self.first
            .iter()
            .find(|(t, _)| t == &nonterm)
            .map(|(_, a)| a.as_ref())
    }
    pub fn first_of_sent(&self, sent: &[Token]) -> Option<Vec<TokenOrEps>> {
        let mut f = vec![];
        for t in sent {
            let first_of_t = self.first(t.clone())?;
            for t in first_of_t {
                if !f.contains(t) && t.is_token() {
                    f.push(t.clone());
                }
            }
            if !first_of_t.contains(&TokenOrEps::Eps) {
                return Some(f);
            }
        }
        f.push(TokenOrEps::Eps);

        Some(f)
    }
}

impl Grammar {
    pub fn parse(&self, word: &[Token]) -> Option<ParseTree> {
        let first = self.first();
        let follow = self.follow(&first);
        let mut stack = vec![];
        let mut next_stack = vec![ParseTree::new(self.start())];

        loop {
            if stack.is_empty() {
                if next_stack.is_empty() {
                    return None;
                }
                std::mem::swap(&mut stack, &mut next_stack);
            }

            'OUTER: while let Some(tree) = stack.pop() {
                let mut had_nonterms = false;
                let mut word_ptr = 0;

                for (i, node) in tree.nodes() {
                    let cur_tok = word
                        .get(word_ptr)
                        .cloned()
                        .map(TokenOrEnd::Token)
                        .unwrap_or(TokenOrEnd::End);

                    if let Node::Leaf(node) = node {
                        if self.is_terminal(node.clone()) {
                            if cur_tok.as_token() == Some(&node) {
                                word_ptr += 1;
                                continue;
                            }

                            continue 'OUTER;
                        }
                        had_nonterms = true;

                        let mut applied_rules = vec![];
                        for (prod_idx, prod) in self.productions().enumerate() {
                            if prod.lhs() != node {
                                continue;
                            }

                            for f in first.first_of_sent(prod.rhs()).unwrap() {
                                match f {
                                    TokenOrEps::Token(t) => {
                                        if cur_tok == TokenOrEnd::Token(t) {
                                            applied_rules.push(prod_idx);
                                            continue;
                                        }
                                    }
                                    TokenOrEps::Eps => {
                                        if follow.follow(prod.lhs()).unwrap().contains(&cur_tok) {
                                            applied_rules.push(prod_idx);
                                            continue;
                                        }
                                    }
                                }
                            }
                        }

                        for prod_idx in applied_rules {
                            let mut new_tree = tree.clone();
                            new_tree.replace_with_production(
                                i,
                                self.productions().nth(prod_idx).unwrap(),
                                prod_idx,
                            );
                            next_stack.push(new_tree);
                        }
                        break;
                    } else {
                        continue;
                    }
                }

                if !had_nonterms {
                    return Some(tree);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::*;

    fn expr_grammar() -> &'static str {
        "S -> E; E -> T add E | T; T -> F mul T | F; F -> id | lp E rp;"
    }
    fn expr_grammar_ll() -> &'static str {
        "S -> E; E -> T Ea; Ea -> add T Ea | ; T -> F Ta; Ta -> mul F Ta | ; F -> lp E rp | id;"
    }

    #[test]
    fn from_string() {
        let g = Grammar::new(
            [
                ("S", vec!["E"]),
                ("E", vec!["T", "add", "E"]),
                ("E", vec!["T"]),
                ("T", vec!["F", "mul", "T"]),
                ("T", vec!["F"]),
                ("F", vec!["id"]),
                ("F", vec!["lp", "E", "rp"]),
            ],
            Token::new("S"),
        );
        assert_eq!(Ok(g), Grammar::from_str(expr_grammar()));
    }

    #[test]
    fn basic() {
        let g = Grammar::from_str(expr_grammar()).unwrap();
        assert_eq!(g.start().name(), "S");
        assert_eq!(g.productions_for(Token::new("S")).count(), 1);
        assert_eq!(g.productions_for(Token::new("E")).count(), 2);
        assert_eq!(g.productions_for(Token::new("T")).count(), 2);
        assert_eq!(g.productions_for(Token::new("F")).count(), 2);
        assert!(g.is_terminal(Token::new("lp")));
        assert!(g.is_terminal(Token::new("rp")));
        assert!(g.is_terminal(Token::new("add")));
        assert!(g.is_terminal(Token::new("mul")));
        assert!(g.is_terminal(Token::new("id")));
        assert!(!g.is_terminal(Token::new("E")));
        assert!(!g.is_terminal(Token::new("T")));
        assert!(!g.is_terminal(Token::new("F")));

        let g = Grammar::from_str(expr_grammar_ll()).unwrap();
        assert_eq!(g.start().name(), "S");
        assert_eq!(g.productions_for(Token::new("S")).count(), 1);
        assert_eq!(g.productions_for(Token::new("E")).count(), 1);
        assert_eq!(g.productions_for(Token::new("Ea")).count(), 2);
        assert_eq!(g.productions_for(Token::new("T")).count(), 1);
        assert_eq!(g.productions_for(Token::new("Ta")).count(), 2);
        assert_eq!(g.productions_for(Token::new("F")).count(), 2);
    }

    #[test]
    fn first() {
        let g = Grammar::from_str(expr_grammar()).unwrap();
        let first = g.first();

        let c = HashSet::from([
            TokenOrEps::Token(Token::new("id")),
            TokenOrEps::Token(Token::new("lp")),
        ]);
        let actual_first = HashMap::from([
            (Token::new("S"), c.clone()),
            (Token::new("E"), c.clone()),
            (Token::new("T"), c.clone()),
            (Token::new("F"), c.clone()),
            (
                Token::new("id"),
                HashSet::from([TokenOrEps::Token(Token::new("id"))]),
            ),
            (
                Token::new("lp"),
                HashSet::from([TokenOrEps::Token(Token::new("lp"))]),
            ),
            (
                Token::new("rp"),
                HashSet::from([TokenOrEps::Token(Token::new("rp"))]),
            ),
        ]);
        for (nt, a) in actual_first {
            assert_eq!(
                HashSet::from_iter(first.first(nt).unwrap().iter().cloned()),
                a
            );
        }

        let g = Grammar::from_str(expr_grammar_ll()).unwrap();
        let first = g.first();

        let actual_first = HashMap::from([
            (
                "E",
                HashSet::from([
                    TokenOrEps::Token(Token::new("lp")),
                    TokenOrEps::Token(Token::new("id")),
                ]),
            ),
            (
                "Ea",
                HashSet::from([TokenOrEps::Token(Token::new("add")), TokenOrEps::Eps]),
            ),
            (
                "T",
                HashSet::from([
                    TokenOrEps::Token(Token::new("lp")),
                    TokenOrEps::Token(Token::new("id")),
                ]),
            ),
            (
                "Ta",
                HashSet::from([TokenOrEps::Token(Token::new("mul")), TokenOrEps::Eps]),
            ),
            (
                "F",
                HashSet::from([
                    TokenOrEps::Token(Token::new("lp")),
                    TokenOrEps::Token(Token::new("id")),
                ]),
            ),
            ("id", HashSet::from([TokenOrEps::Token(Token::new("id"))])),
            ("lp", HashSet::from([TokenOrEps::Token(Token::new("lp"))])),
            ("rp", HashSet::from([TokenOrEps::Token(Token::new("rp"))])),
        ]);

        for (nt, a) in actual_first {
            assert_eq!(
                HashSet::from_iter(first.first(Token::new(nt)).unwrap().iter().cloned()),
                a
            );
        }
    }

    #[test]
    fn follow() {
        let g = Grammar::from_str(expr_grammar()).unwrap();
        let first = g.first();
        let follow = g.follow(&first);
        let actual_follow = HashMap::from([
            ("S", vec![TokenOrEnd::End]),
            (
                "T",
                vec![
                    TokenOrEnd::Token(Token::new("add")),
                    TokenOrEnd::Token(Token::new("rp")),
                    TokenOrEnd::End,
                ],
            ),
            (
                "F",
                vec![
                    TokenOrEnd::Token(Token::new("mul")),
                    TokenOrEnd::Token(Token::new("add")),
                    TokenOrEnd::Token(Token::new("rp")),
                    TokenOrEnd::End,
                ],
            ),
            (
                "E",
                vec![TokenOrEnd::End, TokenOrEnd::Token(Token::new("rp"))],
            ),
        ]);
        for (nt, f) in actual_follow {
            let tok = Token::new(nt);
            let mut f0 = HashSet::new();
            for t in follow.follow(tok).unwrap().iter().cloned() {
                f0.insert(t);
            }
            let mut f1 = HashSet::new();
            for t in f.iter().cloned() {
                f1.insert(t);
            }
            assert_eq!(f0, f1);
        }

        let g = Grammar::from_str(expr_grammar_ll()).unwrap();
        let first = g.first();
        let follow = g.follow(&first);
        let actual_follow = HashMap::from([
            ("S", vec![TokenOrEnd::End]),
            (
                "Ea",
                vec![TokenOrEnd::End, TokenOrEnd::Token(Token::new("rp"))],
            ),
            (
                "T",
                vec![
                    TokenOrEnd::Token(Token::new("add")),
                    TokenOrEnd::Token(Token::new("rp")),
                    TokenOrEnd::End,
                ],
            ),
            (
                "Ta",
                vec![
                    TokenOrEnd::Token(Token::new("add")),
                    TokenOrEnd::Token(Token::new("rp")),
                    TokenOrEnd::End,
                ],
            ),
            (
                "F",
                vec![
                    TokenOrEnd::Token(Token::new("mul")),
                    TokenOrEnd::Token(Token::new("add")),
                    TokenOrEnd::Token(Token::new("rp")),
                    TokenOrEnd::End,
                ],
            ),
            (
                "E",
                vec![TokenOrEnd::End, TokenOrEnd::Token(Token::new("rp"))],
            ),
        ]);

        for (nt, f) in actual_follow {
            let tok = Token::new(nt);
            let mut f0 = HashSet::new();
            for t in follow.follow(tok).unwrap().iter().cloned() {
                f0.insert(t);
            }
            let mut f1 = HashSet::new();
            for t in f.iter().cloned() {
                f1.insert(t);
            }
            assert_eq!(f0, f1);
        }
    }
}
