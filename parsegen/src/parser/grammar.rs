use std::{fmt::Display, str::FromStr};

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
        let mut split = s.split_ascii_whitespace();
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
    pub fn rhs(&self) -> impl Iterator<Item = Token> + '_ {
        self.rhs.iter().cloned()
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
        for prod in s.split(';') {
            if prod.is_empty() {
                continue;
            }
            let prod = Production::from_str(prod)?;
            prods.push(prod);
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
                if productions.iter().all(|p| p.lhs() != rhs_tok) {
                    terminals.push(rhs_tok);
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

    pub fn productions(&self) -> impl Iterator<Item = &Production> {
        self.productions.iter()
    }
    pub fn terminals(&self) -> impl Iterator<Item = Token> + '_ {
        self.terminals.iter().cloned()
    }
    pub fn non_terminals(&self) -> impl Iterator<Item = Token> + '_ {
        self.productions().map(|p| p.lhs())
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

    pub fn first(&self) -> impl Iterator<Item = (Token, Option<Token>)> + '_ {
        let mut first = vec![];
        let mut stack: Vec<_> = self.productions().map(|p| p.lhs()).collect();
        stack.dedup();

        while let Some(nonterm) = stack.pop() {
            println!("nonterm={nonterm}, first={first:?}");
            for prod in self.productions_for(nonterm.clone()) {
                println!("\tprod={prod}");
                if let Some(a) = prod.rhs().next() {
                    if self.is_terminal(a.clone()) {
                        first.push((nonterm.clone(), Some(a)));
                    } else {
                        let precomputed = first.iter().filter(|(t, _)| t == &a).cloned();
                        let mut had_precomputed = false;
                        let mut new = vec![];

                        for (_, t) in precomputed {
                            had_precomputed = true;
                            new.push((nonterm.clone(), t));
                        }
                        first.append(&mut new);
                        if !had_precomputed {
                            stack.push(nonterm.clone());
                            stack.push(a.clone());
                        }
                    }
                } else if !first.contains(&(nonterm.clone(), None)) {
                    first.push((nonterm.clone(), None));
                }
            }
        }

        first.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::*;

    fn expr_grammar() -> &'static str {
        "E -> T add E; E -> T; T -> F mul T; T -> F; F -> id; F -> lp E rp;"
    }
    fn expr_grammar_ll() -> &'static str {
        "E -> T Ea; Ea -> add T Ea; Ea -> ; T -> F Ta; Ta -> mul F Ta; Ta -> ; F -> lp E rp; F -> id;"
    }

    #[test]
    fn from_string() {
        let g = Grammar::new(
            [
                ("E", vec!["T", "add", "E"]),
                ("E", vec!["T"]),
                ("T", vec!["F", "mul", "T"]),
                ("T", vec!["F"]),
                ("F", vec!["id"]),
                ("F", vec!["lp", "E", "rp"]),
            ],
            Token::new("E"),
        );
        assert_eq!(Ok(g), Grammar::from_str(expr_grammar()));
    }

    #[test]
    fn basic() {
        let g = Grammar::from_str(expr_grammar()).unwrap();
        assert_eq!(g.start().name(), "E");
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
        assert_eq!(g.start().name(), "E");
        assert_eq!(g.productions_for(Token::new("E")).count(), 1);
        assert_eq!(g.productions_for(Token::new("Ea")).count(), 2);
        assert_eq!(g.productions_for(Token::new("T")).count(), 1);
        assert_eq!(g.productions_for(Token::new("Ta")).count(), 2);
        assert_eq!(g.productions_for(Token::new("F")).count(), 2);
    }

    #[test]
    fn first() {
        let g = Grammar::from_str(expr_grammar()).unwrap();
        let mut first: HashMap<_, HashSet<_>> = HashMap::new();
        for (nt, t) in g.first() {
            first.entry(nt).or_default().insert(t);
        }

        let c = HashSet::from([Some(Token::new("id")), Some(Token::new("lp"))]);
        let actual_first = HashMap::from([
            (Token::new("E"), c.clone()),
            (Token::new("T"), c.clone()),
            (Token::new("F"), c.clone()),
        ]);
        assert_eq!(first, actual_first);

        let g = Grammar::from_str(expr_grammar_ll()).unwrap();
        println!("{g}");
        let mut first: HashMap<_, HashSet<_>> = HashMap::new();
        for (nt, t) in g.first() {
            first.entry(nt).or_default().insert(t);
        }

        let actual_first = HashMap::from([
            (
                Token::new("E"),
                HashSet::from([Some(Token::new("lp")), Some(Token::new("id"))]),
            ),
            (
                Token::new("Ea"),
                HashSet::from([Some(Token::new("add")), None]),
            ),
            (
                Token::new("T"),
                HashSet::from([Some(Token::new("lp")), Some(Token::new("id"))]),
            ),
            (
                Token::new("Ta"),
                HashSet::from([Some(Token::new("mul")), None]),
            ),
            (
                Token::new("F"),
                HashSet::from([Some(Token::new("lp")), Some(Token::new("id"))]),
            ),
        ]);

        assert_eq!(first, actual_first);
    }
}
