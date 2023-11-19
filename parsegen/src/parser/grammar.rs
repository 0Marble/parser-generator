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

    pub fn first(&self) -> First {
        let mut first: Vec<_> = self.terminals().map(|t| (t.clone(), Some(t))).collect();
        let mut stack: Vec<_> = self.productions().map(|p| p.lhs()).collect();
        stack.dedup();

        while let Some(nonterm) = stack.pop() {
            for prod in self.productions_for(nonterm.clone()) {
                if let Some(a) = prod.rhs().first() {
                    if self.is_terminal(a.clone()) {
                        first.push((nonterm.clone(), Some(a.clone())));
                    } else {
                        let precomputed = first.iter().filter(|(t, _)| t == a).cloned();
                        let mut had_precomputed = false;
                        let mut new = vec![];

                        for (_, t) in precomputed {
                            had_precomputed = true;
                            new.push((nonterm.clone(), t.clone()));
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

        First::new(first)
    }

    pub fn follow(&self, first: &First) -> Follow {
        let mut map = vec![(self.start(), vec![None])];
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

                    let beta = prod.rhs().get(i + 1);
                    // beta might be a terminal, in that case FIRST(beta) = beta
                    // if B is the last symbol in production, FOLLOW(B) contains everything from
                    // FOLLOW(A), or if FOLLOW(beta[0]) contains eps.
                    let beta_first = match beta {
                        None => &[None],
                        Some(beta) => first.first(beta.clone()).unwrap(),
                    };
                    for t in beta_first {
                        if t.is_some() {
                            if b_follow.contains(t) {
                                continue;
                            }

                            b_follow.push(t.clone());
                            changed = true;
                        } else {
                            // everything from A
                            if prod.lhs() == b {
                                continue;
                            }

                            for t in map
                                .iter()
                                .find(|(t, _)| t == &prod.lhs())
                                .map(|(_, t)| t)
                                .unwrap()
                            {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Follow {
    map: Vec<(Token, Vec<Option<Token>>)>,
}

impl Follow {
    pub fn follow(&self, nonterm: Token) -> Option<&[Option<Token>]> {
        self.map
            .iter()
            .find(|(t, _)| t == &nonterm)
            .map(|(_, follow)| follow.as_slice())
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct First {
    first: Vec<(Token, Vec<Option<Token>>)>,
}

impl First {
    fn new(it: impl IntoIterator<Item = (Token, Option<Token>)>) -> Self {
        let mut f: Vec<(Token, Vec<Option<Token>>)> = vec![];
        for (nt, t) in it {
            if let Some((_, a)) = f.iter_mut().find(|(t, _)| *t == nt) {
                a.push(t);
            } else {
                f.push((nt, vec![t]));
            }
        }

        Self { first: f }
    }

    pub fn first(&self, nonterm: Token) -> Option<&[Option<Token>]> {
        self.first
            .iter()
            .find(|(t, _)| t == &nonterm)
            .map(|(_, a)| a.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use super::*;

    fn expr_grammar() -> &'static str {
        "S -> E; E -> T add E; E -> T; T -> F mul T; T -> F; F -> id; F -> lp E rp;"
    }
    fn expr_grammar_ll() -> &'static str {
        "S -> E; E -> T Ea; Ea -> add T Ea; Ea -> ; T -> F Ta; Ta -> mul F Ta; Ta -> ; F -> lp E rp; F -> id;"
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

        let c = HashSet::from([Some(Token::new("id")), Some(Token::new("lp"))]);
        let actual_first = HashMap::from([
            (Token::new("S"), c.clone()),
            (Token::new("E"), c.clone()),
            (Token::new("T"), c.clone()),
            (Token::new("F"), c.clone()),
            (Token::new("id"), HashSet::from([Some(Token::new("id"))])),
            (Token::new("lp"), HashSet::from([Some(Token::new("lp"))])),
            (Token::new("rp"), HashSet::from([Some(Token::new("rp"))])),
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
            ("E", HashSet::from([Some("lp"), Some("id")])),
            ("Ea", HashSet::from([Some("add"), None])),
            ("T", HashSet::from([Some("lp"), Some("id")])),
            ("Ta", HashSet::from([Some("mul"), None])),
            ("F", HashSet::from([Some("lp"), Some("id")])),
            ("id", HashSet::from([Some("id")])),
            ("lp", HashSet::from([Some("lp")])),
            ("rp", HashSet::from([Some("rp")])),
        ]);

        for (nt, f) in actual_first {
            assert_eq!(
                HashSet::<Option<Token>>::from_iter(
                    first.first(Token::new(nt)).unwrap().iter().cloned()
                ),
                HashSet::from_iter(f.iter().map(|t| t.as_ref().map(Token::new)))
            );
        }
    }

    #[test]
    fn follow() {
        let g = Grammar::from_str(expr_grammar()).unwrap();
        let first = g.first();
        let follow = g.follow(&first);
        let actual_follow = HashMap::from([
            ("S", vec![None]),
            ("T", vec![Some("add"), Some("rp"), None]),
            ("F", vec![Some("mul"), Some("add"), Some("rp"), None]),
            ("E", vec![None, Some("rp")]),
        ]);
        for (nt, f) in actual_follow {
            let tok = Token::new(nt);
            let mut f0 = HashSet::new();
            for t in follow.follow(tok).unwrap().iter().cloned() {
                f0.insert(t);
            }
            let mut f1 = HashSet::new();
            for t in f.iter().map(|t| t.map(Token::new)) {
                f1.insert(t);
            }
            assert_eq!(f0, f1);
        }

        let g = Grammar::from_str(expr_grammar_ll()).unwrap();
        let first = g.first();
        let follow = g.follow(&first);
        let actual_follow = HashMap::from([
            ("S", vec![None]),
            ("Ea", vec![None, Some("rp")]),
            ("T", vec![Some("add"), Some("rp"), None]),
            ("Ta", vec![Some("add"), Some("rp"), None]),
            ("F", vec![Some("mul"), Some("add"), Some("rp"), None]),
            ("E", vec![None, Some("rp")]),
        ]);

        for (nt, f) in actual_follow {
            let tok = Token::new(nt);
            let mut f0 = HashSet::new();
            for t in follow.follow(tok).unwrap().iter().cloned() {
                f0.insert(t);
            }
            let mut f1 = HashSet::new();
            for t in f.iter().map(|t| t.map(Token::new)) {
                f1.insert(t);
            }
            assert_eq!(f0, f1);
        }
    }
}
