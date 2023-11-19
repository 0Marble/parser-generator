use std::{io::Cursor, io::Write, str::FromStr, string::FromUtf8Error};

use crate::tokenizer::Token;

use super::{
    grammar::Grammar,
    lgraph::{Lgraph, Stack},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraverseError {
    FromUtf8(FromUtf8Error),
    ConflictOn(usize, Option<Token>, Option<usize>),
    NoWayToContinue(usize, Option<Token>, Option<usize>),
    NotAnEndState(usize),
    StackNotEmptied(Stack),
}

impl From<FromUtf8Error> for TraverseError {
    fn from(v: FromUtf8Error) -> Self {
        Self::FromUtf8(v)
    }
}

#[derive(Debug, Clone, Default)]
struct RuntimeParser {
    g: Option<Lgraph>,
    grammar: Option<Grammar>,
}

impl TestParser for RuntimeParser {
    fn init(&mut self, g: Lgraph, grammar: Grammar) {
        self.g = Some(g);
        self.grammar = Some(grammar);
    }
    fn parse(&self, toks: &[String]) -> Result<String, TraverseError> {
        let mut res = vec![];
        let mut w = Cursor::new(&mut res);
        let w = &mut w;
        let g = self.g.as_ref().unwrap();
        let grammar = self.grammar.as_ref().unwrap();

        let mut state = g.start_nodes().next().unwrap();
        let mut stack = Stack::new();
        for i in 0..=toks.len() {
            let tok = toks.get(i).map(Token::new);
            let next_tok = toks.get(i + 1).map(Token::new);
            let mut consumed = false;

            while !consumed && !(tok == None && g.is_end_node(state) && stack.top().is_none()) {
                consumed = false;
                let mut next = None;
                let top = stack.top();
                let mut bracket = None;
                for (_, letter, to) in g.edges_from(state) {
                    if letter.tok().is_some() {
                        if letter.tok() != tok {
                            continue;
                        }
                        consumed = true;
                    }
                    if let Some(b) = letter.bracket() {
                        if !b.is_open() && Some(b.index()) != top {
                            continue;
                        }
                    }
                    if let Some(look_ahead) = letter.look_ahead() {
                        let cmp = if consumed {
                            next_tok.as_ref()
                        } else {
                            tok.as_ref()
                        };
                        if !look_ahead.iter().any(|t| t.as_ref() == cmp) {
                            continue;
                        }
                    }

                    if next.is_some() || bracket.is_some() {
                        return Err(TraverseError::ConflictOn(state, tok, top));
                    }
                    next = Some(to);
                    bracket = letter.bracket();
                }
                if let Some(next) = next {
                    state = next;
                } else {
                    return Err(TraverseError::NoWayToContinue(state, tok, top));
                }
                if consumed {
                    write!(w, "{}, ", tok.as_ref().unwrap()).unwrap();
                }
                if let Some(b) = bracket {
                    let (next_stack, ok) = stack.try_accept(b);
                    stack = next_stack;
                    assert!(ok);
                    if !b.is_open() {
                        if let Some((j, nt)) = grammar
                            .non_terminals()
                            .enumerate()
                            .find(|(j, _)| b.index() == *j)
                        {
                            write!(w, "{nt}[{j}], ").unwrap();
                        }
                    }
                }
            }
        }

        if !g.is_end_node(state) {
            return Err(TraverseError::NotAnEndState(state));
        }
        if stack.top().is_some() {
            return Err(TraverseError::StackNotEmptied(stack));
        }

        Ok(String::from_utf8(res)?)
    }
}

pub trait TestParser {
    fn init(&mut self, g: Lgraph, grammar: Grammar);
    fn parse(&self, toks: &[String]) -> Result<String, TraverseError>;
}

fn convert_res<S: ToString, T: ToString, R: ToString>(
    g: S,
    it: impl IntoIterator<Item = (impl IntoIterator<Item = T>, Result<R, TraverseError>)>,
) -> (Grammar, Vec<(Vec<String>, Result<String, TraverseError>)>) {
    (
        Grammar::from_str(&g.to_string()).unwrap(),
        it.into_iter()
            .map(|(toks, res)| {
                (
                    toks.into_iter().map(|s| s.to_string()).collect(),
                    res.map(|s| s.to_string()),
                )
            })
            .collect(),
    )
}

type TestData = (Grammar, Vec<(Vec<String>, Result<String, TraverseError>)>);
fn expr_grammar_ll1() -> TestData {
    let g = "
S -> E;
E -> T Ea;
Ea -> add T Ea;
Ea -> ;
T -> F Ta;
Ta -> mul F Ta;
Ta -> ;
F -> lp E rp;
F -> id;";
    convert_res(
        g,
        [(vec!["id"], Ok("id, F[8], Ta[6], T[4], Ea[3], E[1], S[0], "))],
    )
}

fn if_else() -> TestData {
    let g = "S -> i E t S Sa; S -> a; Sa -> e S; E -> b;";
    convert_res(g, [(vec!["a"], Ok("a, S[1], "))])
}

pub fn ll1_gauntlet(t: &mut dyn TestParser) {
    for f in [expr_grammar_ll1, if_else] {
        let (g, tests) = f();
        let ll1 = Lgraph::ll1(&g);
        std::fs::File::create("tests/ll1.dot")
            .unwrap()
            .write_all(ll1.to_string().as_bytes())
            .unwrap();

        t.init(ll1, g);
        for (input, output) in tests {
            assert_eq!(t.parse(&input), output)
        }
    }
}

#[test]
fn runtime_parser() {
    ll1_gauntlet(&mut RuntimeParser::default())
}
