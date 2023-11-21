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
    fn parse(&self, toks: &[Token]) -> Result<String, TraverseError> {
        let mut res = vec![];
        let mut w = Cursor::new(&mut res);
        let w = &mut w;
        let g = self.g.as_ref().unwrap();
        let grammar = self.grammar.as_ref().unwrap();

        let mut state = g.start_nodes().next().unwrap();
        let mut stack = Stack::new();
        for i in 0..=toks.len() {
            let tok = toks.get(i).cloned();
            let next_tok = toks.get(i + 1).cloned();
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
    fn parse(&self, toks: &[Token]) -> Result<String, TraverseError>;
}

fn parens_grammar_simple() -> Grammar {
    Grammar::from_str("S -> a S b S; S -> ;").unwrap()
}

fn empty_language() -> Grammar {
    Grammar::from_str("S -> ;").unwrap()
}

fn finite_language() -> Grammar {
    Grammar::from_str("S -> a b c; S -> d E f; E -> ; E -> e;").unwrap()
}

fn expr_grammar_ll1() -> Grammar {
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
    Grammar::from_str(g).unwrap()
}

fn simple_plang_ll1() -> Grammar {
    let s = "
S -> FUNCTIONS;
FUNCTIONS -> FUNCTION FUNCTIONS;
FUNCTIONS -> ;
FUNCTION -> SIGNATURE BLOCK;
SIGNATURE -> ident lp LIST rp;
BLOCK -> lb STATEMENTS rb;
LIST -> FULLLIST;
LIST -> ;
FULLLIST -> ident coma LIST;
STATEMENTS -> DO;
STATEMENTS -> LET;
STATEMENTS -> IF;
STATEMENTS -> WHILE;
STATEMENTS -> RETURN;
DO -> do ASSIGNABLE eq EXPR;
ASSIGNABLE -> ident;
ASSIGNABLE -> ls EXPR rs ident;
LET -> let ident eq EXPR;
IF -> if EXPR BLOCK;
WHILE -> while EXPR BLOCK;
RETURN -> return EXPR;
EXPR -> ident;
EXPR -> call ident lp EXPRLIST rp;
EXPRLIST -> FULLEXPRLIST;
EXPRLIST -> ;
FULLEXPRLIST -> EXPR coma EXPRLIST;";
    Grammar::from_str(s).unwrap()
}
pub fn ll1_gauntlet(t: &mut dyn TestParser) {
    for grammar in [
        empty_language(),
        finite_language(),
        expr_grammar_ll1(),
        parens_grammar_simple(),
        simple_plang_ll1(),
    ] {
        let g = Lgraph::ll1(&grammar);
        std::fs::File::create("tests/ll1.dot")
            .unwrap()
            .write_all(g.to_string().as_bytes())
            .unwrap();
        t.init(g, grammar.clone());
        for (toks, tree) in grammar.possible_words().take(500) {
            let s = toks.iter().fold(String::new(), |mut acc, tok| {
                acc += tok.name();
                acc += " ";
                acc
            });
            assert_eq!(
                t.parse(&toks),
                Ok(tree),
                "failed on \n\tgrammar={grammar}\n\tinput={s}\n",
            );
        }
    }
}

#[test]
fn runtime_parser() {
    ll1_gauntlet(&mut RuntimeParser::default())
}
