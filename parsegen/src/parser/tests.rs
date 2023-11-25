use std::{io::Cursor, io::Write, str::FromStr, string::FromUtf8Error};

use crate::{parser::lgraph::Bracket, tokenizer::Token};

use super::{
    grammar::{Grammar, TokenOrEnd},
    lgraph::{Lgraph, Stack},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraverseError {
    FromUtf8(FromUtf8Error),
    ConflictOn(usize, TokenOrEnd, Option<usize>),
    NoWayToContinue(usize, TokenOrEnd, Option<usize>),
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
        let terminal_count = grammar.terminals().count() + 1;

        let mut state = g.start_nodes().next().unwrap();
        let mut stack = Stack::new();
        for i in 0..=toks.len() {
            let mut cur_tok = toks
                .get(i)
                .cloned()
                .map(TokenOrEnd::Token)
                .unwrap_or(TokenOrEnd::End);
            let mut next_tok = Some(
                toks.get(i + 1)
                    .cloned()
                    .map(TokenOrEnd::Token)
                    .unwrap_or(TokenOrEnd::End),
            );

            let mut need_to_consume = true;
            let mut next_state = Some(state);
            while let Some(s) = next_state.take() {
                state = s;
                // print!("-> {s} -");
                let mut has_consumed = false;
                let mut bracket = None;

                for (_, item, next) in g.edges_from(state) {
                    let bracket_ok = item.bracket().map_or(true, |b| stack.can_accept(b));
                    let token_match = item.tok().map_or(false, |t| t == cur_tok);
                    let can_consume = need_to_consume && token_match;
                    let token_ok = need_to_consume && token_match || item.tok().is_none();
                    let look_ahead_tok = if !can_consume {
                        &cur_tok
                    } else {
                        next_tok.as_ref().unwrap()
                    };
                    let look_ahead_ok = item
                        .look_ahead()
                        .map_or(true, |l| l.contains(look_ahead_tok));

                    if !bracket_ok || !look_ahead_ok || !token_ok {
                        continue;
                    }
                    if next_state.is_some() {
                        return Err(TraverseError::ConflictOn(state, cur_tok, stack.top()));
                    }

                    bracket = item.bracket();
                    next_state = Some(next);
                    has_consumed = can_consume;
                }
                if has_consumed {
                    // print!("{cur_tok}");
                    need_to_consume = false;
                    // write!(w, "{}, ", cur_tok).unwrap();
                    cur_tok = next_tok.take().unwrap();
                }
                if let Some(b) = bracket {
                    // print!("{}", b);
                    if !b.is_open() {
                        let idx = stack.top().unwrap();
                        if idx < terminal_count {
                            let t = grammar
                                .terminals()
                                .nth(idx)
                                .map(TokenOrEnd::Token)
                                .unwrap_or(TokenOrEnd::End);
                            write!(w, "{t}, ").unwrap();
                        } else {
                            grammar
                                .productions()
                                .nth(idx - terminal_count)
                                .map(|p| write!(w, "{}[{}], ", p.lhs(), idx - terminal_count));
                        }
                    }
                    assert!(stack.try_accept_mut(b));
                }
            }

            if need_to_consume {
                return Err(TraverseError::NoWayToContinue(state, cur_tok, stack.top()));
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

fn non_slr_bu_ll1() -> Grammar {
    Grammar::from_str("S -> A a A b | B b B a; A -> ; B -> ;").unwrap()
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

fn json_grammar() -> Grammar {
    Grammar::from_str(
        "
VALUE -> OBJ | ARRAY | num | string;
OBJ -> lb KEYVALS rb;
ARRAY -> ls VALS rs;
KEYVALS -> KEYVAL coma KEYVALS | ;
KEYVAL -> q ident q semi VALUE;
VALS -> VALUE coma VALS | ;
",
    )
    .unwrap()
}

pub fn ll1_gauntlet(t: &mut dyn TestParser) {
    for (grammar, name) in [
        (empty_language(), "empty_language"),
        (finite_language(), "finite_language"),
        (expr_grammar_ll1(), "expr_grammar_ll1"),
        (parens_grammar_simple(), "parens_grammar_simple"),
        (simple_plang_ll1(), "simple_plang_ll1"),
        (non_slr_bu_ll1(), "non_slr_bu_ll1"),
        (json_grammar(), "json_grammar"),
    ] {
        println!("\t{name}");
        let g = Lgraph::ll1(&grammar);
        std::fs::write(format!("tests/ll1-{}.dot", name), g.to_string()).unwrap();
        assert_eq!(
            g.is_deterministic(),
            None,
            "Non deterministic for {}",
            grammar
        );
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

fn parens_grammar_slr() -> Grammar {
    Grammar::from_str("S -> S a S b; S -> ;").unwrap()
}

fn expr_grammar() -> Grammar {
    Grammar::from_str(" E -> T add E; E -> T; T -> F mul T; T -> F; F -> id; F -> lp E rp;")
        .unwrap()
}

fn stack_expr() -> Grammar {
    Grammar::from_str("S -> S S add; S -> S S mul; S -> id;").unwrap()
}

fn regex_grammar() -> Grammar {
    Grammar::from_str(
        "
R -> V or R | V;
V -> E V | E;
E -> B | S | P | M;
S -> B star;
P -> B plus;
M -> B question;
B -> symbol | lp R rp;",
    )
    .unwrap()
}

fn an_bm_c() -> Grammar {
    Grammar::from_str("S -> a S; S -> T c; T -> a T b; T -> ;").unwrap()
}

pub fn slr_gauntlet(t: &mut dyn TestParser) {
    for (grammar, name) in [
        (an_bm_c(), "an_bm_c"),
        (empty_language(), "empty_language"),
        (finite_language(), "finite_language"),
        (expr_grammar_ll1(), "expr_grammar_ll1"),
        (parens_grammar_simple(), "parens_grammar_simple"),
        (simple_plang_ll1(), "simple_plang_ll1"),
        (parens_grammar_slr(), "parens_grammar_slr"),
        (regex_grammar(), "regex_grammar"),
        (stack_expr(), "stack_expr"),
        (expr_grammar(), "expr_grammar"),
        (json_grammar(), "json_grammar"),
    ] {
        println!("\t{name}");
        let g = Lgraph::slr(&grammar);
        std::fs::write(format!("tests/slr-{}.dot", name), g.to_string()).unwrap();
        assert_eq!(
            g.is_deterministic(),
            None,
            "Non deterministic for {}",
            grammar
        );
        t.init(g, grammar.clone());
        for (toks, tree) in grammar.possible_words().take(500) {
            let s = toks.iter().fold(String::new(), |mut acc, tok| {
                acc += tok.name();
                acc += " ";
                acc
            });
            assert_eq!(
                t.parse(&toks),
                Ok(tree.strip_suffix("$, ").unwrap().to_string()),
                "failed on \n\tgrammar={grammar}\n\tinput={s}\n",
            );
        }
    }
}

#[test]
fn runtime_parser() {
    println!("testing ll1");
    ll1_gauntlet(&mut RuntimeParser::default());
    println!("testing slr");
    slr_gauntlet(&mut RuntimeParser::default());
}
