use std::{io::Cursor, io::Write, str::FromStr, string::FromUtf8Error};

use crate::{parser::lgraph::Bracket, tokenizer::Token};

use super::{
    grammar::{Grammar, GrammarFromStrError, TokenOrEnd},
    lgraph::{Lgraph, Path, Stack},
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
    fn parse(&self, toks: &[Token]) -> Result<Path, TraverseError> {
        let g = self.g.as_ref().unwrap();
        let grammar = self.grammar.as_ref().unwrap();
        let terminal_count = grammar.terminals().count() + 1;
        let mut path = Path::Empty(g.start_nodes().next().unwrap());

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
                let mut has_consumed = false;
                let mut edge = None;

                for (from, item, to) in g.edges_from(state) {
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
                    if edge.is_some() {
                        return Err(TraverseError::ConflictOn(state, cur_tok, stack.top()));
                    }

                    edge = Some((from, item, to));
                    has_consumed = can_consume;
                }

                if let Some(edge) = edge {
                    next_state = Some(edge.2);
                    if let Some(b) = edge.1.bracket() {
                        assert!(stack.try_accept_mut(b));
                    }
                    path = path.append(edge);
                } else {
                    // return Err(TraverseError::NoWayToContinue(state, cur_tok, stack.top()));
                }

                if has_consumed {
                    need_to_consume = false;
                    cur_tok = next_tok.take().unwrap();
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

        Ok(path)
    }
}

pub trait TestParser {
    fn init(&mut self, g: Lgraph, grammar: Grammar);
    fn parse(&self, toks: &[Token]) -> Result<Path, TraverseError>;
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

        // for (toks, tree) in g.possible_words(&grammar).take(100) {
        //     let s = toks.iter().fold(String::new(), |mut acc, tok| {
        //         acc += tok.name();
        //         acc += " ";
        //         acc
        //     });
        //     assert_eq!(
        //         grammar.parse(&toks),
        //         Some(tree.strip_suffix("$, ").unwrap().to_string()),
        //         "failed on \n\tgrammar={grammar}\n\tinput={s}\n",
        //     );
        // }

        t.init(g, grammar.clone());
        for (toks, tree) in grammar.possible_words().take(500) {
            let s = toks.iter().fold(String::new(), |mut acc, tok| {
                acc += tok.name();
                acc += " ";
                acc
            });
            let path = t.parse(&toks).unwrap();
            println!("{}", path);
            let res = path.to_parse_tree(&grammar);
            assert_eq!(res, tree, "\tgrammar: {grammar}\n\ttoks: {s}");
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

        // for (toks, tree) in g.possible_words(&grammar).take(10) {
        //     let s = toks.iter().fold(String::new(), |mut acc, tok| {
        //         acc += tok.name();
        //         acc += " ";
        //         acc
        //     });
        //     assert_eq!(
        //         grammar.parse(&toks),
        //         Some(tree),
        //         "failed on \n\tgrammar={grammar}\n\tinput={s}\n",
        //     );
        // }

        t.init(g, grammar.clone());
        for (toks, tree) in grammar.possible_words().take(500) {
            let s = toks.iter().fold(String::new(), |mut acc, tok| {
                acc += tok.name();
                acc += " ";
                acc
            });
            let res = t.parse(&toks).map(|p| p.to_parse_tree(&grammar));
            assert_eq!(res, Ok(tree), "\tgrammar: {grammar}\n\ttoks: {s}");
        }
    }
}

#[test]
fn runtime_parser() {
    writeln!(std::io::stderr(), "testing ll1").unwrap();
    ll1_gauntlet(&mut RuntimeParser::default());
    writeln!(std::io::stderr(), "testing slr").unwrap();
    slr_gauntlet(&mut RuntimeParser::default());
}
