use std::{collections::VecDeque, str::FromStr, string::FromUtf8Error};

use rand::thread_rng;

use crate::{parser::optimizations::Optimization, Token};

use super::{
    grammar::{Grammar, Node, ParseTree, TokenOrEnd},
    lgraph::{Lgraph, Lookahead, Path, Stack},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraverseError {
    FromUtf8(FromUtf8Error),
    ConflictOn(usize, Option<TokenOrEnd>, Option<usize>),
    NoWayToContinue(usize, Option<TokenOrEnd>, Option<usize>),
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

struct Buffer<I> {
    buf: VecDeque<TokenOrEnd>,
    it: WordReader<I>,
}

struct WordReader<I> {
    it: I,
    had_end: bool,
}

impl<I> Iterator for WordReader<I>
where
    I: Iterator<Item = Token>,
{
    type Item = TokenOrEnd;
    fn next(&mut self) -> Option<Self::Item> {
        match self.it.next() {
            Some(t) => Some(TokenOrEnd::Token(t)),
            None => {
                if self.had_end {
                    None
                } else {
                    self.had_end = true;
                    Some(TokenOrEnd::End)
                }
            }
        }
    }
}

impl<I> Buffer<I>
where
    I: Iterator<Item = Token>,
{
    fn new(it: I) -> Self {
        Self {
            buf: VecDeque::new(),
            it: WordReader { it, had_end: false },
        }
    }

    fn extend(&mut self) -> Option<TokenOrEnd> {
        if let Some(tok) = self.it.next() {
            self.buf.push_back(tok.clone());
            return Some(tok);
        }
        None
    }

    fn eat_token(&mut self) -> Option<TokenOrEnd> {
        self.buf.pop_front()
    }

    fn compare(&mut self, tok: Option<TokenOrEnd>, lk: Option<Lookahead>) -> bool {
        for (i, tok) in tok.into_iter().chain(lk.into_iter().flatten()).enumerate() {
            if let Some(t) = self.buf.get(i).cloned().or_else(|| self.extend()) {
                if t != tok {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }

    fn peek_token(&self) -> Option<TokenOrEnd> {
        self.buf.front().cloned()
    }
}

impl TestParser for RuntimeParser {
    fn init(&mut self, g: Lgraph, grammar: Grammar) {
        self.g = Some(g);
        self.grammar = Some(grammar);
    }
    fn parse(&self, toks: &[Token]) -> Result<ParseTree, TraverseError> {
        let g = self.g.as_ref().unwrap();
        let mut path = Path::Empty(g.start());

        let mut state = g.start();
        let mut stack = Stack::new();
        let mut buf = Buffer::new(toks.iter().cloned());

        loop {
            let mut next = None;
            let mut consumed_tok = None;
            let mut consumed_bracket = None;

            for (_, item, to) in g.edges_from(state) {
                if !buf.compare(item.tok(), item.look_ahead()) {
                    continue;
                }
                if !item.bracket().map_or(true, |b| stack.can_accept(b)) {
                    continue;
                }

                if next.is_some() {
                    return Err(TraverseError::ConflictOn(
                        state,
                        buf.peek_token(),
                        stack.top(),
                    ));
                }
                next = Some(to);
                consumed_bracket = item.bracket();
                consumed_tok = item.tok();
                path = path.append((state, item, to));
            }
            if let Some(t) = consumed_tok {
                assert_eq!(Some(t), buf.eat_token());
            }
            if let Some(b) = consumed_bracket {
                assert!(stack.try_accept_mut(b));
            }

            match next {
                Some(n) => state = n,
                None => break,
            }
        }

        if !g.is_end_node(state) {
            return Err(TraverseError::NotAnEndState(state));
        }
        if stack.top().is_some() {
            return Err(TraverseError::StackNotEmptied(stack));
        }

        Ok(path.to_parse_tree(self.grammar.as_ref().unwrap()))
    }
}

pub trait TestParser {
    fn init(&mut self, g: Lgraph, grammar: Grammar);
    fn parse(&self, toks: &[Token]) -> Result<ParseTree, TraverseError>;
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
SIGNATURE -> t_ident t_lp LIST t_rp;
BLOCK -> t_lb STATEMENTS t_rb;
LIST -> FULLLIST;
LIST -> ;
FULLLIST -> t_ident t_coma LIST;
STATEMENTS -> DO;
STATEMENTS -> LET;
STATEMENTS -> IF;
STATEMENTS -> WHILE;
STATEMENTS -> RETURN;
DO -> t_do ASSIGNABLE t_eq EXPR;
ASSIGNABLE -> t_ident;
ASSIGNABLE -> t_ls EXPR t_rs t_ident;
LET -> t_let t_ident t_eq EXPR;
IF -> t_if EXPR BLOCK;
WHILE -> t_while EXPR BLOCK;
RETURN -> t_return EXPR;
EXPR -> t_ident;
EXPR -> t_call t_ident t_lp EXPRLIST t_rp;
EXPRLIST -> FULLEXPRLIST;
EXPRLIST -> ;
FULLEXPRLIST -> EXPR t_coma EXPRLIST;";
    Grammar::from_str(s).unwrap()
}

fn json_grammar() -> Grammar {
    Grammar::from_str(
        "
VALUE -> OBJ | ARRAY | t_num | t_string;
OBJ -> t_lb KEYVALS t_rb;
ARRAY -> t_ls VALS t_rs;
KEYVALS -> KEYVAL t_coma KEYVALS | ;
KEYVAL -> t_q t_ident t_q t_semi VALUE;
VALS -> VALUE t_coma VALS | ;
",
    )
    .unwrap()
}

pub fn ll1_gauntlet(t: &mut dyn TestParser) {
    let mut rng = thread_rng();
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
        let g = g.optimize(Optimization::all());
        std::fs::write(format!("tests/ll1-{}-optimized.dot", name), g.to_string()).unwrap();
        assert_eq!(
            g.is_deterministic(),
            None,
            "Non deterministic for {}",
            grammar
        );

        t.init(g, grammar.clone());
        for _ in 0..100 {
            let tree = grammar.fuzz(&mut rng, 1000);
            let toks: Vec<_> = tree.leafs().collect();
            let s = toks.iter().fold(String::new(), |mut acc, tok| {
                acc += tok.name();
                acc += " ";
                acc
            });
            // use std::io::Write;
            // writeln!(std::io::stderr(), "{s}").unwrap();

            let nodes = t.parse(&toks).unwrap();
            assert_eq!(nodes, tree, "\tgrammar: {grammar}\n\ttoks: {s}");
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
    let mut rng = thread_rng();
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
        let g = g.optimize(Optimization::all());
        std::fs::write(format!("tests/slr-{}-optimized.dot", name), g.to_string()).unwrap();
        assert_eq!(
            g.is_deterministic(),
            None,
            "Non deterministic for {}",
            grammar
        );

        t.init(g, grammar.clone());
        for _ in 0..100 {
            let tree = grammar.fuzz(&mut rng, 1000);
            let toks: Vec<_> = tree.leafs().collect();
            let s = toks.iter().fold(String::new(), |mut acc, tok| {
                acc += tok.name();
                acc += " ";
                acc
            });
            // use std::io::Write;
            // writeln!(std::io::stderr(), "{s}").unwrap();

            let nodes = t.parse(&toks).unwrap();
            assert_eq!(nodes, tree, "\tgrammar: {grammar}\n\ttoks: {s}");
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
