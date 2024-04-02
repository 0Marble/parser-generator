use std::fmt::Display;

use crate::Token;

use super::{regex::Regex, Lexer};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenOrGarbage {
    Token(Token, usize, usize),
    Garbage(usize, usize),
}

impl TokenOrGarbage {
    pub fn print(&self, src: &str) -> String {
        match self {
            TokenOrGarbage::Token(t, a, b) => format!(
                "{}({})",
                t,
                src.chars().skip(*a).take(*b).collect::<String>()
            ),
            TokenOrGarbage::Garbage(a, b) => format!(
                "Garbage({})",
                src.chars().skip(*a).take(*b).collect::<String>()
            ),
        }
    }
}

pub trait LexerRunner {
    fn set_lexer(&mut self, lexer: Lexer);
    fn traverse(&self, s: &str) -> Vec<TokenOrGarbage>;
}

struct RuntimeLexer {
    l: Option<Lexer>,
}

impl LexerRunner for RuntimeLexer {
    fn set_lexer(&mut self, lexer: Lexer) {
        self.l = Some(lexer);
    }
    fn traverse(&self, s: &str) -> Vec<TokenOrGarbage> {
        let l = self.l.as_ref().unwrap();
        let mut cur = l.start();
        let mut res = vec![];

        let mut tok_start = 0;
        let mut len = 0;
        for (i, c) in s.char_indices() {
            len = i + 1;

            let next = l.step(cur, c);
            if l.is_dead_state(next) {
                let restart = l.step(l.start(), c);
                if !l.is_dead_state(restart) {
                    cur = restart;
                    if let Some(tok) = l.accept_token(cur) {
                        res.push(TokenOrGarbage::Token(tok, tok_start, i - tok_start));
                    } else {
                        res.push(TokenOrGarbage::Garbage(tok_start, i - tok_start));
                    }
                    tok_start = i;
                }
            } else {
                cur = next;
            }
        }

        if let Some(tok) = l.accept_token(cur) {
            res.push(TokenOrGarbage::Token(tok, tok_start, len - tok_start));
        } else {
            res.push(TokenOrGarbage::Garbage(tok_start, len - tok_start));
        }

        res
    }
}

fn empty_string() -> (Lexer, Vec<(String, Vec<String>)>) {
    (
        Lexer::new([(Regex::Empty, Token::new("Empty").unwrap())]),
        vec![
            ("".to_string(), vec!["Empty()".to_string()]),
            ("hello world".to_string(), vec!["Garbage(hello world)".to_string()]),
            ("Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.".to_string(), vec!["Garbage(Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.)".to_string()]),
        ],
    )
}

fn ident_or_keyword() -> (Lexer, Vec<(String, Vec<String>)>) {
    let l = Lexer::new([
        (Regex::ident(), Token::new("Ident").unwrap()),
        (Regex::keyword("if"), Token::new("If").unwrap()),
        (Regex::ascii_whitespace(), Token::new("Ws").unwrap()),
    ]);

    (
        l,
        vec![
            ("if".to_string(), vec!["If(if)".to_string()]),
            ("iffy".to_string(), vec!["Ident(iffy)".to_string()]),
            ("ifif".to_string(), vec!["Ident(ifif)".to_string()]),
            (
                "if a".to_string(),
                vec![
                    "If(if)".to_string(),
                    "Ws( )".to_string(),
                    "Ident(a)".to_string(),
                ],
            ),
            (
                "1if2iffy 3".to_string(),
                vec![
                    "Garbage(1)".to_string(),
                    "If(if)".to_string(),
                    "Garbage(2)".to_string(),
                    "Ident(iffy)".to_string(),
                    "Ws( )".to_string(),
                    "Garbage(3)".to_string(),
                ],
            ),
        ],
    )
}

fn math_expr() -> (Lexer, Vec<(String, Vec<String>)>) {
    (
        Lexer::new([
            (Regex::ident(), Token::new("Ident").unwrap()),
            (Regex::int(), Token::new("Num").unwrap()),
            (Regex::Base('+'), Token::new("Add").unwrap()),
            (Regex::Base('-'), Token::new("Sub").unwrap()),
            (Regex::Base('/'), Token::new("Div").unwrap()),
            (Regex::Base('*'), Token::new("Mul").unwrap()),
            (Regex::Base('('), Token::new("Lp").unwrap()),
            (Regex::Base(')'), Token::new("Rp").unwrap()),
            (Regex::Base(','), Token::new("Coma").unwrap()),
            (Regex::ascii_whitespace(), Token::new("Ws").unwrap()),
        ]),
        vec![
            ("x".to_string(), vec!["Ident(x)".to_string()]),
            (
                "x+4".to_string(),
                vec![
                    "Ident(x)".to_string(),
                    "Add(+)".to_string(),
                    "Num(4)".to_string(),
                ],
            ),
            (
                "1 + 2".to_string(),
                vec![
                    "Num(1)".to_string(),
                    "Ws( )".to_string(),
                    "Add(+)".to_string(),
                    "Ws( )".to_string(),
                    "Num(2)".to_string(),
                ],
            ),
            (
                "sin(10) - my_func(x, cos(-2))".to_string(),
                vec![
                    "Ident(sin)".to_string(),
                    "Lp(()".to_string(),
                    "Num(10)".to_string(),
                    "Rp())".to_string(),
                    "Ws( )".to_string(),
                    "Sub(-)".to_string(),
                    "Ws( )".to_string(),
                    "Ident(my_func)".to_string(),
                    "Lp(()".to_string(),
                    "Ident(x)".to_string(),
                    "Coma(,)".to_string(),
                    "Ws( )".to_string(),
                    "Ident(cos)".to_string(),
                    "Lp(()".to_string(),
                    "Num(-2)".to_string(),
                    "Rp())".to_string(),
                    "Rp())".to_string(),
                ],
            ),
        ],
    )
}

fn rebuild(toks: &[TokenOrGarbage], src: &str) -> String {
    use std::io::Write;
    let mut w = vec![];
    for t in toks {
        let (a, b) = match t {
            TokenOrGarbage::Token(_, a, b) => (a, b),
            TokenOrGarbage::Garbage(a, b) => (a, b),
        };
        write!(
            &mut w,
            "{}",
            src.chars().skip(*a).take(*b).collect::<String>()
        )
        .unwrap();
    }

    String::from_utf8(w).unwrap()
}

pub fn lexer_gauntlet(lr: &mut dyn LexerRunner) {
    for (name, (l, tests)) in [
        ("empty_string", empty_string()),
        ("ident_ok_keyword", ident_or_keyword()),
        ("math_exprt", math_expr()),
    ] {
        lr.set_lexer(l);
        println!("{name}");
        for (input, out) in tests {
            let toks = lr.traverse(&input);
            println!("{:?}", toks);
            assert_eq!(rebuild(&toks, &input), input, "failed on \"{input}\"");
            assert_eq!(toks.len(), out.len(), "failed on \"{input}\"");

            for (tok, expect) in toks.into_iter().zip(out.into_iter()) {
                assert_eq!(tok.print(&input), expect, "failed on \"{input}\"");
            }
        }
    }
}

#[test]
fn runtime_lexer() {
    let mut lr = RuntimeLexer { l: None };
    lexer_gauntlet(&mut lr);
}