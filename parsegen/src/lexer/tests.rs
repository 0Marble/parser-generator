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
                if let Some(tok) = l.accept_token(cur) {
                    res.push(TokenOrGarbage::Token(tok, tok_start, i - tok_start));
                    tok_start = i;
                } else if !l.is_dead_state(restart) {
                    res.push(TokenOrGarbage::Garbage(tok_start, i - tok_start));
                    tok_start = i;
                } else {
                }
                cur = restart;
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

fn empty_string() -> (Vec<(Regex, Token)>, Vec<(String, Vec<String>)>) {
    (
        vec![(Regex::Empty, Token::new("Empty").unwrap())],
        vec![
            ("".to_string(), vec!["Empty()".to_string()]),
            (
                "hello world".to_string(),
                vec!["Empty()".to_string(), "Garbage(hello world)".to_string()],
            ),
            ("Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.".to_string(),vec!["Empty()".to_string(),"Garbage(Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.)".to_string()])
        ],
    )
}

fn ident_or_keyword() -> (Vec<(Regex, Token)>, Vec<(String, Vec<String>)>) {
    (
        vec![
            (Regex::keyword("if"), Token::new("If").unwrap()),
            (Regex::ident(), Token::new("Ident").unwrap()),
            (Regex::ascii_whitespace(), Token::new("Ws").unwrap()),
        ],
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
                "1if!iffy 3".to_string(),
                vec![
                    "Garbage(1)".to_string(),
                    "If(if)".to_string(),
                    "Garbage(!)".to_string(),
                    "Ident(iffy)".to_string(),
                    "Ws( )".to_string(),
                    "Garbage(3)".to_string(),
                ],
            ),
        ],
    )
}

fn math_expr() -> (Vec<(Regex, Token)>, Vec<(String, Vec<String>)>) {
    (
        vec![
            (Regex::int(), Token::new("Num").unwrap()),
            (Regex::Base('+'), Token::new("Add").unwrap()),
            (Regex::Base('-'), Token::new("Sub").unwrap()),
            (Regex::Base('/'), Token::new("Div").unwrap()),
            (Regex::Base('*'), Token::new("Mul").unwrap()),
            (Regex::Base('('), Token::new("Lp").unwrap()),
            (Regex::Base(')'), Token::new("Rp").unwrap()),
            (Regex::Base(','), Token::new("Coma").unwrap()),
            (Regex::ident(), Token::new("Ident").unwrap()),
            (Regex::ascii_whitespace(), Token::new("Ws").unwrap()),
        ],
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
    for (name, (toks, tests)) in [
        ("empty_string", empty_string()),
        ("ident_ok_keyword", ident_or_keyword()),
        ("math_exprt", math_expr()),
    ] {
        let l = Lexer::new(toks);
        println!("{name}");
        std::fs::write(format!("tests/lexer_{name}.dot"), l.to_string()).unwrap();
        lr.set_lexer(l);
        for (input, out) in tests {
            let toks = lr.traverse(&input);
            let s = rebuild(&toks, &input);
            println!("{s}");
            println!("{:?}\n{:?}\n", toks, out);
            assert_eq!(s, input, "failed on \"{input}\"");
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
