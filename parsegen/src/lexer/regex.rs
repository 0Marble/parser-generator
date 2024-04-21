use std::{char, collections::HashSet};

use super::{charset::CharSet, set_automata::SetAutomata};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regex {
    Base(char),
    Concat(Vec<Regex>),
    Variant(Vec<Regex>),
    Star(Box<Regex>),
    Range(char, char),
    NoneOf(Vec<char>),
    NotRange(char, char),
    Option(Box<Regex>),
}

impl Regex {
    pub fn to_nfa(&self) -> SetAutomata {
        match self {
            Regex::Base(c) => SetAutomata::new(0)
                .add_edge(0, CharSet::range(*c, *c), 1)
                .add_end(1),
            Regex::Concat(rs) => {
                let first = if let Some(first) = rs.first() {
                    first
                } else {
                    return SetAutomata::new(0).add_end(0);
                };
                let mut res = first.to_nfa();
                let mut offset = res.nodes().max().unwrap_or_default() + 1;
                for r in &rs[1..] {
                    let r = r.to_nfa();
                    let t = r.nodes().max().unwrap_or_default() + 1;
                    res = res.concat(&r.rename(|x| x + offset)).0;
                    offset += t;
                }
                res
            }
            Regex::Variant(rs) => {
                let first = if let Some(first) = rs.first() {
                    first
                } else {
                    return SetAutomata::new(0).add_end(0);
                };
                let mut res = first.to_nfa();
                let mut offset = res.nodes().max().unwrap_or_default() + 1;
                for r in &rs[1..] {
                    let r = r.to_nfa();
                    let t = r.nodes().max().unwrap_or_default() + 1;
                    res = res.union(&r.rename(|x| x + offset)).0;
                    offset += t;
                }
                res
            }
            Regex::Star(r) => r.to_nfa().star().0,
            Regex::Range(min, max) => SetAutomata::new(0)
                .add_edge(0, CharSet::range(*min, *max), 1)
                .add_end(1),
            Regex::Option(r) => {
                let nfa = r.to_nfa();
                let start = nfa.start();
                nfa.add_end(start)
            }
            Regex::NoneOf(c) => {
                let mut set = CharSet::full();
                for c in c {
                    set.remove(*c);
                }
                SetAutomata::new(0).add_edge(0, set, 1).add_end(1)
            }
            Regex::NotRange(a, b) => SetAutomata::new(0)
                .add_edge(0, CharSet::range(*a, *b).complement(), 1)
                .add_end(1),
        }
    }

    pub fn ascii_letters() -> Regex {
        Regex::Variant(vec![Regex::Range('a', 'z'), Regex::Range('A', 'Z')])
    }
    pub fn ascii_digits() -> Regex {
        Regex::Range('0', '9')
    }
    pub fn ascii_whitespace() -> Regex {
        Regex::Variant(vec![
            Regex::Base(' '),
            Regex::Base('\t'),
            Regex::Base('\r'),
            Regex::Base('\n'),
        ])
    }
    pub fn keyword(word: &str) -> Regex {
        Regex::Concat(word.chars().map(Regex::Base).collect())
    }

    pub fn ident() -> Regex {
        Regex::Concat(vec![
            Regex::Variant(vec![Regex::Base('_'), Regex::ascii_letters()]),
            Regex::Star(Box::new(Regex::Variant(vec![
                Regex::Base('_'),
                Regex::ascii_letters(),
                Regex::ascii_digits(),
            ]))),
        ])
    }
    pub fn uint() -> Regex {
        Regex::Variant(vec![
            Regex::Concat(vec![
                Regex::Range('1', '9'),
                Regex::Star(Box::new(Self::ascii_digits())),
            ]),
            Regex::Base('0'),
        ])
    }
    pub fn int() -> Regex {
        Regex::Variant(vec![
            Regex::Concat(vec![
                Regex::Option(Box::new(Regex::Base('-'))),
                Self::uint(),
            ]),
            Regex::Base('0'),
        ])
    }

    pub fn naive_match(&self, s: &str) -> bool {
        match self {
            Regex::Base(c) => s.chars().count() == 1 && s.starts_with(*c),
            Regex::Concat(regs) => Self::match_concat(&regs, s),
            Regex::Variant(regs) => regs.iter().any(|reg| reg.naive_match(s)),
            Regex::Star(reg) => reg.match_star(s),
            Regex::Range(a, b) => {
                if s.chars().count() != 1 {
                    return false;
                }
                let c = s.chars().next().unwrap();
                (*a..=*b).contains(&c)
            }
            Regex::NoneOf(chars) => {
                if s.chars().count() != 1 {
                    return false;
                }
                let c = s.chars().next().unwrap();
                !chars.contains(&c)
            }
            Regex::NotRange(a, b) => {
                if s.chars().count() != 1 {
                    return false;
                }
                let c = s.chars().next().unwrap();
                !(*a..=*b).contains(&c)
            }
            Regex::Option(reg) => s.is_empty() || reg.naive_match(s),
        }
    }

    fn match_star(&self, s: &str) -> bool {
        if s.is_empty() {
            return true;
        }

        for (i, _) in s.char_indices() {
            if self.naive_match(&s[..i]) && self.match_star(&s[i..]) {
                return true;
            }
        }

        self.naive_match(s)
    }

    fn match_concat(regs: &[Self], s: &str) -> bool {
        if regs.is_empty() {
            return s.is_empty();
        }
        if regs.len() == 1 {
            return regs[0].naive_match(s);
        }

        for i in s
            .char_indices()
            .map(|(i, _)| i)
            .chain(std::iter::once(s.len()))
        {
            if regs[0].naive_match(&s[..i]) && Self::match_concat(&regs[1..], &s[i..]) {
                return true;
            }
        }

        false
    }

    pub fn first(&self) -> HashSet<Option<char>> {
        match self {
            Regex::Base(c) => [Some(*c)].into(),
            Regex::Concat(regs) => {
                let mut res = HashSet::new();
                for r in regs {
                    let mut s = r.first();
                    if !s.remove(&None) {
                        return res.union(&s).cloned().collect();
                    }
                    res = res.union(&s).cloned().collect();
                }
                res
            }
            Regex::Variant(regs) => regs.iter().flat_map(|r| r.first()).collect(),
            Regex::Star(r) => r.first().into_iter().chain([None].into_iter()).collect(),
            Regex::Range(a, b) => {
                let mut res = HashSet::new();
                for i in (*a as u32)..=(*b as u32) {
                    if let Some(c) = char::from_u32(i) {
                        res.insert(Some(c));
                    }
                }
                res
            }
            Regex::NoneOf(chars) => {
                let mut res = HashSet::new();
                for i in 0..=u32::MAX {
                    if let Some(c) = char::from_u32(i) {
                        if chars.contains(&c) {
                            continue;
                        }
                        res.insert(Some(c));
                    }
                }
                res
            }
            Regex::NotRange(a, b) => {
                let mut res = HashSet::new();
                for i in 0..=u32::MAX {
                    if let Some(c) = char::from_u32(i) {
                        if (*a..=*b).contains(&c) {
                            continue;
                        }
                        res.insert(Some(c));
                    }
                }
                res
            }
            Regex::Option(r) => r.first().into_iter().chain([None].into_iter()).collect(),
        }
    }

    pub fn parse(it: impl IntoIterator<Item = char>) -> Self {
        use crate::lexer::reg_lexer::Lexer;
        use crate::lexer::reg_lexer::Token as RegToken;
        use crate::lexer::reg_lexer::TokenType;
        use crate::lexer::reg_parse::Parser;
        use crate::lexer::reg_parse::Symbol;

        let mut lex = Lexer::new();
        let mut toks = vec![];

        for c in it {
            if let Some(tok) = lex.eat_char(c) {
                toks.push(tok);
            }
        }
        toks.push(lex.finalize());

        let mut parse = Parser::new();
        let mut stack = vec![];

        #[derive(Debug)]
        enum StackSym {
            Token(RegToken),
            LetterList(Vec<char>),
            Regex(Regex),
        }
        toks.push(RegToken::Token(TokenType::_End, "".into()));

        let mut tok_count = 0;

        // println!();
        for t in &toks {
            for sym in parse.eat_tok(t.get_type().unwrap()) {
                // println!("{sym:?}, {stack:?}");

                match sym {
                    Symbol::cap
                    | Symbol::dash
                    | Symbol::lp
                    | Symbol::ls
                    | Symbol::or
                    | Symbol::plus
                    | Symbol::question
                    | Symbol::rp
                    | Symbol::rs
                    | Symbol::star
                    | Symbol::letter => {
                        stack.push(StackSym::Token(toks[tok_count].clone()));
                        tok_count += 1;
                    }
                    Symbol::ConcatEnd(2) => {
                        let StackSym::Regex(a) = stack.pop().unwrap() else {
                            panic!()
                        };
                        let StackSym::Regex(b) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.push(StackSym::Regex(Regex::Concat(vec![b, a])))
                    }
                    Symbol::ConcatEnd(3) => {
                        let StackSym::Regex(b) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.push(StackSym::Regex(b))
                    }
                    Symbol::ElemEnd(4) => {
                        let StackSym::Token(RegToken::Token(TokenType::letter, letter)) =
                            stack.pop().unwrap()
                        else {
                            panic!()
                        };
                        stack.push(StackSym::Regex(Regex::Base(Self::escape_reg(&letter))));
                    }
                    Symbol::ElemEnd(5) => {
                        stack.pop();
                        let StackSym::Token(RegToken::Token(TokenType::letter, a)) =
                            stack.pop().unwrap()
                        else {
                            panic!()
                        };
                        stack.pop();
                        let StackSym::Token(RegToken::Token(TokenType::letter, b)) =
                            stack.pop().unwrap()
                        else {
                            panic!()
                        };
                        stack.pop();

                        let a = Self::escape_reg(&a);
                        let b = Self::escape_reg(&b);
                        stack.push(StackSym::Regex(Regex::Range(b, a)));
                    }
                    Symbol::ElemEnd(6) => {
                        stack.pop();
                        let StackSym::Token(RegToken::Token(TokenType::letter, a)) =
                            stack.pop().unwrap()
                        else {
                            panic!()
                        };
                        stack.pop();
                        let StackSym::Token(RegToken::Token(TokenType::letter, b)) =
                            stack.pop().unwrap()
                        else {
                            panic!()
                        };
                        stack.pop();
                        stack.pop();

                        let a = Self::escape_reg(&a);
                        let b = Self::escape_reg(&b);
                        stack.push(StackSym::Regex(Regex::NotRange(b, a)));
                    }
                    Symbol::ElemEnd(7) => {
                        stack.pop();
                        let StackSym::LetterList(l) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.pop();
                        stack.push(StackSym::Regex(Regex::Variant(
                            l.into_iter().map(Regex::Base).collect(),
                        )))
                    }
                    Symbol::ElemEnd(8) => {
                        stack.pop();
                        let StackSym::LetterList(l) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.pop();
                        stack.pop();
                        stack.push(StackSym::Regex(Regex::NoneOf(l)))
                    }
                    Symbol::ElemEnd(9) => {
                        stack.pop();
                        let StackSym::Regex(r) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.pop();

                        stack.push(StackSym::Regex(r))
                    }
                    Symbol::ElemEnd(10) => {
                        stack.pop();
                        let StackSym::Regex(r) = stack.pop().unwrap() else {
                            panic!()
                        };

                        stack.push(StackSym::Regex(Regex::Star(Box::new(r))))
                    }
                    Symbol::ElemEnd(11) => {
                        stack.pop();
                        let StackSym::Regex(r) = stack.pop().unwrap() else {
                            panic!()
                        };

                        stack.push(StackSym::Regex(Regex::Concat(vec![
                            r.clone(),
                            Regex::Star(Box::new(r)),
                        ])))
                    }
                    Symbol::ElemEnd(12) => {
                        stack.pop();
                        let StackSym::Regex(r) = stack.pop().unwrap() else {
                            panic!()
                        };

                        stack.push(StackSym::Regex(Regex::Option(Box::new(r))))
                    }
                    Symbol::LetterListEnd(13) => {
                        let StackSym::LetterList(mut letters) = stack.pop().unwrap() else {
                            panic!()
                        };
                        let StackSym::Token(RegToken::Token(TokenType::letter, letter)) =
                            stack.pop().unwrap()
                        else {
                            panic!()
                        };
                        letters.push(Self::escape_reg(&letter));
                        stack.push(StackSym::LetterList(letters));
                    }
                    Symbol::LetterListEnd(14) => {
                        let StackSym::Token(RegToken::Token(TokenType::letter, letter)) =
                            stack.pop().unwrap()
                        else {
                            panic!()
                        };
                        stack.push(StackSym::LetterList(vec![Self::escape_reg(&letter)]));
                    }
                    Symbol::VariantEnd(0) => {
                        let StackSym::Regex(a) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.pop();
                        let StackSym::Regex(b) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.push(StackSym::Regex(Regex::Variant(vec![b, a])))
                    }
                    Symbol::VariantEnd(1) => {
                        let StackSym::Regex(b) = stack.pop().unwrap() else {
                            panic!()
                        };
                        stack.push(StackSym::Regex(b))
                    }
                    _ => (),
                }
            }
        }

        assert!(parse.is_end());

        let StackSym::Regex(res) = stack.pop().unwrap() else {
            panic!();
        };
        res
    }

    fn escape_reg(s: &str) -> char {
        match s {
            "\\\\" => '\\',
            "\\|" => '|',
            "\\*" => '*',
            "\\+" => '+',
            "\\?" => '?',
            "\\(" => '(',
            "\\)" => ')',
            "\\[" => '[',
            "\\]" => ']',
            "\\-" => '-',
            _ => {
                assert_eq!(s.chars().count(), 1);
                s.chars().next().unwrap()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{process::Command, str::FromStr};

    use rand::thread_rng;

    use crate::{
        codegen::{rs::RustCodegen, Codegen},
        lexer::Lexer,
        parser::{grammar::Grammar, lgraph::Lgraph, optimizations::Optimization},
        Token,
    };

    use super::*;

    #[test]
    fn naive_match() {
        let mut rng = thread_rng();
        let r = Regex::ascii_digits();
        for _ in 0..100 {
            assert!(r.naive_match(&r.fuzz(&mut rng, 10)));
        }

        let r = Regex::keyword("return");
        for _ in 0..100 {
            assert!(r.naive_match(&r.fuzz(&mut rng, 10)));
        }
        let r = Regex::keyword("if");
        for _ in 0..100 {
            assert!(r.naive_match(&r.fuzz(&mut rng, 10)));
        }

        let r = Regex::ident();
        for _ in 0..100 {
            let w = r.fuzz(&mut rng, 10);
            assert!(r.naive_match(&w), "failed on \"{w}\"");
            println!();
        }

        let r = Regex::int();
        for _ in 0..100 {
            assert!(r.naive_match(&r.fuzz(&mut rng, 10)));
        }
    }

    // #[test]
    fn gen_parser() {
        let g = Grammar::from_str(
            "\
Variant -> Concat or Variant | Concat; 
Concat -> Elem Concat | Elem; 
Elem -> letter | ls letter dash letter rs | ls cap letter dash letter rs | 
                ls LetterList rs | ls cap LetterList rs | lp Variant rp |
                Elem star | Elem plus | Elem question;
LetterList -> letter LetterList | letter;",
        )
        .unwrap();
        let lg = Lgraph::slr(&g).optimize(Optimization::all());

        let lex = Lexer::new([
            (Regex::Base('|'), Token::new("or").unwrap()),
            (Regex::Base('*'), Token::new("star").unwrap()),
            (Regex::Base('+'), Token::new("plus").unwrap()),
            (Regex::Base('?'), Token::new("question").unwrap()),
            (Regex::Base('^'), Token::new("cap").unwrap()),
            (Regex::Base('('), Token::new("lp").unwrap()),
            (Regex::Base(')'), Token::new("rp").unwrap()),
            (Regex::Base('['), Token::new("ls").unwrap()),
            (Regex::Base(']'), Token::new("rs").unwrap()),
            (Regex::Base('-'), Token::new("dash").unwrap()),
            (
                Regex::Variant(vec![
                    Regex::NoneOf(vec!['\\', '|', '*', '+', '?', '^', '(', ')', '[', ']', '-']),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('\\')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('|')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('*')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('+')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('?')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('^')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('(')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base(')')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('[')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base(']')]),
                    Regex::Concat(vec![Regex::Base('\\'), Regex::Base('-')]),
                ]),
                Token::new("letter").unwrap(),
            ),
        ]);

        RustCodegen.gen_code(&lex, &lg, &g, "tests", "regex");
    }

    #[test]
    fn parse_regex() {
        let r = Regex::parse("a|b".chars());
        assert!(r.naive_match("a"));
        assert!(r.naive_match("b"));
        assert!(!r.naive_match("c"));

        let r = Regex::parse("a*".chars());
        assert!(r.naive_match(""));
        assert!(r.naive_match("a"));
        assert!(r.naive_match("aaaaaa"));
        assert!(!r.naive_match("aabaaa"));

        let r = Regex::parse("a+".chars());
        assert!(!r.naive_match(""));
        assert!(r.naive_match("a"));
        assert!(r.naive_match("aaaaaa"));
        assert!(!r.naive_match("aabaaa"));

        let r = Regex::parse("a?".chars());
        assert!(r.naive_match(""));
        assert!(r.naive_match("a"));
        assert!(!r.naive_match("b"));
        assert!(!r.naive_match("aaaaaa"));
        assert!(!r.naive_match("aabaaa"));

        let r = Regex::parse("[a-z]".chars());
        assert!(r.naive_match("a"));
        assert!(r.naive_match("p"));
        assert!(r.naive_match("z"));
        assert!(!r.naive_match("0"));

        let r = Regex::parse("[^a-z]".chars());
        assert!(!r.naive_match("a"));
        assert!(!r.naive_match("p"));
        assert!(!r.naive_match("z"));
        assert!(r.naive_match("0"));

        let r = Regex::parse("[abc]".chars());
        assert!(r.naive_match("a"));
        assert!(r.naive_match("b"));
        assert!(r.naive_match("c"));
        assert!(!r.naive_match("d"));

        let r = Regex::parse("[^abc]".chars());
        assert!(!r.naive_match("a"));
        assert!(!r.naive_match("b"));
        assert!(!r.naive_match("c"));
        assert!(r.naive_match("d"));

        let r = Regex::parse("abcde".chars());
        assert!(r.naive_match("abcde"));
        assert!(!r.naive_match(""));
        assert!(!r.naive_match("a"));
        assert!(!r.naive_match("ab"));
        assert!(!r.naive_match("abc"));
        assert!(!r.naive_match("abcd"));

        let r = Regex::parse("\\-?[0-9]+(.[0-9]+)?".chars());
        assert!(r.naive_match("0"));
        assert!(r.naive_match("1.2"));
        assert!(r.naive_match("23141.12311"));
        assert!(r.naive_match("-12.23"));
        assert!(!r.naive_match("0."));
        assert!(!r.naive_match(".212"));
        assert!(!r.naive_match("12.21.123"));
        assert!(!r.naive_match("hello"));
    }
}
