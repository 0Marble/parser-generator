use super::{charset::CharSet, set_automata::SetAutomata};

pub enum Regex {
    None,
    Empty,
    Base(char),
    Concat(Vec<Regex>),
    Variant(Vec<Regex>),
    Star(Box<Regex>),
    Range(char, char),
    Not(Box<Regex>),
    Option(Box<Regex>),
}

impl Regex {
    pub fn to_nfa(&self) -> SetAutomata {
        match self {
            Regex::None => SetAutomata::new(0),
            Regex::Empty => SetAutomata::new(0).add_end(0),
            Regex::Base(c) => SetAutomata::new(0)
                .add_edge(0, Some(CharSet::range(*c, *c)), 1)
                .add_end(1),
            Regex::Concat(rs) => {
                let first = if let Some(first) = rs.first() {
                    first
                } else {
                    return SetAutomata::new(0).add_end(0);
                };
                let mut res = first.to_nfa();
                for r in &rs[1..] {
                    res = res.concat(&r.to_nfa());
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
                for r in &rs[1..] {
                    res = res.union(&r.to_nfa());
                }
                res
            }
            Regex::Star(r) => r.to_nfa().star(),
            Regex::Range(min, max) => SetAutomata::new(0)
                .add_edge(0, Some(CharSet::range(*min, *max)), 1)
                .add_end(1),
            Regex::Not(r) => r.to_nfa().complement(),
            Regex::Option(r) => {
                let nfa = r.to_nfa();
                let start = nfa.start();
                nfa.add_end(start)
            }
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

    pub fn double_slash_comment() -> Regex {
        Regex::Concat(vec![
            Regex::Base('/'),
            Regex::Base('/'),
            Regex::Star(Box::new(Regex::Not(Box::new(Regex::Base('\n'))))),
            Regex::Base('\n'),
        ])
    }

    pub fn multiline_comment() -> Regex {
        Regex::Concat(vec![
            Regex::Base('/'),
            Regex::Base('*'),
            Regex::Not(Box::new(Regex::Concat(vec![
                Regex::Base('*'),
                Regex::Base('/'),
            ]))),
            Regex::Base('*'),
            Regex::Base('/'),
        ])
    }
}
