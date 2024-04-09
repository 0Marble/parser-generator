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
        println!("matching \"{s}\" with {self:?}");
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
}

#[cfg(test)]
mod tests {
    use rand::thread_rng;

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
}
