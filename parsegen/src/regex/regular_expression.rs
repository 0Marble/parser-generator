//

use super::state_machine::{Dfa, Nfa};

#[derive(Debug, Clone, PartialEq)]
pub enum Regex<T> {
    Base(T),
    Seq(Vec<Regex<T>>),
    Star(Box<Regex<T>>),
    Plus(Box<Regex<T>>),
    Maybe(Box<Regex<T>>),
    Variant(Vec<Regex<T>>),
}

impl<T> Regex<T>
where
    T: PartialEq + Clone,
{
    pub fn compile(&self) -> Dfa<T> {
        self.compile_rec().determine().minimize()
    }
    fn compile_rec(&self) -> Nfa<T> {
        match self {
            Regex::Base(c) => Nfa::new()
                .add_edge(0, Some(c.clone()), 1)
                .add_start_node(0)
                .add_end_node(1),
            Regex::Seq(a) => {
                let mut res = a[0].compile_rec();
                for a in a.iter().skip(1) {
                    res = res.concat(&a.compile_rec());
                }

                res
            }
            Regex::Star(r) => r.compile_rec().star(),
            Regex::Plus(r) => {
                let res = r.compile_rec();
                res.concat(&res.star())
            }
            Regex::Maybe(r) => r
                .compile_rec()
                .union(&Nfa::new().add_start_node(0).add_end_node(0)),
            Regex::Variant(a) => {
                let mut res = a[0].compile_rec();

                for a in a.iter().skip(1) {
                    res = res.union(&a.compile_rec());
                }
                res
            }
        }
    }
}

impl Regex<char> {
    pub fn range(from: char, to: char) -> Self {
        Self::Variant((from..=to).map(Self::Base).collect())
    }
    pub fn uint() -> Self {
        Self::Plus(Box::new(Self::range('0', '9')))
    }
    pub fn ident() -> Self {
        Self::Seq(vec![
            Self::Variant(vec![Self::Base('_'), Self::ascii_letters()]),
            Self::Star(Box::new(Self::Variant(vec![
                Self::Base('_'),
                Self::ascii_letters(),
                Self::numbers(),
            ]))),
        ])
    }
    pub fn numbers() -> Self {
        Self::range('0', '9')
    }
    pub fn ascii_letters() -> Self {
        Self::Variant(vec![Self::range('a', 'z'), Self::range('A', 'Z')])
    }
    pub fn ascii_whitespace() -> Self {
        Self::Variant(vec![Self::Base(' '), Self::Base('\n'), Self::Base('\t')])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reg1() -> Regex<char> {
        Regex::Seq(vec![
            Regex::Plus(Box::new(Regex::Seq(vec![
                Regex::Star(Box::new(Regex::Variant(vec![
                    Regex::Base('a'),
                    Regex::Base('b'),
                ]))),
                Regex::Base('a'),
                Regex::Star(Box::new(Regex::Variant(vec![
                    Regex::Base('a'),
                    Regex::Base('b'),
                ]))),
                Regex::Base('a'),
                Regex::Star(Box::new(Regex::Variant(vec![
                    Regex::Base('a'),
                    Regex::Base('b'),
                ]))),
            ]))),
            Regex::Base('b'),
        ])
    }
    fn reg2() -> Regex<char> {
        Regex::Seq(vec![
            Regex::Plus(Box::new(Regex::Seq(vec![
                Regex::Base('a'),
                Regex::Star(Box::new(Regex::Variant(vec![
                    Regex::Base('a'),
                    Regex::Base('b'),
                ]))),
            ]))),
            Regex::Base('b'),
        ])
    }

    fn sequence() -> Regex<char> {
        Regex::Seq(vec![Regex::Base('a'), Regex::Base('b'), Regex::Base('c')])
    }
    fn variant() -> Regex<char> {
        Regex::Variant(vec![Regex::Base('a'), Regex::Base('b'), Regex::Base('c')])
    }
    fn star() -> Regex<char> {
        Regex::Star(Box::new(Regex::Base('a')))
    }
    fn simple_rec() -> Regex<char> {
        Regex::Star(Box::new(Regex::Variant(vec![
            Regex::Base('a'),
            Regex::Base('b'),
        ])))
    }

    #[test]
    fn basic() {
        for (name, m, examples) in [
            (
                "star",
                star().compile(),
                vec![
                    ("", true),
                    ("a", true),
                    ("aaaaaaa", true),
                    ("aaaaba", false),
                ],
            ),
            (
                "sequence",
                sequence().compile(),
                vec![("abc", true), ("a", false), ("ab", false), ("", false)],
            ),
            (
                "variant",
                variant().compile(),
                vec![("a", true), ("b", true), ("c", true), ("ab", false)],
            ),
            (
                "simple_rec",
                simple_rec().compile(),
                vec![("", true), ("a", true), ("aabaa", true), ("aababc", false)],
            ),
            (
                "reg2",
                reg2().compile(),
                vec![
                    ("ab", true),
                    ("abababbab", true),
                    ("bbb", false),
                    ("babababa", false),
                ],
            ),
            (
                "reg1",
                reg1().compile(),
                vec![
                    ("aab", true),
                    ("aaaaaab", true),
                    ("ababbababab", true),
                    ("bbbabbbaab", true),
                    ("abb", false),
                    ("ababaa", false),
                    ("aaaa", false),
                    ("", false),
                    ("bbbabb", false),
                ],
            ),
        ] {
            for (s, res) in examples {
                assert_eq!(m.traverse(s.chars()), res, "{name}: failed on s=\"{}\"", s);
            }
        }
    }

    #[test]
    fn uint() {
        let m = Regex::uint().compile();

        for (s, res) in vec![
            ("", false),
            ("1", true),
            ("10", true),
            ("2312314312", true),
            ("12312w221", false),
        ] {
            assert_eq!(m.traverse(s.chars()), res, "failed on s={}", s);
        }

        assert_eq!(m.nodes().count(), 2);
        assert_eq!(m.end_nodes().count(), 1);
        assert_eq!(m.edges().count(), 10 * 2);
    }

    #[test]
    fn ident() {
        let m = Regex::ident().compile();

        for (s, res) in vec![
            ("foo", true),
            ("_type", true),
            ("AbstractFactoryFactory", true),
            ("M1xed_C4s3_1230", true),
            ("1m_a_failure", false),
            ("oops whitespace", false),
        ] {
            assert_eq!(m.traverse(s.chars()), res, "failed on s=\"{}\"", s);
        }

        assert_eq!(m.nodes().count(), 2);
        assert_eq!(m.end_nodes().count(), 1);
        assert_eq!(m.edges().count(), 2 + 4 * 26 + 10);
    }
}
