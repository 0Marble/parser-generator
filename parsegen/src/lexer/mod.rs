use std::fmt::Display;

use crate::Token;

use self::{charset::CharSet, regex::Regex, set_automata::SetAutomata};

pub mod charset;
pub mod regex;
pub mod set_automata;

pub struct Lexer {
    dfa: SetAutomata,
    tokens: Vec<(usize, Token)>,
    dead_states: Vec<usize>,
}

impl Display for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{\nnode [shape=circle];\nQ1 [style=invisible, height=0, width=0, fixedsize=true];")?;
        writeln!(f, "Q1 -> {};", self.start())?;
        for (from, set, to) in self.edges() {
            writeln!(f, "{} -> {} [label=\"{}\"];", from, to, set)?;
        }

        for n in self.ends() {
            writeln!(f, "{n} [shape=\"doublecircle\"]; ")?;
        }

        for (node, tok) in self.tokens.iter().rev() {
            writeln!(f, "{node} [label=\"{tok}\"];")?;
        }
        for dead in self.dead_states() {
            writeln!(f, "{dead} [label=\"☠\"];")?;
        }

        writeln!(f, "}}")
    }
}

impl Lexer {
    pub fn new(regexes: impl IntoIterator<Item = (Regex, Token)>) -> Self {
        let mut nfa = SetAutomata::new(0);
        let mut tokens = vec![];

        for (rg, tok) in regexes {
            let rg_nfa = rg.to_nfa().determine().0.minimize(false).0;
            std::fs::write(format!("tests/lexer_nfa_{tok}.dot"), rg_nfa.to_string()).unwrap();
            let t = nfa.nodes().max().unwrap_or_default() + 1;
            let (new_nfa, new_names) = nfa.union(&rg_nfa);
            nfa = new_nfa;
            for end in rg_nfa.ends() {
                let node = if end == rg_nfa.start() {
                    nfa.start()
                } else {
                    end + t
                };
                assert_eq!(
                    new_names
                        .iter()
                        .filter(|(old, _)| *old == end)
                        .map(|(_, x)| *x)
                        .collect::<Vec<_>>(),
                    vec![node],
                );
                tokens.push((node, tok.clone()));
            }
        }

        let (dfa, dfa_nodes) = nfa.determine();
        let (mdfa, mdfa_nodes) = dfa.minimize(true);
        println!("{tokens:?}\n{dfa_nodes:?}\n{mdfa_nodes:?}\n");
        std::fs::write("tests/lexer_nfa.dot", nfa.to_string()).unwrap();
        std::fs::write("tests/lexer_dfa.dot", dfa.to_string()).unwrap();
        std::fs::write("tests/lexer_mdfa.dot", mdfa.to_string()).unwrap();

        let mut new_tokens = vec![];
        for (old_node, tok) in tokens {
            for (dfa_node, _) in dfa_nodes
                .iter()
                .enumerate()
                .filter(|(_, set)| set.contains(&old_node))
            {
                for (_, mdfa_node) in mdfa_nodes.iter().filter(|(n, _)| *n == dfa_node) {
                    new_tokens.push((*mdfa_node, tok.clone()));
                }
            }
        }
        println!("{new_tokens:?}");

        Lexer {
            dead_states: mdfa.dead_states().collect(),
            dfa: mdfa.remove_unreachable(),
            tokens: new_tokens,
        }
    }

    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.dfa.nodes()
    }

    pub fn edges(&self) -> impl Iterator<Item = (usize, &CharSet, usize)> {
        self.dfa.edges().map(|(from, set, to)| (from, set, to))
    }

    pub fn edges_from(&self, node: usize) -> impl Iterator<Item = (usize, &CharSet, usize)> {
        self.dfa
            .edges_from(node)
            .map(|(from, set, to)| (from, set, to))
    }

    pub fn edges_to(&self, node: usize) -> impl Iterator<Item = (usize, &CharSet, usize)> {
        self.dfa
            .edges_to(node)
            .map(|(from, set, to)| (from, set, to))
    }

    pub fn ends(&self) -> impl Iterator<Item = usize> + '_ {
        self.dfa.ends()
    }

    pub fn dead_states(&self) -> impl Iterator<Item = usize> + '_ {
        self.dead_states.iter().cloned()
    }

    pub fn is_dead_state(&self, node: usize) -> bool {
        self.dead_states.contains(&node)
    }

    pub fn accept_token(&self, node: usize) -> Option<Token> {
        self.tokens
            .iter()
            .find(|(n, _)| *n == node)
            .map(|(_, tok)| tok.clone())
    }

    pub fn start(&self) -> usize {
        self.dfa.start()
    }

    pub fn step(&self, node: usize, letter: char) -> usize {
        self.edges_from(node)
            .find(|(_, set, _)| set.contains(letter))
            .unwrap()
            .2
    }
}

#[cfg(test)]
mod tests;
