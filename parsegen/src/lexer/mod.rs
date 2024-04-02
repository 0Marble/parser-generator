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

impl Lexer {
    pub fn new(regexes: impl IntoIterator<Item = (Regex, Token)>) -> Self {
        let mut nfa = SetAutomata::new(0);
        let mut offset = 1;
        let mut tokens = vec![];

        for (rg, tok) in regexes {
            let rg_nfa = rg.to_nfa().remove_nones().rename(|n| n + offset);
            for end in rg_nfa.ends() {
                tokens.push((end, tok.clone()));
                nfa = nfa.add_end(end);
            }
            offset += rg_nfa.nodes().max().unwrap() + 1;
            for (from, item, to) in rg_nfa.edges() {
                nfa = nfa.add_edge(from, item.clone(), to);
            }
            nfa = nfa.add_edge(0, None, rg_nfa.start());
        }

        let (dfa, dfa_nodes) = nfa.remove_nones().determine();
        let (mdfa, mdfa_nodes) = dfa.minimize();
        println!("{tokens:?}\n{dfa_nodes:?}\n{mdfa_nodes:?}\n");
        std::fs::write("tests/lexer_nfa.dot", nfa.to_string()).unwrap();
        std::fs::write("tests/lexer_dfa.dot", dfa.to_string()).unwrap();
        std::fs::write("tests/lexer_mdfa.dot", mdfa.to_string()).unwrap();

        let mut new_tokens = vec![];
        for (old_node, tok) in tokens {
            let (dfa_node, _) = dfa_nodes
                .iter()
                .enumerate()
                .find(|(_, set)| set.contains(&old_node))
                .unwrap();
            let (_, mdfa_node) = mdfa_nodes.iter().find(|(n, _)| *n == dfa_node).unwrap();
            new_tokens.push((mdfa_node + 1, tok));
        }

        Lexer {
            dead_states: mdfa.dead_states().collect(),
            dfa: mdfa,
            tokens: new_tokens,
        }
    }

    pub fn nodes(&self) -> impl Iterator<Item = usize> + '_ {
        self.dfa.nodes()
    }

    pub fn edges(&self) -> impl Iterator<Item = (usize, &CharSet, usize)> {
        self.dfa
            .edges()
            .map(|(from, set, to)| (from, set.as_ref().unwrap(), to))
    }

    pub fn edges_from(&self, node: usize) -> impl Iterator<Item = (usize, &CharSet, usize)> {
        self.dfa
            .edges_from(node)
            .map(|(from, set, to)| (from, set.as_ref().unwrap(), to))
    }

    pub fn edges_to(&self, node: usize) -> impl Iterator<Item = (usize, &CharSet, usize)> {
        self.dfa
            .edges_to(node)
            .map(|(from, set, to)| (from, set.as_ref().unwrap(), to))
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
