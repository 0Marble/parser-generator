//
pub mod lgraph;

use std::{any::type_name, io::Cursor, io::Write};

use crate::{
    regex::{regular_expression::Regex, state_machine::StateMachine},
    tokenizer::Token,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    lgraph: StateMachine<(Option<(usize, bool)>, Option<Token>)>,
    rule_names: Vec<Token>,
    token_names: Vec<Token>,
    sec_starts: Vec<usize>,
    sec_ends: Vec<Vec<usize>>,
}

impl Parser {
    pub fn to_dot(&self) -> String {
        let mut s = vec![];
        let mut f = Cursor::new(&mut s);
        writeln!(f, "digraph {{\n  node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];").unwrap();
        writeln!(
            f,
            "  node_type = \"{}\";\n  item_type = \"{}\";\n  kind = nfa;",
            type_name::<usize>(),
            type_name::<(Option<(usize, bool)>, Option<Token>)>()
        )
        .unwrap();

        for n in self.lgraph.start_nodes() {
            writeln!(f, "  Q1 -> \"{}\";", n).unwrap();
        }
        for (from, (bracket, token), to) in self.lgraph.edges() {
            write!(f, "{from} -> {to} [label=\"").unwrap();
            if let Some(tok) = token {
                write!(f, "{tok}").unwrap();
            }
            if let Some((id, is_open)) = bracket {
                if is_open {
                    write!(f, "({id}").unwrap();
                } else {
                    write!(f, "){id}").unwrap();
                }
            }
            writeln!(f, "\"];").unwrap();
        }
        for node in self.lgraph.end_nodes() {
            writeln!(f, "  \"{}\" [shape=doublecircle];", node).unwrap();
        }

        writeln!(f, "}}").unwrap();

        String::from_utf8(s).unwrap()
    }
    pub fn new(rules: impl IntoIterator<Item = (Token, Regex<Token>)>) -> Self {
        let mut rule_names = vec![];
        let mut lgraph = StateMachine::new();
        let mut sec_starts = vec![];
        let mut sec_ends = vec![];
        let mut deltas = vec![];

        let mut dfas = vec![];
        for (name, prod) in rules.into_iter() {
            rule_names.push(name);
            let dfa = prod.compile();
            let delta = lgraph.nodes().count();
            for n in dfa.nodes() {
                lgraph = lgraph.add_node(n + delta);
            }
            sec_starts.push(dfa.start_node() + delta);
            let mut ends = vec![];
            for n in dfa.end_nodes() {
                ends.push(n + delta);
            }
            sec_ends.push(ends);

            dfas.push(dfa);
            deltas.push(delta);
        }

        let mut token_names = vec![];
        dbg!(&rule_names);
        let mut bracket_count = 1;
        for (i, dfa) in dfas.iter().enumerate() {
            for (from, tok, to) in dfa.edges() {
                if let Some((j, _)) = rule_names
                    .iter()
                    .enumerate()
                    .find(|(_, t)| t.name() == tok.name())
                {
                    // TODO need to use an extra table, because the `tok` here may itself be a rule
                    // name, like in the `expr` example
                    // and also connecting to the second nodes like that may lead to some nodes
                    // being left hanging, like in the `balanced_brackets` example
                    for (_, tok, other_to) in dfas[j].edges_from(dfas[j].start_node()) {
                        lgraph = lgraph.add_edge(
                            from + deltas[i],
                            (Some((bracket_count, true)), Some(tok)),
                            other_to + deltas[j],
                        );
                    }

                    for other_from in sec_ends[j].iter() {
                        lgraph = lgraph.add_edge(
                            other_from + deltas[j],
                            (Some((bracket_count, false)), None),
                            to + deltas[i],
                        );
                    }

                    bracket_count += 1;
                } else {
                    if !token_names.contains(&tok) {
                        token_names.push(tok.clone());
                    }
                    lgraph = lgraph.add_edge(from + deltas[i], (None, Some(tok)), to + deltas[i]);
                }
            }
        }

        lgraph = lgraph.add_start_node(sec_starts[0]);
        for end in &sec_ends[0] {
            lgraph = lgraph.add_end_node(*end);
        }

        for n in lgraph.nodes() {
            let mut seen: Vec<(Option<(usize, bool)>, Option<Token>)> = vec![];
            for (_, item, to) in lgraph.edges_from(n) {
                assert_ne!(
                    item,
                    (None, None),
                    "Empty edge {n} -> {to} (shouldnt ever happen)"
                );

                if matches!(item.0, Some((_, true)),) {
                    assert_ne!(
                        item.1, None,
                        "Single open bracket on edge {n} -> {to} (shouldnt ever happen)"
                    );
                }

                if let Some(token) = &item.1 {
                    for (other_bracket, other_token) in &seen {
                        if other_token.as_ref() == Some(token) {
                            match (other_bracket, item.0) {
                                (Some((other_idx, other_is_open)), Some((idx, is_open))) => {
                                    assert!(
                                        !is_open && !other_is_open && *other_idx != idx,
                                        "Conflict on node {n}!"
                                    )
                                }
                                _ => panic!("Conflict on node {n}!"),
                            }
                        }
                    }
                }
                seen.push(item);
            }
        }

        Self {
            lgraph,
            rule_names,
            sec_starts,
            sec_ends,
            token_names,
        }
    }

    pub fn to_rs(&self, w: &mut dyn std::io::Write) -> std::io::Result<()> {
        write!(
            w,
            "// This file is autogenerated
/*graph: 
        {}
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {{
",
            self.to_dot()
        )?;
        for tok in &self.token_names {
            write!(w, "{},\n", tok.name())?;
        }
        write!(
            w,
            "}}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Node {{
"
        )?;
        for rule in &self.rule_names {
            write!(w, "{}Start,\n", rule.name())?;
            write!(w, "{}End,\n", rule.name())?;
        }
        for tok in &self.token_names {
            write!(w, "{}(usize),\n", tok.name())?;
        }
        write!(w, "}} use std::fmt::Display; impl Display for Token {{ fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{ match self {{ ")?;
        for tok in &self.token_names {
            writeln!(
                w,
                "Self::{} => write!(fmt, \"{}\"),",
                tok.name(),
                tok.name()
            )?;
        }
        writeln!(w, "}} }} }}")?;

        write!(w, " impl Display for Node {{ fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{ match self {{ ")?;
        for tok in &self.token_names {
            writeln!(
                w,
                "Self::{}(i) => write!(fmt, \"{}({{i}})\"),",
                tok.name(),
                tok.name()
            )?;
        }
        for rule in &self.rule_names {
            writeln!(
                w,
                "Self::{}Start => write!(fmt, \"{}Start\"),",
                rule.name(),
                rule.name()
            )?;
            writeln!(
                w,
                "Self::{}End=> write!(fmt, \"{}End\"),",
                rule.name(),
                rule.name()
            )?;
        }
        writeln!(w, "}} }} }}")?;

        write!(
            w,
            "
#[derive(Debug, Clone, PartialEq)]
pub struct Parser {{
    state: usize,
    brackets: Vec<usize>,
    tokens_eaten: usize,
}}

impl Parser {{
    pub fn new() -> Self {{
        Self {{ state: {}, brackets: vec![0], tokens_eaten: 0 }}
    }}

    fn top_bracket(&self) -> usize {{ self.brackets.last().cloned().unwrap() }}

    pub fn eat_token(&mut self, tok: Token) -> Vec<Node> {{
        let top = self.top_bracket();
        let mut res = vec![];
        let mut consumed = false;
        while !consumed {{
        match self.state {{
            ",
            self.lgraph.start_nodes().next().unwrap()
        )?;

        for n in self.lgraph.nodes() {
            for (_, (bracket, token), to) in self.lgraph.edges_from(n) {
                match (bracket, token.as_ref()) {
                    (None, None) => unreachable!(),
                    (None, Some(tok)) => write!(w, "{} if tok == Token::{} => {{ ", n, tok)?,
                    (Some((id, is_open)), None) => {
                        if is_open {
                            write!(w, "{} => {{ self.brackets.push({}); ", n, id)?;
                        } else {
                            write!(w, "{} if top == {} => {{ self.brackets.pop(); ", n, id)?;
                        }
                    }
                    (Some((id, is_open)), Some(tok)) => {
                        write!(w, "{n} if tok == Token::{tok} ")?;
                        if is_open {
                            write!(w, "=> {{ self.brackets.push({id});")?;
                        } else {
                            write!(w, "&& top == {id} => {{ self.brackets.pop(); ")?;
                        }
                    }
                }

                if let Some((idx, _)) = self
                    .sec_starts
                    .iter()
                    .enumerate()
                    .find(|(_, node)| **node == n)
                {
                    write!(w, "res.push(Node::{}Start); ", self.rule_names[idx].name())?;
                }
                if let Some(tok) = token.as_ref() {
                    write!(
                        w,
                        "res.push(Node::{}(self.tokens_eaten)); self.tokens_eaten += 1; consumed = true; ",
                        tok
                    )?;
                }

                if let Some((idx, _)) = self
                    .sec_ends
                    .iter()
                    .enumerate()
                    .find(|(_, ends)| ends.contains(&to))
                {
                    write!(w, "res.push(Node::{}End); ", self.rule_names[idx].name())?;
                }

                writeln!(w, "self.state = {to}; }}")?;
            }
        }
        writeln!(
            w,
            "
            _ => panic!(\"Could not continue on {{}}, {{top}}, {{tok}}!\", self.state) 
        }} }}
    return res;
    }}

    pub fn is_in_end_state(&self) -> bool {{
        if self.brackets.len() != 1 || self.brackets[0] != 0 {{ return false; }}
        match self.state {{"
        )?;
        for n in self.lgraph.end_nodes() {
            writeln!(w, "{n} => true,")?;
        }
        writeln!(w, "_ => false, }} }} }}")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fmt::Display,
        io::Write,
        process::{Command, Stdio},
    };

    use super::*;

    fn get_results<S: Display>(
        t: &Parser,
        inputs: impl IntoIterator<Item = S>,
        path: &str,
    ) -> Vec<String> {
        let mut res = vec![];

        for input in inputs {
            Command::new("cargo").args(["init", path]).output().unwrap();
            let mut main_src = std::fs::File::create(format!("{path}/src/main.rs")).unwrap();
            writeln!(
                main_src,
                "
mod parser;
use parser::*;

fn main() {{
    use Token::*;
    let input = [{}];
    let mut p = Parser::new();
    
    for tok in input {{
        for node in p.eat_token(tok) {{
            print!(\"{{}}\", node);
        }}
    }}
    assert!(p.is_in_end_state());
}}
",
                input
            )
            .unwrap();

            t.to_rs(&mut std::fs::File::create(format!("{path}/src/parser.rs")).unwrap())
                .unwrap();

            let mut process = Command::new("cargo")
                .args(["run", "--manifest-path", &format!("{path}/Cargo.toml")])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap();
            let mut stdin = process.stdin.take().unwrap();
            writeln!(&mut stdin, "{}", input.to_string()).unwrap();
            drop(stdin);
            let output = process.wait_with_output().unwrap().stdout;
            let output = String::from_utf8(output).unwrap();
            Command::new("rm").args(["-rf", path]).output().unwrap();

            res.push(output);
        }
        res
    }

    fn expr_grammar() -> Vec<(Token, Regex<Token>)> {
        let expr = Token::new("Expr");
        let term = Token::new("Term");
        let factor = Token::new("Factor");
        let num = Token::new("Number");
        let ident = Token::new("Ident");
        let add = Token::new("Add");
        let mul = Token::new("Mul");
        let lp = Token::new("Lp");
        let rp = Token::new("Rp");

        vec![
            (
                expr.clone(),
                Regex::Seq(vec![
                    Regex::Base(term.clone()),
                    Regex::Star(Box::new(Regex::Seq(vec![
                        Regex::Base(add),
                        Regex::Base(term.clone()),
                    ]))),
                ]),
            ), // Expr -> Term (+ Term)*;
            (
                term,
                Regex::Seq(vec![
                    Regex::Base(factor.clone()),
                    Regex::Star(Box::new(Regex::Seq(vec![
                        Regex::Base(mul),
                        Regex::Base(factor.clone()),
                    ]))),
                ]),
            ), // Term -> Factor (* Factor)*;
            (
                factor,
                Regex::Variant(vec![
                    Regex::Base(num),
                    Regex::Base(ident),
                    Regex::Seq(vec![Regex::Base(lp), Regex::Base(expr), Regex::Base(rp)]),
                ]),
            ), // Factor -> Number | Ident | '(' Expr ')'
        ]
    }

    fn balanced_brackets_grammar() -> Vec<(Token, Regex<Token>)> {
        let lp = Token::new("Lp");
        let rp = Token::new("Rp");
        let a = Token::new("A");

        vec![(
            a.clone(),
            Regex::Maybe(Box::new(Regex::Seq(vec![
                Regex::Base(lp),
                Regex::Base(a.clone()),
                Regex::Base(rp),
                Regex::Base(a),
            ]))),
        )]
    }

    #[test]
    fn parsers() {
        for (name, grammar, inputs, outputs) in [
            (
                "balanced_bracket",
                balanced_brackets_grammar(),
                vec!["Lp, Rp"],
                vec![],
            ),
            (
                "expr",
                expr_grammar(),
                vec!["Ident,Add, Ident"],
                vec!["asd"],
            ),
        ] {
            let p = Parser::new(grammar);
            p.to_rs(&mut std::fs::File::create(format!("src/{name}.rs")).unwrap())
                .unwrap();
            for (out, actual_out) in get_results(&p, inputs, name)
                .into_iter()
                .zip(outputs.into_iter())
            {
                assert_eq!(out, actual_out, "{} Failed", name);
            }
        }
    }
}
