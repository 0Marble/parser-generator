//
pub mod lgraph;

use std::{any::type_name, collections::HashSet, fmt::Display, io::Cursor, io::Write};

use crate::{
    regex::{regular_expression::Regex, state_machine::StateMachine},
    tokenizer::Token,
};

#[derive(Debug, Clone, PartialEq)]
enum TokenOrEnd {
    Token(Token),
    End,
}

impl Display for TokenOrEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenOrEnd::Token(t) => write!(f, "{t}"),
            TokenOrEnd::End => write!(f, "_End"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Item {
    OpenBracket(usize, Token),
    CloseBracket(usize, Token),
    Token(TokenOrEnd),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    lgraph: StateMachine<Item>,
    rule_names: Vec<Token>,
    token_names: Vec<Token>,
    open_table: Vec<(usize, Vec<TokenOrEnd>)>,
    close_table: Vec<(usize, Vec<TokenOrEnd>)>,
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
            type_name::<Item>()
        )
        .unwrap();

        for n in self.lgraph.start_nodes() {
            writeln!(f, "  Q1 -> \"{}\";", n).unwrap();
        }
        for (from, item, to) in self.lgraph.edges() {
            write!(f, "{from} -> {to} [label=\"").unwrap();
            match item {
                Item::OpenBracket(i, _) => write!(f, "({i}").unwrap(),
                Item::CloseBracket(i, _) => write!(f, "){i}").unwrap(),
                Item::Token(t) => write!(f, "{t}").unwrap(),
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
        let mut deltas = vec![];

        let mut dfas = vec![];
        for (name, prod) in rules.into_iter() {
            rule_names.push(name);
            let dfa = prod.compile();
            let delta = lgraph.nodes().count();
            for n in dfa.nodes() {
                lgraph = lgraph.add_node(n + delta);
            }
            let mut ends = vec![];
            for n in dfa.end_nodes() {
                ends.push(n + delta);
            }

            dfas.push(dfa);
            deltas.push(delta);
        }

        let mut token_names = vec![];
        dbg!(&rule_names);
        let mut bracket_count = 1;

        let mut needs_table = vec![];
        for (i, dfa) in dfas.iter().enumerate() {
            for (from, tok, to) in dfa.edges() {
                if let Some((j, _)) = rule_names
                    .iter()
                    .enumerate()
                    .find(|(_, t)| t.name() == tok.name())
                {
                    lgraph = lgraph.add_edge(
                        from + deltas[i],
                        Item::OpenBracket(bracket_count, rule_names[j].clone()),
                        dfas[j].start_node() + deltas[j],
                    );
                    needs_table.push((
                        from + deltas[i],
                        Item::OpenBracket(bracket_count, rule_names[j].clone()),
                        dfas[j].start_node() + deltas[j],
                    ));

                    for other_from in dfas[j].end_nodes() {
                        lgraph = lgraph.add_edge(
                            other_from + deltas[j],
                            Item::CloseBracket(bracket_count, rule_names[j].clone()),
                            to + deltas[i],
                        );
                        needs_table.push((
                            other_from + deltas[j],
                            Item::CloseBracket(bracket_count, rule_names[j].clone()),
                            to + deltas[i],
                        ));
                    }

                    bracket_count += 1;
                } else {
                    if !token_names.contains(&tok) {
                        token_names.push(tok.clone());
                    }
                    lgraph = lgraph.add_edge(
                        from + deltas[i],
                        Item::Token(TokenOrEnd::Token(tok)),
                        to + deltas[i],
                    );
                }
            }
        }

        let start_node = lgraph.nodes().count();
        let pre_final_node = start_node + 1;
        let final_node = start_node + 2;
        lgraph = lgraph.add_start_node(start_node);
        lgraph = lgraph.add_edge(
            start_node,
            Item::OpenBracket(0, rule_names[0].clone()),
            dfas[0].start_node() + deltas[0],
        );
        needs_table.push((
            start_node,
            Item::OpenBracket(0, rule_names[0].clone()),
            dfas[0].start_node() + deltas[0],
        ));

        for end in dfas[0].end_nodes() {
            lgraph = lgraph.add_edge(
                end + deltas[0],
                Item::CloseBracket(0, rule_names[0].clone()),
                pre_final_node,
            );
            needs_table.push((
                end + deltas[0],
                Item::CloseBracket(0, rule_names[0].clone()),
                pre_final_node,
            ));
        }
        lgraph = lgraph.add_edge(pre_final_node, Item::Token(TokenOrEnd::End), final_node);
        lgraph = lgraph.add_end_node(final_node);

        let mut open_table = vec![];
        let mut close_table = vec![];
        for (_, bracket, to) in needs_table {
            let mut visited = HashSet::new();
            let mut stack: Vec<_> = vec![to];

            let mut tokens = vec![];
            while let Some(node) = stack.pop() {
                visited.insert(node);
                for (_, item, to) in lgraph.edges_from(node) {
                    match item {
                        Item::Token(t) => {
                            if !tokens.contains(&t) {
                                tokens.push(t)
                            }
                        }
                        _ => {
                            if !visited.contains(&to) {
                                stack.push(to);
                            }
                        }
                    }
                }
            }

            match bracket {
                Item::OpenBracket(i, _) => open_table.push((i, tokens)),
                Item::CloseBracket(i, _) => close_table.push((i, tokens)),
                Item::Token(_) => unreachable!(),
            }
        }

        // check the parser for determinism
        for n in lgraph.nodes() {
            // let mut seen = vec![];
            for (_, item, to) in lgraph.edges_from(n) {
                //
            }
        }

        dbg!(&open_table);

        Self {
            lgraph,
            rule_names,
            token_names,
            open_table,
            close_table,
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
    _End,
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
        write!(w, "}} use std::fmt::Display; impl Display for Token {{ fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{ match self {{ Self::_End => write!(fmt, \"_End\"),")?;
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
        let mut res = vec![];
        let mut consumed = false;
        while !consumed {{
        let top = self.top_bracket();
        match self.state {{
            ",
            self.lgraph.start_nodes().next().unwrap()
        )?;

        for n in self.lgraph.nodes() {
            for (from, item, to) in self.lgraph.edges_from(n) {
                match &item {
                    Item::CloseBracket(i, _) => {
                        write!(w, "{from} if top == {i} && matches!(tok, ")?;
                        let next_letters = self.close_table.iter().find(|(idx, _)| idx == i);
                        assert!(next_letters.is_some(), "No next_letters for {from}, {i}");
                        let next_letters = &next_letters.unwrap().1;
                        assert!(
                            !next_letters.is_empty(),
                            "Empty next_letters for {from}, {i}"
                        );
                        write!(w, "Token::{} ", next_letters[0])?;
                        for t in next_letters.iter().skip(1) {
                            write!(w, "| Token::{t} ")?;
                        }
                        write!(w, ") => {{ self.brackets.pop(); ")?;
                    }
                    Item::OpenBracket(i, _) => {
                        write!(w, "{from} if matches!(tok, ")?;
                        let next_letters = self.open_table.iter().find(|(idx, _)| idx == i);
                        assert!(next_letters.is_some(), "No next_letters for {from}, {i}");
                        let next_letters = &next_letters.unwrap().1;
                        assert!(
                            !next_letters.is_empty(),
                            "Empty next_letters for {from}, {i}"
                        );
                        write!(w, "Token::{} ", next_letters[0])?;
                        for t in next_letters.iter().skip(1) {
                            write!(w, "| Token::{t} ")?;
                        }
                        write!(w, ") => {{ self.brackets.push({i}); ")?;
                    }
                    Item::Token(t) => {
                        write!(w, "{n} if tok == Token::{t} => {{ consumed = true; ")?
                    }
                }

                if let Item::OpenBracket(_, node) = &item {
                    write!(w, "res.push(Node::{node}Start); ")?;
                }

                if let Item::Token(TokenOrEnd::Token(t)) = &item {
                    write!(
                        w,
                        "res.push(Node::{t}(self.tokens_eaten)); self.tokens_eaten += 1;"
                    )?;
                }
                if let Item::CloseBracket(_, node) = &item {
                    write!(w, "res.push(Node::{node}End); ")?;
                }

                writeln!(w, "self.state = {to}; }}")?;
            }
        }
        writeln!(
            w,
            "
            _ => panic!(\"Could not continue on {{}}, {{top}}, {{tok}}!\", self.state) 
        }} }}
    res
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
    let input = [{}, _End];
    let mut p = Parser::new();
    
    for tok in input {{
        for node in p.eat_token(tok) {{
            print!(\"{{}},\", node);
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
            let output = process.wait_with_output().unwrap();
            assert!(
                output.status.success(),
                "stderr: {}",
                String::from_utf8(output.stderr).unwrap()
            );
            let output = String::from_utf8(output.stdout).unwrap();
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
                vec!["Lp, Rp", "Lp,Lp,Rp,Rp"],
                vec![
                    "AStart,Lp(0),AStart,AEnd,Rp(1),AStart,AEnd,AEnd,",
                    "AStart,Lp(0),AStart,Lp(1),AStart,AEnd,Rp(2),AStart,AEnd,AEnd,Rp(3),AStart,AEnd,AEnd,",
                ],
            ),
            (
                "expr",
                expr_grammar(),
                vec![
                    "Ident,Add,Ident", 
                    "Ident,Mul,Lp,Ident,Add,Ident,Rp"
                ],
                vec![
                    "ExprStart,TermStart,FactorStart,Ident(0),FactorEnd,TermEnd,Add(1),TermStart,FactorStart,Ident(2),FactorEnd,TermEnd,ExprEnd,", 
                    "ExprStart,TermStart,FactorStart,Ident(0),FactorEnd,Mul(1),FactorStart,Lp(2),ExprStart,TermStart,FactorStart,Ident(3),FactorEnd,TermEnd,Add(4),TermStart,FactorStart,Ident(5),FactorEnd,TermEnd,ExprEnd,Rp(6),FactorEnd,TermEnd,ExprEnd,",
                ],
            ),
        ] {
            let p = Parser::new(grammar);
            println!("{}", p.to_dot());
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
