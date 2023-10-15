//
pub mod lgraph;

use std::{any::type_name, collections::HashSet, fmt::Display, io::Cursor, io::Write};

use crate::{
    regex::{regular_expression::Regex, state_machine::StateMachine},
    tokenizer::Token,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    OpenBracket(usize, Token, Vec<TokenOrEnd>),
    CloseBracket(usize, Token, Vec<TokenOrEnd>),
    Token(TokenOrEnd),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    lgraph: StateMachine<Item>,
    rule_names: Vec<Token>,
    token_names: Vec<Token>,
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
                Item::OpenBracket(i, n, t) => {
                    write!(f, "({i}\", start=\"{n}\", follow=\"[").unwrap();
                    for t in t {
                        write!(f, "{t},").unwrap();
                    }
                    writeln!(f, "]\"];").unwrap();
                }
                Item::CloseBracket(i, n, t) => {
                    write!(f, "){i}\", end=\"{n}\", follow=\"[").unwrap();
                    for t in t {
                        write!(f, "{t},").unwrap();
                    }
                    writeln!(f, "]\"];").unwrap();
                }
                Item::Token(t) => writeln!(f, "{t}\"]").unwrap(),
            }
        }
        for node in self.lgraph.end_nodes() {
            writeln!(f, "  \"{}\" [shape=doublecircle];", node).unwrap();
        }

        writeln!(f, "}}").unwrap();

        String::from_utf8(s).unwrap()
    }

    fn gen_follow_table(lg: &StateMachine<Item>, node: usize) -> Vec<TokenOrEnd> {
        let mut visited = HashSet::new();
        let mut stack: Vec<_> = vec![node];

        let mut tokens = vec![];
        while let Some(node) = stack.pop() {
            visited.insert(node);
            for (_, item, to) in lg.edges_from(node) {
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
        tokens
    }

    pub fn get_token_names(&self) -> impl Iterator<Item = Token> + '_ {
        self.token_names.iter().cloned()
    }
    pub fn get_rule_names(&self) -> impl Iterator<Item = Token> + '_ {
        self.rule_names.iter().cloned()
    }

    pub fn new(rules: impl IntoIterator<Item = (Token, Regex<Token>)>) -> Self {
        let mut rule_names = vec![];
        let mut untabled_lgraph = StateMachine::new();
        let mut deltas = vec![];

        let mut dfas = vec![];
        for (name, prod) in rules.into_iter() {
            rule_names.push(name);
            let dfa = prod.compile();
            let delta = untabled_lgraph.nodes().count();
            for n in dfa.nodes() {
                untabled_lgraph = untabled_lgraph.add_node(n + delta);
            }
            let mut ends = vec![];
            for n in dfa.end_nodes() {
                ends.push(n + delta);
            }

            dfas.push(dfa);
            deltas.push(delta);
        }

        let mut token_names = vec![];
        let mut bracket_count = 1;

        for (i, dfa) in dfas.iter().enumerate() {
            for (from, tok, to) in dfa.edges() {
                if let Some((j, _)) = rule_names
                    .iter()
                    .enumerate()
                    .find(|(_, t)| t.name() == tok.name())
                {
                    untabled_lgraph = untabled_lgraph.add_edge(
                        from + deltas[i],
                        Item::OpenBracket(bracket_count, rule_names[j].clone(), vec![]),
                        dfas[j].start_node() + deltas[j],
                    );

                    for other_from in dfas[j].end_nodes() {
                        untabled_lgraph = untabled_lgraph.add_edge(
                            other_from + deltas[j],
                            Item::CloseBracket(bracket_count, rule_names[j].clone(), vec![]),
                            to + deltas[i],
                        );
                    }

                    bracket_count += 1;
                } else {
                    if !token_names.contains(&tok) {
                        token_names.push(tok.clone());
                    }
                    untabled_lgraph = untabled_lgraph.add_edge(
                        from + deltas[i],
                        Item::Token(TokenOrEnd::Token(tok)),
                        to + deltas[i],
                    );
                }
            }
        }

        let start_node = untabled_lgraph.nodes().count();
        let pre_final_node = start_node + 1;
        let final_node = start_node + 2;
        untabled_lgraph = untabled_lgraph.add_start_node(start_node);
        untabled_lgraph = untabled_lgraph.add_edge(
            start_node,
            Item::OpenBracket(0, rule_names[0].clone(), vec![]),
            dfas[0].start_node() + deltas[0],
        );

        for end in dfas[0].end_nodes() {
            untabled_lgraph = untabled_lgraph.add_edge(
                end + deltas[0],
                Item::CloseBracket(0, rule_names[0].clone(), vec![]),
                pre_final_node,
            );
        }
        untabled_lgraph =
            untabled_lgraph.add_edge(pre_final_node, Item::Token(TokenOrEnd::End), final_node);
        untabled_lgraph = untabled_lgraph.add_end_node(final_node);

        let mut lgraph = StateMachine::new()
            .add_start_node(start_node)
            .add_end_node(final_node);
        for (from, item, to) in untabled_lgraph.edges() {
            match &item {
                Item::OpenBracket(i, n, _) => {
                    lgraph = lgraph.add_edge(
                        from,
                        Item::OpenBracket(
                            *i,
                            n.clone(),
                            Self::gen_follow_table(&untabled_lgraph, to),
                        ),
                        to,
                    )
                }
                Item::CloseBracket(i, n, _) => {
                    lgraph = lgraph.add_edge(
                        from,
                        Item::CloseBracket(
                            *i,
                            n.clone(),
                            Self::gen_follow_table(&untabled_lgraph, to),
                        ),
                        to,
                    )
                }
                Item::Token(_) => lgraph = lgraph.add_edge(from, item, to),
            }
        }

        // check the parser for determinism
        for n in lgraph.nodes() {
            let mut tokens_and_rb = HashSet::new();
            let mut tokens = HashSet::new();
            for (_, item, _) in lgraph.edges_from(n) {
                match item {
                    Item::OpenBracket(_, _, t) => {
                        for t in t {
                            assert!(tokens.insert(t), "Conflict on {n}")
                        }
                    }
                    Item::CloseBracket(i, _, t) => {
                        for t in t {
                            assert!(!tokens.contains(&t), "Conflict on {n}");
                            assert!(tokens_and_rb.insert((i, t)), "Conflict on {n}");
                        }
                    }
                    Item::Token(t) => assert!(tokens.insert(t), "Conflict on {n}"),
                }
            }
        }

        Self {
            lgraph,
            rule_names,
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
                    Item::CloseBracket(i, _, follow) => {
                        write!(w, "{from} if top == {i} && matches!(tok, ")?;
                        write!(w, "Token::{} ", follow[0])?;
                        for t in follow.iter().skip(1) {
                            write!(w, "| Token::{t} ")?;
                        }
                        write!(w, ") => {{ self.brackets.pop(); ")?;
                    }
                    Item::OpenBracket(i, _, follow) => {
                        write!(w, "{from} if matches!(tok, ")?;
                        write!(w, "Token::{} ", follow[0])?;
                        for t in follow.iter().skip(1) {
                            write!(w, "| Token::{t} ")?;
                        }
                        write!(w, ") => {{ self.brackets.push({i}); ")?;
                    }
                    Item::Token(t) => {
                        write!(w, "{n} if tok == Token::{t} => {{ consumed = true; ")?
                    }
                }

                if let Item::OpenBracket(_, node, _) = &item {
                    write!(w, "res.push(Node::{node}Start); ")?;
                }

                if let Item::Token(TokenOrEnd::Token(t)) = &item {
                    write!(
                        w,
                        "res.push(Node::{t}(self.tokens_eaten)); self.tokens_eaten += 1;"
                    )?;
                }
                if let Item::CloseBracket(_, node, _) = &item {
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
            for (out, actual_out) in get_results(&p, inputs, name)
                .into_iter()
                .zip(outputs.into_iter())
            {
                assert_eq!(out, actual_out, "{} Failed", name);
            }
        }
    }
}
