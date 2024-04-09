use std::{
    char,
    collections::{HashMap, HashSet},
    io::Write,
    process::Command,
};

use crate::{
    lexer::Lexer,
    parser::{
        grammar::{Grammar, Node, TokenOrEnd},
        lgraph::{Bracket, Item, Lgraph},
    },
};

use super::Codegen;

pub struct RustCodegen;

fn next_char(mut u: u32) -> Option<char> {
    loop {
        if let Some(c) = char::from_u32(u) {
            return Some(c);
        }

        if u == u32::MAX {
            return None;
        }
        u += 1;
    }
}
fn prev_char(mut u: u32) -> Option<char> {
    loop {
        if let Some(c) = char::from_u32(u) {
            return Some(c);
        }

        if u == 0 {
            return None;
        }
        u -= 1;
    }
}

impl RustCodegen {
    fn write_lexer(&self, w: &mut dyn Write, lexer: &Lexer) -> std::io::Result<()> {
        writeln!(
            w,
            "#[derive(Debug, Clone, PartialEq, Eq, Hash)]\npub enum TokenType {{\n_End,\n"
        )?;
        let toks: HashSet<_> = lexer.tokens().map(|(_, t)| t).collect();
        for t in &toks {
            writeln!(w, "  {t},")?;
        }
        writeln!(w, "}}\n")?;

        writeln!(
            w,
            "impl TokenType {{\npub fn from_str(s: &str) -> Self {{\nmatch s {{"
        )?;
        for t in &toks {
            writeln!(w, "\"{t}\" => Self::{t},")?;
        }
        writeln!(w, "x => panic!(\"Not a token: {{x}}\"),}}\n}}\n}}")?;

        writeln!(
            w,
            "#[derive(Debug, Clone, PartialEq, Eq, Hash)]\npub enum Token {{\n  Token(TokenType, String),\n  Garbage(String)\n}}\n"
        )?;
        writeln!(w, "impl Token {{ pub fn get_type(&self) -> Option<TokenType> {{ match self {{ Self::Token(t, _) => Some(t.clone()), _ => None }} }} pub fn get_text(&self) -> &str {{ match self {{ Self::Token(_, s) => s, Self::Garbage(s) => s }} }} }}")?;

        writeln!(
            w,
            "pub struct Lexer {{\n  cur: usize,\n  cur_word: String,  start: usize,\n}}\nimpl Lexer {{"
        )?;

        writeln!(
            w,
            "// Tries to move from node on letter c, panics if no way to move\nfn step(&self, node: usize, c: char) -> usize {{\n  let d = c as u32;\n  match node {{\n"
        )?;

        for (from, set, to) in lexer.edges() {
            write!(w, "    {from} if ")?;

            for (min, max) in set.ranges() {
                write!(w, "d >= {min} && d <= {max} ||")?;
            }

            writeln!(w, "false => return {to},")?;
        }

        writeln!(w, "    x => panic!(\"No way to continue: {{x}}\")")?;
        writeln!(w, "  }}\n}}\n")?;

        writeln!(
            w,
            "// Checks if a node is a dead state\nfn is_dead_state(&self, node: usize) -> bool {{\n  match node {{\n"
        )?;
        for n in lexer.dead_states() {
            writeln!(w, "    {n} => return true,")?;
        }
        writeln!(w, "    _ => return false,\n  }}\n}}\n")?;

        writeln!(w, "// Checks if a node is an end node and returns the associated token type if so\nfn is_end(&self, node: usize) -> Option<TokenType> {{\n")?;
        writeln!(w, "  match node {{")?;
        let mut ends = HashMap::new();
        for (end, tok) in lexer.tokens() {
            if ends.contains_key(&end) {
                continue;
            }
            ends.insert(end, tok);
        }
        for (end, tok) in ends {
            writeln!(w, "    {end} => return Some(TokenType::{tok}),")?;
        }
        writeln!(w, "    _ => return None,\n  }}\n}}")?;

        writeln!(
            w,
            "// Eats a character c and possibly returns the next token
pub fn eat_char(&mut self, c: char) -> Option<Token> {{ 
  let mut res = None;
  let next = self.step(self.cur, c);

  if self.is_dead_state(next) {{
    let restart = self.step(self.start, c);
    if let Some(tok) = self.is_end(self.cur) {{
      res = Some(Token::Token(tok, std::mem::take(&mut self.cur_word)));
    }} else if !self.is_dead_state(restart) {{
      res = Some(Token::Garbage(std::mem::take(&mut self.cur_word)));
    }}
    self.cur = restart;
  }} else {{
    self.cur = next;
  }}
  self.cur_word.push(c);
  
  return res;
}}"
        )?;

        writeln!(
            w,
            "// When the text ends, returns the last token
pub fn finalize(&mut self) -> Token {{
  if let Some(tok) = self.is_end(self.cur) {{
    return Token::Token(tok, std::mem::take(&mut self.cur_word));
  }} else {{
    return Token::Garbage(std::mem::take(&mut self.cur_word));
  }}
}}"
        )?;

        writeln!(
            w,
            "// Create a new Lexer
pub fn new() -> Self {{
  Self {{
    cur: {},
    cur_word: String::new(),
    start: {},
  }}
}}",
            lexer.start(),
            lexer.start()
        )?;

        writeln!(w, "}} // impl Lexer\n")?;
        Ok(())
    }

    fn write_parser(
        &self,
        w: &mut dyn Write,
        parser: &Lgraph,
        grammar: &Grammar,
    ) -> std::io::Result<()> {
        writeln!(
            w,
            "use crate::lexer::{{TokenType, Token}};\nuse std::collections::VecDeque;\n"
        )?;

        writeln!(
            w,
            "#[derive(Debug, Clone, PartialEq, Eq, Hash)]\npub enum Symbol {{"
        )?;
        for t in grammar.terminals() {
            writeln!(w, "  {t},")?;
        }
        for t in grammar.non_terminals() {
            writeln!(w, "  {t}Start(usize),\n  {t}End(usize),")?;
        }
        writeln!(w, "}}\n")?;

        writeln!(
            w,
            "pub struct Parser {{
  cur: usize,
  stack: Vec<usize>,
  buf: VecDeque<TokenType>,
}}"
        )?;

        writeln!(w, "impl Parser {{")?;
        writeln!(
            w,
            "// Checks the buffer contents, return 0 in case of a match, 1 if buf is a prefix of toks, and 2 otherwise
  fn check_buf(&self, toks: &[TokenType]) -> u8 {{
    for (i, tok) in toks.iter().cloned().enumerate() {{
      if self.buf.len() <= i {{
        return 1;
      }}
      if self.buf[i] != tok {{
        return 2;
      }}
    }}
    return 0;
  }}"
        )?;

        writeln!(w, "// Checks if the parser has reached a valid end state\npub fn is_end(&self) -> bool {{ if !self.stack.is_empty() || !self.buf.is_empty() {{ return false; }}\nmatch self.cur {{")?;
        for end in parser.end_nodes() {
            writeln!(w, "{end} => true,")?;
        }
        writeln!(w, "_ => false,\n}}\n}}\n")?;

        writeln!(
            w,
            "// Traverses the Lgraph while it can, before stopping at a point where eating a token is required
  fn traverse(&mut self) -> Vec<Symbol> {{
    let mut res = vec![];
    loop {{
      let mut had_pref = false;
      match self.cur {{"
        )?;
        for node in parser.nodes() {
            write!(w, "{node} =>")?;
            let (with_closed, without_closed): (Vec<_>, _) = parser
                .edges_from(node)
                .partition(|(_, x, _)| x.bracket().map_or(false, |x| !x.is_open()));
            if !with_closed.is_empty() {
                writeln!(w, "match self.stack.last() {{")?;
                for (_, item, to) in &with_closed {
                    self.write_lgraph_traverse_item(w, item, *to)?;
                }
                write!(w, "_ => ")?;
            }

            writeln!(w, "{{")?;
            for (_, item, to) in without_closed {
                self.write_lgraph_traverse_item(w, &item, to)?;
            }
            writeln!(w, "}}")?;

            if !with_closed.is_empty() {
                writeln!(w, "}}")?;
            }
        }

        writeln!(w, "_ => {{  }}")?;

        writeln!(
            w,
            "
        }}
        if had_pref || self.is_end() {{ 
            return res;
        }} else {{
            panic!(\"Could not continue from node {{}}, stack {{:?}}, buf {{:?}}\", self.cur, self.stack, self.buf);
        }}
    }}
}}"
        )?;

        writeln!(
            w,
            "// Eats a token, produces a vector of corresponding symbols
pub fn eat_tok(&mut self, tok: TokenType) -> Vec<Symbol> {{
  let mut res = self.traverse();
  self.buf.push_back(tok);
  res.append(&mut self.traverse());
  return res;
}}

// Create a new Parser
pub fn new() -> Self {{
    Self {{
        cur: {},
        stack: vec![],
        buf: VecDeque::new(),
    }}
}}
",
            parser.start(),
        )?;

        writeln!(w, "}} // impl Parser")?;

        Ok(())
    }

    fn write_lgraph_traverse_item(
        &self,
        w: &mut dyn Write,
        item: &Item,
        to: usize,
    ) -> std::io::Result<()> {
        let mut buf = vec![];
        write!(&mut buf, "[")?;
        for t in item.tok_lk() {
            match t {
                TokenOrEnd::Token(t) => write!(&mut buf, "TokenType::{t}, ")?,
                TokenOrEnd::End => write!(&mut buf, "TokenType::_End, ")?,
            }
        }
        write!(&mut buf, "]")?;
        let lookahead = String::from_utf8(buf).unwrap();

        match item.bracket() {
            Some(Bracket::Closed(x)) => {
                write!(w, "Some({x}) if self.check_buf(&{lookahead}) != 2 => {{")?
            }
            Some(Bracket::Wildcard) => {
                write!(w, "Some(_)  if self.check_buf(&{lookahead}) != 2 => {{")?
            }
            _ => (),
        }

        writeln!(
            w,
            "let r = self.check_buf(&{lookahead});
if r == 1 {{ had_pref = true; }}
else if r == 0 {{"
        )?;
        match item.bracket() {
            Some(Bracket::Open(x)) => writeln!(w, "self.stack.push({x});")?,
            Some(_) => writeln!(w, "self.stack.pop();")?,
            None => (),
        }
        match item.tok() {
            Some(_) => writeln!(w, "self.buf.pop_front();")?,
            None => (),
        }
        match item.output() {
            Some(Node::Leaf(t)) => writeln!(w, "res.push(Symbol::{t});")?,
            Some(Node::RuleStart(i, t)) => writeln!(w, "res.push(Symbol::{t}Start({i}));")?,
            Some(Node::RuleEnd(i, t)) => writeln!(w, "res.push(Symbol::{t}End({i}));")?,
            None => (),
        }

        writeln!(w, "self.cur = {to};\ncontinue;\n}}")?;
        match item.bracket() {
            Some(Bracket::Closed(_)) => writeln!(w, "}}")?,
            Some(Bracket::Wildcard) => writeln!(w, "}}")?,
            _ => (),
        }

        Ok(())
    }
}

impl Codegen for RustCodegen {
    fn gen_code(&self, lexer: &Lexer, parser: &Lgraph, grammar: &Grammar, path: &str, name: &str) {
        std::fs::create_dir_all(format!("{path}/{name}/src")).unwrap();

        std::fs::write(
            format!("{path}/{name}/Cargo.toml"),
            format!(
                r#"[package]
name = "{name}"
version = "0.1.0"
edition = "2021"

[dependencies]"#
            ),
        )
        .unwrap();

        std::fs::write(
            format!("{path}/{name}/src/lib.rs"),
            "pub mod lexer; pub mod parser;",
        )
        .unwrap();

        let mut lexer_file = std::fs::File::create(format!("{path}/{name}/src/lexer.rs")).unwrap();
        self.write_lexer(&mut lexer_file, lexer).unwrap();

        let mut parser_file =
            std::fs::File::create(format!("{path}/{name}/src/parser.rs")).unwrap();
        self.write_parser(&mut parser_file, parser, grammar)
            .unwrap();

        Command::new("cargo")
            .current_dir(format!("{path}/{name}"))
            .args(["clippy", "--fix", "--allow-no-vcs"])
            .output()
            .unwrap();
        Command::new("rustfmt")
            .args([format!("{path}/{name}/src/*")])
            .output()
            .unwrap();
    }
}

#[cfg(test)]
mod tests {
    use std::{process::Stdio, str::FromStr};

    use crate::{
        lexer::{
            regex::Regex,
            tests::{lexer_gauntlet, LexerRunner, TokenOrGarbage},
        },
        parser::{
            grammar::ParseTree,
            tests::{ll1_gauntlet, slr_gauntlet, TestParser, TraverseError},
        },
        Token,
    };

    use super::*;

    struct RustLexerRunner(String);

    impl LexerRunner for RustLexerRunner {
        fn set_lexer(&mut self, lexer: Lexer) {
            let grammar = Grammar::from_str("E -> ;").unwrap();
            RustCodegen.gen_code(
                &lexer,
                &Lgraph::ll1(&grammar),
                &grammar,
                "tests",
                &format!("{}_lexer_crate", self.0),
            );
            let src = format!(
                r#"
            use {}_lexer_crate::lexer::*;
            use std::io::Read;

            fn print_tok(tok: Token, i: &mut usize) {{
                    match &tok {{
                        Token::Token(t, s) => println!("{{t:?}}({{i}}, {{}})", s.len()),
                        Token::Garbage(s) => println!("Garbage({{i}}, {{}})", s.len()),
                    }}
                    *i += tok.get_text().len();
            }}

            fn main() {{
                let mut lex = Lexer::new();
                let mut i = 0;
                let mut src = String::new();
                std::io::stdin().read_to_string(&mut src).unwrap();
                for c in src.chars() {{
                    if let Some(tok) = lex.eat_char(c) {{
                        print_tok(tok, &mut i);
                    }}
                }}

                print_tok(lex.finalize(), &mut i);
            }}"#,
                self.0
            );

            let conf = format!(
                r#"[package]
name = "{}_lexer_prog"
version = "0.1.0"
edition = "2021"

[dependencies]
{}_lexer_crate = {{ path = "../{}_lexer_crate" }}
"#,
                self.0, self.0, self.0
            );

            std::fs::create_dir_all(format!("tests/{}_lexer_prog/src", self.0)).unwrap();
            std::fs::write(format!("tests/{}_lexer_prog/src/main.rs", self.0), src).unwrap();
            std::fs::write(format!("tests/{}_lexer_prog/Cargo.toml", self.0), conf).unwrap();
        }

        fn traverse(&self, s: &str) -> Vec<TokenOrGarbage> {
            let mut proc = Command::new("cargo")
                .current_dir(format!("tests/{}_lexer_prog", self.0))
                .args(["run"])
                .stdin(Stdio::piped())
                .stderr(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
            let mut input = proc.stdin.take().unwrap();
            write!(input, "{}", s).unwrap();
            drop(input);

            let output = proc.wait_with_output().unwrap();
            assert!(
                output.status.success(),
                "stderr: {}",
                String::from_utf8(output.stderr).unwrap()
            );

            let output = String::from_utf8(output.stdout).unwrap();
            let mut res = vec![];

            for tok in output.lines() {
                println!("{tok}");
                let (tok_type, after_type) = tok.split_once('(').unwrap();
                let (start, len) = after_type.split_once(',').unwrap();
                let len = len.trim().strip_suffix(')').unwrap();
                let start = start.parse().unwrap();
                let len = len.parse().unwrap();

                match tok_type {
                    "Garbage" => res.push(TokenOrGarbage::Garbage(start, len)),
                    tok => res.push(TokenOrGarbage::Token(Token::new(tok).unwrap(), start, len)),
                }
            }

            res
        }
    }

    struct RustParserRunner(String, Option<Grammar>);

    impl TestParser for RustParserRunner {
        fn init(&mut self, g: Lgraph, grammar: Grammar) {
            let lex = Lexer::new(grammar.terminals().map(|t| (Regex::Empty, t)));

            RustCodegen.gen_code(
                &lex,
                &g,
                &grammar,
                "tests",
                &format!("{}_parser_crate", self.0),
            );
            self.1 = Some(grammar);

            let src = format!(
                r#"
            use {}_parser_crate::{{lexer::*, parser::*}};
            use std::io::Read;

            fn main() {{
                let mut parser = Parser::new();
                let mut src = String::new();
                std::io::stdin().read_to_string(&mut src).unwrap();

                let mut res = vec![];
                for line in src.lines() {{
                    let tt = TokenType::from_str(line);
                    res.append(&mut parser.eat_tok(tt));
                }}
                res.append(&mut parser.eat_tok(TokenType::_End));
                assert!(parser.is_end());

                for sym in res {{
                    println!("{{sym:?}}");
                }}
            }}
            "#,
                self.0
            );
            let conf = format!(
                r#"[package]
name = "{}_parser_prog"
version = "0.1.0"
edition = "2021"

[dependencies]
{}_parser_crate = {{ path = "../{}_parser_crate" }}
"#,
                self.0, self.0, self.0
            );

            std::fs::create_dir_all(format!("tests/{}_parser_prog/src", self.0)).unwrap();
            std::fs::write(format!("tests/{}_parser_prog/src/main.rs", self.0), src).unwrap();
            std::fs::write(format!("tests/{}_parser_prog/Cargo.toml", self.0), conf).unwrap();
        }

        fn parse(&self, toks: &[Token]) -> Result<ParseTree, TraverseError> {
            let mut proc = Command::new("cargo")
                .current_dir(format!("tests/{}_parser_prog", self.0))
                .args(["run"])
                .stdin(Stdio::piped())
                .stderr(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
            let mut input = proc.stdin.take().unwrap();
            for t in toks {
                writeln!(input, "{t}").unwrap();
            }
            drop(input);

            let output = proc.wait_with_output().unwrap();
            assert!(
                output.status.success(),
                "stderr: {}",
                String::from_utf8(output.stderr).unwrap()
            );

            let output = String::from_utf8(output.stdout).unwrap();

            let mut stack = vec![];
            for line in output.lines() {
                println!("{line}");
                let n = if let Some(t) = line.strip_suffix(')') {
                    let (rule, num) = t.split_once('(').unwrap();
                    let num = num.parse().unwrap();

                    if let Some(rule) = rule.strip_suffix("End") {
                        Node::RuleEnd(num, Token::new(rule).unwrap())
                    } else if let Some(rule) = rule.strip_suffix("Start") {
                        Node::RuleStart(num, Token::new(rule).unwrap())
                    } else {
                        panic!("Invalid symbol");
                    }
                } else {
                    Node::Leaf(Token::new(line).unwrap())
                };

                match n {
                    Node::Leaf(t) => stack.push(ParseTree::new(t)),
                    Node::RuleEnd(prod_idx, _) => {
                        let prod = self
                            .1
                            .as_ref()
                            .unwrap()
                            .productions()
                            .nth(prod_idx)
                            .unwrap();
                        let mut parent = ParseTree::new(prod.lhs());
                        parent.replace_with_production(0, prod, prod_idx);

                        let body = stack.split_off(stack.len() - prod.rhs().len());
                        assert_eq!(body.len(), prod.rhs().len());
                        for (i, child) in body.into_iter().enumerate().rev() {
                            parent.replace_with_subtree(i + 1, child);
                        }
                        stack.push(parent);
                    }
                    _ => (),
                }
            }

            Ok(stack.pop().unwrap())
        }
    }

    #[test]
    fn lexer() {
        lexer_gauntlet(&mut RustLexerRunner("rust_lexer".to_string()));
    }

    #[test]
    fn parser() {
        let mut runner = RustParserRunner("rust_parser".to_string(), None);
        ll1_gauntlet(&mut runner);
        slr_gauntlet(&mut runner);
    }
}
