use std::{fmt::Display, io::Write, rc::Rc};

use crate::regex::{regular_expression::Regex, state_machine::Dfa};

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd, Eq, Ord)]
pub struct Token {
    name: Rc<str>,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Token {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn new(s: impl ToString) -> Self {
        let s = s.to_string();
        for c in s.chars() {
            assert!(c.is_ascii_alphabetic());
        }

        Self { name: s.into() }
    }
}

pub struct Tokenizer {
    dfas: Vec<(Token, Dfa<char>)>,
}

impl Tokenizer {
    pub fn get_token_names(&self) -> impl Iterator<Item = Token> + '_ {
        self.dfas.iter().map(|(t, _)| t.clone())
    }

    pub fn new(regexes: Vec<(Token, Regex<char>)>) -> Self {
        Self {
            dfas: regexes.into_iter().map(|(t, r)| (t, r.compile())).collect(),
        }
    }

    pub fn to_rs(&self, f: &mut dyn Write) -> std::io::Result<()> {
        write!(
            f,
            "
// This file is auto-generated
const DFA_COUNT: usize = {};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {{ 
        _Garbage, ",
            self.dfas.len()
        )?;
        for (t, _) in &self.dfas {
            write!(f, "{}, ", t)?;
        }
        write!(
            f,
            "
}} 
impl TokenKind {{ 
    fn from_num(i: usize) -> Self {{ 
        match i {{"
        )?;
        for (i, (t, _)) in self.dfas.iter().enumerate() {
            write!(f, "{} => Self::{}, ", i, t)?;
        }
        write!(f, "_ => unreachable!(), }} }} }}")?;
        write!(
            f,
            "
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {{ 
    kind: TokenKind, 
    start: usize, 
    end: usize 
}} 
pub struct Tokenizer {{ 
    garbage: bool, 
    cur_states: [usize; DFA_COUNT], 
    status: [u8; DFA_COUNT], 
    cur_word_start: usize, 
    cur_word_end: usize, 
}}",
        )?;

        write!(
            f,
            "
impl Token {{ 
    pub fn get_kind(&self) -> TokenKind {{ self.kind }} 
    pub fn get_start(&self) -> usize {{ self.start }} 
        pub fn get_end(&self) -> usize {{ self.end }} 
}}"
        )?;

        write!(f, "impl Tokenizer {{ ")?;
        for (_, (t, dfa)) in self.dfas.iter().enumerate() {
            write!(
                f,
                "/* 0: couldnt move, 1: moved, 2: reached end */fn step_{}(c: char, s: &mut usize) -> u8 {{ match s {{",
                t.name().to_lowercase(),
            )?;
            for (a, l, b) in dfa.edges() {
                writeln!(f, "{} if c as u32 == {} => *s = {}, ", a, l as u32, b)?;
            }
            write!(f, "_ => return 0, }} match s {{ ")?;
            for n in dfa.end_nodes() {
                write!(f, "{} => return 2,", n)?;
            }
            write!(f, "_ => return 1, }} }}")?;
        }

        write!(
            f,
            "
pub fn new() -> Self {{ 
    Self {{ 
        garbage: false, 
        status: [1; DFA_COUNT], 
        cur_word_start: 0, 
        cur_word_end: 0, 
        cur_states: [ ",
        )?;
        for (_, dfa) in &self.dfas {
            write!(f, "{}, ", dfa.start_node())?;
        }
        write!(f, "] }} }} ")?;

        write!(f, "pub fn eat_char(&mut self, c: char) -> Option<Token> {{")?;
        write!(
            f,
            "let mut new_status = [0; DFA_COUNT]; let mut consumed = false; for (i, step_dfa) in ["
        )?;
        for (_, (t, _)) in self.dfas.iter().enumerate() {
            write!(f, "Self::step_{}, ", t.name().to_lowercase())?;
        }
        write!(f, "].iter().enumerate() {{")?;
        write!(f, "if self.status[i] == 0 {{ continue; }} new_status[i] = step_dfa(c, &mut self.cur_states[i]);")?;
        write!(f, "if new_status[i] != 0 {{ consumed = true; }} }}")?;
        write!(
            f,
            "if consumed {{
self.cur_word_end += 1;
self.status = new_status;
if self.garbage {{
self.cur_word_end -= 1;
let tok = Token {{
kind: TokenKind::_Garbage,
start: self.cur_word_start,
end: self.cur_word_end,
}};
self.cur_word_start = self.cur_word_end;
self.cur_word_end += 1;
self.garbage = false;
return Some(tok);
}}
return None;
}}
for (i, (old, new)) in self.status.iter_mut().zip(new_status).enumerate() {{
if *old == 2 && new == 0 {{
let tok = Token {{
kind: TokenKind::from_num(i),
start: self.cur_word_start,
end: self.cur_word_end,
}};
self.status = [1; DFA_COUNT];
self.cur_word_start = self.cur_word_end;
self.cur_states = [
"
        )?;
        for (_, (_, d)) in self.dfas.iter().enumerate() {
            write!(f, "{}, ", d.start_node())?;
        }
        write!(f, "];")?;

        write!(
            f,
            "assert_eq!(self.eat_char(c), None);
return Some(tok);
}}
*old = new;
}}
self.garbage = true;
self.status = [1; DFA_COUNT];
self.cur_word_end += 1;
self.cur_states = [
"
        )?;

        for (_, (_, d)) in self.dfas.iter().enumerate() {
            write!(f, "{}, ", d.start_node())?;
        }
        write!(f, "]; None }} ")?;

        write!(
            f,
            "
    pub fn flush(self) -> Option<Token> {{
        for (i, status) in self.status.into_iter().enumerate() {{
            if status == 2 {{
                return Some(Token {{
                    kind: TokenKind::from_num(i),
                    start: self.cur_word_start,
                    end: self.cur_word_end + 1,
                }});
            }}
        }}

        if self.cur_word_start != self.cur_word_end {{
            return Some(Token {{
                kind: TokenKind::_Garbage,
                start: self.cur_word_start,
                end: self.cur_word_end,
            }});
        }}
        None
    }}
            }}
"
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::process::{Command, Stdio};

    use super::*;

    fn get_results<S: Display>(
        t: &Tokenizer,
        inputs: impl IntoIterator<Item = S>,
        path: &str,
    ) -> Vec<String> {
        Command::new("cargo").args(["init", path]).output().unwrap();
        let main_src = r#"
mod tokenizer;
use tokenizer::Tokenizer;
fn main() {
    let mut tok = Tokenizer::new();
    for s in std::io::stdin().lines() {
        let s = s.unwrap();
        for c in s.chars() {
            if let Some(t) = tok.eat_char(c) {
                print!("{:?}(", t.get_kind());
                for c in s
                    .chars()
                    .skip(t.get_start())
                    .take(t.get_end() - t.get_start())
                {
                    print!("{}", c);
                }
                print!("),");
            }
        }
        if let Some(t) = tok.flush() {
            print!("{:?}(", t.get_kind());
            for c in s
                .chars()
                .skip(t.get_start())
                .take(t.get_end() - t.get_start())
            {
                print!("{}", c);
            }
            print!(")");
        }
        tok = Tokenizer::new();
        println!();
    }
}"#;
        std::fs::write(format!("{path}/src/main.rs"), main_src).unwrap();
        t.to_rs(&mut std::fs::File::create(format!("{path}/src/tokenizer.rs")).unwrap())
            .unwrap();

        let mut process = Command::new("cargo")
            .args(["run", "--manifest-path", &format!("{path}/Cargo.toml")])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let mut stdin = process.stdin.take().unwrap();
        for input in inputs {
            writeln!(&mut stdin, "{}", input.to_string()).unwrap();
        }
        drop(stdin);
        let output = process.wait_with_output().unwrap().stdout;
        let output = String::from_utf8(output).unwrap();
        Command::new("rm").args(["-rf", path]).output().unwrap();
        output.lines().map(|s| s.to_string()).collect()
    }

    #[test]
    fn uint_tok() {
        let t = Tokenizer::new(vec![(Token::new("Uint".to_string()), Regex::uint())]);

        for (a, b) in get_results(
            &t,
            ["12345", "12345 12345", "1 1 1 ", "qwerty"],
            "test_uint_tok",
        )
        .into_iter()
        .zip([
            "Uint(12345)",
            "Uint(12345),_Garbage( ),Uint(12345)",
            "Uint(1),_Garbage( ),Uint(1),_Garbage( ),Uint(1),_Garbage( )",
            "_Garbage(qwerty)",
        ]) {
            assert_eq!(a, b);
        }
    }

    #[test]
    fn math_tokens() {
        let nums = Regex::uint();
        let ops = Regex::Variant(vec![
            Regex::Base('+'),
            Regex::Base('-'),
            Regex::Base('*'),
            Regex::Base('/'),
        ]);
        let lparen = Regex::Base('(');
        let rparen = Regex::Base(')');
        let cos = Regex::Seq(vec![Regex::Base('c'), Regex::Base('o'), Regex::Base('s')]);
        let sin = Regex::Seq(vec![Regex::Base('s'), Regex::Base('i'), Regex::Base('n')]);
        let ws = Regex::ascii_whitespace();
        let idents = Regex::ident();

        let t = Tokenizer::new(vec![
            (Token::new("Num"), nums),
            (Token::new("Op"), ops),
            (Token::new("Lparen"), lparen),
            (Token::new("Rparen"), rparen),
            (Token::new("Cos"), cos),
            (Token::new("Sin"), sin),
            (Token::new("Ident"), idents),
            (Token::new("Ws"), ws),
        ]);

        for (a, b) in get_results(
            &t,
            ["123", "a+b", "cos(x)", "a * b", "co*cos(x)"],
            "test_math_tokens",
        )
        .into_iter()
        .zip([
            "Num(123)",
            "Ident(a),Op(+),Ident(b)",
            "Cos(cos),Lparen((),Ident(x),Rparen())",
            "Ident(a),Ws( ),Op(*),Ws( ),Ident(b)",
            "Ident(co),Op(*),Cos(cos),Lparen((),Ident(x),Rparen())",
        ]) {
            assert_eq!(a, b);
        }
    }
}
