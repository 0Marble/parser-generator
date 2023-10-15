mod codegen;
mod parser;
mod regex;
mod tokenizer;

fn main() {
    unimplemented!();
}

#[cfg(test)]
mod tests {
    use std::{
        fmt::Display,
        io::Cursor,
        process::{Command, Stdio},
    };

    use crate::{
        parser::Parser,
        regex::regular_expression::Regex,
        tokenizer::{Token, Tokenizer},
    };

    use super::*;
    use std::io::Write;

    fn get_basic_output<S: Display>(
        t: &Tokenizer,
        p: &Parser,
        path: &str,
        input: impl IntoIterator<Item = S>,
    ) -> Vec<String> {
        Command::new("cargo").args(["init", path]).output().unwrap();
        t.to_rs(&mut std::fs::File::create(format!("{path}/src/tokenizer.rs")).unwrap())
            .unwrap();
        p.to_rs(&mut std::fs::File::create(format!("{path}/src/parser.rs")).unwrap())
            .unwrap();

        let mut binding = vec![];
        let mut w = Cursor::new(&mut binding);
        for tt in t.get_token_names() {
            writeln!(&mut w, "TokenKind::{tt} => PT::{tt},").unwrap();
        }

        writeln!(
            std::fs::File::create(format!("{path}/src/main.rs")).unwrap(),
            r#"
mod parser;
use parser::{{Parser, Node, Token as PT}};
mod tokenizer;
use tokenizer::{{Tokenizer, Token as TT, TokenKind}};

impl From<TokenKind> for PT {{
    fn from(val: TokenKind) -> PT {{
        match val {{
            TokenKind::_Garbage => unreachable!(),
            {}
        }}
    }}
}}

fn main() {{
    let mut t = Tokenizer::new();
    let mut p = Parser::new();

    for line in std::io::stdin().lines() {{
        for c in line.unwrap().chars() {{
            if let Some(tok) = t.eat_char(c) {{
                for node in p.eat_token(tok.get_kind().into()) {{
                    print!("{{node}}, ");
                }}
            }}
        }}
        if let Some(tok) = t.flush() {{
            for node in p.eat_token(tok.get_kind().into()) {{
                print!("{{node}}, ");
            }}
        }}
        for node in p.eat_token(PT::_End) {{
            print!("{{node}}, ");
        }}
        t = Tokenizer::new();
        p = Parser::new();
        println!();
    }}
}}
"#,
            String::from_utf8(binding).unwrap()
        )
        .unwrap();

        let mut process = Command::new("cargo")
            .args(["run", "--manifest-path", &format!("{path}/Cargo.toml")])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let mut stdin = process.stdin.take().unwrap();
        for input in input {
            writeln!(&mut stdin, "{}", input.to_string()).unwrap();
        }
        drop(stdin);
        let output = process.wait_with_output().unwrap();
        assert!(
            output.status.success(),
            "stderr: {}",
            String::from_utf8(output.stderr).unwrap()
        );
        let output = String::from_utf8(output.stdout).unwrap();
        Command::new("rm").args(["-rf", path]).output().unwrap();

        output
            .lines()
            .map(|l| l.strip_suffix(", ").unwrap().to_string())
            .collect()
    }

    fn regex() -> (Tokenizer, Parser) {
        let plus = Token::new("Plus");
        let star = Token::new("Star");
        let lp = Token::new("Lp");
        let rp = Token::new("Rp");
        let or = Token::new("Or");
        let maybe = Token::new("Maybe");
        let base = Token::new("Base");

        let plus_rule = Regex::Base('+');
        let star_rule = Regex::Base('*');
        let or_rule = Regex::Base('|');
        let lp_rule = Regex::Base('(');
        let rp_rule = Regex::Base(')');
        let maybe_rule = Regex::Base('?');
        let mut base_chars: Vec<_> = (0..=255)
            .filter_map(|i| char::from_u32(i))
            .filter(|c| {
                let c = *c;
                c != '(' && c != ')' && c != '\\' && c != '+' && c != '*' && c != '?' && c != '|'
            })
            .map(Regex::Base)
            .collect();
        base_chars.push(Regex::Seq(vec![Regex::Base('\\'), Regex::Base('\\')]));
        base_chars.push(Regex::Seq(vec![Regex::Base('\\'), Regex::Base('+')]));
        base_chars.push(Regex::Seq(vec![Regex::Base('\\'), Regex::Base('*')]));
        base_chars.push(Regex::Seq(vec![Regex::Base('\\'), Regex::Base('?')]));
        base_chars.push(Regex::Seq(vec![Regex::Base('\\'), Regex::Base('|')]));
        base_chars.push(Regex::Seq(vec![Regex::Base('\\'), Regex::Base('(')]));
        base_chars.push(Regex::Seq(vec![Regex::Base('\\'), Regex::Base(')')]));
        let base_rule = Regex::Variant(base_chars);

        let tokenizer = Tokenizer::new(vec![
            (plus.clone(), plus_rule),
            (star.clone(), star_rule),
            (lp.clone(), lp_rule),
            (rp.clone(), rp_rule),
            (or.clone(), or_rule),
            (maybe.clone(), maybe_rule),
            (base.clone(), base_rule),
        ]);

        let element = Token::new("E");
        let repeatition = Token::new("R");
        let concatination = Token::new("C");
        let variation = Token::new("V");

        let element_rule = Regex::Variant(vec![
            Regex::Base(base.clone()),
            Regex::Seq(vec![
                Regex::Base(lp.clone()),
                Regex::Base(variation.clone()),
                Regex::Base(rp.clone()),
            ]),
        ]);
        let repeatition_rule = Regex::Seq(vec![
            Regex::Base(element.clone()),
            Regex::Maybe(Box::new(Regex::Variant(vec![
                Regex::Base(star.clone()),
                Regex::Base(plus.clone()),
                Regex::Base(maybe.clone()),
            ]))),
        ]);
        let concatination_rule = Regex::Plus(Box::new(Regex::Base(repeatition.clone())));
        let variation_rule = Regex::Seq(vec![
            Regex::Base(concatination.clone()),
            Regex::Star(Box::new(Regex::Seq(vec![
                Regex::Base(or.clone()),
                Regex::Base(concatination.clone()),
            ]))),
        ]);

        let parser = Parser::new(vec![
            (variation, variation_rule),
            (concatination, concatination_rule),
            (repeatition, repeatition_rule),
            (element, element_rule),
        ]);
        (tokenizer, parser)
    }

    #[test]
    fn create_regex_parser() {
        let (t, p) = regex();

        let input = [
            "a",        //
            "ab",       //
            "a*",       //
            "a|b|c",    //
            "a?(b|c)+", //
        ];
        let output = [
            "VStart, CStart, RStart, EStart, Base(0), EEnd, REnd, CEnd, VEnd", 
        "VStart, CStart, RStart, EStart, Base(0), EEnd, REnd, RStart, EStart, Base(1), EEnd, REnd, CEnd, VEnd",
            "VStart, CStart, RStart, EStart, Base(0), EEnd, Star(1), REnd, CEnd, VEnd",
            "VStart, CStart, RStart, EStart, Base(0), EEnd, REnd, CEnd, Or(1), CStart, RStart, EStart, Base(2), EEnd, REnd, CEnd, Or(3), CStart, RStart, EStart, Base(4), EEnd, REnd, CEnd, VEnd",
            "VStart, CStart, RStart, EStart, Base(0), EEnd, Maybe(1), REnd, RStart, EStart, Lp(2), VStart, CStart, RStart, EStart, Base(3), EEnd, REnd, CEnd, Or(4), CStart, RStart, EStart, Base(5), EEnd, REnd, CEnd, VEnd, Rp(6), EEnd, Plus(7), REnd, CEnd, VEnd"
        ];

        for ((a, b), c) in get_basic_output(&t, &p, "regex_full", input)
            .iter()
            .zip(output)
            .zip(input)
        {
            assert_eq!(a, b, "Failed on {c}");
        }
    }
}
