use std::{io::Write, rc::Rc};

use crate::{
    regex::regular_expression::Regex,
    tokenizer::{Token, Tokenizer},
};

use super::{
    assembler::Assembler,
    bytecode::ByteCode,
    tokenizer::{BytecodeTokenizerGenerator, TokenizerGenerator},
};

const PRELUDE: &'static str = "
// prelude started
importfunc print_int void 1 n uint
importfunc print_char void 1 c char
importfunc readline string 0
importfunc str_to_int uint 1 s ref string

func readint uint 0
create s string
call s readline 0
create a uint
call a str_to_int 1 ref s
destroy s
ret cp a
funcend
// prelude ended
";

fn conv<I: IntoIterator<Item = (T1, T2)>, T1: ToString, T2: ToString>(
    it: I,
) -> Vec<(String, String)> {
    it.into_iter()
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect()
}

fn basic_math() -> (&'static str, String, Vec<(String, String)>) {
    let name = "basic_math";
    let src = "
func main void 0
create a uint
create b uint
create c uint
create d uint
call a readint 0
call b readint 0
call c readint 0

// d = a * (b  + c)
add d cp b cp c
mul d cp d cp a
callvoid print_int 1 cp d
funcend
";

    (
        name,
        PRELUDE.to_string() + src,
        conv([
            ("0\n0\n0\n", "0\n"),
            ("1\n0\n0\n", "0\n"),
            ("0\n1\n0\n", "0\n"),
            ("10\n20\n30\n", "500\n"),
        ]),
    )
}

fn basic_if() -> (&'static str, String, Vec<(String, String)>) {
    let name = "basic_if";
    let src = "
func main void 0
create a uint
create b uint
create cond bool
call a readint 0
call b readint 0

// if a < b { print('a') } else { print('b') }
lt cond cp a cp b
if cp cond
callvoid print_char 1 'a'
else
callvoid print_char 1 'b'
ifend
funcend
";
    (
        name,
        PRELUDE.to_string() + src,
        conv([("10\n20", "a\n"), ("20\n10\n", "b\n"), ("10\n10\n", "b\n")]),
    )
}

fn basic_while() -> (&'static str, String, Vec<(String, String)>) {
    let name = "basic_while";
    let src = "
func main void 0
create n uint
create i uint
create cond bool
assign i 0
call n readint 0

lt cond cp i cp n
while cp cond
callvoid print_int 1 cp i

add i cp i 1
lt cond cp i cp n
whileend

funcend
";

    (
        name,
        PRELUDE.to_string() + src,
        conv([
            ("0\n", ""),
            ("1\n", "0\n"),
            ("10\n", "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"),
        ]),
    )
}

fn fizz_buzz() -> (&'static str, String, Vec<(String, String)>) {
    let name = "fizz_buzz";
    let src = "
func main void 0
create n uint
create i uint
create cond bool
create m uint

call n readint 0
assign i 0
lt cond cp i cp n
while cp cond

mod m cp i 3
eq cond cp m 0
if cp cond
callvoid print_char 1 'f'
ifend


mod m cp i 5
eq cond cp m 0
if cp cond
callvoid print_char 1 'b'
ifend

add i cp i 1
lt cond cp i cp n
whileend

funcend
";

    (
        name,
        PRELUDE.to_string() + src,
        conv((0..40).map(|n| {
            (
                n,
                (0..n)
                    .filter_map(|i| {
                        if i % 3 == 0 && i % 5 == 0 {
                            Some("f\nb\n")
                        } else if i % 3 == 0 {
                            Some("f\n")
                        } else if i % 5 == 0 {
                            Some("b\n")
                        } else {
                            None
                        }
                    })
                    .fold(String::new(), |mut acc, s| {
                        acc += s;
                        acc
                    }),
            )
        })),
    )
}

fn struct_and_method() -> (&'static str, String, Vec<(String, String)>) {
    let name = "struct_and_method";
    let src = "
struct Foo
field a uint
structend

func bar void 1 foo ref Foo
create a uint
call a readint 0
dotset foo a cp a
funcend
    
func main void 0
create foo Foo
dotset foo a 0
callvoid bar 1 ref foo

create a uint
dotget a foo a
callvoid print_int 1 cp a
funcend
";

    (
        name,
        PRELUDE.to_string() + src,
        conv((0..20).map(|a| (a, a.to_string() + "\n"))),
    )
}

fn field_ref() -> (&'static str, String, Vec<(String, String)>) {
    let name = "field_ref";
    let src = "
struct Foo
field bar arr uint 2
structend

func do_stuff void 1 foo ref Foo
create bar_ref ref arr uint 2
dotgetref bar_ref foo bar

create a uint
call a readint 0
indexset bar_ref 0 cp a
funcend

func main void 0
create foo Foo
create bar arr uint 2
indexset bar 0 0
indexset bar 1 1
dotset foo bar cp bar

callvoid do_stuff 1 ref foo
dotget bar foo bar
create a uint
indexget a bar 0
callvoid print_int 1 cp a
funcend

";

    (
        name,
        PRELUDE.to_string() + src,
        conv((0..20).map(|a| (a, a.to_string() + "\n"))),
    )
}

const TOKENIZER: &'static str = "
importfunc strlen uint 1 s ref string

func print_tok void 2 tok Token s ref string
create i uint
create cond bool
create n uint
dotget i tok start
dotget n tok end

lt cond cp i cp n
while cp cond
create c char
indexget c s cp i
callvoid print_char 1 cp c

add i cp i 1
lt cond cp i cp n
whileend

funcend

func main void 0
create t Tokenizer
call t new_tokenizer 0
create tok Token
create s string
call s readline 0

create kind uint
create i uint
create cond bool
create n uint 
assign i 0
call n strlen 1 ref s

lt cond cp i cp n
while cp cond
create c char
indexget c s cp i
call tok eat_char 2 ref t cp c

dotget kind tok kind
eq cond cp kind 0
not cond cp cond
if cp cond
callvoid print_int 1 cp kind
callvoid print_tok 2 cp tok ref s
ifend

add i cp i 1
lt cond cp i cp n
whileend

call tok flush 1 ref t
dotget kind tok kind
eq cond cp kind 0
not cond cp cond
if cp cond
callvoid print_int 1 cp kind
callvoid print_tok 2 cp tok ref s
ifend

funcend

";

fn tokenizer_simple() -> (&'static str, String, Vec<(String, String)>) {
    let t = Tokenizer::new(vec![(Token::new("Tok"), Regex::Base('a'))]);
    let mut bcg = BytecodeTokenizerGenerator::new();
    bcg.init(Rc::new(t));
    bcg.gen_tokenizer();
    let res = bcg.write();
    let (_, src) = &res[0];

    (
        "tokenizer_simple",
        PRELUDE.to_string() + src + TOKENIZER,
        conv([
            ("a", "2\na\n"),
            ("aaa", "2\na\n2\na\n2\na\n"),
            ("b", "1\nb\n"),
            ("bbb", "1\nb\nb\nb\n"),
            ("aba", "2\na\n1\nb\n2\na\n"),
            ("ab", "2\na\n1\nb\n"),
            ("ba", "1\nb\n2\na\n"),
        ]),
    )
}

fn tokenizer_idents() -> (&'static str, String, Vec<(String, String)>) {
    let t = Tokenizer::new(vec![
        (Token::new("Ident"), Regex::ident()),
        (Token::new("Ws"), Regex::ascii_whitespace()),
    ]);
    let mut bcg = BytecodeTokenizerGenerator::new();
    bcg.init(Rc::new(t));
    bcg.gen_tokenizer();
    let res = bcg.write();
    let (_, src) = &res[0];

    (
        "tokenizer_idents",
        PRELUDE.to_string() + src + TOKENIZER,
        conv([("Foo", "2\nFoo\n"), ("Foo Bar", "2\nFoo\n3\n \n2\nBar\n")]),
    )
}

fn tokenizer_common() -> (&'static str, String, Vec<(String, String)>) {
    let t = Tokenizer::new(vec![
        (
            // 2
            Token::new("If"),
            Regex::Seq(vec![Regex::Base('i'), Regex::Base('f')]),
        ),
        (
            // 3
            Token::new("Let"),
            Regex::Seq(vec![Regex::Base('l'), Regex::Base('e'), Regex::Base('t')]),
        ),
        (Token::new("Assign"), Regex::Base('=')), // 4
        (
            // 5
            Token::new("Eq"),
            Regex::Seq(vec![Regex::Base('='), Regex::Base('=')]),
        ),
        (Token::new("Lp"), Regex::Base('(')),          //6
        (Token::new("Rp"), Regex::Base(')')),          //7
        (Token::new("Lb"), Regex::Base('{')),          // 8
        (Token::new("Rb"), Regex::Base('}')),          //9
        (Token::new("Semi"), Regex::Base(';')),        //10
        (Token::new("Number"), Regex::uint()),         //11
        (Token::new("Ident"), Regex::ident()),         //12
        (Token::new("Ws"), Regex::ascii_whitespace()), //13
    ]);
    let mut bcg = BytecodeTokenizerGenerator::new();
    bcg.init(Rc::new(t));
    bcg.gen_tokenizer();
    let res = bcg.write();
    let (_, src) = &res[0];

    (
    "tokenizer_common",
        PRELUDE.to_string()+src+TOKENIZER,
        conv([
            ("let", "3\nlet\n"),
            ("if", "2\nif\n"),
            ("iffy", "12\niffy\n"),
            ("123", "11\n123\n"),
            ("x1", "12\nx1\n"),
            ("x 1", "12\nx\n13\n \n11\n1\n"),
        ("let x = 10; if x == 10 { print(x) }", "3\nlet\n13\n \n12\nx\n13\n \n4\n=\n13\n \n11\n10\n10\n;\n13\n \n2\nif\n13\n \n12\nx\n13\n \n5\n==\n13\n \n11\n10\n13\n \n8\n{\n13\n \n12\nprint\n6\n(\n12\nx\n7\n)\n13\n \n9\n}\n")

        ])
)
}

fn all_tests() -> impl Iterator<Item = (&'static str, String, Vec<(String, String)>)> {
    [
        basic_math,
        basic_if,
        basic_while,
        fizz_buzz,
        struct_and_method,
        field_ref,
        tokenizer_simple,
        tokenizer_idents,
        tokenizer_common,
    ]
    .map(|f| {
        let (name, src, tests) = f();
        (name, src, tests)
    })
    .into_iter()
}

#[test]
fn assembler_success() {
    for (name, src, _) in all_tests() {
        let mut a = Assembler::new();
        println!("Assembling {}", name);
        std::fs::File::create(format!("tests/{name}.bc"))
            .unwrap()
            .write_all(src.as_bytes())
            .unwrap();

        for line in src.lines() {
            a.parse_op(line);
        }
        let _ = a.finish();
    }
}

pub trait TestRunner {
    fn clear(&mut self);
    fn init(&mut self, name: &str, bc: &[ByteCode]);
    fn run_test(&self, input: &str) -> String;
}

pub fn gauntlet(tr: &mut dyn TestRunner) {
    tr.clear();
    for (name, src, tests) in all_tests() {
        let mut a = Assembler::new();
        for line in src.lines() {
            a.parse_op(line);
        }
        let bc = a.finish();
        tr.init(name, &bc);

        for (a, b) in tests {
            assert_eq!(
                tr.run_test(&a),
                b,
                "Failed on {name} input={a:?}, output={b:?}"
            );
        }
        tr.clear();
    }
}
