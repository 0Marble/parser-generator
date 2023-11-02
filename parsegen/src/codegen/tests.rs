use super::{assembler::Assembler, bytecode::ByteCode};

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
        conv((0..20).map(|a| (a, a))),
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
        conv((0..20).map(|a| (a, a))),
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
