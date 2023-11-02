use std::io::{Cursor, Write};

use crate::tokenizer::Tokenizer;

use super::translator::Translator;

pub trait TokenizerGenerator: Translator {
    fn generate(&mut self, t: &Tokenizer) -> String {
        let mut buf = vec![];
        let mut w = Cursor::new(&mut buf);
        let w = &mut w;

        let dfa_count = t.dfa_count();
        write!(
            w,
            "
// kind: 0 - none, 1 - garbage, i + 2 - ith token
struct Token
field kind uint
field start uint
field end uint
structend

struct Tokenizer
field garbage bool
field state arr uint {dfa_count}
field status arr uint {dfa_count}
field word_end uint
field word_start uint
structend

// status: 0 - couldnt move, 1 - moved, 2 - in end state
struct StepResult
field state uint
field status uint
structend

func new_tokenizer Tokenizer 0
create t Tokenizer
dotset t garbage false
dotset t word_end 0
dotset t word_start 0

create ref_state ref arr uint {dfa_count}
create ref_status ref arr uint {dfa_count}
",
        )
        .unwrap();

        for (i, (_, dfa)) in t.dfas().enumerate() {
            writeln!(w, "indexset ref_state {i} {}", dfa.start_node()).unwrap();
            writeln!(w, "indexset ref_status {i} 0").unwrap();
        }
        write!(
            w,
            "

ret mv t
funcend
"
        )
        .unwrap();

        for (i, (_, dfa)) in t.dfas().enumerate() {
            write!(
                w,
                "
func step{i} StepResult 2 c char state uint
create res StepResult
dotset res status 0
dotset res state cp state

switch cp state
"
            )
            .unwrap();

            for node in dfa.nodes() {
                write!(
                    w,
                    "
case {node}
switch cp c
"
                )
                .unwrap();
                for (_, l, to) in dfa.edges_from(node) {
                    let status = if dfa.is_end_node(to) { 2 } else { 1 };
                    write!(
                        w,
                        "
case '{l}'
dotset res state {to}
dotset res status {status}
ret mv res
caseend
"
                    )
                    .unwrap()
                }
                write!(
                    w,
                    "
default
ret mv res
caseend
switchend
caseend
"
                )
                .unwrap();
            }
        }
        write!(
            w,
            "
default
ret mv res
caseend
switchend
ret mv res
funcend
"
        )
        .unwrap();

        write!(
            w,
            "
func eat_char Token 2 t ref Tokenizer c char
create consumed bool
assign consumed false
create state_ref ref arr uint {dfa_count}
dotget state_ref t state
create status_ref ref arr uint {dfa_count}
dotget status_ref t status 
create new_status arr uint {dfa_count}
"
        )
        .unwrap();

        for i in 0..dfa_count {
            writeln!(w, "indexset new_status {i} 0").unwrap();
        }
        write!(
            w,
            "
create cond bool
create status uint
create state uint
create step_res StepResult
"
        )
        .unwrap();
        for (i, (_, _)) in t.dfas().enumerate() {
            write!(
                w,
                "
indexget status status_ref {i}
eq cond cp status 0
not cond cp cond
if cp cond 
indexget state state_ref {i}
call step_res step{i} 2 cp c cp state
dotget state step_res state
dotget status step_res status
indexset new_status {i} cp status
indexset state_ref {i} cp state
eq cond cp status 0
not cond cp cond

if cp cond
assign consumed cp cond
ifend

ifend
",
            )
            .unwrap()
        }

        write!(
            w,
            "
create word_end uint
create word_start uint
dotget word_end t word_end
dotget word_start t word_start
        
create garbage bool
dotget garbage t garbage

create res Token
dotset res kind 0
dotset res start cp word_start
dotset res end cp word_end

if cp consumed
add word_end cp word_end 1
dotset t status mv new_status

if cp garbage
sub word_start cp word_end 1
dotset t word_end cp word_end
dotset t word_start cp word_start
dotset t garbage false

ifend
ret mv res
ifend

create a uint
create b uint
create cond2 bool
create not_has_res bool
assign not_has_res false
"
        )
        .unwrap();

        for (i, (_, dfa)) in t.dfas().enumerate() {
            write!(
                w,
                "
indexget a status_ref {i}
indexget b new_status {i}
eq cond cp a 2
eq cond2 cp b 0
and cond cp cond cp cond2
and cond cp has_res cp cond
if cond
dotset res kind {}
assign not_has_res false
ifend

indexset status_ref {i} 1
indexset state_ref {i} {}

",
                i + 2,
                dfa.start_node()
            )
            .unwrap();
        }

        write!(
            w,
            "
not cond cp not_has_res
if cp cond
ret mv res
ifend

dotset t garbage true
add word_end cp word_end 1
dotset t word_end cp word_end
ret cp res
funcend

func flush Token 1 t ref Tokenizer
create status uint
create cond bool
create i uint
assign i 0
create status_ref ref arr uint {dfa_count}
dotget status_ref t status

create word_end uint
create word_start uint
dotget word_end t word_end
dotget word_start t word_start
create res Token
dotset res kind 0
dotset res start cp word_start
dotset res end cp word_end

le cond cp i {dfa_count}
while cp cond
indexget status status_ref cp i
eq cond cp status 2

if cp cond
add i cp i 2
dotset res kind cp i
add word_end cp word_end 1 
dotset res end word_end
ret mv res
ifend

add i cp i 1
le cond cp i {dfa_count}
whilend

eq cond cp word_start cp word_end
not cond cp cond
if cond
dotset res kind 1
ret mv res
ifend

ret mv res
funcend
"
        )
        .unwrap();

        String::from_utf8(buf).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        codegen::{assembler::BytecodeParser, translator::js::Js},
        regex::regular_expression::Regex,
        tokenizer::Token,
    };

    use super::*;

    impl TokenizerGenerator for Js {}

    #[test]
    fn idk() {
        let t = Tokenizer::new(vec![(Token::new("Tok"), Regex::Base('a'))]);
        let s = Js.generate(&t);
        println!("{}", s);
        let mut p = BytecodeParser::new();
        for s in s.lines() {
            p.parse_op(s);
        }
        let js_source = Js.translate_program(&p.finish()).unwrap();
        println!("{}", js_source);
        panic!()
    }
}
