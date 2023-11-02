use std::{
    io::{Cursor, Write},
    rc::Rc,
};

use crate::{
    regex::{regular_expression::Regex, state_machine::Dfa},
    tokenizer::{Token, Tokenizer},
};

use super::{
    assembler::Assembler,
    translator::{js::Js, Translator},
};

pub trait TokenizerGenerator {
    fn init(&mut self, t: Rc<Tokenizer>);
    fn gen_tokenizer(&mut self);
}

pub struct BytecodeTokenizerGenerator {
    tr: Box<dyn Translator>,
    t: Option<Rc<Tokenizer>>,
    buf: Vec<u8>,
}

impl BytecodeTokenizerGenerator {
    pub fn new(tr: Box<dyn Translator>) -> Self {
        Self {
            tr,
            t: None,
            buf: vec![],
        }
    }

    fn token_def(&mut self) -> std::io::Result<()> {
        write!(
            self.buf,
            "
struct Token
field kind uint
field start uint
field end uint
structend

func new_token Token 3 kind uint start uint end uint
create t Token
dotset t kind cp kind
dotset t start cp start
dotset t end cp end 
ret mv t
funcend
        "
        )
    }

    fn step_res_def(&mut self) -> std::io::Result<()> {
        write!(
            self.buf,
            "
struct StepResult
field state uint
field status uint
structend

func new_step_res StepResult 2 state uint status uint
create res StepResult
dotset res state cp state
dotset res status cp status
ret mv res
funcend
"
        )
    }

    fn tokenizer_def(&mut self) -> std::io::Result<()> {
        let t = self.t.as_ref().unwrap();
        let dfa_count = t.dfa_count();

        write!(
            self.buf,
            "
struct Tokenizer
field garbage bool
field state arr uint {dfa_count}
field status arr uint {dfa_count}
field word_end uint
field word_start uint
structend

func set_ith_state void 3 t ref Tokenizer i uint state uint
create a ref arr uint {dfa_count}
dotgetref a t state
indexset a cp i cp state
funcend

func set_ith_status void 3 t ref Tokenizer i uint status uint
create a ref arr uint {dfa_count}
dotgetref a t status
indexset a cp i cp status
funcend

func get_ith_state uint 2 t ref Tokenizer i uint
create a ref arr uint {dfa_count}
dotgetref a t state
create b uint
indexget b a cp i
ret cp b
funcend

func get_ith_status uint 2 t ref Tokenizer i uint
create a ref arr uint {dfa_count}
dotgetref a t status
create b uint
indexget b a cp i
ret cp b
funcend

func new_tokenizer Tokenizer 0
create t Tokenizer
dotset t garbage false
dotset t word_end 0
dotset t word_start 0
create state_ref ref arr uint {dfa_count}
dotgetref state_ref t state
create status_ref ref arr uint {dfa_count}
dotgetref status_ref t status
"
        )?;
        for (i, (_, dfa)) in t.dfas().enumerate() {
            writeln!(self.buf, "indexset state_ref {i} {}", dfa.start_node())?;
            writeln!(self.buf, "indexset status_ref {i} 1")?;
        }

        writeln!(self.buf, "ret mv t\nfuncend")
    }

    fn dfa_step(&mut self, dfa: &Dfa<char>, n: usize) -> std::io::Result<()> {
        write!(
            self.buf,
            "
func step_{n} StepResult 2 state uint c char
create res StepResult
dotset res status 0
dotset res state cp state
switch cp state
"
        )
        .unwrap();

        for node in dfa.nodes() {
            write!(
                self.buf,
                "
case {node}
switch cp c
"
            )
            .unwrap();
            for (_, l, to) in dfa.edges_from(node) {
                let status = if dfa.is_end_node(to) { 2 } else { 1 };
                write!(
                    self.buf,
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
                self.buf,
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

        write!(
            self.buf,
            "
default
ret mv res
caseend
switchend
ret mv res
funcend
"
        )?;

        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let t = self.t.as_ref().unwrap();
        let dfa_count = t.dfa_count();
        write!(
            self.buf,
            "
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

lt cond cp i {dfa_count}
while cp cond
indexget status status_ref cp i
eq cond cp status 2

if cp cond
add i cp i 2
dotset res kind cp i
ret mv res
ifend

add i cp i 1
lt cond cp i {dfa_count}
whileend

eq cond cp word_start cp word_end
not cond cp cond
if cp cond
dotset res kind 1
ret mv res
ifend

ret mv res
funcend
        "
        )
    }

    fn eat_char(&mut self) -> std::io::Result<()> {
        let t = self.t.as_ref().unwrap();
        let dfa_count = t.dfa_count();

        write!(
            self.buf,
            "
        // try to step with the current character.
        // if we can step, return true and set new states and statuses
        // if we cant step, we return false, keep states and statuses
        func try_eat_char bool 2 t ref Tokenizer c char
        create res StepResult
        create cond bool
        create consumed bool
        assign consumed false
        create status uint
        create state uint
        create status_ref ref arr uint {dfa_count}
        create state_ref ref arr uint {dfa_count}
        dotgetref state_ref t state
        dotgetref status_ref t status
        create new_status arr uint {dfa_count}

        // if the dfa is not failed (status != 0), try to step it
"
        )?;
        for i in 0..dfa_count {
            write!(
                self.buf,
                "
            indexget status status_ref {i}
            eq cond cp status 0
            not cond cp cond

            if cp cond
            indexget state state_ref {i}
            call res step_{i} 2 cp state cp c
            dotget state res state
            dotget status res status
            indexset state_ref {i} cp state
            indexset new_status {i} cp status
            eq cond cp status 0
            not cond cp cond
            if cp cond
            assign consumed true 
            ifend
            ifend
            "
            )?;
        }

        write!(
            self.buf,
            "
        // if consumed, we transfer over the status, if not we have failed
        create i uint
        assign i 0
        lt cond cp i {dfa_count}
        and cond cp cond cp consumed
        while cp cond
        indexget status new_status cp i
        indexset status_ref cp i cp status
        add i cp i 1
        lt cond cp i {dfa_count}
        whileend

        ret cp consumed
        funcend

        "
        )?;

        write!(
            self.buf,
            "
        // resets the states and statuses of dfas
        func reset void 1 t ref Tokenizer
        create state_ref ref arr uint {dfa_count}
        create status_ref ref arr uint {dfa_count}
        dotgetref state_ref t state
        dotgetref status_ref t status
        
        "
        )?;
        for (i, (_, dfa)) in t.dfas().enumerate() {
            writeln!(self.buf, "indexset state_ref {i} {}", dfa.start_node())?;
            writeln!(self.buf, "indexset status_ref {i} 1")?;
        }
        writeln!(self.buf, "funcend")?;

        write!(
            self.buf,
            "
        func eat_char Token 2 t ref Tokenizer c char
        create consumed bool
        create cond bool
        create word_end uint
        create word_start uint
        dotget word_start t word_start
        dotget word_end t word_end
        create res Token

        // try to consume cur character
        call consumed try_eat_char 2 mv t cp c
        
        // were able to consume char, continue eating
        if cp consumed
        add word_end cp word_end 1
        dotset t word_end cp word_end

        // if we were reading garbage before, return garbage
        // since we could consume this char, garbage has ended
        dotget cond t garbage
        if cp cond
        call res new_token 3 1 cp word_start cp word_end
        dotset t garbage false
        sub word_end cp word_end 1
        dotset t word_start cp word_end
        ret mv res
        ifend

        // were not reading garbage, no token produced
        call res new_token 3 0 0 0
        ret mv res
        ifend
        
        // could not consume, see if one of the dfas was at the final state:
        create i uint
        assign i 0
        le cond cp i {dfa_count}
        
        while cp cond
        create st uint
        call st get_ith_status 2 mv t cp i

        // ith dfa was at the end state before
        eq cond cp st 2
        if cp cond
        create tok_id uint
        add tok_id cp i 2
        call res new_token 3 cp tok_id cp word_start cp word_end

        // next token starts from the previous ones end
        // and its end is at lease after this char
        dotset t word_start cp word_end
        add word_end cp word_end 1
        dotset t word_end cp word_end
        
        // reset the states to start reading next token from the current character
        // if we couldnt eat the current character, the next token is garbage
        callvoid reset 1 mv t
        call consumed try_eat_char 2 mv t cp c
        not consumed cp consumed
        dotset t garbage cp consumed
        
        ret mv res
        ifend // if dfa was at the end state before
        
        add i cp i 1
        le cond cp i {dfa_count}
        whileend

        // could not consume, and no dfas were at the final state
        // that means we have entered garbage territory, skip this
        // character and return nothing
        // we also have to reset in case new token starts
        add word_end cp word_end 1
        dotset t word_end cp word_end
        dotset t garbage true
        call res new_token 3 0 0 0
        callvoid reset 1 mv t
        ret cp res
        funcend
        "
        )?;

        Ok(())
    }
}

impl TokenizerGenerator for BytecodeTokenizerGenerator {
    fn init(&mut self, t: Rc<Tokenizer>) {
        self.buf.clear();
        self.t = Some(t);

        self.tr.init();
    }

    fn gen_tokenizer(&mut self) {
        let t = self.t.as_ref().unwrap().clone();
        self.token_def().unwrap();
        self.step_res_def().unwrap();
        self.tokenizer_def().unwrap();
        for (i, (_, dfa)) in t.dfas().enumerate() {
            self.dfa_step(dfa, i).unwrap();
        }
        self.flush().unwrap();
        self.eat_char().unwrap();

        let src = String::from_utf8(std::mem::take(&mut self.buf)).unwrap();
        println!("{src}");
        let mut a = Assembler::new();
        for line in src.lines() {
            a.parse_op(line);
        }
        let bc = a.finish();
        for bc in bc {
            self.tr.translate_op(bc);
        }
        self.tr.finalize();
    }
}

#[test]
fn hello() {
    let t = Tokenizer::new(vec![(
        Token::new("Hello"),
        Regex::Star(Box::new(Regex::Base('a'))),
    )]);
    let mut tg = BytecodeTokenizerGenerator::new(Box::new(Js::new()));
    tg.init(Rc::new(t));
    tg.gen_tokenizer();
}
