use crate::{
    codegen::{bytecode::Ident, validator::Validator},
    regex::state_machine::Dfa,
    tokenizer::{Token, Tokenizer},
};

use super::{bytecode::ByteCode, translator::Translator};

use crate::codegen::bytecode::ByteCode as B;
use crate::codegen::bytecode::Type as T;
use crate::codegen::bytecode::Val as V;
pub trait TokenizerCodegen {
    fn gen_code(&mut self, t: &Tokenizer) -> Vec<String>;
}

pub struct BytecodeTokenizerCodegen {
    translator: Box<dyn Translator>,
}

impl BytecodeTokenizerCodegen {
    pub fn new(translator: Box<dyn Translator>) -> Self {
        Self { translator }
    }
}

fn token_struct(t: &Tokenizer, bc: &mut Vec<ByteCode>) {
    bc.push(B::Struct("Token".into()));
    bc.push(B::Field(("kind", T::Uint).into()));
    bc.push(B::Field(("start", T::Uint).into()));
    bc.push(B::Field(("end", T::Uint).into()));
    bc.push(B::StructEnd);
}

fn tokenizer_struct(t: &Tokenizer, bc: &mut Vec<ByteCode>) {
    bc.push(B::Struct("Tokenizer".into()));
    bc.push(B::Field(("garbage", T::Bool).into()));
    bc.push(B::Field(("cur_word_start", T::Uint).into()));
    bc.push(B::Field(("cur_word_end", T::Uint).into()));
    bc.push(B::Field(("cur_state", T::Array(Box::new(T::Uint), t.dfa_count())).into()).into());
    bc.push(B::Field(("cur_status", T::Array(Box::new(T::Uint), t.dfa_count())).into()).into());
    bc.push(B::StructEnd);
}

fn all_dfa_step_funcs(t: &Tokenizer, bc: &mut Vec<ByteCode>) {
    bc.push(B::Struct("StepResult".into()));
    bc.push(B::Field(("state", T::Uint).into()));
    bc.push(B::Field(("status", T::Uint).into()));
    bc.push(B::StructEnd);

    for (tok, dfa) in t.dfas() {
        let c: Ident = ("c", T::Char).into();
        let state: Ident = ("state", T::Uint).into();
        let status: Ident = ("status", T::Uint).into();
        let res: Ident = ("res", T::Struct("StepResult".into())).into();

        bc.push(B::Func(
            (
                format!("step_{}", tok.name().to_lowercase()),
                T::Struct("StepResult".into()),
            )
                .into(),
            vec![c.clone(), state.clone()].into(),
        ));
        bc.push(B::Create(res.clone()));
        bc.push(B::DotSet(
            res.clone(),
            state.clone(),
            V::Copy(state.clone()),
        ));
        bc.push(B::DotSet(res.clone(), status.clone(), V::Uint(0)));

        bc.push(B::Switch(V::Copy(state.clone())));
        for n in dfa.nodes() {
            bc.push(B::SwitchCase(V::Uint(n)));
            bc.push(B::Switch(V::Copy(c.clone())));

            for (_, l, next) in dfa.edges_from(n) {
                bc.push(B::SwitchCase(V::Char(l)));
                bc.push(B::DotSet(res.clone(), state.clone(), V::Uint(next)));
                let step_status = if dfa.is_end_node(next) { 2 } else { 1 };
                bc.push(B::DotSet(res.clone(), status.clone(), V::Uint(step_status)));
                bc.push(B::Ret(V::Move(res.clone())));

                bc.push(B::SwitchCaseEnd);
            }
            bc.push(B::SwitchDefaultCase);
            bc.push(B::Ret(V::Move(res.clone())));
            bc.push(B::SwitchCaseEnd);

            bc.push(B::SwitchEnd);
            bc.push(B::SwitchCaseEnd);
        }
        bc.push(B::SwitchDefaultCase);
        bc.push(B::Ret(V::Move(res.clone())));
        bc.push(B::SwitchCaseEnd);
        bc.push(B::SwitchEnd);
        bc.push(B::FuncEnd);
    }
}

fn new_tokenizer(t: &Tokenizer, bc: &mut Vec<ByteCode>) {
    let res: Ident = ("res", T::Struct("Tokenizer".into())).into();
    let status: Ident = ("cur_status", T::Array(Box::new(T::Uint), t.dfa_count())).into();
    let state: Ident = ("cur_state", T::Array(Box::new(T::Uint), t.dfa_count())).into();

    bc.push(B::Func(
        ("new_tokenizer", T::Struct("Tokenizer".into())).into(),
        vec![].into(),
    ));
    bc.push(B::Create(res.clone()));
    bc.push(B::DotSet(
        res.clone(),
        ("cur_word_end", T::Uint).into(),
        V::Uint(0),
    ));
    bc.push(B::DotSet(
        res.clone(),
        ("cur_word_start", T::Uint).into(),
        V::Uint(0),
    ));
    bc.push(B::DotSet(
        res.clone(),
        ("garbage", T::Bool).into(),
        V::Bool(false),
    ));
    bc.push(B::Create(status.clone()));
    bc.push(B::Create(state.clone()));
    for (i, (_, dfa)) in t.dfas().enumerate() {
        bc.push(B::IndexSet(status.clone(), V::Uint(i), V::Uint(1)));
        bc.push(B::IndexSet(
            state.clone(),
            V::Uint(i),
            V::Uint(dfa.start_node()),
        ));
    }
    bc.push(B::DotSet(
        res.clone(),
        state.clone(),
        V::Move(state.clone()),
    ));
    bc.push(B::DotSet(
        res.clone(),
        status.clone(),
        V::Move(status.clone()),
    ));
    bc.push(B::Ret(V::Move(res.clone())));
    bc.push(B::FuncEnd);
}
fn eat_char(t: &Tokenizer, bc: &mut Vec<ByteCode>) {
    let new_status: Ident = ("new_status", T::Array(Box::new(T::Uint), t.dfa_count())).into();
    let consumed: Ident = ("consumed", T::Bool).into();

    let cur_status: Ident = ("cur_status", T::Array(Box::new(T::Uint), t.dfa_count())).into();
    let cur_state: Ident = ("cur_state", T::Array(Box::new(T::Uint), t.dfa_count())).into();

    let cur_state_ref: Ident = (
        "cur_state_ref",
        T::Ref(Box::new(T::Array(Box::new(T::Uint), t.dfa_count()))),
    )
        .into();
    let cur_status_ref: Ident = (
        "cur_status_ref",
        T::Ref(Box::new(T::Array(Box::new(T::Uint), t.dfa_count()))),
    )
        .into();
    let tokenizer: Ident = ("tokenizer", T::Ref(Box::new(T::Struct("Tokenizer".into())))).into();
    let c: Ident = ("c", T::Char).into();
    let res: Ident = ("res", T::Struct("Token".into())).into();

    bc.push(B::Func(
        ("eat_char", res.t.clone()).into(),
        vec![tokenizer.clone(), c.clone()].into(),
    ));
    bc.push(B::Create(new_status.clone()));
    bc.push(B::Create(consumed.clone()));
    bc.push(B::Create(cur_status_ref.clone()));
    bc.push(B::Create(cur_state_ref.clone()));
    bc.push(B::DotGet(
        cur_status_ref.clone(),
        tokenizer.clone(),
        cur_status.clone(),
    ));
    bc.push(B::DotGet(
        cur_state_ref.clone(),
        tokenizer.clone(),
        cur_state.clone(),
    ));

    bc.push(B::Assign(consumed.clone(), V::Bool(false)));
    for i in 0..t.dfa_count() {
        bc.push(B::IndexSet(new_status.clone(), V::Uint(i), V::Uint(0)));
    }
    let status: Ident = ("status", T::Uint).into();
    let state: Ident = ("state", T::Uint).into();
    bc.push(B::Create(status.clone()));
    bc.push(B::Create(state.clone()));
    let cond: Ident = ("cond", T::Bool).into();
    bc.push(B::Create(cond.clone()));
    let step_res: Ident = ("step_res", T::Struct("StepResult".into())).into();
    bc.push(B::Create(step_res.clone()));

    for (i, (tok, _)) in t.dfas().enumerate() {
        bc.push(B::IndexGet(
            status.clone(),
            cur_status_ref.clone(),
            V::Uint(i),
        ));
        bc.push(B::Eq(cond.clone(), V::Copy(status.clone()), V::Uint(0)));
        bc.push(B::Not(cond.clone(), V::Copy(cond.clone())));
        bc.push(B::If(V::Copy(cond.clone())));

        bc.push(B::IndexGet(
            state.clone(),
            cur_state_ref.clone(),
            V::Uint(i),
        ));
        bc.push(B::Call(
            step_res.clone(),
            (
                format!("step_{}", tok.name().to_lowercase()),
                T::Struct("StepResult".into()),
            )
                .into(),
            vec![V::Copy(c.clone()), V::Copy(state.clone())].into(),
        ));
        bc.push(B::DotGet(status.clone(), step_res.clone(), status.clone()));
        bc.push(B::DotGet(state.clone(), step_res.clone(), state.clone()));
        bc.push(B::IndexSet(
            cur_state_ref.clone(),
            V::Uint(i),
            V::Copy(state.clone()),
        ));
        bc.push(B::IndexSet(
            new_status.clone(),
            V::Uint(i),
            V::Copy(status.clone()),
        ));
        bc.push(B::Eq(cond.clone(), V::Copy(status.clone()), V::Uint(0)));
        bc.push(B::Not(cond.clone(), V::Copy(cond.clone())));
        bc.push(B::If(V::Copy(cond.clone())));
        bc.push(B::Assign(consumed.clone(), V::Bool(true)));
        bc.push(B::IfEnd);
        bc.push(B::IfEnd);
    }

    let cur_word_end: Ident = ("cur_word_end", T::Uint).into();
    let cur_word_start: Ident = ("cur_word_start", T::Uint).into();
    bc.push(B::Create(cur_word_end.clone()));
    bc.push(B::Create(cur_word_start.clone()));
    let res: Ident = ("res", T::Struct("Token".into())).into();
    bc.push(B::Create(res.clone()));
    bc.push(B::DotSet(res.clone(), ("kind", T::Uint).into(), V::Uint(0)));
    bc.push(B::DotSet(
        res.clone(),
        ("start", T::Uint).into(),
        V::Uint(0),
    ));
    bc.push(B::DotSet(res.clone(), ("end", T::Uint).into(), V::Uint(0)));

    bc.push(B::If(V::Copy(consumed.clone())));
    bc.push(B::Add(
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
        V::Uint(1),
    ));
    bc.push(B::DotSet(
        tokenizer.clone(),
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
    ));
    bc.push(B::DotSet(
        tokenizer.clone(),
        cur_status.clone(),
        V::Move(new_status.clone()),
    ));
    bc.push(B::DotGet(
        cond.clone(),
        tokenizer.clone(),
        ("garbage", T::Bool).into(),
    ));
    bc.push(B::DotSet(res.clone(), ("end", T::Uint).into(), V::Uint(0)));

    bc.push(B::If(V::Copy(cond.clone())));
    bc.push(B::DotGet(
        cur_word_end.clone(),
        tokenizer.clone(),
        cur_word_end.clone(),
    ));
    bc.push(B::DotGet(
        cur_word_start.clone(),
        tokenizer.clone(),
        cur_word_start.clone(),
    ));
    bc.push(B::Sub(
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
        V::Uint(1),
    ));
    bc.push(B::DotSet(
        tokenizer.clone(),
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
    ));

    bc.push(B::DotSet(res.clone(), ("kind", T::Uint).into(), V::Uint(1)));
    bc.push(B::DotSet(
        res.clone(),
        ("start", T::Uint).into(),
        V::Copy(cur_word_start.clone()),
    ));
    bc.push(B::DotSet(
        res.clone(),
        ("end", T::Uint).into(),
        V::Copy(cur_word_end.clone()),
    ));
    bc.push(B::DotSet(
        tokenizer.clone(),
        cur_word_start.clone(),
        V::Copy(cur_word_end.clone()),
    ));
    bc.push(B::Add(
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
        V::Uint(1),
    ));
    bc.push(B::DotSet(
        tokenizer.clone(),
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
    ));
    bc.push(B::Ret(V::Move(res.clone())));
    bc.push(B::IfEnd); // if tokenizer.garbage
    bc.push(B::Ret(V::Move(res.clone())));
    bc.push(B::IfEnd); // if consumed

    let new: Ident = ("n", T::Uint).into();
    let old: Ident = ("o", T::Uint).into();
    bc.push(B::Create(new.clone()));
    bc.push(B::Create(old.clone()));
    let cond2: Ident = ("cond2", T::Bool).into();
    bc.push(B::Create(cond2.clone()));

    for (i, (_, dfa)) in t.dfas().enumerate() {
        bc.push(B::IndexGet(new.clone(), new_status.clone(), V::Uint(i)));
        bc.push(B::IndexGet(old.clone(), cur_status_ref.clone(), V::Uint(i)));
        bc.push(B::Eq(cond.clone(), V::Copy(new.clone()), V::Uint(0)));
        bc.push(B::Eq(cond2.clone(), V::Copy(old.clone()), V::Uint(2)));

        bc.push(B::And(
            cond.clone(),
            V::Copy(cond.clone()),
            V::Copy(cond2.clone()),
        ));

        bc.push(B::IndexSet(
            cur_state_ref.clone(),
            V::Uint(i),
            V::Uint(dfa.start_node()),
        ));

        bc.push(B::If(V::Copy(cond.clone())));

        bc.push(B::DotSet(
            res.clone(),
            ("kind", T::Uint).into(),
            V::Uint(i + 2),
        ));
        bc.push(B::DotGet(
            cur_word_end.clone(),
            tokenizer.clone(),
            cur_word_end.clone(),
        ));
        bc.push(B::DotGet(
            cur_word_start.clone(),
            tokenizer.clone(),
            cur_word_start.clone(),
        ));
        bc.push(B::DotSet(
            tokenizer.clone(),
            cur_word_start.clone(),
            V::Copy(cur_word_end.clone()),
        ));
        bc.push(B::DotSet(
            res.clone(),
            ("start", T::Uint).into(),
            V::Copy(cur_word_start.clone()),
        ));
        bc.push(B::DotSet(
            res.clone(),
            ("end", T::Uint).into(),
            V::Copy(cur_word_end.clone()),
        ));

        bc.push(B::IndexSet(cur_status_ref.clone(), V::Uint(i), V::Uint(1)));

        bc.push(B::Ret(V::Move(res.clone())));
        bc.push(B::IfEnd); // if new == 0 && old == 2
        bc.push(B::IndexSet(cur_state_ref.clone(), V::Uint(i), V::Uint(1)));
    }
    bc.push(B::DotSet(
        tokenizer.clone(),
        ("garbage", T::Bool).into(),
        V::Bool(true),
    ));
    bc.push(B::Add(
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
        V::Uint(1),
    ));
    bc.push(B::DotSet(
        tokenizer.clone(),
        cur_word_end.clone(),
        V::Copy(cur_word_end.clone()),
    ));
    bc.push(B::Ret(V::Move(res.clone())));

    bc.push(B::FuncEnd);
}

fn gen_flush(t: &Tokenizer, bc: &mut Vec<ByteCode>) {
    let tokenizer: Ident = ("tokenizer", T::Ref(Box::new(T::Struct("Tokenizer".into())))).into();
    bc.push(B::Func(
        ("flush", T::Struct("Token".into())).into(),
        vec![tokenizer.clone()].into(),
    ));
    let status: Ident = ("status", T::Uint).into();
    bc.push(B::Create(status.clone()));
    let cond: Ident = ("cond", T::Bool).into();
    bc.push(B::Create(cond.clone()));
    let cur_status_ref: Ident = (
        "cur_status_ref",
        T::Ref(Box::new(T::Array(Box::new(T::Uint), t.dfa_count()))),
    )
        .into();
    bc.push(B::Create(cur_status_ref.clone()));
    bc.push(B::DotGet(
        cur_status_ref.clone(),
        tokenizer.clone(),
        ("cur_status", T::Array(Box::new(T::Uint), t.dfa_count())).into(),
    ));
    let cur_word_start: Ident = ("cur_word_start", T::Uint).into();
    let cur_word_end: Ident = ("cur_word_end", T::Uint).into();
    bc.push(B::DotGet(
        cur_word_end.clone(),
        tokenizer.clone(),
        cur_word_end.clone(),
    ));
    bc.push(B::DotGet(
        cur_word_start.clone(),
        tokenizer.clone(),
        cur_word_start.clone(),
    ));
    let res: Ident = ("res", T::Struct("Token".into())).into();
    bc.push(B::Create(res.clone()));
    bc.push(B::DotSet(res.clone(), ("kind", T::Uint).into(), V::Uint(0)));
    bc.push(B::DotSet(
        res.clone(),
        ("start", T::Uint).into(),
        V::Copy(cur_word_start.clone()),
    ));
    bc.push(B::DotSet(
        res.clone(),
        ("end", T::Uint).into(),
        V::Copy(cur_word_end.clone()),
    ));

    for (i, (_, _)) in t.dfas().enumerate() {
        bc.push(B::IndexGet(
            status.clone(),
            cur_status_ref.clone(),
            V::Uint(i),
        ));
        bc.push(B::Eq(cond.clone(), V::Copy(status.clone()), V::Uint(2)));
        bc.push(B::If(V::Copy(cond.clone())));
        bc.push(B::DotSet(
            res.clone(),
            ("kind", T::Uint).into(),
            V::Uint(i + 2),
        ));
        bc.push(B::Add(
            cur_word_end.clone(),
            V::Copy(cur_word_end.clone()),
            V::Uint(1),
        ));
        bc.push(B::DotSet(
            res.clone(),
            ("end", T::Uint).into(),
            V::Copy(cur_word_end.clone()),
        ));
        bc.push(B::Ret(V::Move(res.clone())));

        bc.push(B::IfEnd); // if status == 2
    }

    bc.push(B::Eq(
        cond.clone(),
        V::Copy(cur_word_end.clone()),
        V::Copy(cur_word_start.clone()),
    ));
    bc.push(B::Not(cond.clone(), V::Copy(cond.clone())));
    bc.push(B::If(V::Copy(cond.clone())));
    bc.push(B::DotSet(res.clone(), ("kind", T::Uint).into(), V::Uint(1)));
    bc.push(B::Ret(V::Move(res.clone())));
    bc.push(B::IfEnd); // if cur_word_end != cur_word_start
    bc.push(B::Ret(V::Move(res.clone())));

    bc.push(B::FuncEnd); // fn flush()
}

impl TokenizerCodegen for BytecodeTokenizerCodegen {
    fn gen_code(&mut self, t: &Tokenizer) -> Vec<String> {
        let mut bc = vec![];

        token_struct(t, &mut bc);
        tokenizer_struct(t, &mut bc);
        all_dfa_step_funcs(t, &mut bc);
        new_tokenizer(t, &mut bc);
        eat_char(t, &mut bc);
        gen_flush(t, &mut bc);

        assert_eq!(Validator::new().validate_bc(&bc), Ok(()));
        let res = vec![self.translator.translate_program(&bc).unwrap()];
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::{codegen::translator::js::Js, regex::regular_expression::Regex};

    use super::*;

    #[test]
    fn idk() {
        let t = Tokenizer::new(vec![(Token::new("Hello"), Regex::Base('a'))]);
        let mut gen = BytecodeTokenizerCodegen::new(Box::new(Js));
        let res = gen.gen_code(&t);
        println!("{}", res[0]);
        panic!();
    }
}
