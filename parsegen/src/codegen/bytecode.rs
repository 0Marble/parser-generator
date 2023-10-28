use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Char,
    Uint,
    Array(Box<Type>, usize),
    String,
    Vec(Box<Type>),
    Struct(Rc<str>),
    Ref(Box<Type>),
}

impl Type {
    pub fn is_basic(&self) -> bool {
        matches!(self, Self::Bool | Self::Char | Self::Uint)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Bool(bool),
    Char(char),
    Uint(usize),
    Copy(Ident),
    Ref(Ident),
    Move(Ident),
}

impl Val {
    pub fn get_type(&self) -> Type {
        match self {
            Val::Bool(_) => Type::Bool,
            Val::Char(_) => Type::Char,
            Val::Uint(_) => Type::Uint,
            Val::Copy(i) => i.t.clone(),
            Val::Ref(i) => Type::Ref(Box::new(i.t.clone())),
            Val::Move(i) => i.t.clone(),
        }
    }
    pub fn deref(&self) -> Val {
        if let Val::Ref(id) = self {
            Self::Copy(id.clone())
        } else {
            unreachable!("Deref of a non-ref type, should have been caught by the Validator");
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: Rc<str>,
    pub t: Type,
}

impl<'a> From<&'a Ident> for Val {
    fn from(value: &'a Ident) -> Self {
        match value.t {
            Type::Void => unreachable!(),
            Type::Bool => Self::Copy(value.clone()),
            Type::Char => Self::Copy(value.clone()),
            Type::Uint => Self::Copy(value.clone()),
            Type::Array(_, _) => Self::Ref(value.clone()),
            Type::String => Self::Ref(value.clone()),
            Type::Vec(_) => Self::Ref(value.clone()),
            Type::Struct(_) => Self::Ref(value.clone()),
            Type::Ref(_) => Self::Copy(value.clone()),
        }
    }
}

impl<T1: Into<Rc<str>>, T2: Into<Type>> From<(T1, T2)> for Ident {
    fn from(value: (T1, T2)) -> Self {
        Self {
            name: value.0.into(),
            t: value.1.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCode {
    Create(Ident),
    Destroy(Ident),

    Add(Ident, Val, Val),
    Sub(Ident, Val, Val),
    Mul(Ident, Val, Val),
    Div(Ident, Val, Val),
    Mod(Ident, Val, Val),
    Eq(Ident, Val, Val),
    Lt(Ident, Val, Val),
    Le(Ident, Val, Val),
    And(Ident, Val, Val),
    Or(Ident, Val, Val),
    Not(Ident, Val),
    Assign(Ident, Val),

    IndexGet(Ident, Ident, Val),
    IndexSet(Ident, Val, Val),
    DotGet(Ident, Ident, Ident),
    DotSet(Ident, Ident, Val),
    Call(Ident, Ident, Rc<[Val]>),
    CallVoid(Ident, Rc<[Val]>),

    While(Val),
    WhileEnd,
    If(Val),
    Else,
    IfEnd,

    Switch(Val),
    SwitchEnd,
    SwitchCase(Val),
    SwitchDefaultCase,
    SwitchCaseEnd,

    Func(Ident, Rc<[Ident]>),
    Ret(Val),
    FuncEnd,

    Struct(Rc<str>),
    Field(Ident),
    StructEnd,
}

#[cfg(test)]
pub mod programs {
    use crate::codegen::{translator::Translator, validator::Validator};

    use super::*;
    use ByteCode as B;
    use Type as T;

    fn add_nums() -> (Vec<ByteCode>, &'static str) {
        let a: Ident = ("a", T::Uint).into();
        let b: Ident = ("b", T::Uint).into();
        let c: Ident = ("c", T::Uint).into();
        let s: Ident = ("s", T::String).into();

        let res = vec![
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(a.clone()),
            B::Create(b.clone()),
            B::Create(s.clone()),
            B::Call(s.clone(), ("read_line", T::String).into(), vec![].into()),
            B::Call(
                a.clone(),
                ("str_to_int", T::Uint).into(),
                vec![(Val::Ref(s.clone()))].into(),
            ),
            B::Destroy(s.clone()),
            B::Call(s.clone(), ("read_line", T::String).into(), vec![].into()),
            B::Call(
                b.clone(),
                ("str_to_int", T::Uint).into(),
                vec![(Val::Ref(s.clone()))].into(),
            ),
            B::Destroy(s.clone()),
            B::Create(c.clone()),
            B::Add(c.clone(), Val::Copy(a.clone()), Val::Copy(b.clone())),
            B::CallVoid(("print", T::Void).into(), vec![Val::Copy(c.clone())].into()),
            B::FuncEnd,
        ];

        (res, "basic_add")
    }

    fn basic_if() -> (Vec<ByteCode>, &'static str) {
        let a: Ident = ("a", T::Uint).into();
        let cond: Ident = ("alt2", T::Bool).into();
        let s: Ident = ("s", T::String).into();
        let print: Ident = ("print", T::Void).into();
        let read_line: Ident = ("read_line", T::String).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        let res = vec![
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(s.clone()),
            B::Call(s.clone(), read_line.clone(), vec![].into()),
            B::Call(
                a.clone(),
                str_to_int.clone(),
                vec![Val::Ref(s.clone())].into(),
            ),
            B::Destroy(s.clone()),
            B::Create(cond.clone()),
            B::Lt(cond.clone(), Val::Copy(a.clone()), Val::Uint(2)),
            B::If(Val::Copy(cond.clone())),
            B::CallVoid(print.clone(), vec![Val::Char('A')].into()),
            B::Else,
            B::Eq(cond.clone(), Val::Copy(a.clone()), Val::Uint(2)),
            B::If(Val::Copy(cond.clone())),
            B::CallVoid(print.clone(), vec![Val::Char('B')].into()),
            B::Else,
            B::Eq(cond.clone(), Val::Copy(a.clone()), Val::Uint(3)),
            B::If(Val::Copy(cond)),
            B::CallVoid(print.clone(), vec![Val::Char('C')].into()),
            B::Else,
            B::CallVoid(print.clone(), vec![Val::Char('D')].into()),
            B::IfEnd,
            B::IfEnd,
            B::IfEnd,
            B::FuncEnd,
        ];

        (res, "basic_if")
    }

    fn basic_ref_obj() -> (Vec<ByteCode>, &'static str) {
        let x: Ident = ("x", T::Uint).into();
        let s: Ident = ("s", T::String).into();
        let a: Ident = ("a", T::Struct("MyStruct".into())).into();
        let b: Ident = ("b", T::Ref(Box::new(T::Struct("MyStruct".into())))).into();
        let print: Ident = ("print", T::Void).into();
        let read_line: Ident = ("read_line", T::String).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        let res = vec![
            B::Struct("MyStruct".into()),
            B::Field(x.clone()),
            B::StructEnd,
            //
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(s.clone()),
            B::Call(s.clone(), read_line.clone(), vec![].into()),
            B::Create(x.clone()),
            B::Call(x.clone(), str_to_int.clone(), vec![(&s).into()].into()),
            B::Destroy(s.clone()),
            //
            B::Create(a.clone()),
            B::DotSet(a.clone(), x.clone(), (&x).into()),
            B::Create(b.clone()),
            B::Assign(b.clone(), (&a).into()),
            B::DotSet(b.clone(), x.clone(), Val::Uint(4)),
            B::DotGet(x.clone(), a.clone(), x.clone()),
            B::CallVoid(print.clone(), vec![(&x).into()].into()),
            B::FuncEnd,
        ];
        (res, "basic_ref_obj")
    }

    fn basic_copy_obj() -> (Vec<ByteCode>, &'static str) {
        let x: Ident = ("x", T::Uint).into();
        let s: Ident = ("s", T::String).into();
        let a: Ident = ("a", T::Struct("MyStruct".into())).into();
        let b: Ident = ("b", T::Struct("MyStruct".into())).into();
        let print: Ident = ("print", T::Void).into();
        let read_line: Ident = ("read_line", T::String).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        let res = vec![
            B::Struct("MyStruct".into()),
            B::Field(x.clone()),
            B::StructEnd,
            //
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(s.clone()),
            B::Call(s.clone(), read_line.clone(), vec![].into()),
            B::Create(x.clone()),
            B::Call(x.clone(), str_to_int.clone(), vec![(&s).into()].into()),
            B::Destroy(s.clone()),
            //
            B::Create(a.clone()),
            B::DotSet(a.clone(), x.clone(), (&x).into()),
            B::Create(b.clone()),
            B::Assign(b.clone(), Val::Copy(a.clone())),
            B::DotSet(b.clone(), x.clone(), Val::Uint(4)),
            B::DotGet(x.clone(), a.clone(), x.clone()),
            B::CallVoid(print.clone(), vec![(&x).into()].into()),
            B::FuncEnd,
        ];
        (res, "basic_copy_obj")
    }

    fn change_ref() -> (Vec<ByteCode>, &'static str) {
        let x: Ident = ("x", T::Uint).into();
        let s: Ident = ("s", T::String).into();
        let a: Ident = ("a", T::Struct("MyStruct".into())).into();
        let b: Ident = ("b", T::Ref(Box::new(T::Struct("MyStruct".into())))).into();
        let c: Ident = ("c", T::Struct("MyStruct".into())).into();
        let print: Ident = ("print", T::Void).into();
        let read_line: Ident = ("read_line", T::String).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        let res = vec![
            B::Struct("MyStruct".into()),
            B::Field(x.clone()),
            B::StructEnd,
            //
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(s.clone()),
            B::Create(x.clone()),
            // let a = { x = str_to_int(read_line()) };
            B::Call(s.clone(), read_line.clone(), vec![].into()),
            B::Call(x.clone(), str_to_int.clone(), vec![(&s).into()].into()),
            B::Create(a.clone()),
            B::DotSet(a.clone(), x.clone(), (&x).into()),
            // let c = { x = str_to_int(read_line()) };
            B::Call(s.clone(), read_line.clone(), vec![].into()),
            B::Call(x.clone(), str_to_int.clone(), vec![(&s).into()].into()),
            B::Create(c.clone()),
            B::DotSet(c.clone(), x.clone(), (&x).into()),
            // let b: Ref<MyStruct> = &a;
            // b = &c
            B::Create(b.clone()),
            B::Assign(b.clone(), Val::Ref(a.clone())),
            B::Assign(b.clone(), Val::Ref(c.clone())),
            // print(a.x) - should still be the original a.x, not the c.x
            B::DotGet(x.clone(), a.clone(), x.clone()),
            B::CallVoid(print.clone(), vec![(&x).into()].into()),
            B::DotGet(x.clone(), c.clone(), x.clone()),
            B::CallVoid(print.clone(), vec![(&x).into()].into()),
            B::Destroy(s.clone()),
            B::FuncEnd,
        ];
        (res, "change_ref")
    }

    fn fizz_buzz() -> (Vec<ByteCode>, &'static str) {
        let n: Ident = ("n", T::Uint).into();
        let s: Ident = ("s", T::String).into();
        let read_line: Ident = ("read_line", T::String).into();
        let i: Ident = ("i", T::Uint).into();
        let m: Ident = ("m", T::Uint).into();
        let d: Ident = ("d", T::Bool).into();
        let print: Ident = ("print", T::Void).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        let res = vec![
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(n.clone()),
            B::Create(s.clone()),
            B::Call(s.clone(), read_line.clone(), vec![].into()),
            B::Call(
                n.clone(),
                str_to_int.clone(),
                vec![Val::Ref(s.clone())].into(),
            ),
            B::Destroy(s.clone()),
            B::Create(i.clone()),
            B::Create(m.clone()),
            B::Create(d.clone()),
            B::Lt(d.clone(), Val::Copy(i.clone()), Val::Copy(n.clone())),
            //
            B::While(Val::Copy(d.clone())),
            B::CallVoid(print.clone(), vec![Val::Copy(i.clone())].into()),
            B::Mod(m.clone(), Val::Copy(i.clone()), Val::Uint(3)),
            B::Eq(d.clone(), Val::Copy(m.clone()), Val::Uint(0)),
            B::If(Val::Copy(d.clone())),
            B::CallVoid(print.clone(), vec![Val::Char('f')].into()),
            B::IfEnd,
            //
            B::Mod(m.clone(), Val::Copy(i.clone()), Val::Uint(5)),
            B::Eq(d.clone(), Val::Copy(m.clone()), Val::Uint(0)),
            B::If(Val::Copy(d.clone())),
            B::CallVoid(print.clone(), vec![Val::Char('b')].into()),
            B::IfEnd,
            //
            B::Add(i.clone(), Val::Copy(i.clone()), Val::Uint(1)),
            B::Lt(d.clone(), Val::Copy(i.clone()), Val::Copy(n.clone())),
            B::WhileEnd,
            B::FuncEnd,
        ];
        (res, "fizz_buzz")
    }

    fn objects_copied() -> (Vec<ByteCode>, &'static str) {
        let f: Ident = ("f", T::Void).into();
        let a: Ident = ("a", T::Struct("MyStruct".into())).into();
        let b: Ident = ("b", T::Struct("MyStruct".into())).into();
        let x: Ident = ("x", T::Uint).into();
        let s: Ident = ("s", T::String).into();
        let print: Ident = ("print", T::Void).into();
        let read_line: Ident = ("read_line", T::String).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        let res = vec![
            B::Struct("MyStruct".into()),
            B::Field(x.clone()),
            B::StructEnd,
            //
            B::Func(f.clone(), vec![a.clone()].into()),
            B::DotSet(a.clone(), x.clone(), Val::Uint(4)),
            B::FuncEnd,
            //
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(x.clone()),
            B::Create(s.clone()),
            B::Call(s.clone(), read_line.clone(), vec![].into()),
            B::Call(
                x.clone(),
                str_to_int.clone(),
                vec![Val::Ref(s.clone())].into(),
            ),
            B::Destroy(s.clone()),
            //
            B::Create(a.clone()),
            B::DotSet(a.clone(), x.clone(), Val::Copy(x.clone())),
            B::Create(b.clone()),
            B::Assign(b.clone(), Val::Copy(a.clone())),
            B::DotSet(b.clone(), x.clone(), Val::Uint(3)),
            //
            B::CallVoid(f.clone(), vec![Val::Copy(b.clone())].into()),
            B::DotGet(x.clone(), a.clone(), x.clone()),
            B::CallVoid(print.clone(), vec![Val::Copy(x.clone())].into()),
            B::DotGet(x.clone(), b.clone(), x.clone()),
            B::CallVoid(print.clone(), vec![Val::Copy(x.clone())].into()),
            B::FuncEnd,
        ];
        (res, "objects_copied")
    }

    pub trait TestRunner: Translator {
        fn run_test(&mut self, src: &str, input: &str) -> String;
    }

    fn fb(n: usize) -> String {
        let mut s = String::new();
        for i in 0..n {
            s += &format!("{i}\n");
            if i % 3 == 0 {
                s += "f\n";
            }
            if i % 5 == 0 {
                s += "b\n";
            }
        }
        s
    }

    #[test]
    fn programs_valid() {
        for (bc, name) in [
            add_nums(),
            basic_if(),
            basic_copy_obj(),
            basic_ref_obj(),
            change_ref(),
            fizz_buzz(),
            objects_copied(),
        ] {
            let mut v = Validator::new();
            let v_res = v.validate_bc(&bc);
            assert_eq!(v_res, Ok(()), "Failed on {name}")
        }
    }

    pub fn gauntlet(tr: &mut dyn TestRunner) {
        let (bc, name) = add_nums();
        let s = tr.translate_program(&bc).unwrap();
        for (a, b) in [(0, 0), (1, 0), (10, 20), (2000, 1234)] {
            assert_eq!(
                tr.run_test(&s, &format!("{a}\n{b}\n")),
                format!("{}\n", a + b),
                "error on {name} a={a}, b={b}"
            );
        }

        let (bc, name) = basic_if();
        let s = tr.translate_program(&bc).unwrap();
        for a in 0..10 {
            assert_eq!(
                tr.run_test(&s, &format!("{a}\n")),
                format!(
                    "{}\n",
                    if a < 2 {
                        'A'
                    } else if a == 2 {
                        'B'
                    } else if a == 3 {
                        'C'
                    } else {
                        'D'
                    }
                ),
                "error on {name} a={a}"
            );
        }

        let (bc, name) = basic_copy_obj();
        let s = tr.translate_program(&bc).unwrap();
        println!("{s}");
        for x in [0, 10, 20] {
            assert_eq!(
                tr.run_test(&s, &x.to_string()),
                format!("{x}\n"),
                "error on {name} x={x}"
            );
        }

        let (bc, name) = basic_ref_obj();
        let s = tr.translate_program(&bc).unwrap();
        for x in [0, 10, 20] {
            assert_eq!(
                tr.run_test(&s, &x.to_string()),
                "4\n",
                "error on {name} x={x}"
            );
        }

        let (bc, name) = change_ref();
        let s = tr.translate_program(&bc).unwrap();
        for (a, c) in [(0, 0), (0, 1), (1, 0), (100, 200)] {
            assert_eq!(
                tr.run_test(&s, &format!("{}\n{}\n", a, c)),
                format!("{a}\n{c}\n"),
                "error on {name} a={a}, c={c}"
            );
        }

        let (bc, name) = fizz_buzz();
        let s = tr.translate_program(&bc).unwrap();

        for n in [0, 10, 100] {
            assert_eq!(
                tr.run_test(&s, &n.to_string()),
                fb(n),
                "error on {name} n={n}"
            );
        }

        let (bc, name) = objects_copied();
        let s = tr.translate_program(&bc).unwrap();
        for x in [0, 10, 20, 30] {
            assert_eq!(
                tr.run_test(&s, &x.to_string()),
                format!("{x}\n3\n"),
                "error on {name} x={x}"
            )
        }
    }
}
