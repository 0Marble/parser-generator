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

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Bool(bool),
    Char(char),
    Uint(usize),
    Copy(Ident),
    Ref(Ident),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: Rc<str>,
    pub t: Type,
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
    Le(Ident, Val, Val),
    Leq(Ident, Val, Val),
    And(Ident, Val, Val),
    Or(Ident, Val, Val),
    Not(Ident, Val),
    Assign(Ident, Val),

    IndexGet(Ident, Ident, usize),
    IndexSet(Ident, usize, Val),
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
    SwitchCaseStart(Val),
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
    use crate::codegen::interpreter::Interpreter;

    use super::*;
    use ByteCode as B;
    use Type as T;

    fn add_nums() -> Vec<ByteCode> {
        let a: Ident = ("a", T::Uint).into();
        let b: Ident = ("b", T::Uint).into();
        let c: Ident = ("c", T::Uint).into();
        let s: Ident = ("s", T::String).into();

        vec![
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
        ]
    }

    fn fizz_buzz() -> Vec<ByteCode> {
        let n: Ident = ("n", T::Uint).into();
        let s: Ident = ("s", T::Uint).into();
        let read_line: Ident = ("read_line", T::String).into();
        let i: Ident = ("i", T::Uint).into();
        let m: Ident = ("m", T::Uint).into();
        let d: Ident = ("d", T::Bool).into();
        let print: Ident = ("print", T::Void).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        vec![
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
            B::Le(d.clone(), Val::Copy(i.clone()), Val::Copy(n.clone())),
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
            B::Le(d.clone(), Val::Copy(i.clone()), Val::Copy(n.clone())),
            B::WhileEnd,
            B::FuncEnd,
        ]
    }

    fn objects_copied() -> Vec<ByteCode> {
        let f: Ident = ("f", T::Void).into();
        let a: Ident = ("a", T::Struct("MyStruct".into())).into();
        let b: Ident = ("b", T::Struct("MyStruct".into())).into();
        let x: Ident = ("x", T::Uint).into();
        let s: Ident = ("s", T::String).into();
        let print: Ident = ("print", T::Void).into();
        let read_line: Ident = ("read_line", T::String).into();
        let str_to_int: Ident = ("str_to_int", T::Uint).into();

        vec![
            B::Struct("MyStruct".into()),
            B::Field(x.clone()),
            B::StructEnd,
            //
            B::Func(f.clone(), vec![a.clone()].into()),
            B::DotSet(a.clone(), x.clone(), Val::Uint(4)),
            B::FuncEnd,
            //
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(a.clone()),
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
        ]
    }

    pub trait TestRunner: Interpreter {
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

    pub fn gauntlet(tr: &mut dyn TestRunner) {
        let bc = add_nums();
        let s = tr.exec(&bc).unwrap();
        for (a, b) in [(0, 0), (1, 0), (10, 20), (2000, 1234)] {
            assert_eq!(
                tr.run_test(&s, &format!("{a}\n{b}\n")),
                format!("{}\n", a + b),
                "error on add_nums a={a}, b={b}"
            );
        }
        let bc = fizz_buzz();
        let s = tr.exec(&bc).unwrap();
        println!("{s}");

        for n in [0, 10, 100] {
            assert_eq!(
                tr.run_test(&s, &n.to_string()),
                fb(n),
                "error on fizz_buzz n={n}"
            );
        }

        let bc = objects_copied();
        let s = tr.exec(&bc).unwrap();
        for x in [0, 10, 20, 30] {
            assert_eq!(
                tr.run_test(&s, &x.to_string()),
                format!("{x}\n3\n"),
                "error on objects x={x}"
            )
        }
    }
}
