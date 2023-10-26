use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Char,
    Uint,
    Array(Box<Type>, usize),
    Vec(Box<Type>),
    Struct(Rc<str>, Rc<[(Rc<str>, Type)]>),
    Ref(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Bool(bool),
    Char(char),
    Uint(usize),
    Variable(Ident),
    Ref(Rc<Val>),
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
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use ByteCode as B;
    use Type as T;

    pub fn add_nums(a: usize, b: usize) -> Vec<ByteCode> {
        vec![
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(("c", T::Uint).into()),
            B::Add(("c", T::Uint).into(), Val::Uint(a), Val::Uint(b)),
            B::CallVoid(
                ("print", T::Void).into(),
                vec![Val::Variable(("c", T::Uint).into())].into(),
            ),
            B::FuncEnd,
        ]
    }

    pub fn fizz_buzz(n: usize) -> Vec<ByteCode> {
        let i: Ident = ("i", T::Uint).into();
        let cond: Ident = ("cond", T::Bool).into();
        let m: Ident = ("m", T::Uint).into();
        let d: Ident = ("d", T::Bool).into();
        let print: Ident = ("print", T::Void).into();

        vec![
            B::Func(("main", T::Void).into(), vec![].into()),
            B::Create(i.clone()),
            B::Create(cond.clone()),
            B::While(Val::Variable(cond.clone())),
            B::CallVoid(print.clone(), vec![Val::Variable(i.clone())].into()),
            B::Create(m.clone()),
            B::Create(d.clone()),
            B::Mod(m.clone(), Val::Variable(i.clone()), Val::Uint(3)),
            B::Eq(d.clone(), Val::Variable(m.clone()), Val::Uint(0)),
            B::If(Val::Variable(d.clone())),
            B::CallVoid(print.clone(), vec![Val::Char('f')].into()),
            B::IfEnd,
            B::Mod(m.clone(), Val::Variable(i.clone()), Val::Uint(5)),
            B::Eq(d.clone(), Val::Variable(m.clone()), Val::Uint(0)),
            B::If(Val::Variable(d.clone())),
            B::CallVoid(print.clone(), vec![Val::Char('b')].into()),
            B::IfEnd,
            B::Add(i.clone(), Val::Variable(i.clone()), Val::Uint(1)),
            B::Le(cond.clone(), Val::Variable(i.clone()), Val::Uint(n)),
            B::WhileEnd,
            B::FuncEnd,
        ]
    }
}
