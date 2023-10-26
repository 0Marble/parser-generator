use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Char,
    Uint,
    Array(Box<Type>, usize),
    Vec(Box<Type>),
    Struct(Rc<[(Rc<str>, Type)]>),
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
    name: Rc<str>,
    t: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    name: Rc<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCode {
    Create(Ident),
    Destroy(Ident),

    Plus(Ident, Val, Val),
    Minus(Ident, Val, Val),
    Mul(Ident, Val, Val),
    Div(Ident, Val, Val),
    Eq(Ident, Val, Val),
    And(Ident, Val, Val),
    Or(Ident, Val, Val),
    Not(Ident, Val, Val),

    IndexGet(Ident, Ident, usize),
    IndexSet(Ident, usize, Ident),
    DotGet(Ident, Ident, Ident),
    DotSet(Ident, Ident, Ident),
    Call(Ident, Val, Rc<[Val]>),

    While(Val),
    WhileEnd,
    If(Val),
    ElseIf(Val),
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
