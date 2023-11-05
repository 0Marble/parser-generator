use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub fn deref(&self) -> Type {
        if let Type::Ref(t) = self {
            t.deref()
        } else {
            self.clone()
        }
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
    Deref(Ident, Ident),

    IndexGet(Ident, Ident, Val),
    IndexSet(Ident, Val, Val),
    DotGet(Ident, Ident, Ident),
    DotSet(Ident, Ident, Val),
    IndexGetRef(Ident, Ident, Val),
    DotGetRef(Ident, Ident, Ident),

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

    ImportFunc(Ident, Rc<[Ident]>),
    Func(Ident, Rc<[Ident]>),
    Ret(Val),
    FuncEnd,

    ImportStruct(Rc<str>, Rc<[Ident]>),
    Struct(Rc<str>),
    Field(Ident),
    StructEnd,
}
