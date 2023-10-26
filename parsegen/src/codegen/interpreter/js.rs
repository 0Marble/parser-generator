//

use std::{
    fmt::Display,
    io::{Cursor, Write},
};

use crate::codegen::bytecode::{ByteCode, Ident, Type, Val};

use super::{Error, Interpreter};

pub struct Js;

impl Js {
    fn default_val(t: &Type, w: &mut dyn Write) -> std::io::Result<()> {
        match t {
            Type::Void => write!(w, ""),
            Type::Bool => write!(w, "false"),
            Type::Char => write!(w, "'a'"),
            Type::Uint => write!(w, "0"),
            Type::Array(t, size) => {
                write!(w, "[")?;
                for _ in 0..*size {
                    Js::default_val(t, w)?;
                    write!(w, ", ")?;
                }
                write!(w, "]")
            }
            Type::Vec(_) => write!(w, "[]"),
            Type::Struct(_, fields) => {
                write!(w, "{{")?;
                for (name, t) in fields.iter() {
                    write!(w, "{}: ", name)?;
                    Js::default_val(t, w)?;
                    write!(w, ",")?;
                }
                write!(w, "}}")
            }
            Type::Ref(t) => Js::default_val(t, w),
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Bool(b) => write!(f, "{}", b),
            Val::Char(c) => write!(f, "'{}'", c),
            Val::Uint(x) => write!(f, "{}", x),
            Val::Variable(n) => write!(f, "{}", n.name),
            Val::Ref(v) => write!(f, "{}", v),
        }
    }
}

impl Interpreter for Js {
    fn exec(&mut self, entry: Ident, bc: &[ByteCode]) -> Result<String, Error> {
        let mut res = vec![];
        let mut w = Cursor::new(&mut res);
        let w = &mut w;

        for c in bc {
            match c {
                ByteCode::Create(ident) => {
                    write!(w, "var {} = ", ident.name)?;
                    Js::default_val(&ident.t, w)?;
                    writeln!(w, ";")?;
                }
                ByteCode::Destroy(_) => (),
                ByteCode::Add(dest, lhs, rhs) => writeln!(w, "{} = {} + {};", dest.name, lhs, rhs)?,
                ByteCode::Sub(dest, lhs, rhs) => writeln!(w, "{} = {} - {};", dest.name, lhs, rhs)?,
                ByteCode::Mul(dest, lhs, rhs) => writeln!(w, "{} = {} * {};", dest.name, lhs, rhs)?,
                ByteCode::Div(dest, lhs, rhs) => writeln!(w, "{} = {} / {};", dest.name, lhs, rhs)?,
                ByteCode::Mod(dest, lhs, rhs) => writeln!(w, "{} = {} % {};", dest.name, lhs, rhs)?,
                ByteCode::Le(dest, lhs, rhs) => writeln!(w, "{} = {} < {};", dest.name, lhs, rhs)?,
                ByteCode::Leq(dest, lhs, rhs) => {
                    writeln!(w, "{} = {} <== {};", dest.name, lhs, rhs)?
                }
                ByteCode::Eq(dest, lhs, rhs) => {
                    writeln!(w, "{} = {} === {};", dest.name, lhs, rhs)?
                }
                ByteCode::And(dest, lhs, rhs) => {
                    writeln!(w, "{} = {} && {};", dest.name, lhs, rhs)?
                }
                ByteCode::Or(dest, lhs, rhs) => writeln!(w, "{} = {} || {};", dest.name, lhs, rhs)?,
                ByteCode::Not(dest, val) => writeln!(w, "{} = !{};", dest.name, val)?,
                ByteCode::IndexGet(dest, arr, idx) => {
                    writeln!(w, "{} = {}[{}];", dest.name, arr.name, idx)?
                }
                ByteCode::IndexSet(arr, idx, val) => {
                    writeln!(w, "{}[{}] = {};", arr.name, idx, val)?
                }
                ByteCode::DotGet(dest, obj, field) => {
                    writeln!(w, "{} = {}.{};", dest.name, obj.name, field.name)?
                }
                ByteCode::DotSet(obj, field, val) => {
                    writeln!(w, "{}.{} = {};", obj.name, field.name, val)?
                }
                ByteCode::Call(dest, func, args) => {
                    write!(w, "{} = {}(", dest.name, func.name)?;
                    for arg in args.iter() {
                        write!(w, "{}, ", arg)?;
                    }
                    writeln!(w, ");")?;
                }
                ByteCode::CallVoid(func, args) => {
                    write!(w, "{}(", func.name)?;
                    for arg in args.iter() {
                        write!(w, "{}, ", arg)?;
                    }
                    writeln!(w, ");")?;
                }
                ByteCode::While(cond) => writeln!(w, "while({}) {{", cond)?,
                ByteCode::WhileEnd => writeln!(w, "}}")?,
                ByteCode::If(cond) => writeln!(w, "if ({}) {{", cond)?,
                ByteCode::Else => writeln!(w, "}} else {{ ")?,
                ByteCode::IfEnd => writeln!(w, "}}")?,
                ByteCode::Switch(val) => writeln!(w, "switch ({}) {{", val)?,
                ByteCode::SwitchEnd => writeln!(w, "}}")?,
                ByteCode::SwitchCaseStart(val) => writeln!(w, "case {}:", val)?,
                ByteCode::SwitchDefaultCase => writeln!(w, "default:")?,
                ByteCode::SwitchCaseEnd => writeln!(w, "break;")?,
                ByteCode::Func(name, args) => {
                    write!(w, "function {}(", name.name)?;
                    for arg in args.iter() {
                        write!(w, "{}, ", arg.name)?;
                    }
                    writeln!(w, ") {{")?;
                }
                ByteCode::Ret(val) => writeln!(w, "return {};", val)?,
                ByteCode::FuncEnd => writeln!(w, "}}")?,
            }
        }

        writeln!(w, "{}();", entry.name)?;

        Ok(String::from_utf8(res).unwrap())
    }
}
