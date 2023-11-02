use std::{fmt::Display, io::Write};

use crate::codegen::bytecode::{ByteCode, Type, Val};

use super::Translator;

pub struct Js {
    buf: Vec<u8>,
}

impl Js {
    pub fn new() -> Self {
        Self { buf: vec![] }
    }

    fn default_val(&mut self, t: &Type) -> std::io::Result<()> {
        match t {
            Type::Void => write!(self.buf, ""),
            Type::Bool => write!(self.buf, "false"),
            Type::Char => write!(self.buf, "'a'"),
            Type::Uint => write!(self.buf, "0"),
            Type::Array(t, size) => {
                write!(self.buf, "[")?;
                for _ in 0..*size {
                    self.default_val(t)?;
                    write!(self.buf, ", ")?;
                }
                write!(self.buf, "]")
            }
            Type::String => write!(self.buf, "\"\""),
            Type::Vec(_) => write!(self.buf, "[]"),
            Type::Struct(name) => {
                write!(self.buf, "new {}()", name)
            }
            Type::Ref(_) => write!(self.buf, "null"),
        }
    }

    fn deep_copy_obj(&mut self) -> std::io::Result<()> {
        write!(
            self.buf,
            r#"
function deep_copy_obj(a) {{
    switch (typeof a) {{
        case "number": return a
        case "boolean": return a
        case "object": {{
            let b = {{}}

            for (const k in a) {{
                b[k] = deep_copy_obj(a[k])
            }}
            return b
        }}
        case "string": return a.repeat(1)
    }}
}}
        "#
        )
    }

    fn eat_bc(&mut self, bc: ByteCode) -> std::io::Result<()> {
        match bc {
            ByteCode::Create(ident) => {
                write!(self.buf, "var {} = ", ident.name)?;
                self.default_val(&ident.t)?;
                writeln!(self.buf, ";")?;
            }
            ByteCode::Destroy(_) => (),
            ByteCode::Add(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} + {};", dest.name, lhs, rhs)?
            }
            ByteCode::Sub(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} - {};", dest.name, lhs, rhs)?
            }
            ByteCode::Mul(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} * {};", dest.name, lhs, rhs)?
            }
            ByteCode::Div(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} / {};", dest.name, lhs, rhs)?
            }
            ByteCode::Mod(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} % {};", dest.name, lhs, rhs)?
            }
            ByteCode::Lt(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} < {};", dest.name, lhs, rhs)?
            }
            ByteCode::Le(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} <= {};", dest.name, lhs, rhs)?
            }
            ByteCode::Eq(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} === {};", dest.name, lhs, rhs)?
            }
            ByteCode::And(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} && {};", dest.name, lhs, rhs)?
            }
            ByteCode::Or(dest, lhs, rhs) => {
                writeln!(self.buf, "{} = {} || {};", dest.name, lhs, rhs)?
            }
            ByteCode::Not(dest, val) => writeln!(self.buf, "{} = !{};", dest.name, val)?,
            ByteCode::Assign(dest, val) => writeln!(self.buf, "{} = {};", dest.name, val)?,
            ByteCode::Deref(dest, id) => {
                writeln!(self.buf, "{} = deep_copy_obj({})", dest.name, id.name)?
            }

            ByteCode::IndexGet(dest, arr, idx) => writeln!(
                self.buf,
                "{} = deep_copy_obj({}[{}]);",
                dest.name, arr.name, idx
            )?,
            ByteCode::IndexGetRef(dest, arr, idx) => {
                writeln!(self.buf, "{} = {}[{}];", dest.name, arr.name, idx)?
            }
            ByteCode::IndexSet(arr, idx, val) => {
                writeln!(self.buf, "{}[{}] = {};", arr.name, idx, val)?
            }
            ByteCode::DotGet(dest, obj, field) => writeln!(
                self.buf,
                "{} = deep_copy_obj({}.{});",
                dest.name, obj.name, field.name
            )?,
            ByteCode::DotGetRef(dest, obj, field) => {
                writeln!(self.buf, "{} = {}.{};", dest.name, obj.name, field.name)?
            }
            ByteCode::DotSet(obj, field, val) => {
                writeln!(self.buf, "{}.{} = {};", obj.name, field.name, val)?
            }
            ByteCode::Call(dest, func, args) => {
                write!(self.buf, "{} = {}(", dest.name, func.name)?;
                for arg in args.iter() {
                    write!(self.buf, "{}, ", arg)?;
                }
                writeln!(self.buf, ");")?;
            }
            ByteCode::CallVoid(func, args) => {
                write!(self.buf, "{}(", func.name)?;
                for arg in args.iter() {
                    write!(self.buf, "{}, ", arg)?;
                }
                writeln!(self.buf, ");")?;
            }
            ByteCode::While(cond) => writeln!(self.buf, "while({}) {{", cond)?,
            ByteCode::WhileEnd => writeln!(self.buf, "}}")?,
            ByteCode::If(cond) => writeln!(self.buf, "if ({}) {{", cond)?,
            ByteCode::Else => writeln!(self.buf, "}} else {{ ")?,
            ByteCode::IfEnd => writeln!(self.buf, "}}")?,
            ByteCode::Switch(val) => writeln!(self.buf, "switch ({}) {{", val)?,
            ByteCode::SwitchEnd => writeln!(self.buf, "}}")?,
            ByteCode::SwitchCase(val) => writeln!(self.buf, "case {}:", val)?,
            ByteCode::SwitchDefaultCase => writeln!(self.buf, "default:")?,
            ByteCode::SwitchCaseEnd => writeln!(self.buf, "break;")?,

            ByteCode::ImportFunc(id, _) => match id.name.as_ref() {
                "print_int" => writeln!(self.buf, "function print_int(a) {{ console.log(a) }}")?,
                "print_char" => writeln!(self.buf, "function print_char(a) {{ console.log(a) }}")?,
                "readline" => writeln!(self.buf, "function readline() {{ let res = INPUTS[INPUTS_READ]; INPUTS_READ++; return res; }}")?,
                "str_to_int" => writeln!(self.buf, "function str_to_int(s) {{ return +s }}")?,
                _ => (),
            },
            ByteCode::Func(name, args) => {
                write!(self.buf, "function {}(", name.name)?;
                for arg in args.iter() {
                    write!(self.buf, "{}, ", arg.name)?;
                }
                writeln!(self.buf, ") {{")?;
            }
            ByteCode::Ret(val) => writeln!(self.buf, "return {};", val)?,
            ByteCode::FuncEnd => writeln!(self.buf, "}}")?,

            ByteCode::ImportStruct(_, _) => todo!(),
            ByteCode::Struct(s) => {
                write!(self.buf, "class {} {{\nconstructor() {{", s)?;
            }
            ByteCode::Field(field) => {
                write!(self.buf, "this.{} = ", field.name)?;
                self.default_val(&field.t)?;
                writeln!(self.buf, ";")?
            }
            ByteCode::StructEnd => writeln!(self.buf, "}}\n}}")?,
        }
        Ok(())
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Bool(b) => write!(f, "{}", b),
            Val::Char(c) => write!(f, "'{}'", c),
            Val::Uint(x) => write!(f, "{}", x),
            Val::Copy(n) => match n.t {
                Type::Void => unreachable!(),
                Type::Array(_, _) => write!(f, "deep_copy_obj({})", n.name),
                Type::String => write!(f, "deep_copy_obj({})", n.name),
                Type::Vec(_) => write!(f, "deep_copy_obj({})", n.name),
                Type::Struct(_) => write!(f, "deep_copy_obj({})", n.name),
                _ => write!(f, "{}", n.name),
            },
            Val::Move(n) => write!(f, "{}", n.name),
            Val::Ref(n) => write!(f, "{}", n.name),
        }
    }
}

impl Translator for Js {
    fn init(&mut self) {
        self.buf.clear();
        self.deep_copy_obj().unwrap();
    }

    fn translate_op(&mut self, op: ByteCode) {
        self.eat_bc(op).unwrap();
    }

    fn finalize(&mut self) {
        std::fs::File::create(format!("tests/dump.js"))
            .unwrap()
            .write_all(&self.buf)
            .unwrap();
    }
}

#[cfg(test)]
pub mod tests {
    use std::process::{Command, Stdio};

    use crate::codegen::tests::{gauntlet, TestRunner};

    use super::*;

    #[derive(Default)]
    pub struct NodeTestRunner {
        src: String,
        name: String,
    }
    impl TestRunner for NodeTestRunner {
        fn clear(&mut self) {
            self.src.clear();
        }

        fn init(&mut self, name: &str, bc: &[ByteCode]) {
            self.name = name.to_string();
            let mut js = Js::new();

            js.init();
            for bc in bc {
                js.translate_op(bc.clone());
            }
            js.finalize();

            self.src = String::from_utf8(js.buf).unwrap();
        }

        fn run_test(&self, input: &str) -> String {
            let mut src = self.src.as_bytes().clone().to_vec();
            writeln!(src, "var INPUTS_READ=0;\nvar INPUTS=[").unwrap();
            for line in input.lines() {
                write!(src, "\"{line}\",").unwrap();
            }
            writeln!(src, "];\nmain()").unwrap();

            let mut node_process = Command::new("node")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap();
            let Some(mut node_stdin) = node_process.stdin.take() else { unreachable!() };

            node_stdin.write_all(&src).unwrap();
            drop(node_stdin);
            let output = node_process.wait_with_output().unwrap();
            assert!(
                output.status.success(),
                "stderr: {}",
                String::from_utf8(output.stderr).unwrap()
            );
            String::from_utf8(output.stdout).unwrap()
        }
    }

    #[test]
    fn bytecode_gauntlet() {
        let mut tr = NodeTestRunner::default();
        gauntlet(&mut tr);
    }
}
