use std::{rc::Rc, str::FromStr};

use super::bytecode::{ByteCode, Ident, Type, Val};

#[derive(Debug, Clone, PartialEq)]
struct Struct {
    name: Rc<str>,
    fields: Rc<[Ident]>,
}

#[derive(Debug, Clone, PartialEq)]
struct Func {
    name: Rc<str>,
    ret: Type,
    args: Rc<[Ident]>,
}

pub struct Assembler {
    structs: Vec<Struct>,
    funcs: Vec<Func>,
    locals: Vec<Ident>,
    res: Vec<ByteCode>,
    line: usize,
    cur_struct: Option<(Rc<str>, Vec<Ident>)>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            structs: vec![],
            funcs: vec![],
            locals: vec![],
            res: vec![],
            line: 0,
            cur_struct: None,
        }
    }

    pub fn parse_op(&mut self, s: &str) {
        let mut split = s.split_whitespace();
        let split = &mut split;
        self.line += 1;

        let Some(op_name) = split.next() else { return; };

        match op_name.trim_start() {
            "//" => (),
            "create" => {
                let var_name = split
                    .next()
                    .unwrap_or_else(|| panic!("No var_name given on line {}", self.line));
                let t = self.parse_type(split);
                let ident: Ident = (var_name, t).into();
                self.add_local(ident.clone());
                self.res.push(ByteCode::Create(ident));
            }
            "destroy" => {
                let id = self.parse_ident(split);
                self.res.push(ByteCode::Destroy(id));
            }

            "add" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Add(res, lhs, rhs));
            }
            "sub" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Sub(res, lhs, rhs));
            }
            "mul" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Mul(res, lhs, rhs));
            }
            "mod" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Mod(res, lhs, rhs));
            }
            "eq" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Eq(res, lhs, rhs));
            }
            "lt" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Lt(res, lhs, rhs));
            }
            "le" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Le(res, lhs, rhs));
            }
            "and" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::And(res, lhs, rhs));
            }
            "or" => {
                let res = self.parse_ident(split);
                let lhs = self.parse_val(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Or(res, lhs, rhs));
            }
            "not" => {
                let res = self.parse_ident(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Not(res, rhs));
            }
            "assign" => {
                let res = self.parse_ident(split);
                let rhs = self.parse_val(split);
                self.res.push(ByteCode::Assign(res, rhs));
            }
            "deref" => {
                let res = self.parse_ident(split);
                let rhs = self.parse_ident(split);
                self.res.push(ByteCode::Deref(res, rhs));
            }

            "indexget" => {
                let res = self.parse_ident(split);
                let arr = self.parse_ident(split);
                let idx = self.parse_val(split);
                self.res.push(ByteCode::IndexGet(res, arr, idx));
            }
            "indexgetref" => {
                let res = self.parse_ident(split);
                let arr = self.parse_ident(split);
                let idx = self.parse_val(split);
                self.res.push(ByteCode::IndexGetRef(res, arr, idx));
            }
            "indexset" => {
                let arr = self.parse_ident(split);
                let idx = self.parse_val(split);
                let res = self.parse_val(split);
                self.res.push(ByteCode::IndexSet(arr, idx, res));
            }
            "dotget" => {
                let res = self.parse_ident(split);
                let obj = self.parse_ident(split);
                let field = self.parse_field(split, &obj);
                self.res.push(ByteCode::DotGet(res, obj, field));
            }
            "dotgetref" => {
                let res = self.parse_ident(split);
                let obj = self.parse_ident(split);
                let field = self.parse_field(split, &obj);
                self.res.push(ByteCode::DotGetRef(res, obj, field));
            }
            "dotset" => {
                let obj = self.parse_ident(split);
                let field = self.parse_field(split, &obj);
                let res = self.parse_val(split);
                self.res.push(ByteCode::DotSet(obj, field, res));
            }
            "call" => {
                let res = self.parse_ident(split);
                let func_name = split
                    .next()
                    .unwrap_or_else(|| panic!("Expected func name on line {}", self.line));
                let func = self
                    .get_func(func_name)
                    .unwrap_or_else(|| panic!("Unknown func {} on line {}", func_name, self.line));

                let arg_cnt = self.parse_arc_count(split);
                assert_eq!(
                    arg_cnt,
                    func.args.len(),
                    "Invalid number of arguments for {} (got {} expected {}), on line {}",
                    func_name,
                    arg_cnt,
                    func.args.len(),
                    self.line
                );

                let mut args = vec![];
                for _ in 0..arg_cnt {
                    args.push(self.parse_val(split));
                }
                self.res.push(ByteCode::Call(
                    res,
                    (func.name, func.ret).into(),
                    args.into(),
                ));
            }
            "callvoid" => {
                let func_name = split
                    .next()
                    .unwrap_or_else(|| panic!("Expected func name on line {}", self.line));
                let func = self
                    .get_func(func_name)
                    .unwrap_or_else(|| panic!("Unknown func {} on line {}", func_name, self.line));
                let arg_cnt = self.parse_arc_count(split);
                assert_eq!(
                    arg_cnt,
                    func.args.len(),
                    "Invalid number of arguments for {} (got {} expected {}), on line {}",
                    func_name,
                    arg_cnt,
                    func.args.len(),
                    self.line
                );
                let mut args = vec![];
                for _ in 0..arg_cnt {
                    args.push(self.parse_val(split));
                }
                self.res.push(ByteCode::CallVoid(
                    (func.name, func.ret).into(),
                    args.into(),
                ));
            }

            "while" => {
                let cond = self.parse_val(split);
                self.res.push(ByteCode::While(cond));
            }
            "whileend" => self.res.push(ByteCode::WhileEnd),
            "if" => {
                let cond = self.parse_val(split);
                self.res.push(ByteCode::If(cond));
            }
            "else" => self.res.push(ByteCode::Else),
            "ifend" => self.res.push(ByteCode::IfEnd),

            "switch" => {
                let val = self.parse_val(split);
                self.res.push(ByteCode::Switch(val));
            }
            "switchend" => self.res.push(ByteCode::SwitchEnd),
            "case" => {
                let val = self.parse_val(split);
                self.res.push(ByteCode::SwitchCase(val));
            }
            "default" => self.res.push(ByteCode::SwitchDefaultCase),
            "caseend" => self.res.push(ByteCode::SwitchCaseEnd),

            "importfunc" => {
                let func = self.parse_ident_type(split);
                let arg_cnt = self.parse_arc_count(split);
                let mut args = vec![];
                for _ in 0..arg_cnt {
                    let id = self.parse_ident_type(split);
                    args.push(id);
                }
                let args: Rc<[_]> = args.into();
                self.res
                    .push(ByteCode::ImportFunc(func.clone(), args.clone()));
                self.add_func(Func {
                    name: func.name,
                    ret: func.t,
                    args,
                });
            }
            "func" => {
                let func = self.parse_ident_type(split);
                let arg_cnt = self.parse_arc_count(split);
                let mut args = vec![];
                for _ in 0..arg_cnt {
                    let id = self.parse_ident_type(split);
                    self.add_local(id.clone());
                    args.push(id);
                }
                let args: Rc<[_]> = args.into();
                self.res.push(ByteCode::Func(func.clone(), args.clone()));
                self.add_func(Func {
                    name: func.name,
                    ret: func.t,
                    args,
                });
            }
            "ret" => {
                let res = self.parse_val(split);
                self.res.push(ByteCode::Ret(res))
            }
            "funcend" => {
                self.locals.clear();
                self.res.push(ByteCode::FuncEnd)
            }

            "importstruct" => {
                let struct_name = split
                    .next()
                    .unwrap_or_else(|| panic!("No struct name given on line {}", self.line));
                let cnt = self.parse_arc_count(split);
                let mut fields = vec![];
                for _ in 0..cnt {
                    fields.push(self.parse_ident_type(split));
                }
                self.res
                    .push(ByteCode::ImportStruct(struct_name.into(), fields.into()));
            }
            "struct" => {
                let struct_name = split
                    .next()
                    .unwrap_or_else(|| panic!("No struct name given on line {}", self.line));
                assert_eq!(self.cur_struct, None, "Nested struct on line {}", self.line);
                let t: Rc<str> = struct_name.into();
                self.res.push(ByteCode::Struct(t.clone()));
                self.cur_struct = Some((t, vec![]));
            }
            "field" => {
                let name = split
                    .next()
                    .unwrap_or_else(|| panic!("Expected field name on line {}", self.line));
                let t = self.parse_type(split);
                let (_, fields) = self
                    .cur_struct
                    .as_mut()
                    .unwrap_or_else(|| panic!("Field to no struct at line {}", self.line));

                let id: Ident = (name, t).into();
                fields.push(id.clone());
                self.res.push(ByteCode::Field(id));
            }
            "structend" => {
                let (s_name, fields) = self
                    .cur_struct
                    .take()
                    .unwrap_or_else(|| panic!("Ending no struct on line {}", self.line));
                self.add_struct(Struct {
                    name: s_name,
                    fields: fields.into(),
                });
                self.res.push(ByteCode::StructEnd)
            }

            _ => panic!("Unknown op_name: {}", op_name),
        }
    }

    pub fn finish(self) -> Vec<ByteCode> {
        self.res
    }

    fn get_struct(&self, name: &str) -> Option<Struct> {
        self.structs
            .iter()
            .find(|s| s.name.as_ref() == name)
            .cloned()
    }
    fn add_struct(&mut self, s: Struct) {
        assert_eq!(
            self.get_struct(s.name.as_ref()),
            None,
            "Struct '{}' already exists",
            s.name
        );
        self.structs.push(s);
    }
    fn add_func(&mut self, func: Func) {
        assert_eq!(
            self.get_func(func.name.as_ref()),
            None,
            "Function '{}' already exists",
            func.name
        );
        self.funcs.push(func);
    }
    fn get_func(&self, name: &str) -> Option<Func> {
        self.funcs.iter().find(|f| f.name.as_ref() == name).cloned()
    }

    fn get_local(&self, name: &str) -> Option<Ident> {
        self.locals
            .iter()
            .find(|l| l.name.as_ref() == name)
            .cloned()
    }
    fn add_local(&mut self, l: Ident) {
        assert_eq!(
            self.get_local(l.name.as_ref()),
            None,
            "Local '{}' already exists",
            l.name
        );
        self.locals.push(l);
    }

    fn parse_ident_type<'a>(&mut self, split: &mut impl Iterator<Item = &'a str>) -> Ident {
        let name = split
            .next()
            .unwrap_or_else(|| panic!("Expected name on line {}", self.line));
        let t = self.parse_type(split);
        (name, t).into()
    }
    fn parse_ident<'a>(&mut self, split: &mut impl Iterator<Item = &'a str>) -> Ident {
        let var_name = split
            .next()
            .unwrap_or_else(|| panic!("No ident given on line {}", self.line));
        self.get_local(var_name)
            .unwrap_or_else(|| panic!("Unknown ident: {} on line {}", var_name, self.line))
    }
    fn parse_val<'a>(&mut self, split: &mut impl Iterator<Item = &'a str>) -> Val {
        match split
            .next()
            .unwrap_or_else(|| panic!("Expected val on line {}", self.line))
        {
            "true" => Val::Bool(true),
            "false" => Val::Bool(false),
            "mv" => Val::Move(self.parse_ident(split)),
            "cp" => Val::Move(self.parse_ident(split)),
            "ref" => Val::Move(self.parse_ident(split)),
            s => {
                if let Ok(num) = usize::from_str(s) {
                    Val::Uint(num)
                } else if s.starts_with('\'') && s.ends_with('\'') {
                    Val::Char(
                        s.chars()
                            .nth(1)
                            .unwrap_or_else(|| panic!("invalid char at line {}", self.line)),
                    )
                } else {
                    panic!("Can not parse val: {} line {}", s, self.line);
                }
            }
        }
    }
    fn parse_arc_count<'a>(&mut self, split: &mut impl Iterator<Item = &'a str>) -> usize {
        usize::from_str(split.next().unwrap())
            .unwrap_or_else(|e| panic!("Expected count, got {e} on line {}", self.line))
    }
    fn parse_field<'a>(&mut self, split: &mut impl Iterator<Item = &'a str>, s: &Ident) -> Ident {
        let name = split
            .next()
            .unwrap_or_else(|| panic!("Expected field on line {}", self.line));
        if let Type::Struct(s) = &s.t.deref() {
            self.get_struct(s.as_ref())
                .unwrap_or_else(|| panic!("No such struct {} on line {}", s, self.line))
                .fields
                .iter()
                .find(|f| f.name.as_ref() == name)
                .unwrap_or_else(|| panic!("No field {} on struct {}, line {}", name, s, self.line))
                .clone()
        } else {
            panic!("{:?} is not a struct on line {}", s, self.line);
        }
    }

    fn parse_type<'a>(&mut self, split: &mut impl Iterator<Item = &'a str>) -> Type {
        match split
            .next()
            .unwrap_or_else(|| panic!("Expected type on line {}", self.line))
        {
            "void" => Type::Void,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "uint" => Type::Uint,
            "string" => Type::String,
            "arr" => {
                let t = self.parse_type(split);
                let cnt = usize::from_str(
                    split
                        .next()
                        .unwrap_or_else(|| panic!("Expected array size on line {}", self.line)),
                )
                .unwrap_or_else(|e| panic!("Expected array size: {e} on line {}", self.line));
                Type::Array(Box::new(t), cnt)
            }
            "vec" => {
                let t = self.parse_type(split);
                Type::Vec(Box::new(t))
            }
            "ref" => {
                let t = self.parse_type(split);
                Type::Ref(Box::new(t))
            }
            s => Type::Struct(s.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn math() -> String {
        "
func main void 0
create a uint
create b uint
assign a 10
assign b 20
create c uint
// c = a + b
add c cp a cp b
funcend
"
        .to_string()
    }

    #[test]
    fn compile() {
        let mut p = Assembler::new();
        for s in math().lines() {
            p.parse_op(&s);
        }
        for (i, bc) in p.finish().into_iter().enumerate() {
            println!("[{i}] {:?}", bc);
        }
    }
}
