use std::fmt::Display;

use super::bytecode::ByteCode;

pub struct Validator {}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    TypeError(usize, ByteCode, &'static str),
    IndexOverflow(usize, ByteCode),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

/*
* Program -> (TypeDef | FuncDef)*
* TypeDef -> B::Struct (B::Field)* B::StructEnd
* FuncDef -> B::Func (Statement)*  B::FuncEnd
* Statement -> Expr | If | While | Switch | B::Ret
* If -> B::If (Statement)* (B::Else (Statement)* ) B::IfEnd
* While -> B::While (Statement)* B::WhileEnd
* Expr -> all the other guys
*/

use super::bytecode::Type as T;
use super::bytecode::Val;

impl Validator {
    pub fn new() -> Self {
        Self {}
    }

    // for now only check immediately available types
    pub fn validate_bc(&mut self, bc: &[ByteCode]) -> Result<(), Error> {
        for (i, bc) in bc.iter().enumerate() {
            match bc {
                // register variable as created, check names
                ByteCode::Create(_) => (),
                ByteCode::Destroy(_) => (),
                // only numbers allowed
                ByteCode::Add(a, b, c) => {
                    if a.t != T::Uint || b.get_type() != T::Uint || c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Uints"));
                    }
                }
                ByteCode::Sub(a, b, c) => {
                    if a.t != T::Uint || b.get_type() != T::Uint || c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Uints"));
                    }
                }
                ByteCode::Mul(a, b, c) => {
                    if a.t != T::Uint || b.get_type() != T::Uint || c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Uints"));
                    }
                }
                ByteCode::Div(a, b, c) => {
                    if a.t != T::Uint || b.get_type() != T::Uint || c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Uints"));
                    }
                }
                ByteCode::Mod(a, b, c) => {
                    if a.t != T::Uint || b.get_type() != T::Uint || c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Uints"));
                    }
                }
                // bool any any
                ByteCode::Eq(a, b, c) => {
                    if a.t != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected a Bool lhs"));
                    }
                    if b.get_type() != c.get_type() {
                        return Err(Error::TypeError(i, bc.clone(), "b and c type mismatch"));
                    }
                }
                // bool number number
                ByteCode::Le(a, b, c) => {
                    if a.t != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected a Bool lhs"));
                    }
                    if b.get_type() != T::Uint || c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Expected Uint b and c"));
                    }
                }
                ByteCode::Leq(a, b, c) => {
                    if a.t != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected a Bool lhs"));
                    }
                    if b.get_type() != T::Uint || c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Expected Uint b and c"));
                    }
                }
                // bool bool bool
                ByteCode::And(a, b, c) => {
                    if a.t != T::Bool || b.get_type() != T::Bool || c.get_type() != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Bool"));
                    }
                }
                ByteCode::Or(a, b, c) => {
                    if a.t != T::Bool || b.get_type() != T::Bool || c.get_type() != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Bool"));
                    }
                }
                ByteCode::Not(a, b) => {
                    if a.t != T::Bool || b.get_type() != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected all Bool"));
                    }
                }
                // same types
                ByteCode::Assign(a, b) => {
                    if a.t != b.get_type() {
                        return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                    }
                }
                ByteCode::Deref(a, b) => {
                    if let T::Ref(t) = b.get_type() {
                        if t.as_ref() != &a.t {
                            return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                        }
                    } else {
                        return Err(Error::TypeError(i, bc.clone(), "Not a reference"));
                    }
                }
                // x is the same type as rhs elems, index in number
                ByteCode::IndexGet(a, b, c) => {
                    if c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Index not Uint"));
                    }
                    if let T::Array(elem, size) = &b.t {
                        if elem.as_ref() != &a.t {
                            return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                        }
                        if let Val::Uint(n) = c {
                            if n >= size {
                                return Err(Error::IndexOverflow(i, bc.clone()));
                            }
                        }
                    } else if let T::Vec(elem) = &b.t {
                        if elem.as_ref() != &a.t {
                            return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                        }
                    } else if let T::String = b.t {
                        if T::Char != a.t {
                            return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                        }
                    } else {
                        return Err(Error::TypeError(i, bc.clone(), "Not indexable"));
                    }
                }
                ByteCode::IndexSet(b, c, a) => {
                    if c.get_type() != T::Uint {
                        return Err(Error::TypeError(i, bc.clone(), "Index not Uint"));
                    }
                    if let T::Array(elem, size) = &b.t {
                        if elem.as_ref() != &a.get_type() {
                            return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                        }
                        if let Val::Uint(n) = c {
                            if n >= size {
                                return Err(Error::IndexOverflow(i, bc.clone()));
                            }
                        }
                    } else if let T::Vec(elem) = &b.t {
                        if elem.as_ref() != &a.get_type() {
                            return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                        }
                    } else if let T::String = b.t {
                        if T::Char != a.get_type() {
                            return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                        }
                    } else {
                        return Err(Error::TypeError(i, bc.clone(), "Not indexable"));
                    }
                } // x is the same type as the field, struct has field
                ByteCode::DotGet(a, _, c) => {
                    if a.t != c.t {
                        return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                    }
                }
                ByteCode::DotSet(_, c, a) => {
                    if a.get_type() != c.t {
                        return Err(Error::TypeError(i, bc.clone(), "Type mismatch"));
                    }
                } // x is the same type as func return, args are the same type as func signature
                ByteCode::Call(a, b, _) => {
                    if a.t != b.t {
                        return Err(Error::TypeError(
                            i,
                            bc.clone(),
                            "Function return type mismatch",
                        ));
                    }
                }
                // res is actually void, args correct
                ByteCode::CallVoid(a, _) => {
                    if a.t != T::Void {
                        return Err(Error::TypeError(
                            i,
                            bc.clone(),
                            "Function return value ignored",
                        ));
                    }
                }
                // cond is bool
                ByteCode::While(a) => {
                    if a.get_type() != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected Bool"));
                    }
                }
                ByteCode::WhileEnd => (),
                // cond is bool
                ByteCode::If(a) => {
                    if a.get_type() != T::Bool {
                        return Err(Error::TypeError(i, bc.clone(), "Expected Bool"));
                    }
                }
                ByteCode::Else => (),
                ByteCode::IfEnd => (),
                // val is basic type
                ByteCode::Switch(a) => {
                    if !a.get_type().is_basic() {
                        return Err(Error::TypeError(i, bc.clone(), "Expected a basic type"));
                    }
                }
                ByteCode::SwitchEnd => (),
                // val is the same type as the switch requires
                ByteCode::SwitchCase(a) => {
                    if !a.get_type().is_basic() {
                        return Err(Error::TypeError(i, bc.clone(), "Expected a basic type"));
                    }
                }
                ByteCode::SwitchDefaultCase => (),
                ByteCode::SwitchCaseEnd => (),
                // register function, check names
                ByteCode::Func(_, _) => (),
                // ret type is correct
                ByteCode::Ret(_) => (),
                ByteCode::FuncEnd => (),
                // register stuff, check names
                ByteCode::Struct(_) => (),
                ByteCode::Field(_) => (),
                ByteCode::StructEnd => (),
            }
        }

        Ok(())
    }
}
