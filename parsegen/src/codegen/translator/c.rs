//

use std::{collections::HashMap, io::Write, rc::Rc};

use super::{super::bytecode::*, Translator};

#[derive(Default)]
pub struct CTranslator {
    src_buf: Vec<u8>,
    header_buf: Vec<u8>,

    // keep toll of what we encounter in the bytecode
    functions: Vec<(Ident, Rc<[Ident]>)>,
    structs: Vec<(Rc<str>, Rc<[Ident]>)>,
    arrays: Vec<(Type, usize)>,
    vecs: Vec<Type>,
    refs: Vec<Type>,
    type_name: HashMap<Type, String>,

    bc: Vec<ByteCode>,
    cur_struct: Option<(Rc<str>, Vec<Ident>)>,
}

impl CTranslator {
    pub fn new() -> Self {
        Self::default()
    }

    fn generate_definitions(&mut self) {
        self.generate_string();
        for (t, n) in std::mem::take(&mut self.arrays) {
            self.generate_array(t, n);
        }
        for t in std::mem::take(&mut self.vecs) {
            self.generate_vec(t);
        }
        for t in std::mem::take(&mut self.refs) {
            self.generate_ref(t);
        }
        for (name, fields) in std::mem::take(&mut self.structs) {
            self.generate_struct(name, fields);
        }
    }

    fn is_type_public(&self, t: Type) -> bool {
        for (func, args) in &self.functions {
            if func.t == t || args.iter().any(|id| id.t == t) {
                return true;
            }
        }
        for (_, fields) in &self.structs {
            if fields.iter().any(|id| id.t == t) {
                return true;
            }
        }

        false
    }

    fn generate_string(&mut self) {
        let is_public = self.is_type_public(Type::String);
        let mut buf = if is_public {
            std::mem::take(&mut self.header_buf)
        } else {
            std::mem::take(&mut self.src_buf)
        };

        let name = self.type_name.get(&Type::String).unwrap();
        self.generate_vec_like(Type::Char, name, &mut buf);

        if is_public {
            _ = std::mem::replace(&mut self.header_buf, buf);
        } else {
            _ = std::mem::replace(&mut self.src_buf, buf);
        }
    }

    fn generate_vec_like(&mut self, elem: Type, name: &str, buf: &mut dyn Write) {
        let elem_name = self.type_name.get(&elem).unwrap();
        writeln!(
            buf,
            r#"
typedef struct {{
    {elem_name}* ptr;
    uint64_t len;
    uint64_t cap;
}} {name};

// {name} constructor
{name} new{name}() {{
    {name} res = {{ NULL, 0, 0}};
    return res;
}}

// {name} destructor
void destroy{name}({name}* s) {{
    assert(s != NULL);
    if (s->ptr != NULL) {{
        free(s->ptr);
        s->ptr = NULL;
        s->cap = 0;
        s->len = 0;
    }}
}}

// Deep copy {name}
{name} deepCopy{name}({name}* s) {{
    assert(s != NULL);
    {name} other = newString{name}();
    other.ptr = ({elem_name}*)malloc(s->len * sizeof({elem_name}));
    other.cap = s->len;
    assert(other.ptr != NULL);
    for (uint64_t i = 0; i < s->len; i++) {{
        other.ptr[i] = s->ptr[i];
    }}
    return other;
}}

// index {name}
char index{name}({name}* s, uint64_t i) {{
    assert(s != NULL);
    assert(s->len > i);
    return s->ptr[i];
}}

// push a {elem_name} into {name}
void push{name}({name}* s, {elem_name} c) {{
    assert(s != NULL);
    if (s->ptr == NULL) {{
        s->ptr = ({elem_name}*)malloc(10 * sizeof({elem_name}));
        assert(s->prt != NULL);
        s->cap = 10;
    }}
    if (s->len + 1 >= s->cap) {{
        char* newPtr = ({elem_name}*)realloc(s->ptr, 2 * s->cap * sizeof({elem_name}));
        assert(newPtr != NULL);
        s->cap *= 2;
        s->ptr = newPtr;
    }}
    s->ptr[s->len] = c;
    s->len++;
}}

// is {name} empty:
bool isEmpty{name}({name}* s) {{
    assert(s != NULL);
    return s->len == 0;
}}
"#
        )
        .unwrap();
    }
    fn generate_array(&mut self, t: Type, n: usize) {
        todo!()
    }
    fn generate_ref(&mut self, t: Type) {
        todo!()
    }
    fn generate_struct(&mut self, name: Rc<str>, fields: Rc<[Ident]>) {
        todo!()
    }

    fn deep_copy_name(&self, t: Type) -> &str {
        todo!()
    }
    fn index_name(&self, t: Type) -> &str {
        todo!()
    }
}

// we want to note which functions and structs we find, and put all of them into the .h file
// if we encounter a compound type (Array<T,N>, Vec<T>, Ref<T>), we will want to create this
// type (in .c if it is only present in the function bodies, otherwise in .h)
// we also need to provide "constructors" and "destructors" for these types, and make sure
// their names are not clashing with the names of other symbols. We should also generate
// deep copy functions for all encountered structs (either user requested or auto-generated)
impl Translator for CTranslator {
    fn init(&mut self) {
        todo!()
    }

    fn translate_op(&mut self, op: ByteCode) {
        match op.clone() {
            ByteCode::Create(id) => match id.t {
                Type::Array(t, n) => {
                    if !self.arrays.contains(&(*t.clone(), n)) {
                        self.arrays.push((*t, n))
                    }
                }
                Type::Vec(t) => {
                    if !self.vecs.contains(t.as_ref()) {
                        self.vecs.push(*t);
                    }
                }
                Type::Ref(t) => {
                    if !self.refs.contains(t.as_ref()) {
                        self.refs.push(*t);
                    }
                }
                _ => (),
            },
            ByteCode::ImportFunc(func, args) => self.functions.push((func, args)),
            ByteCode::Func(func, args) => self.functions.push((func, args)),
            ByteCode::ImportStruct(name, fields) => self.structs.push((name, fields)),
            ByteCode::Struct(name) => self.cur_struct = Some((name, vec![])),
            ByteCode::Field(id) => {
                if let Some((_, fields)) = self.cur_struct.as_mut() {
                    fields.push(id);
                }
            }
            ByteCode::FuncEnd => {
                let (name, fields) = self.cur_struct.take().unwrap();
                self.structs.push((name, fields.into()));
            }
            _ => (),
        }
        self.bc.push(op);
    }

    fn finalize(&mut self) {
        for op in &self.bc {
            match op {
                ByteCode::Create(_) => todo!(),
                ByteCode::Destroy(_) => todo!(),

                ByteCode::Add(_, _, _) => todo!(),
                ByteCode::Sub(_, _, _) => todo!(),
                ByteCode::Mul(_, _, _) => todo!(),
                ByteCode::Div(_, _, _) => todo!(),
                ByteCode::Mod(_, _, _) => todo!(),
                ByteCode::Eq(_, _, _) => todo!(),
                ByteCode::Lt(_, _, _) => todo!(),
                ByteCode::Le(_, _, _) => todo!(),
                ByteCode::And(_, _, _) => todo!(),
                ByteCode::Or(_, _, _) => todo!(),
                ByteCode::Not(_, _) => todo!(),
                ByteCode::Assign(_, _) => todo!(),
                ByteCode::Deref(_, _) => todo!(),

                ByteCode::IndexGet(_, _, _) => todo!(),
                ByteCode::IndexSet(_, _, _) => todo!(),
                ByteCode::DotGet(_, _, _) => todo!(),
                ByteCode::DotSet(_, _, _) => todo!(),
                ByteCode::IndexGetRef(_, _, _) => todo!(),
                ByteCode::DotGetRef(_, _, _) => todo!(),

                ByteCode::Call(_, _, _) => todo!(),
                ByteCode::CallVoid(_, _) => todo!(),

                ByteCode::While(_) => todo!(),
                ByteCode::WhileEnd => todo!(),
                ByteCode::If(_) => todo!(),
                ByteCode::Else => todo!(),
                ByteCode::IfEnd => todo!(),
                ByteCode::Switch(_) => todo!(),
                ByteCode::SwitchEnd => todo!(),
                ByteCode::SwitchCase(_) => todo!(),
                ByteCode::SwitchDefaultCase => todo!(),
                ByteCode::SwitchCaseEnd => todo!(),

                ByteCode::ImportFunc(_, _) => todo!(),
                ByteCode::Func(_, _) => todo!(),
                ByteCode::Ret(_) => todo!(),
                ByteCode::FuncEnd => todo!(),

                ByteCode::ImportStruct(_, _) => todo!(),
                ByteCode::Struct(_) => todo!(),
                ByteCode::Field(_) => todo!(),
                ByteCode::StructEnd => todo!(),
            }
        }
    }
}
