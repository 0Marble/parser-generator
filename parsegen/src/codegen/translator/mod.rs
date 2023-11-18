use super::bytecode::ByteCode;

pub trait Translator {
    fn init(&mut self);
    fn translate_op(&mut self, op: ByteCode);
    fn finalize(&mut self) -> Vec<(String, String)>;
}

pub mod js;
