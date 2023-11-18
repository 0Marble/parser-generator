use self::{bytecode::ByteCode, translator::Translator};

pub mod assembler;
pub mod bytecode;
pub mod tokenizer;
pub mod translator;

#[cfg(test)]
mod tests;

pub trait Codegen {
    fn generate(&mut self) -> Vec<(String, String)>;
}

pub trait IntoBytecode {
    fn into_bytecode(&self) -> Vec<ByteCode>;
}

pub struct BytecodeCodegen {
    into_bc: Box<dyn IntoBytecode>,
    translator: Box<dyn Translator>,
}

impl Codegen for BytecodeCodegen {
    fn generate(&mut self) -> Vec<(String, String)> {
        let bc = self.into_bc.into_bytecode();
        self.translator.init();
        for bc in bc {
            self.translator.translate_op(bc);
        }
        self.translator.finalize()
    }
}
