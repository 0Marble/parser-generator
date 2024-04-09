pub mod cfg;
pub mod regex;

pub trait Rng {
    fn gen(&mut self) -> usize;
}

use rand::rngs::ThreadRng;

impl Rng for ThreadRng {
    fn gen(&mut self) -> usize {
        rand::Rng::gen(self)
    }
}
