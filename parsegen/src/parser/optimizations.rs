use super::lgraph::{Item, Lgraph};

impl Lgraph {
    pub fn optimize(mut self) -> Self {
        self
    }

    // If from nodes q1, q2 there are only edges of type (q1, xi, p), (q2, xi, p), we can merge q1
    // and q2
    fn back_merge(self, edges_to: &mut Vec<Vec<(usize, usize)>>) -> (Self, bool) {
        todo!()
    }
}
