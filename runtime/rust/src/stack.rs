// Stack management

use super::defs::*;

pub struct Stack {
    data: Vec<Word>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            data: Vec::with_capacity(INITIAL_STACK_CAPACITY),
        }
    }

    pub fn push(&mut self, x: Word) {
        self.data.push(x);
    }

    pub fn pop(&mut self) -> Word {
        self.data.pop().expect("stack underflow")
    }

    pub fn top(&self) -> Word {
        self.data[self.data.len() - 1]
    }
}
