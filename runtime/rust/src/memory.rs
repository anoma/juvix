// Heap memory management

use super::defs::*;

pub struct Memory {
    memory: Vec<Word>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            memory: Vec::with_capacity(INITIAL_MEMORY_CAPACITY),
        }
    }

    pub fn alloc(&mut self, size: usize) -> Pointer {
        let idx = self.memory.len();
        self.memory.resize(idx + size, 0);
        idx as Pointer
    }
}

impl<Idx> std::ops::Index<Idx> for Memory
where
    Idx: std::slice::SliceIndex<[Word]>,
{
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.memory[index]
    }
}

impl<Idx> std::ops::IndexMut<Idx> for Memory
where
    Idx: std::slice::SliceIndex<[Word]>,
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        &mut self.memory[index]
    }
}
