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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory() {
        let mut mem = Memory::new();
        for k in 0..100 {
            let p = mem.alloc(1000);
            for j in 1..1001 {
                mem[(p + j - 1) as usize] = k * 1000 + j;
                assert_eq!(mem[(p + j - 1) as usize], k * 1000 + j);
            }
        }
        for i in 0..100_000 {
            assert_eq!(mem[i as usize], i + 1);
        }
    }
}
