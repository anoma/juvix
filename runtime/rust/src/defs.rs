// Runtime definitions

// Type of words
pub type Word = u32;

// Type of pointers
pub type Pointer = Word;

pub const INITIAL_STACK_CAPACITY: usize = 1024;
pub const INITIAL_MEMORY_CAPACITY: usize = 1024;

pub fn to_usize(s: Word) -> usize {
    s as usize
}

pub fn to_word(s: usize) -> Word {
    s as Word
}
