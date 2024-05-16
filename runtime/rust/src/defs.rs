// Runtime definitions

// Type of words
pub type Word = u32;

// Type of pointers
pub type Pointer = Word;

// Type of signed small integers that fit into a single word
pub type SmallInt = i32;

pub const INITIAL_STACK_CAPACITY: usize = 1024;
pub const INITIAL_MEMORY_CAPACITY: usize = 1024;

pub fn word_to_usize(s: Word) -> usize {
    s as usize
}

pub fn usize_to_word(s: usize) -> Word {
    s as Word
}

pub fn word_to_smallint(x: Word) -> SmallInt {
    x as SmallInt
}

pub fn smallint_to_word(x: SmallInt) -> Word {
    x as Word
}

pub fn bool_to_word(x: bool) -> Word {
    x as Word
}

pub fn word_to_bool(x: Word) -> bool {
    if x == 0 {
        false
    } else {
        true
    }
}
