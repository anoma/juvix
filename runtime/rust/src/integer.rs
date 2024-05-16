// Integers

use super::defs::*;

pub fn smallint_add(x: Word, y: Word) -> Word {
    x + y
}

pub fn smallint_sub(x: Word, y: Word) -> Word {
    smallint_to_word(word_to_smallint(x) - word_to_smallint(y))
}

pub fn smallint_mul(x: Word, y: Word) -> Word {
    smallint_to_word(word_to_smallint(x) * word_to_smallint(y))
}

pub fn smallint_div(x: Word, y: Word) -> Word {
    smallint_to_word(word_to_smallint(x) / word_to_smallint(y))
}

pub fn smallint_mod(x: Word, y: Word) -> Word {
    smallint_to_word(word_to_smallint(x) % word_to_smallint(y))
}

pub fn smallint_lt(x: Word, y: Word) -> Word {
    bool_to_word(word_to_smallint(x) < word_to_smallint(y))
}

pub fn smallint_le(x: Word, y: Word) -> Word {
    bool_to_word(word_to_smallint(x) <= word_to_smallint(y))
}
