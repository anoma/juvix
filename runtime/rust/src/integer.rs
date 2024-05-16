// Integers

use super::defs::*;

pub fn word_to_smallint(x: Word) -> SmallInt {
    x as SmallInt
}

pub fn smallint_to_word(x: SmallInt) -> Word {
    x as Word
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_conversion() {
        for x in 0..100 {
            assert_eq!(word_to_smallint(smallint_to_word(x)), x);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        for x in 1..100 {
            let y = smallint_div(
                smallint_mul(
                    smallint_sub(
                        smallint_add(smallint_to_word(x), smallint_to_word(4)),
                        smallint_to_word(4),
                    ),
                    smallint_to_word(7),
                ),
                smallint_to_word(7),
            );
            assert_eq!(smallint_to_word(x), y);
        }
    }
}
