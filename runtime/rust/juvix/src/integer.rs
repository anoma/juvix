// Integers

use super::defs::*;

pub fn make_smallint(x: SmallInt) -> Word {
    x as Word
}

pub fn smallint_value(x: Word) -> SmallInt {
    x as SmallInt
}

pub fn smallint_add(x: Word, y: Word) -> Word {
    make_smallint(smallint_value(x) + smallint_value(y))
}

pub fn smallint_sub(x: Word, y: Word) -> Word {
    make_smallint(smallint_value(x) - smallint_value(y))
}

pub fn smallint_mul(x: Word, y: Word) -> Word {
    make_smallint(smallint_value(x) * smallint_value(y))
}

pub fn smallint_div(x: Word, y: Word) -> Word {
    make_smallint(smallint_value(x) / smallint_value(y))
}

pub fn smallint_mod(x: Word, y: Word) -> Word {
    make_smallint(smallint_value(x) % smallint_value(y))
}

pub fn smallint_lt(x: Word, y: Word) -> Word {
    bool_to_word(smallint_value(x) < smallint_value(y))
}

pub fn smallint_le(x: Word, y: Word) -> Word {
    bool_to_word(smallint_value(x) <= smallint_value(y))
}


pub fn uint8_to_int(x : Word) -> Word {
    x
}

pub fn int_to_uint8(x : Word) -> Word {
    make_smallint(smallint_value(x).rem_euclid(256))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_conversion() {
        for x in 0..100 {
            assert_eq!(smallint_value(make_smallint(x)), x);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        for x in 1..100 {
            let y = smallint_div(
                smallint_mul(
                    smallint_sub(
                        smallint_add(make_smallint(x), make_smallint(4)),
                        make_smallint(4),
                    ),
                    make_smallint(7),
                ),
                make_smallint(7),
            );
            assert_eq!(make_smallint(x), y);
        }
    }

    #[test]
    fn test_int_to_uint8() {
        assert_eq!(smallint_value(int_to_uint8(make_smallint(-1))), 255);
        assert_eq!(smallint_value(int_to_uint8(make_smallint(255))), 255);
        assert_eq!(smallint_value(int_to_uint8(make_smallint(-256))), 0);
        assert_eq!(smallint_value(int_to_uint8(make_smallint(256))), 0);
    }
}
