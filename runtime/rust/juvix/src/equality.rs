// Equality

use super::defs::*;

// Check for equality. The implementation is incorrect for complex objects, but
// complex objects are not compared in Juvix with built-in equality for now.
pub fn juvix_equal(x: Word, y: Word) -> Word {
    bool_to_word(x == y)
}
