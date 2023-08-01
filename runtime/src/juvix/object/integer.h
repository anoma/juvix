#ifndef JUVIX_OBJECT_INTEGER_H
#define JUVIX_OBJECT_INTEGER_H

/*
This module provides an implementation of Juvix integers.

Assumption: Signed machine integers are represented in two's complement.
*/

#include <juvix/object/boolean.h>
#include <juvix/object/object.h>

#ifdef SIMPLE_INTS

static inline word_t make_smallint(word_t x) { return x; }
static inline int_t get_smallint(word_t x) { return x; }

static inline word_t smallint_add(word_t x, word_t y) { return x + y; }
static inline word_t smallint_sub(word_t x, word_t y) { return x - y; }

#else

static inline word_t make_smallint(word_t x) { return make_unboxed(x); }
static inline int_t get_smallint(word_t x) { return get_unboxed_int(x); }

static inline word_t smallint_add(word_t x, word_t y) { return x - 1 + y; }
static inline word_t smallint_sub(word_t x, word_t y) { return x - y + 1; }

#endif  // not SIMPLE_INTS

static inline word_t smallint_mul(word_t x, word_t y) {
    return make_smallint(get_smallint(x) * get_smallint(y));
}
static inline word_t smallint_div(word_t x, word_t y) {
    return make_smallint(get_smallint(x) / get_smallint(y));
}
static inline word_t smallint_mod(word_t x, word_t y) {
    return make_smallint(get_smallint(x) % get_smallint(y));
}

static inline word_t smallint_lt(word_t x, word_t y) {
    return make_bool(get_smallint(x) < get_smallint(y));
}
static inline word_t smallint_le(word_t x, word_t y) {
    return make_bool(get_smallint(x) <= get_smallint(y));
}

#endif
