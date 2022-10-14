#ifndef JUVIX_OBJECT_EQUALITY_H
#define JUVIX_OBJECT_EQUALITY_H

#include <juvix/object/object.h>

bool juvix_constr_equal(word_t x, word_t y);

static inline bool juvix_equal(word_t x, word_t y) {
    return x == y || (is_ptr(x) && is_ptr(y) && juvix_constr_equal(x, y));
}

#endif
