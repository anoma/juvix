#ifndef JUVIX_EQUALITY_H
#define JUVIX_EQUALITY_H

#include <juvix/object.h>

bool juvix_do_equal(word_t x, word_t y);

static inline bool juvix_equal(word_t x, word_t y) {
    return x == y || (!is_unboxed(x) && GET_KIND(x) == GET_KIND(y) &&
                      juvix_do_equal(x, y));
}

#endif
