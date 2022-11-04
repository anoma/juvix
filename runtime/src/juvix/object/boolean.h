#ifndef JUVIX_OBJECT_BOOLEAN_H
#define JUVIX_OBJECT_BOOLEAN_H

#include <juvix/object/object.h>

#define BOOL_TRUE MAKE_HEADER(UID_TRUE, 0)
#define BOOL_FALSE MAKE_HEADER(UID_FALSE, 0)

static inline word_t make_bool(bool b) { return b ? BOOL_TRUE : BOOL_FALSE; }
static inline bool is_true(word_t x) { return x == BOOL_TRUE; }

#endif
