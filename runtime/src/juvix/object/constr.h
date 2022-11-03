#ifndef JUVIX_OBJECT_CONSTR_H
#define JUVIX_OBJECT_CONSTR_H

#include <juvix/mem/alloc.h>
#include <juvix/object/object.h>

#define ALLOC_CONSTR_UNBOXED(var, uid) \
    do {                               \
        var = make_header(uid, 0);     \
    } while (0)

#define ALLOC_CONSTR_BOXED(var, uid, nfields)      \
    do {                                           \
        void *tmp;                                 \
        ALLOC(tmp, (nfields) + 1);                 \
        var = (word_t)tmp;                         \
        FIELD(var, 0) = make_header(uid, nfields); \
    } while (0)

#define ALLOC_CONSTR_BOXED_TAG(var, uid)     \
    do {                                     \
        void *tmp;                           \
        ALLOC(tmp, 2);                       \
        var = (word_t)tmp;                   \
        FIELD(var, 0) = make_header(uid, 1); \
        FIELD(var, 1) = 1;                   \
    } while (0)

#define ALLOC_CONSTR_PAIR(var) \
    do {                       \
        void *tmp;             \
        ALLOC(tmp, 2);         \
        var = (word_t)tmp;     \
    } while (0)

#define FST(var) FIELD(var, 0)
#define SND(var) FIELD(var, 1)

#define CONSTR_ARG(var, n) FIELD(var, (n) + 1)

static inline size_t get_constr_nargs(word_t x) { return get_nfields(x); }
static inline word_t *get_constr_args(word_t x) { return (word_t *)x + 1; }

// Memory pointers (see alloc.h) need to be saved before calling the following
// functions (calls alloc).
static inline word_t alloc_constr(uint uid, size_t nfields) {
    word_t var = (word_t)alloc(nfields + 1);
    FIELD(var, 0) = make_header(uid, nfields);
    return var;
}

static inline word_t alloc_pair() { return (word_t)alloc(2); }

#endif
