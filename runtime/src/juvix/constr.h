#ifndef JUVIX_CONSTR_H
#define JUVIX_CONSTR_H

#include <juvix/mem/alloc.h>
#include <juvix/object.h>

#define ALLOC_CONSTR_UNBOXED(var, uid) \
    do {                               \
        var = make_header(uid, 0);     \
    } while (0)

#define ALLOC_CONSTR_BOXED(var, uid, nfields, SAVE, RESTORE)  \
    do {                                                      \
        ALLOC((word_t *)(var), (nfields) + 1, SAVE, RESTORE); \
        FIELD(var, 0) = make_header(uid, nfields);            \
    } while (0)

#define ALLOC_CONSTR_PAIR(var, SAVE, RESTORE) \
    ALLOC((word_t *)(var), 2, SAVE, RESTORE)

#define FST(var) FIELD(var, 0)
#define SND(var) FIELD(var, 1)

#define CONSTR_ARG(var, n) FIELD(var, (n) + 1)

static inline size_t get_constr_nargs(word_t x) { return get_nfields(x); }
static inline word_t *get_constr_args(word_t x) { return (word_t *)x + 1; }

// Memory pointers (see alloc.h) need to be saved before calling the following
// functions
static inline word_t alloc_constr(uint uid, size_t nfields) {
    word_t var = (word_t)alloc(nfields + 1);
    FIELD(var, 0) = make_header(uid, nfields);
    return var;
}

static inline word_t alloc_pair() { return (word_t)alloc(2); }

#endif
