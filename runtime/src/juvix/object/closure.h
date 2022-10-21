#ifndef JUVIX_OBJECT_CLOSURE_H
#define JUVIX_OBJECT_CLOSURE_H

#include <juvix/mem/alloc.h>
#include <juvix/mem/mem.h>
#include <juvix/object/object.h>

#ifdef DEBUG
#define CLOSURE_SKIP 2
#else
#define CLOSURE_SKIP 1
#endif

// The number of arguments stored in the closure.
static inline size_t get_closure_nargs(word_t cl) {
    return get_nfields(cl) - CLOSURE_SKIP;
}

// The number of arguments remaining for a call to the
// function.
static inline word_t get_closure_largs(word_t x) { return get_reserved(x); }

#ifdef DEBUG
static inline size_t get_closure_fuid(word_t cl) { return FIELD(cl, 1); }
#endif

static inline label_addr_t get_closure_addr(word_t cl) {
    return (label_addr_t)FIELD(cl, CLOSURE_SKIP);
}

static inline word_t *get_closure_args(word_t cl) {
    return (word_t *)cl + CLOSURE_SKIP + 1;
}

#define CLOSURE_ARG(var, n) FIELD(var, (n) + CLOSURE_SKIP + 1)

#define INIT_CLOSURE0(var, addr, nargs, largs)                              \
    FIELD(var, 0) = make_special_header(SUID_CLOSURE, nargs + CLOSURE_SKIP, \
                                        CLOSURE_SKIP, largs);

#ifdef DEBUG
#define INIT_CLOSURE(var, fuid, addr, nargs, largs) \
    do {                                            \
        INIT_CLOSURE0(var, addr, nargs, largs);     \
        FIELD(var, 1) = fuid;                       \
        FIELD(var, 2) = (word_t)addr;               \
    } while (0)
#else
#define INIT_CLOSURE(var, fuid, addr, nargs, largs) \
    do {                                            \
        INIT_CLOSURE0(var, addr, nargs, largs);     \
        FIELD(var, 1) = (word_t)addr;               \
    } while (0)
#endif

#define ALLOC_CLOSURE(var, fuid, addr, nargs, largs) \
    do {                                             \
        word_t *tmp;                                 \
        ALLOC(tmp, (nargs) + CLOSURE_SKIP + 1);      \
        var = (word_t)tmp;                           \
        INIT_CLOSURE(var, fuid, addr, nargs, largs); \
    } while (0)

#define COPY_EXTEND_CLOSURE(dest, src, n, nfields)                            \
    do {                                                                      \
        ASSERT(n < get_closure_largs(src));                                   \
        FIELD(dest, 0) = make_special_header(                                 \
            SUID_CLOSURE, nfields, CLOSURE_SKIP, get_closure_largs(src) - n); \
        memcopy((word_t *)(dest) + 1, (word_t *)(src) + 1, nfields - n);      \
    } while (0)

#define EXTEND_CLOSURE(dest, src, n)                \
    do {                                            \
        void *tmp;                                  \
        size_t nfields = get_nfields(src) + n;      \
        ALLOC(tmp, nfields + 1);                    \
        dest = (word_t)tmp;                         \
        COPY_EXTEND_CLOSURE(dest, src, n, nfields); \
    } while (0)

// Memory pointers (see alloc.h) need to be saved before calling the following
// functions (they call alloc).
word_t alloc_closure(uint fuid, label_addr_t addr, uint nargs, uint largs);
word_t extend_closure(word_t closure, uint n);

#endif
