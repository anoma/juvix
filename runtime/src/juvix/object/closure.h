#ifndef JUVIX_OBJECT_CLOSURE_H
#define JUVIX_OBJECT_CLOSURE_H

#include <juvix/mem/alloc.h>
#include <juvix/mem/mem.h>
#include <juvix/object/object.h>

/* Closure header:
  |NARGS:8|FUID:24|
 */

#define FUNCTION_UID_MASK 0x00ffffff
#define FUNCTION_NARGS_MASK 0xff000000
#define FUNCTION_NARGS_SHIFT 24U

static inline word_t get_function_uid(word_t x) {
    return x & FUNCTION_UID_MASK;
}

static inline word_t get_function_nargs(word_t x) {
    return x >> FUNCTION_NARGS_SHIFT;
}

static inline word_t make_function_header(word_t fuid, size_t nargs) {
    return (nargs << FUNCTION_NARGS_SHIFT) | fuid;
}

// The number of arguments stored in the closure.
static inline size_t get_closure_nargs(word_t cl) {
    return get_function_nargs(FIELD(cl, 1));
}

// The number of arguments remaining for a call to the
// function.
static inline word_t get_closure_largs(word_t x) {
    return get_nfields(x) - get_closure_nargs(x) - 2;
}

static inline size_t get_closure_fuid(word_t cl) {
    return get_function_uid(FIELD(cl, 1));
}

static inline label_addr_t get_closure_addr(word_t cl) {
    return (label_addr_t)FIELD(cl, 2);
}

static inline word_t *get_closure_args(word_t cl) { return (word_t *)cl + 3; }

#define CLOSURE_ARG(var, n) FIELD(var, (n) + 3)

#define INIT_CLOSURE(var, fuid, addr, nfields, nargs)      \
    do {                                                   \
        FIELD(var, 0) = make_header(UID_CLOSURE, nfields); \
        FIELD(var, 1) = make_function_header(fuid, nargs); \
        FIELD(var, 2) = addr;                              \
    } while (0)

#define ALLOC_CLOSURE(var, fuid, addr, nfields, nargs, SAVE, RESTORE) \
    do {                                                              \
        ALLOC((word_t *)(var), (nfields) + 1, SAVE, RESTORE);         \
        INIT_CLOSURE(var, fuid, addr, nfields, nargs);                \
    } while (0)

#define COPY_EXTEND_CLOSURE(dest, src, n)                          \
    do {                                                           \
        FIELD(dest, 0) = FIELD(src, 0);                            \
        size_t nargs = get_closure_nargs(src);                     \
        word_t fuid = get_closure_fuid(src);                       \
        ASSERT(nargs + n < get_nfields(src));                      \
        FIELD(dest, 1) = make_function_header(fuid, nargs + n);    \
        FIELD(dest, 2) = FIELD(src, 2);                            \
        memcopy((word_t *)(dest) + 3, (word_t *)(src) + 3, nargs); \
    } while (0)

#define EXTEND_CLOSURE(dest, src, n, SAVE, RESTORE)                   \
    do {                                                              \
        ALLOC((word_t *)(dest), get_nfields(src) + 1, SAVE, RESTORE); \
        COPY_EXTEND_CLOSURE(dest, src, n);                            \
    } while (0)

// Memory pointers (see alloc.h) need to be saved before calling the following
// functions.
word_t alloc_closure(uint fuid, label_addr_t addr, uint nfields, uint nargs);
word_t extend_closure(word_t closure, uint n);

#endif
