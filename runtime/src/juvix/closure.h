#ifndef JUVIX_CLOSURE_H
#define JUVIX_CLOSURE_H

#include <juvix/mem/mem.h>
#include <juvix/object.h>

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

static inline size_t get_closure_nargs(word_t cl) {
    return get_function_nargs(FIELD(cl, 1));
}

static inline size_t get_closure_fuid(word_t cl) {
    return get_function_uid(FIELD(cl, 1));
}

static inline void *get_closure_addr(word_t cl) { return (void *)FIELD(cl, 2); }

static inline word_t *get_closure_args(word_t cl) { return (word_t *)cl + 3; }

#define CLOSURE_ARG(var, n) FIELD(var, (n) + 3)

#define ALLOC_CLOSURE(var, fuid, addr, nfields, nargs, SAVE, RESTORE) \
    do {                                                              \
        ALLOC(var, (nfields) + 1, SAVE, RESTORE);                     \
        FIELD(var, 0) = make_header(UID_CLOSURE, nfields);            \
        FIELD(var, 1) = make_function_header(fuid, nargs);            \
        FIELD(var, 2) = addr;                                         \
    } while (0)

#define EXTEND_CLOSURE(dest, src, n, SAVE, RESTORE)                \
    do {                                                           \
        word_t juvix_header = FIELD(src, 0);                       \
        ALLOC(dest, GET_NFIELDS(juvix_header) + 1, SAVE, RESTORE); \
        FIELD(dest, 0) = juvix_header;                             \
        size_t nargs = get_closure_nargs(src);                     \
        word_t fuid = get_closure_fuid(src);                       \
        ASSERT(nargs + n < nfields);                               \
        FIELD(dest, 1) = make_function_header(fuid, nargs + n);    \
        FIELD(dest, 2) = FIELD(src, 2);                            \
        memcopy(dest + 3, src + 3, nargs);                         \
    } while (0)

#endif
