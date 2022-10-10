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

#define GET_FUNCTION_UID(x) ((x)&FUNCTION_UID_MASK)
#define GET_FUNCTION_NARGS(x) ((x) >> FUNCTION_NARGS_SHIFT)

static inline word_t make_function_header(word_t fuid, size_t nargs) {
    return (nargs << FUNCTION_NARGS_SHIFT) | fuid;
}

static inline size_t get_closure_nargs(word_t cl) {
    return GET_FUNCTION_NARGS(GET_FIELD(cl, 1));
}

static inline size_t get_closure_fuid(word_t cl) {
    return GET_FUNCTION_UID(GET_FIELD(cl, 1));
}

static inline void *get_closure_addr(word_t cl) {
    return (void *)GET_FIELD(cl, 2);
}

#define GET_CLOSURE_ARG(var, n) GET_FIELD(var, (n) + 3)
#define SET_CLOSURE_ARG(var, n, val) SET_FIELD(var, (n) + 3, val)

#define ALLOC_CLOSURE(var, fuid, addr, nfields, nargs, mp, SAVE, RESTORE) \
    do {                                                                  \
        ALLOC(var, (nfields) + 3, mp, SAVE, RESTORE);                     \
        SET_FIELD(var, 0, make_header(UID_CLOSURE, nfields));             \
        SET_FIELD(var, 1, make_function_header(fuid, nargs));             \
        SET_FIELD(var, 2, addr);                                          \
    } while (0)

#define EXTEND_CLOSURE(dest, src, n, mp, SAVE, RESTORE)                \
    do {                                                               \
        word_t juvix_header = GET_FIELD(src, 0);                       \
        ALLOC(dest, GET_NFIELDS(juvix_header) + 3, mp, SAVE, RESTORE); \
        SET_FIELD(dest, 0, juvix_header);                              \
        size_t nargs = GET_FUNCTION_NARGS(GET_FIELD(src, 1));          \
        word_t fuid = GET_FUNCTION_UID(GET_FIELD(var, n));             \
        ASSERT(nargs + n < nfields);                                   \
        SET_FIELD(dest, 1, make_function_header(fuid, nargs + n));     \
        SET_FIELD(dest, 2, GET_FIELD(src, 2));                         \
        MEMCOPY(dest + 3, src + 3, nargs);                             \
    } while (0)

#endif
