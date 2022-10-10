#ifndef JUVIX_CONSTR_H
#define JUVIX_CONSTR_H

#include <juvix/mem/alloc.h>
#include <juvix/object.h>

#define ALLOC_CONSTR_UNBOXED(var, uid) \
    do {                               \
        var = make_header(uid, 0);     \
    } while (0)

#define ALLOC_CONSTR_BOXED(var, uid, nfields, mp, SAVE, RESTORE)  \
    do {                                                          \
        ALLOC((word_t *)(var), (nfields) + 1, mp, SAVE, RESTORE); \
        SET_FIELD(var, 0, make_header(uid, nfields));             \
    } while (0)

#define ALLOC_CONSTR_PAIR(var, mp, SAVE, RESTORE) \
    ALLOC((word_t *)(var), 2, mp, SAVE, RESTORE)

#define GET_FST(var) GET_FIELD(var, 0)
#define SET_FST(var, val) SET_FIELD(var, 0, val)

#define GET_SND(var) GET_FIELD(var, 1)
#define SET_SND(var, val) SET_FIELD(var, 1, val)

#endif
