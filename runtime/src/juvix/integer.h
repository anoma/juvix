#ifndef JUVIX_INTEGER_H
#define JUVIX_INTEGER_H

#include <juvix/mem/alloc.h>
#include <juvix/object.h>

#define INT_REF(var) (*get_dword_ptr(var))

#define ALLOC_INT(var, N, beg, end, SAVE, RESTORE)            \
    do {                                                      \
        DALLOC((dword_t *)(var), 1, beg, end, SAVE, RESTORE); \
        *(dword_t *)(var) = N;                                \
        var = make_dword_ptr(var);                            \
    } while (0)

#define INT_OP(OP, vret, var1, var2, beg, end, SAVE, RESTORE) \
    do {                                                      \
        DALLOC((dword_t *)(var), 1, beg, end, SAVE, RESTORE); \
        *(dword_t *)(vret) = INT_REF(var1) OP INT_REF(var2);  \
        vret = make_dword_ptr(vret);                          \
    } while (0)

#define INT_ADD(vret, var1, var2, beg, end, SAVE, RESTORE) \
    INT_OP(+, vret, var1, var2, beg, end, SAVE, RESTORE)

#define INT_SUB(vret, var1, var2, beg, end, SAVE, RESTORE) \
    INT_OP(-, vret, var1, var2, beg, end, SAVE, RESTORE)

#define INT_MUL(vret, var1, var2, beg, end, SAVE, RESTORE) \
    INT_OP(*, vret, var1, var2, beg, end, SAVE, RESTORE)

#define INT_DIV(vret, var1, var2, beg, end, SAVE, RESTORE) \
    INT_OP(/, vret, var1, var2, beg, end, SAVE, RESTORE)

#define INT_MOD(vret, var1, var2, beg, end, SAVE, RESTORE) \
    INT_OP(%, vret, var1, var2, beg, end, SAVE, RESTORE)

#define INT_ADD_NOALLOC(vret, var1, var2)             \
    do {                                              \
        INT_REF(vret) = INT_REF(var1) + INT_REF(var2) \
    } while (0)

#define INT_SUB_NOALLOC(vret, var1, var2)             \
    do {                                              \
        INT_REF(vret) = INT_REF(var1) - INT_REF(var2) \
    } while (0)

#define INT_MUL_NOALLOC(vret, var1, var2)             \
    do {                                              \
        INT_REF(vret) = INT_REF(var1) * INT_REF(var2) \
    } while (0)

#define INT_DIV_NOALLOC(vret, var1, var2)             \
    do {                                              \
        INT_REF(vret) = INT_REF(var1) / INT_REF(var2) \
    } while (0)

#define INT_MOD_NOALLOC(vret, var1, var2)             \
    do {                                              \
        INT_REF(vret) = INT_REF(var1) % INT_REF(var2) \
    } while (0)

#endif
