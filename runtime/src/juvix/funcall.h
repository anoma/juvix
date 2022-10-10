#ifndef JUVIX_FUNCALL_H
#define JUVIX_FUNCALL_H

#include <juvix/closure.h>
#include <juvix/mem/stack.h>

#define DECL_REG_ARG(k)            \
    register word_t juvix_arg_##k; \
    word_t juvix_arg_tmp_##k
#define DECL_ARG(k)       \
    word_t juvix_arg_##k; \
    word_t juvix_arg_tmp_##k

#define SET_ARG_TMP(k, val) (juvix_arg_tmp_##k = val)
#define GET_ARG_TMP(k) juvix_arg_##k

#define SET_ARG(k, val) (juvix_arg_##k = val)
#define GET_ARG(k) juvix_arg_##k

#ifdef EXT_LABEL_AS_VALUE
#define GET_LABEL_ADDR(label) &&label
#define STORED_GOTO(ptr) goto *(ptr)
#else
#error \
    "The \"labels as values\" compiler extension is required (use GCC or clang)."
#endif

/*
    Expected macro sequence for a function call:

    STACK_ENTER(sp, n + 1);
    STACK_PUSH(sp, var1);
    ...
    STACK_PUSH(sp, varn);
    SET_ARG_TMP(0, arg0);
    ...
    SET_ARG_TMP(m, argm);
    SET_ARG(0, arg0);
    ...
    SET_ARG(m, argm);
    CALL(function, sp, unique_label);
    STACK_POP(sp, varn);
    ...
    STACK_POP(sp, var1);
    STACK_LEAVE(sp);
*/

#define CALL(fun, sp, label)               \
    STACK_PUSH(sp, GET_LABEL_ADDR(label)); \
    goto fun;                              \
    label:                                 \
    --sp;

#define TAIL_CALL(fun) goto fun

#define CALL_CLOSURE(cl, sp, label)        \
    STACK_PUSH(sp, GET_LABEL_ADDR(label)); \
    STORED_GOTO(get_closure_addr(cl));     \
    label:                                 \
    --sp;

#define TAIL_CALL_CLOSURE(cl) goto *get_closure_addr(cl)

/*
    Expected macro sequence for the dispatch loop:

    STACK_ENTER(sp, n + m + 3);
    STACK_PUSH(sp, var1);
    ...
    STACK_PUSH(sp, varn);
    STACK_PUSH(sp, argm);
    ...
    STACK_PUSH(sp, arg1);
    CALL_CLOSURES(m, unique_label);
    STACK_POP(sp, varn);
    ...
    STACK_POP(sp, var1);
    STACK_LEAVE(sp);
*/

#define CALL_CLOSURES(m, label)                   \
    do {                                          \
        SET_ARG(0, m);                            \
        juvix_ccl_return = GET_LABEL_ADDR(label); \
        goto juvix_ccl_start;                     \
    label:                                        \
        --sp;                                     \
    } while (0)

#define TAIL_CALL_CLOSURES(m)    \
    do {                         \
        SET_ARG(0, m);           \
        juvix_ccl_return = NULL; \
        goto juvix_ccl_start;    \
    } while (0)

#define DECL_CALL_CLOSURES(sp, mp)                                         \
    void *juvix_ccl_return;                                                \
    juvix_ccl_start:                                                       \
    do {                                                                   \
        size_t juvix_ccl_sargs = GET_ARG(0);                               \
    juvix_ccl_continue:                                                    \
        word_t juvix_ccl_closure;                                          \
        STACK_POP(sp, juvix_ccl_closure);                                  \
        size_t juvix_ccl_nargs = get_closure_nargs(juvix_ccl_closure);     \
        size_t juvix_ccl_nfields = get_nfields(juvix_ccl_closure);         \
        void *juvix_ccl_addr = get_closure_addr(juvix_ccl_closure);        \
        size_t juvix_ccl_largs = juvix_ccl_nfields - juvix_ccl_nargs;      \
        if (juvix_ccl_largs <= juvix_ccl_sargs) {                          \
            SET_ARG(0, juvix_ccl_addr);                                    \
            juvix_ccl_sargs -= juvix_ccl_largs;                            \
            if (juvix_ccl_sargs > 0) {                                     \
                STACK_PUSH(sp, GET_LABEL_ADDR(juvix_ccl_continue));        \
            } else if (juvix_ccl_return != NULL) {                         \
                STACK_PUSH(sp, juvix_ccl_return);                          \
            }                                                              \
            STORED_GOTO(juvix_ccl_dispatch[juvix_ccl_largs]);              \
        } else {                                                           \
            EXTEND_CLOSURE(                                                \
                GET_ARG(0), juvix_ccl_closure, juvix_ccl_sargs, mp,        \
                {                                                          \
                    STACK_SAVE(sp);                                        \
                    STACK_PUSH(sp, juvix_ccl_closure);                     \
                },                                                         \
                { STACK_POP(sp, juvix_ccl_closure); });                    \
            for (size_t juvix_idx = 0; juvix_idx < juvix_ccl_sargs;        \
                 ++juvix_idx) {                                            \
                STACK_POP(sp, GET_FIELD(GET_ARG(0),                        \
                                        juvix_idx + juvix_ccl_nargs + 3)); \
            }                                                              \
            break;                                                         \
        }                                                                  \
        if (juvix_ccl_return == NULL) {                                    \
            RETURN(sp);                                                    \
        } else {                                                           \
            STORED_GOTO(juvix_ccl_return);                                 \
        }                                                                  \
    } while (0)

#define RETURN(sp)                   \
    do {                             \
        word_t juvix_return;         \
        STACK_POP(sp, juvix_return); \
        STORED_GOTO(juvix_return);   \
    } while (0)

#endif
