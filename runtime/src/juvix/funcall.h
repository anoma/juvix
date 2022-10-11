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

// Function result is stored in juvix_result
#define DECL_RESULT word_t juvix_result

#define SET_ARG_TMP(k, val) (juvix_arg_tmp_##k = val)
#define GET_ARG_TMP(k) juvix_arg_##k

#define SET_ARG(k, val) (juvix_arg_##k = val)
#define GET_ARG(k) juvix_arg_##k

#ifdef EXT_LABELS_AS_VALUES
#define GET_LABEL_ADDR(label) &&label
#define STORED_GOTO(ptr) goto *(ptr)
typedef void *label_addr_t;
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

/*
    Expected macro sequence for a function tail-call:

    SET_ARG_TMP(0, arg0);
    ...
    SET_ARG_TMP(m, argm);
    SET_ARG(0, arg0);
    ...
    SET_ARG(m, argm);
    TAIL_CALL(function);
*/

#define TAIL_CALL(fun) goto fun

#define CALL_CLOSURE(cl, sp, label)        \
    STACK_PUSH(sp, GET_LABEL_ADDR(label)); \
    STORED_GOTO(get_closure_addr(cl));     \
    label:                                 \
    --sp;

#define TAIL_CALL_CLOSURE(cl) goto *get_closure_addr(cl)

#define RETURN(sp) \
    --sp;          \
    STORED_GOTO(*(sp + 1));

/*
    Expected macro sequence for calling the dispatch loop:

    STACK_ENTER(sp, n + m + DISPATCH_STACK_SIZE);
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
        juvix_ccl_sargs = m;                      \
        juvix_ccl_return = GET_LABEL_ADDR(label); \
        goto juvix_ccl_start;                     \
    label:                                        \
    } while (0)

/*
    Expected macro sequence for tail-calling the dispatch loop:

    STACK_ENTER(sp, m + DISPATCH_STACK_SIZE);
    STACK_PUSH(sp, argm);
    ...
    STACK_PUSH(sp, arg1);
    TAIL_CALL_CLOSURES(m);
*/

#define TAIL_CALL_CLOSURES(m)    \
    do {                         \
        juvix_ccl_sargs = m;     \
        juvix_ccl_return = NULL; \
        goto juvix_ccl_start;    \
    } while (0)

#define DECL_CALL_CLOSURES(MAX_ARGS, sp, mp)                               \
    ASSERT(MAX_ARGS <= MAX_FUNCTION_ARGS);                                 \
    label_addr_t juvix_ccl_dispatch[MAX_ARGS];                             \
    label_addr_t juvix_ccl_return;                                         \
    label_addr_t juvix_ccl_addr;                                           \
    word_t *juvix_ccl_sp;                                                  \
    size_t juvix_ccl_sargs;                                                \
    juvix_ccl_start:                                                       \
    do {                                                                   \
        word_t juvix_ccl_closure;                                          \
        STACK_POP(sp, juvix_ccl_closure);                                  \
        size_t juvix_ccl_nargs = get_closure_nargs(juvix_ccl_closure);     \
        size_t juvix_ccl_nfields = get_nfields(juvix_ccl_closure);         \
        juvix_ccl_addr = get_closure_addr(juvix_ccl_closure);              \
        size_t juvix_ccl_largs = juvix_ccl_nfields - juvix_ccl_nargs;      \
        ASSERT(juvix_ccl_largs <= MAX_ARGS);                               \
        if (juvix_ccl_largs <= juvix_ccl_sargs) {                          \
            juvix_ccl_sargs -= juvix_ccl_largs;                            \
            sp -= juvix_ccl_largs;                                         \
            ASSERT(is_same_page(sp, sp + juvix_ccl_largs));                \
            juvix_ccl_sp = sp;                                             \
            STORED_GOTO(juvix_ccl_dispatch[juvix_ccl_largs]);              \
        } else {                                                           \
            EXTEND_CLOSURE(                                                \
                juvix_result, juvix_ccl_closure, juvix_ccl_sargs, mp,      \
                {                                                          \
                    STACK_SAVE(sp);                                        \
                    STACK_PUSH(sp, juvix_ccl_closure);                     \
                },                                                         \
                { STACK_POP(sp, juvix_ccl_closure); });                    \
            for (size_t juvix_idx = 0; juvix_idx < juvix_ccl_sargs;        \
                 ++juvix_idx) {                                            \
                STACK_POP(sp, GET_FIELD(juvix_result,                      \
                                        juvix_idx + juvix_ccl_nargs + 3)); \
            }                                                              \
            if (juvix_ccl_return == NULL) {                                \
                STACK_LEAVE(sp);                                           \
                RETURN(sp);                                                \
            } else {                                                       \
                STORED_GOTO(juvix_ccl_return);                             \
            }                                                              \
        }                                                                  \
    } while (0)

#define DECL_DISPATCH(N, label) (juvix_ccl_dispatch[N] = GET_LABEL_ADDR(label))

#define DISPATCH(label)                                     \
    if (juvix_ccl_sargs == 0 && juvix_ccl_return == NULL) { \
        STACK_LEAVE(sp);                                    \
        STORED_GOTO(juvix_ccl_addr);                        \
    } else {                                                \
        STACK_PUSH(sp, juvix_ccl_return);                   \
        STACK_PUSH(sp, juvix_ccl_sargs);                    \
        STACK_PUSH(sp, GET_LABEL_ADDR(label));              \
        STORED_GOTO(juvix_ccl_addr);                        \
    label:                                                  \
        --sp;                                               \
        STACK_POP(sp, juvix_ccl_sargs);                     \
        STACK_POP(sp, juvix_ccl_return);                    \
        if (juvix_ccl_sargs > 0) {                          \
            goto juvix_ccl_start;                           \
        } else {                                            \
            ASSERT(juvix_ccl_return != NULL);               \
            STORED_GOTO(juvix_ccl_return);                  \
        }                                                   \
    }

#define DISPATCH_STACK_SIZE 3

/*
    Expected macro sequence for argument dispatch declaration:

    DECL_DISPATCH(N+1, dispatch_label_N);
    DECL_DISPATCH(N, dispatch_label_N-1);
    ...
    DECL_DISPATCH(1, dispatch_label_0);
    ...
dispatch_label_N:
    SET_ARG(N, *juvix_ccl_sp++);
dispatch_label_N-1:
    SET_ARG(N-1, *juvix_ccl_sp++);
    ...
dispatch_label_0:
    SET_ARG(0, *juvix_ccl_sp++);
    DISPATCH(unique_label);
*/

#endif
