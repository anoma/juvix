#ifndef JUVIX_FUNCALL_APPLY_H
#define JUVIX_FUNCALL_APPLY_H

#include <juvix/funcall/funcall.h>

#define DECL_APPLY_CLOSURE
#define juvix_apply_closure juvix_ccl_closure

#define GOTO_APPLY_CLOSURE                                  \
    STACKTRACE_PUSH(get_closure_fuid(juvix_apply_closure)); \
    STORED_GOTO(get_closure_addr(juvix_apply_closure));

#define TAIL_GOTO_APPLY_CLOSURE                                \
    STACKTRACE_REPLACE(get_closure_fuid(juvix_apply_closure)); \
    STACK_LEAVE;                                               \
    STORED_GOTO(get_closure_addr(juvix_apply_closure))

#define RETURN_APPLY    \
    STACKTRACE_PUSH(0); \
    RETURN_NS;

#define RETURN_TAIL_APPLY RETURN

#define GEN_APPLY_1(GOTO_CLOSURE, RETURN_CLOSURE, label)            \
    do {                                                            \
        size_t juvix_apply_largs;                                   \
    label:                                                          \
        juvix_apply_largs = get_closure_largs(juvix_apply_closure); \
        if (juvix_apply_largs == 1) {                               \
            GOTO_CLOSURE;                                           \
        } else {                                                    \
            ASSERT(juvix_apply_largs > 1);                          \
            size_t n = get_closure_nargs(juvix_apply_closure);      \
            PREALLOC(                                               \
                n + CLOSURE_SKIP + 1 + 1,                           \
                {                                                   \
                    STACK_PUSH(juvix_apply_closure);                \
                    STACK_PUSH(CARG(n));                            \
                },                                                  \
                {                                                   \
                    STACK_POP(CARG(n));                             \
                    STACK_POP(juvix_apply_closure);                 \
                });                                                 \
            EXTEND_CLOSURE(juvix_result, juvix_apply_closure, 1, {  \
                CLOSURE_ARG(juvix_result, juvix_closure_nargs) =    \
                    CARG(juvix_closure_nargs);                      \
            });                                                     \
            RETURN_CLOSURE;                                         \
        }                                                           \
    } while (0)

#define GEN_APPLY_2(GOTO_CLOSURE, RETURN_CLOSURE, label)             \
    do {                                                             \
        size_t juvix_apply_largs;                                    \
    label:                                                           \
        juvix_apply_largs = get_closure_largs(juvix_apply_closure);  \
        if (juvix_apply_largs == 1) {                                \
            size_t n = get_closure_nargs(juvix_apply_closure);       \
            STACK_PUSH(CARG(n + 1));                                 \
            CALL_CLOSURE(juvix_apply_closure, label##_return_1);     \
            juvix_apply_closure = juvix_result;                      \
            ASSIGN_CARGS(juvix_apply_closure,                        \
                         { STACK_POP(CARG(juvix_closure_nargs)); }); \
            goto juvix_apply_1;                                      \
        } else if (juvix_apply_largs == 2) {                         \
            GOTO_CLOSURE;                                            \
        } else {                                                     \
            ASSERT(juvix_apply_largs > 2);                           \
            size_t n = get_closure_nargs(juvix_apply_closure);       \
            PREALLOC(                                                \
                n + CLOSURE_SKIP + 1 + 2,                            \
                {                                                    \
                    STACK_PUSH(juvix_apply_closure);                 \
                    STACK_PUSH(CARG(n));                             \
                    STACK_PUSH(CARG(n + 1));                         \
                },                                                   \
                {                                                    \
                    STACK_POP(CARG(n + 1));                          \
                    STACK_POP(CARG(n));                              \
                    STACK_POP(juvix_apply_closure);                  \
                });                                                  \
            EXTEND_CLOSURE(juvix_result, juvix_apply_closure, 2, {   \
                CLOSURE_ARG(juvix_result, juvix_closure_nargs) =     \
                    CARG(juvix_closure_nargs);                       \
                CLOSURE_ARG(juvix_result, juvix_closure_nargs + 1) = \
                    CARG(juvix_closure_nargs + 1);                   \
            });                                                      \
            RETURN_CLOSURE;                                          \
        }                                                            \
    } while (0)

#define GEN_APPLY_3(GOTO_CLOSURE, RETURN_CLOSURE, label)                 \
    do {                                                                 \
    label:                                                               \
        switch (get_closure_largs(juvix_apply_closure)) {                \
            case 1: {                                                    \
                size_t n = get_closure_nargs(juvix_apply_closure);       \
                STACK_PUSH(CARG(n + 2));                                 \
                STACK_PUSH(CARG(n + 1));                                 \
                CALL_CLOSURE(juvix_apply_closure, label##_return_1);     \
                juvix_apply_closure = juvix_result;                      \
                ASSIGN_CARGS(juvix_apply_closure, {                      \
                    STACK_POP(CARG(juvix_closure_nargs));                \
                    STACK_POP(CARG(juvix_closure_nargs + 1));            \
                });                                                      \
                goto juvix_apply_2;                                      \
            }                                                            \
            case 2: {                                                    \
                size_t n = get_closure_nargs(juvix_apply_closure);       \
                STACK_PUSH(CARG(n + 2));                                 \
                CALL_CLOSURE(juvix_apply_closure, label##_return_2);     \
                juvix_apply_closure = juvix_result;                      \
                ASSIGN_CARGS(juvix_apply_closure,                        \
                             { STACK_POP(CARG(juvix_closure_nargs)); }); \
                goto juvix_apply_1;                                      \
            }                                                            \
            case 3:                                                      \
                GOTO_CLOSURE;                                            \
            default: {                                                   \
                size_t n = get_closure_nargs(juvix_apply_closure);       \
                PREALLOC(                                                \
                    n + CLOSURE_SKIP + 1 + 3,                            \
                    {                                                    \
                        STACK_PUSH(juvix_apply_closure);                 \
                        STACK_PUSH(CARG(n));                             \
                        STACK_PUSH(CARG(n + 1));                         \
                        STACK_PUSH(CARG(n + 2));                         \
                    },                                                   \
                    {                                                    \
                        STACK_POP(CARG(n + 2));                          \
                        STACK_POP(CARG(n + 1));                          \
                        STACK_POP(CARG(n));                              \
                        STACK_POP(juvix_apply_closure);                  \
                    });                                                  \
                EXTEND_CLOSURE(juvix_result, juvix_apply_closure, 3, {   \
                    CLOSURE_ARG(juvix_result, juvix_closure_nargs) =     \
                        CARG(juvix_closure_nargs);                       \
                    CLOSURE_ARG(juvix_result, juvix_closure_nargs + 1) = \
                        CARG(juvix_closure_nargs + 1);                   \
                    CLOSURE_ARG(juvix_result, juvix_closure_nargs + 2) = \
                        CARG(juvix_closure_nargs + 2);                   \
                });                                                      \
                RETURN_CLOSURE;                                          \
            }                                                            \
        }                                                                \
    } while (0)

#define DECL_APPLY_1 \
    GEN_APPLY_1(GOTO_APPLY_CLOSURE, RETURN_APPLY, juvix_apply_1)

#define DECL_TAIL_APPLY_1 \
    GEN_APPLY_1(TAIL_GOTO_APPLY_CLOSURE, RETURN_TAIL_APPLY, juvix_tail_apply_1)

#define DECL_APPLY_2 \
    GEN_APPLY_2(GOTO_APPLY_CLOSURE, RETURN_APPLY, juvix_apply_2)

#define DECL_TAIL_APPLY_2 \
    GEN_APPLY_2(TAIL_GOTO_APPLY_CLOSURE, RETURN_TAIL_APPLY, juvix_tail_apply_2)

#define DECL_APPLY_3 \
    GEN_APPLY_3(GOTO_APPLY_CLOSURE, RETURN_APPLY, juvix_apply_3)

#define DECL_TAIL_APPLY_3 \
    GEN_APPLY_3(TAIL_GOTO_APPLY_CLOSURE, RETURN_TAIL_APPLY, juvix_tail_apply_3)

#define APPLY_1(cl, label)              \
    juvix_apply_closure = cl;           \
    STACK_PUSH_ADDR(LABEL_ADDR(label)); \
    goto juvix_apply_1;                 \
    label:                              \
    STACK_POPT;                         \
    STACKTRACE_POP;

#define TAIL_APPLY_1(cl)      \
    juvix_apply_closure = cl; \
    goto juvix_tail_apply_1;

#define APPLY_2(cl, label)              \
    juvix_apply_closure = cl;           \
    STACK_PUSH_ADDR(LABEL_ADDR(label)); \
    goto juvix_apply_2;                 \
    label:                              \
    STACK_POPT;                         \
    STACKTRACE_POP;

#define TAIL_APPLY_2(cl)      \
    juvix_apply_closure = cl; \
    goto juvix_tail_apply_2;

#define APPLY_3(cl, label)              \
    juvix_apply_closure = cl;           \
    STACK_PUSH_ADDR(LABEL_ADDR(label)); \
    goto juvix_apply_3;                 \
    label:                              \
    STACK_POPT;                         \
    STACKTRACE_POP;

#define TAIL_APPLY_3(cl)      \
    juvix_apply_closure = cl; \
    goto juvix_tail_apply_3;

#define APPLY(cl, N, label)                                            \
    do {                                                               \
        if (get_closure_largs(cl) == N) {                              \
            CALL_CLOSURE(cl, label##_apply_return_1);                  \
        } else {                                                       \
            for (int juvix_idx = N - 1; juvix_idx >= 0; --juvix_idx) { \
                STACK_PUSH(CARG(juvix_idx));                           \
            }                                                          \
            CALL_CLOSURES(cl, N, label##_apply_return_2);              \
        }                                                              \
    } while (0)

#define TAIL_APPLY(cl, N)                                              \
    do {                                                               \
        if (get_closure_largs(cl) == N) {                              \
            TAIL_CALL_CLOSURE(cl);                                     \
        } else {                                                       \
            for (int juvix_idx = N - 1; juvix_idx >= 0; --juvix_idx) { \
                STACK_PUSH(CARG(juvix_idx));                           \
            }                                                          \
            TAIL_CALL_CLOSURES(cl, N);                                 \
        }                                                              \
    } while (0)

#endif  // JUVIX_FUNCALL_APPLY_H
