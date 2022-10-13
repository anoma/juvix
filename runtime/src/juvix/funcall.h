#ifndef JUVIX_FUNCALL_H
#define JUVIX_FUNCALL_H

#include <juvix/closure.h>
#include <juvix/mem/stack.h>
#include <juvix/stacktrace.h>

#define DECL_REG_ARG(k) register word_t juvix_arg_##k
#define DECL_ARG(k) word_t juvix_arg_##k

// Function result is stored in juvix_result
#define DECL_RESULT word_t juvix_result

#define ARG(k) juvix_arg_##k

/*
    Macro sequence for a function call:

    STACK_ENTER(sp, n + 1);
    STACK_PUSH(sp, var1);
    ...
    STACK_PUSH(sp, varn);
    ARG(0) = arg0;
    ...
    ARG(m) = argm;
    CALL(fuid, function, sp, unique_label);
    STACK_POP(sp, varn);
    ...
    STACK_POP(sp, var1);
    STACK_LEAVE(sp);

    arg0, ..., argm cannot contain references to ARG(k) for any k
*/

#define CALL(fuid, fun, sp, label)     \
    STACKTRACE_PUSH(fuid);             \
    STACK_PUSH(sp, LABEL_ADDR(label)); \
    goto fun;                          \
    label:                             \
    --sp;                              \
    STACKTRACE_POP;

/*
    Macro sequence for a function tail-call:

    ARG(0) = arg0;
    ...
    ARG(m) = argm;
    TAIL_CALL(fuid, function);

    arg0, ..., argm cannot contain references to ARG(k) for any k
*/

#define TAIL_CALL(fuid, fun)  \
    STACKTRACE_REPLACE(fuid); \
    goto fun

#define CALL_CLOSURE(cl, sp, label)        \
    STACKTRACE_PUSH(get_closure_fuid(cl)); \
    STACK_PUSH(sp, LABEL_ADDR(label));     \
    STORED_GOTO(get_closure_addr(cl));     \
    label:                                 \
    --sp;                                  \
    STACKTRACE_POP;

#define TAIL_CALL_CLOSURE(cl)                 \
    STACKTRACE_REPLACE(get_closure_fuid(cl)); \
    goto *get_closure_addr(cl)

#define RETURN(sp) STORED_GOTO(*sp);

/*
    Macro sequence for calling the dispatch loop:

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

#define CALL_CLOSURES(m, label)               \
    do {                                      \
        juvix_ccl_sargs = m;                  \
        juvix_ccl_return = LABEL_ADDR(label); \
        goto juvix_ccl_start;                 \
    label:                                    \
    } while (0)

/*
    Macro sequence for tail-calling the dispatch loop:

    STACK_ENTER(sp, m);
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

#define DECL_CALL_CLOSURES(MAX_ARGS, sp)                                 \
    ASSERT(MAX_ARGS <= MAX_FUNCTION_ARGS);                               \
    label_addr_t juvix_ccl_dispatch[MAX_ARGS];                           \
    label_addr_t juvix_ccl_return;                                       \
    label_addr_t juvix_ccl_addr;                                         \
    word_t *juvix_ccl_sp;                                                \
    size_t juvix_ccl_sargs;                                              \
    word_t juvix_ccl_closure;                                            \
    juvix_ccl_start:                                                     \
    do {                                                                 \
        STACK_POP(sp, juvix_ccl_closure);                                \
        size_t juvix_ccl_largs = get_closure_largs(juvix_ccl_closure);   \
        ASSERT(juvix_ccl_largs <= MAX_ARGS);                             \
        if (juvix_ccl_largs <= juvix_ccl_sargs) {                        \
            juvix_ccl_sargs -= juvix_ccl_largs;                          \
            sp -= juvix_ccl_largs;                                       \
            ASSERT(is_same_page(sp, sp + juvix_ccl_largs));              \
            juvix_ccl_sp = sp;                                           \
            STORED_GOTO(juvix_ccl_dispatch[juvix_ccl_largs]);            \
        } else {                                                         \
            EXTEND_CLOSURE(                                              \
                juvix_result, juvix_ccl_closure, juvix_ccl_sargs,        \
                {                                                        \
                    STACK_SAVE(sp);                                      \
                    STACK_PUSH(sp, juvix_ccl_closure);                   \
                },                                                       \
                { STACK_POP(sp, juvix_ccl_closure); });                  \
            for (size_t juvix_idx = 0; juvix_idx < juvix_ccl_sargs;      \
                 ++juvix_idx) {                                          \
                STACK_POP(sp, CLOSURE_ARG(juvix_result,                  \
                                          juvix_ccl_nargs + juvix_idx)); \
            }                                                            \
            if (juvix_ccl_return == NULL) {                              \
                STACK_LEAVE(sp);                                         \
                RETURN(sp);                                              \
            } else {                                                     \
                STORED_GOTO(juvix_ccl_return);                           \
            }                                                            \
        }                                                                \
    } while (0)

#define DECL_DISPATCH(N, label) (juvix_ccl_dispatch[N] = LABEL_ADDR(label))

#define DISPATCH(label)                                          \
    if (juvix_ccl_sargs == 0 && juvix_ccl_return == NULL) {      \
        STACKTRACE_REPLACE(get_closure_fuid(juvix_ccl_closure)); \
        STACK_LEAVE(sp);                                         \
        STORED_GOTO(juvix_ccl_addr);                             \
    } else {                                                     \
        STACKTRACE_PUSH(get_closure_fuid(juvix_ccl_closure));    \
        STACK_PUSH(sp, juvix_ccl_return);                        \
        STACK_PUSH(sp, juvix_ccl_sargs);                         \
        STACK_PUSH(sp, LABEL_ADDR(label));                       \
        STORED_GOTO(juvix_ccl_addr);                             \
    label:                                                       \
        STACKTRACE_POP;                                          \
        --sp;                                                    \
        STACK_POP(sp, juvix_ccl_sargs);                          \
        STACK_POP(sp, juvix_ccl_return);                         \
        if (juvix_ccl_sargs > 0) {                               \
            goto juvix_ccl_start;                                \
        } else {                                                 \
            ASSERT(juvix_ccl_return != NULL);                    \
            STORED_GOTO(juvix_ccl_return);                       \
        }                                                        \
    }

#define DISPATCH_STACK_SIZE 3

/*
    Macro sequence for argument dispatch declaration:

    DECL_DISPATCH(N+1, dispatch_label_N);
    DECL_DISPATCH(N, dispatch_label_N-1);
    ...
    DECL_DISPATCH(1, dispatch_label_0);
    ...
dispatch_label_N:
    ARG(N) = *juvix_ccl_sp++;
dispatch_label_N-1:
    ARG(N-1) = *juvix_ccl_sp++;
    ...
dispatch_label_0:
    ARG(0) = *juvix_ccl_sp++;
    DISPATCH(unique_label);
*/

#endif
