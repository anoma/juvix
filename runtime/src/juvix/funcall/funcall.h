#ifndef JUVIX_FUNCALL_FUNCALL_H
#define JUVIX_FUNCALL_FUNCALL_H

#include <juvix/funcall/stacktrace.h>
#include <juvix/mem/stack.h>
#include <juvix/object/closure.h>

#define DECL_REG_ARG(k) register word_t juvix_arg_##k
#define DECL_ARG(k) word_t juvix_arg_##k

// Function result is stored in juvix_result
#define DECL_RESULT word_t juvix_result
#define DECL_CLOSURE UNUSED word_t juvix_closure

#define ARG(k) juvix_arg_##k

// TODO: The labels on the stack will need to have their least significant bit
// set for the GC.
#define STACK_PUSH_ADDR(addr) (STACK_PUSH(addr))
#define STACK_POP_ADDR(var)    \
    do {                       \
        var = STACK_TOP_ADDR;  \
        --juvix_stack_pointer; \
    } while (0)
#define STACK_TOP_ADDR (((label_addr_t)(STACK_TOP)))

/*
    Macro sequence for a direct function call:

    STACK_ENTER(n + 1);
    STACK_PUSH(var1);
    ...
    STACK_PUSH(varn);
    ARG(0) = arg0;
    ...
    ARG(m) = argm;
    CALL(fuid, function, unique_label);
    STACK_POP(varn);
    ...
    STACK_POP(var1);
    STACK_LEAVE;

    arg0, ..., argm cannot contain references to ARG(k) for any k
*/

#define CALL(fuid, fun, label)          \
    STACKTRACE_PUSH(fuid);              \
    STACK_PUSH_ADDR(LABEL_ADDR(label)); \
    goto fun;                           \
    label:                              \
    --juvix_stack_pointer;              \
    STACKTRACE_POP;

/*
    Macro sequence for a direct function tail-call:

    ARG(0) = arg0;
    ...
    ARG(m) = argm;
    TAIL_CALL(fuid, function);

    arg0, ..., argm cannot contain references to ARG(k) for any k
*/

#define TAIL_CALL(fuid, fun)  \
    STACKTRACE_REPLACE(fuid); \
    goto fun

/*
    Macro sequence for an indirect function call:

    STACK_ENTER(n + 1);
    STACK_PUSH(var1);
    ...
    STACK_PUSH(varn);
    CLOSURE_ARG(cl, get_closure_nargs(cl)) = arg0;
    ...
    CLOSURE_ARG(cl, get_closure_nargs(cl) + m) = argm;
    CALL_CLOSURE(cl, unique_label);
    STACK_POP(varn);
    ...
    STACK_POP(var1);
    STACK_LEAVE;

    arg0, ..., argm cannot contain references to ARG(k) for any k
*/

#define CALL_CLOSURE(cl, label)            \
    STACKTRACE_PUSH(get_closure_fuid(cl)); \
    STACK_PUSH_ADDR(LABEL_ADDR(label));    \
    juvix_closure = cl;                    \
    STORED_GOTO(get_closure_addr(cl));     \
    label:                                 \
    --juvix_stack_pointer;                 \
    STACKTRACE_POP;

#define TAIL_CALL_CLOSURE(cl)                 \
    STACKTRACE_REPLACE(get_closure_fuid(cl)); \
    goto *get_closure_addr(cl)

#define RETURN STORED_GOTO(STACK_TOP_ADDR);

/*
    Macro sequence for calling the dispatch loop:

    STACK_ENTER(n + m + DISPATCH_STACK_SIZE);
    STACK_PUSH(var1);
    ...
    STACK_PUSH(varn);
    STACK_PUSH(argm);
    ...
    STACK_PUSH(arg1);
    CALL_CLOSURES(m, unique_label);
    STACK_POP(varn);
    ...
    STACK_POP(var1);
    STACK_LEAVE;
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

    STACK_ENTER(m);
    STACK_PUSH(argm);
    ...
    STACK_PUSH(arg1);
    TAIL_CALL_CLOSURES(m);
*/

#define TAIL_CALL_CLOSURES(m)    \
    do {                         \
        juvix_ccl_sargs = m;     \
        juvix_ccl_return = NULL; \
        goto juvix_ccl_start;    \
    } while (0)

#define DECL_CALL_CLOSURES(MAX_ARGS)                                          \
    ASSERT(MAX_ARGS <= MAX_FUNCTION_ARGS);                                    \
    label_addr_t juvix_ccl_dispatch[MAX_ARGS];                                \
    label_addr_t juvix_ccl_return;                                            \
    word_t *juvix_ccl_sp;                                                     \
    size_t juvix_ccl_sargs;                                                   \
    word_t juvix_ccl_closure;                                                 \
    juvix_ccl_start:                                                          \
    do {                                                                      \
        STACK_POP(juvix_ccl_closure);                                         \
        size_t juvix_ccl_largs = get_closure_largs(juvix_ccl_closure);        \
        ASSERT(juvix_ccl_largs <= MAX_ARGS);                                  \
        if (juvix_ccl_largs <= juvix_ccl_sargs) {                             \
            juvix_ccl_sargs -= juvix_ccl_largs;                               \
            juvix_stack_pointer -= juvix_ccl_largs;                           \
            ASSERT(is_same_page(juvix_stack_pointer,                          \
                                juvix_stack_pointer + juvix_ccl_largs));      \
            juvix_ccl_sp = juvix_stack_pointer;                               \
            STORED_GOTO(juvix_ccl_dispatch[juvix_ccl_largs]);                 \
        } else {                                                              \
            EXTEND_CLOSURE(juvix_result, juvix_ccl_closure, juvix_ccl_sargs); \
            for (size_t juvix_idx = 0; juvix_idx < juvix_ccl_sargs;           \
                 ++juvix_idx) {                                               \
                STACK_POP(CLOSURE_ARG(                                        \
                    juvix_result,                                             \
                    get_closure_nargs(juvix_ccl_closure) + juvix_idx));       \
            }                                                                 \
            if (juvix_ccl_return == NULL) {                                   \
                STACK_LEAVE;                                                  \
                RETURN;                                                       \
            } else {                                                          \
                STORED_GOTO(juvix_ccl_return);                                \
            }                                                                 \
        }                                                                     \
    } while (0)

#define INIT_DISPATCH(N, label) (juvix_ccl_dispatch[N] = LABEL_ADDR(label))

#define DISPATCH(label)                                          \
    if (juvix_ccl_sargs == 0 && juvix_ccl_return == NULL) {      \
        STACKTRACE_REPLACE(get_closure_fuid(juvix_ccl_closure)); \
        STACK_LEAVE;                                             \
        STORED_GOTO(get_closure_addr(juvix_ccl_closure));        \
    } else {                                                     \
        STACKTRACE_PUSH(get_closure_fuid(juvix_ccl_closure));    \
        STACK_PUSH_ADDR(juvix_ccl_return);                       \
        STACK_PUSH(juvix_ccl_sargs);                             \
        STACK_PUSH_ADDR(LABEL_ADDR(label));                      \
        STORED_GOTO(get_closure_addr(juvix_ccl_closure));        \
    label:                                                       \
        STACKTRACE_POP;                                          \
        --juvix_stack_pointer;                                   \
        STACK_POP(juvix_ccl_sargs);                              \
        STACK_POP_ADDR(juvix_ccl_return);                        \
        if (juvix_ccl_sargs > 0) {                               \
            goto juvix_ccl_start;                                \
        } else {                                                 \
            ASSERT(juvix_ccl_return != NULL);                    \
            STORED_GOTO(juvix_ccl_return);                       \
        }                                                        \
    }

#define DISPATCH_STACK_SIZE 3

/*

    Macro sequence for argument dispatch initialization (N = maximum number of
    function arguments):

    INIT_DISPATCH(N-1, dispatch_label_N-1);
    INIT_DISPATCH(N-2, dispatch_label_N-2);
    ...
    INIT_DISPATCH(0, dispatch_label_0);

    Macro sequence for argument dispatch declaration (N = maximum number of
    function arguments):

dispatch_label_N-1:
    ARG(N-1) = *juvix_ccl_sp++;
dispatch_label_N-2:
    ARG(N-2) = *juvix_ccl_sp++;
    ...
dispatch_label_0:
    ARG(0) = *juvix_ccl_sp;
    DISPATCH(unique_label);
*/

#endif
