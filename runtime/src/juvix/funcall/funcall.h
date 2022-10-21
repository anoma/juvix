#ifndef JUVIX_FUNCALL_FUNCALL_H
#define JUVIX_FUNCALL_FUNCALL_H

#include <juvix/funcall/stacktrace.h>
#include <juvix/mem/stack.h>
#include <juvix/object/closure.h>

#define DECL_REG_ARG(k) register word_t juvix_arg_##k
#define DECL_ARG(k) word_t juvix_arg_##k

// Function result is stored in juvix_result
#define DECL_RESULT word_t juvix_result
#define DECL_CARGS(MAX_ARGS) word_t juvix_carg[MAX_ARGS]

#define ARG(k) juvix_arg_##k
#define CARG(k) juvix_carg[k]

// TODO: The labels on the stack will need to have their least significant bit
// set for the GC.
#define STACK_PUSH_ADDR(addr) (STACK_PUSH(addr))
#define STACK_POP_ADDR(var)   \
    do {                      \
        var = STACK_TOP_ADDR; \
        STACK_POPT;           \
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
    STACK_POPT;                         \
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

#define ASSIGN_CARGS(cl, CODE)                                               \
    do {                                                                     \
        size_t juvix_closure_nargs = get_closure_nargs(cl);                  \
        for (size_t juvix_closure_idx = 0;                                   \
             juvix_closure_idx < juvix_closure_nargs; ++juvix_closure_idx) { \
            CARG(juvix_closure_idx) = CLOSURE_ARG(cl, juvix_closure_idx);    \
        }                                                                    \
        CODE;                                                                \
    } while (0)

/*
    Macro sequence for an indirect function call:

    STACK_ENTER(n + 1);
    STACK_PUSH(var1);
    ...
    STACK_PUSH(varn);
    ASSIGN_CARGS(cl, {
      CARG(juvix_closure_nargs) = arg0;
      ...
      CARG(juvix_closure_nargs + m) = argm;
    });
    CALL_CLOSURE(closure, unique_label);
    STACK_POP(varn);
    ...
    STACK_POP(var1);
    STACK_LEAVE;

    arg0, ..., argm cannot contain references to CARG(k) for any k
*/

#define CALL_CLOSURE(cl, label)            \
    STACKTRACE_PUSH(get_closure_fuid(cl)); \
    STACK_PUSH_ADDR(LABEL_ADDR(label));    \
    STORED_GOTO(get_closure_addr(cl));     \
    label:                                 \
    STACK_POPT;                            \
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
    CALL_CLOSURES(closure, m, unique_label);
    STACK_POP(varn);
    ...
    STACK_POP(var1);
    STACK_LEAVE;
*/

#define CALL_CLOSURES(cl, m, label)       \
    juvix_ccl_sargs = m;                  \
    juvix_ccl_return = LABEL_ADDR(label); \
    juvix_ccl_closure = cl;               \
    goto juvix_ccl_start;                 \
    label:

/*
    Macro sequence for tail-calling the dispatch loop:

    STACK_ENTER(m + DISPATCH_STACK_SIZE);
    STACK_PUSH(argm);
    ...
    STACK_PUSH(arg1);
    TAIL_CALL_CLOSURES(closure, m);
*/

#define TAIL_CALL_CLOSURES(cl, m) \
    juvix_ccl_sargs = m;          \
    juvix_ccl_return = NULL;      \
    juvix_ccl_closure = cl;       \
    goto juvix_ccl_start;

#define DECL_CALL_CLOSURES                                                    \
    label_addr_t juvix_ccl_return;                                            \
    size_t juvix_ccl_sargs;                                                   \
    word_t juvix_ccl_closure;                                                 \
    juvix_ccl_start:                                                          \
    do {                                                                      \
        size_t juvix_ccl_largs = get_closure_largs(juvix_ccl_closure);        \
        size_t juvix_ccl_nargs = get_closure_nargs(juvix_ccl_closure);        \
        if (juvix_ccl_largs <= juvix_ccl_sargs) {                             \
            juvix_ccl_sargs -= juvix_ccl_largs;                               \
            for (size_t juvix_idx = 0; juvix_idx < juvix_ccl_nargs;           \
                 ++juvix_idx) {                                               \
                CARG(juvix_idx) = CLOSURE_ARG(juvix_ccl_closure, juvix_idx);  \
            }                                                                 \
            for (size_t juvix_idx = 0; juvix_idx < juvix_ccl_largs;           \
                 ++juvix_idx) {                                               \
                STACK_POP(CARG(juvix_ccl_nargs + juvix_idx));                 \
            }                                                                 \
            if (juvix_ccl_sargs == 0 && juvix_ccl_return == NULL) {           \
                STACKTRACE_REPLACE(get_closure_fuid(juvix_ccl_closure));      \
                STACK_LEAVE;                                                  \
                STORED_GOTO(get_closure_addr(juvix_ccl_closure));             \
            } else {                                                          \
                STACK_PUSH_ADDR(juvix_ccl_return);                            \
                STACK_PUSH(juvix_ccl_sargs);                                  \
                CALL_CLOSURE(juvix_ccl_closure,                               \
                             juvix_ccl_return_from_closure);                  \
                STACK_POP(juvix_ccl_sargs);                                   \
                STACK_POP_ADDR(juvix_ccl_return);                             \
                if (juvix_ccl_sargs > 0) {                                    \
                    juvix_ccl_closure = juvix_result;                         \
                    goto juvix_ccl_start;                                     \
                } else {                                                      \
                    ASSERT(juvix_ccl_return != NULL);                         \
                    STORED_GOTO(juvix_ccl_return);                            \
                }                                                             \
            }                                                                 \
        } else {                                                              \
            EXTEND_CLOSURE(juvix_result, juvix_ccl_closure, juvix_ccl_sargs); \
            for (size_t juvix_idx = 0; juvix_idx < juvix_ccl_sargs;           \
                 ++juvix_idx) {                                               \
                STACK_POP(                                                    \
                    CLOSURE_ARG(juvix_result, juvix_ccl_nargs + juvix_idx));  \
            }                                                                 \
            if (juvix_ccl_return == NULL) {                                   \
                STACK_LEAVE;                                                  \
                RETURN;                                                       \
            } else {                                                          \
                STORED_GOTO(juvix_ccl_return);                                \
            }                                                                 \
        }                                                                     \
    } while (0)

#define DISPATCH_STACK_SIZE 3

#endif
