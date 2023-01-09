#ifndef JUVIX_API_H
#define JUVIX_API_H

#include <juvix/funcall.h>
#include <juvix/info.h>
#include <juvix/io.h>
#include <juvix/mem.h>
#include <juvix/object.h>

#define JUVIX_INIT \
    MEM_INIT;      \
    funcall_init()

// MAX_ARGS is the maximum number of function arguments, at least 1.
#define JUVIX_PROLOGUE(MAX_ARGS)                    \
    MEM_DECLS;                                      \
    UNUSED word_t juvix_temp_var;                   \
    FUNCALL_DECLS(MAX_ARGS);                        \
    JUVIX_INIT;                                     \
    STACK_PUSH_ADDR(LABEL_ADDR(juvix_program_end)); \
    goto juvix_program_start;                       \
    DECL_CALL_CLOSURES;                             \
    juvix_program_start:

#define JUVIX_EPILOGUE \
    juvix_program_end: \
    STACK_POPT;        \
    IO_INTERPRET;      \
    io_print_toplevel(juvix_result);

// Temporary vars
#define DECL_TMP(k) UNUSED word_t juvix_tmp_##k
#define TMP(k) juvix_tmp_##k

// Value stack temporary vars
#define DECL_STMP(k) word_t juvix_stmp_##k
#define STMP(k) juvix_stmp_##k

// Begin a function definition. `max_stack` is the maximum stack allocation in
// the function.
#define JUVIX_FUNCTION(label, max_stack) \
    label:                               \
    STACK_ENTER((max_stack))

/*
    Macro sequence for function definition:

closure_label:
    ARG(0) = CARG(0);
    ...
    ARG(m) = CARG(m);
    JUVIX_FUNCTION(function_label, max_stack);
    {
        DECL_TMP(0);
        DECL_TMP(1);
        ...
        DECL_TMP(max_tmp-1);
        <code>
    }
*/

// Begin a function with no stack allocation.
#define JUVIX_FUNCTION_NS(label) \
    label:

#define JUVIX_INT_ADD(var0, var1, var2) (var0 = smallint_add(var1, var2))
#define JUVIX_INT_SUB(var0, var1, var2) (var0 = smallint_sub(var1, var2))
#define JUVIX_INT_MUL(var0, var1, var2) (var0 = smallint_mul(var1, var2))
#define JUVIX_INT_DIV(var0, var1, var2) (var0 = smallint_div(var1, var2))
#define JUVIX_INT_MOD(var0, var1, var2) (var0 = smallint_mod(var1, var2))

#define JUVIX_INT_LT(var0, var1, var2) (var0 = smallint_lt(var1, var2))
#define JUVIX_INT_LE(var0, var1, var2) (var0 = smallint_le(var1, var2))
#define JUVIX_INT_EQ(var0, var1, var2) (var0 = make_bool(var1 == var2))

#define JUVIX_VAL_EQ(var0, var1, var2) \
    (var0 = make_bool(juvix_equal(var1, var2)))

#define JUVIX_ASSIGN(var0, val) (var0 = val)

#define JUVIX_TRACE(val) (io_trace(val))
#define JUVIX_DUMP (stacktrace_dump())
#define JUVIX_FAILURE(val) \
    do {                   \
        io_trace(val);     \
        error_exit();      \
    } while (0)

#define JUVIX_ALLOC_INT(var, val) (var = make_smallint(val))
// ALLOC_CONSTR_BOXED(var, uid, nargs)
// ALLOC_CONSTR_BOXED_TAG(var, uid)
// ALLOC_CONSTR_UNBOXED(var, uid)
// ALLOC_CONSTR_PAIR(var)

// ALLOC_CSTRING(var, str)

// ALLOC_CLOSURE(var, fuid, addr, nargs, largs)
// EXTEND_CLOSURE(dest, src, n)

// CALL(fuid, fun_label, return_label)
// TAIL_CALL(fuid, fun_label)
// CALL_CLOSURE(cl)
// TAIL_CALL_CLOSURE(cl)
// CALL_CLOSURES(nargs, return_label)
// TAIL_CALL_CLOSURES(nargs)
// RETURN

#define JUVIX_BRANCH(val, CODE_TRUE, CODE_FALSE) \
    if (is_true(val)) {                          \
        CODE_TRUE;                               \
    } else {                                     \
        CODE_FALSE;                              \
    }

/*
    Compilation of the `Case` instruction:

    switch (var) {
        case UNBOXED_CONSTR_HEADER_1: {<code>}; break;
        ...
        case UNBOXED_CONSTR_HEADER_N: {<code>}; break;
        default:
            switch (get_header(var)) {
                case BOXED_CONSTR_HEADER_1: {<code>}; break;
                ...
                case BOXED_CONSTR_HEADER_M: {<code>}; break;
                default:
                    <code> (or UNREACHABLE)
            }
    }

    Single-branch switches shouldn't be output.
*/

#endif
