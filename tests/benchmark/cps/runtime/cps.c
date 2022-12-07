/* Compute the Nth Fibonacci number modulo 2^28 with CPS */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2);    \
    DECL_REG_ARG(3);

#define CONSTRS_NUM BUILTIN_UIDS_NUM

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {BUILTIN_UIDS_INFO};

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(3);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    DECL_ZEROARG_CLOSURE(fib_go, 0, 3);

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_NS(fib_step);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = ARG(1);
                RETURN_NS;
            },
            {
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                TMP(0) = ARG(1);
                ARG(1) = ARG(2);
                JUVIX_INT_ADD(ARG(2), ARG(2), TMP(0));
                JUVIX_INT_MOD(ARG(2), ARG(2), make_smallint(1 << 28));
                ASSIGN_CARGS(ARG(3), {
                    CARG(juvix_closure_nargs) = ARG(0);
                    CARG(juvix_closure_nargs + 1) = ARG(1);
                    CARG(juvix_closure_nargs + 2) = ARG(2);
                });
                TAIL_CALL_CLOSURE_NS(ARG(3));
            });
    }

juvix_closure_fib_go:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    ARG(2) = CARG(2);
    JUVIX_FUNCTION_NS(fib_go);
    {
        ARG(3) = juvix_zeroarg_closure_fib_go;
        TAIL_CALL_NS(0, fib_step);
    }

    JUVIX_FUNCTION_NS(fib);
    {
        ARG(1) = make_smallint(0);
        ARG(2) = make_smallint(1);
        TAIL_CALL_NS(0, fib_go);
    }

    JUVIX_FUNCTION_NS(juvix_function_main);
    {
        ARG(0) = make_smallint(100000000);
        TAIL_CALL_NS(0, fib);
    }

    JUVIX_EPILOGUE;
    return 0;
}
