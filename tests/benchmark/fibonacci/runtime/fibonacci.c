/* Compute the Nth Fibonacci number modulo 2^28 */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2)

#define CONSTRS_NUM BUILTIN_UIDS_NUM

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {BUILTIN_UIDS_INFO};

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(3);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_NS(fib_go);
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
                TAIL_CALL_NS(0, fib_go);
            });
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
