/* Tail recursion through higher-order functions */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2)

static constr_info_t juvix_constr_info_array[BUILTIN_UIDS_NUM] = {
    BUILTIN_UIDS_INFO};

int main() {
    JUVIX_PROLOGUE(3, JUVIX_DECL_ARGS);

    juvix_constrs_num = BUILTIN_UIDS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_NS(juvix_function_sumb);
    {
        DECL_TMP(0);
        JUVIX_VAL_EQ(TMP(0), ARG(1), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = ARG(2);
                RETURN_NS;
            },
            {
                JUVIX_INT_SUB(TMP(0), ARG(1), make_smallint(1));
                ASSIGN_CARGS(ARG(0), {
                    CARG(juvix_closure_nargs) = TMP(0);
                    CARG(juvix_closure_nargs + 1) = ARG(2);
                });
                TAIL_CALL_CLOSURE_NS(ARG(0));
            });
    }

juvix_closure_sum_prim:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION(juvix_function_sum_prim, 2);
    {
        DECL_TMP(0);
        DECL_TMP(1);
        JUVIX_INT_ADD(TMP(0), ARG(0), ARG(1));
        PREALLOC(
            CLOSURE_SKIP + 1,
            {
                STACK_PUSH(ARG(0));
                STACK_PUSH(TMP(0));
            },
            {
                STACK_POP(TMP(0));
                STACK_POP(ARG(0));
            });
        ALLOC_CLOSURE(TMP(1), 0, LABEL_ADDR(juvix_closure_sum_prim), 0, 2);
        ARG(2) = TMP(0);
        ARG(1) = ARG(0);
        ARG(0) = TMP(1);
        TAIL_CALL(0, juvix_function_sumb);
    }

    JUVIX_FUNCTION_NS(juvix_function_sum);
    {
        ARG(1) = make_smallint(0);
        TAIL_CALL_NS(0, juvix_function_sum_prim);
    }

    JUVIX_FUNCTION_NS(juvix_function_main);
    {
        ARG(0) = make_smallint(10000);
        TAIL_CALL_NS(0, juvix_function_sum);
    }

    JUVIX_EPILOGUE;
    return 0;
}
