/* Recursion through higher-order functions */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1)

static constr_info_t juvix_constr_info_array[BUILTIN_UIDS_NUM] = {
    BUILTIN_UIDS_INFO};

int main() {
    JUVIX_PROLOGUE(2, JUVIX_DECL_ARGS);

    juvix_constrs_num = BUILTIN_UIDS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_NS(juvix_function_g);
    {
        DECL_TMP(0);
        JUVIX_VAL_EQ(TMP(0), ARG(1), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = make_smallint(0);
                RETURN_NS;
            },
            {
                JUVIX_INT_SUB(TMP(0), ARG(1), make_smallint(1));
                ASSIGN_CARGS(ARG(0), { CARG(juvix_closure_nargs) = TMP(0); });
                TAIL_CALL_CLOSURE_NS(ARG(0));
            });
    }

juvix_closure_f:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION(juvix_function_f, 2);
    {
        DECL_TMP(0);
        PREALLOC(
            CLOSURE_SKIP + 1, { STACK_PUSH(ARG(0)); }, { STACK_POP(ARG(0)); });
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_f), 0, 1);
        STACK_PUSH(ARG(0));
        ARG(1) = ARG(0);
        ARG(0) = TMP(0);
        CALL(0, juvix_function_g, juvix_label_g_1);
        STACK_POP(ARG(0));
        JUVIX_INT_ADD(juvix_result, juvix_result, ARG(0));
        RETURN;
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        ARG(0) = make_smallint(10000);
        TAIL_CALL(0, juvix_function_f);
    }

    JUVIX_EPILOGUE;
    return 0;
}
