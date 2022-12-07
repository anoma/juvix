/* Compute the Nth Fibonacci number modulo 2^28 */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1)

#define CONSTRS_NUM BUILTIN_UIDS_NUM

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {BUILTIN_UIDS_INFO};

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(2);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION(go, 3);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(1), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = make_smallint(1);
                RETURN;
            },
            {
                JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
                JUVIX_BRANCH(
                    TMP(0),
                    {
                        juvix_result = make_smallint(0);
                        RETURN;
                    },
                    {
                        JUVIX_INT_LT(TMP(0), ARG(1), make_smallint(0));
                        JUVIX_BRANCH(
                            TMP(0),
                            {
                                juvix_result = make_smallint(0);
                                RETURN;
                            },
                            {
                                STACK_PUSH(ARG(0));
                                STACK_PUSH(ARG(1));
                                JUVIX_INT_SUB(ARG(1), ARG(1), ARG(0));
                                CALL(0, go, juvix_label_go_1);
                                STACK_POP(ARG(1));
                                STACK_POP(ARG(0));
                                STACK_PUSH(juvix_result);
                                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                                CALL(0, go, juvix_label_go_2);
                                STACK_POP(TMP(0));
                                JUVIX_INT_ADD(juvix_result, juvix_result,
                                              TMP(0));
                                RETURN;
                            });
                    });
            });
    }

    JUVIX_FUNCTION_NS(combinations);
    {
        ARG(1) = ARG(0);
        TAIL_CALL_NS(0, go);
    }

    JUVIX_FUNCTION_NS(juvix_function_main);
    {
        ARG(0) = make_smallint(100);
        TAIL_CALL_NS(0, combinations);
    }

    JUVIX_EPILOGUE;
    return 0;
}
