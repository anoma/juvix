/* Optionally sum N integers from a binary tree K times */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1)

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 4)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {
    BUILTIN_UIDS_INFO, {"leaf"}, {"node"}, {"nothing"}, {"just"}};

#define UID_LEAF FIRST_USER_UID
#define UID_NODE (FIRST_USER_UID + 1)
#define UID_NOTHING (FIRST_USER_UID + 2)
#define UID_JUST (FIRST_USER_UID + 3)

#define CONSTR_LEAF MAKE_HEADER(UID_LEAF, 0)
#define CONSTR_NOTHING MAKE_HEADER(UID_NOTHING, 0)

int main() {
    JUVIX_PROLOGUE(2, JUVIX_DECL_ARGS);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION(gen, 2);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = CONSTR_LEAF;
                RETURN;
            },
            {
                STACK_PUSH(ARG(0));
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                CALL(0, gen, juvix_label_gen_1);
                STACK_POP(ARG(0));
                PREALLOC(
                    4,
                    {
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(juvix_result);
                    },
                    {
                        STACK_POP(juvix_result);
                        STACK_POP(ARG(0));
                    });
                ALLOC_CONSTR_BOXED(TMP(0), UID_NODE, 3);
                CONSTR_ARG(TMP(0), 0) = ARG(0);
                CONSTR_ARG(TMP(0), 1) = juvix_result;
                CONSTR_ARG(TMP(0), 2) = juvix_result;
                juvix_result = TMP(0);
                RETURN;
            });
    }

    JUVIX_FUNCTION(sum, 3);
    {
        switch (ARG(1)) {
            case CONSTR_LEAF:
                juvix_result = make_smallint(0);
                RETURN;
            default: {
                DECL_TMP(0);
                JUVIX_INT_EQ(TMP(0), CONSTR_ARG(ARG(1), 0), ARG(0));
                JUVIX_BRANCH(
                    TMP(0),
                    {
                        juvix_result = CONSTR_NOTHING;
                        RETURN;
                    },
                    {
                        STACK_PUSH(ARG(1));
                        STACK_PUSH(ARG(0));
                        ARG(1) = CONSTR_ARG(ARG(1), 1);
                        CALL(0, sum, juvix_label_sum_1);
                        STACK_POP(ARG(0));
                        STACK_POP(ARG(1));
                        TMP(0) = juvix_result;
                        switch (TMP(0)) {
                            case CONSTR_NOTHING:
                                juvix_result = CONSTR_NOTHING;
                                RETURN;
                            default: {
                                DECL_TMP(1);
                                STACK_PUSH(ARG(1));
                                STACK_PUSH(TMP(0));
                                ARG(1) = CONSTR_ARG(ARG(1), 2);
                                CALL(0, sum, juvix_label_sum_2);
                                STACK_POP(TMP(0));
                                STACK_POP(ARG(1));
                                TMP(1) = juvix_result;
                                switch (TMP(1)) {
                                    case CONSTR_NOTHING:
                                        juvix_result = CONSTR_NOTHING;
                                        RETURN;
                                    default: {
                                        JUVIX_INT_ADD(juvix_result, TMP(0),
                                                      TMP(1));
                                        JUVIX_INT_ADD(juvix_result,
                                                      juvix_result,
                                                      CONSTR_ARG(ARG(1), 0));
                                        RETURN;
                                    }
                                }
                            }
                        }
                    });
            }
        }
    }

    JUVIX_FUNCTION(run, 3);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0), { TAIL_CALL(0, sum); },
            {
                STACK_PUSH(ARG(0));
                STACK_PUSH(ARG(1));
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                CALL(0, run, juvix_label_run_1);
                STACK_POP(ARG(1));
                STACK_POP(ARG(0));
                TMP(0) = juvix_result;
                switch (TMP(0)) {
                    case CONSTR_NOTHING:
                        TAIL_CALL(0, sum);
                    default: {
                        STACK_PUSH(TMP(0));
                        CALL(0, sum, juvix_label_run_2);
                        STACK_POP(TMP(0));
                        switch (juvix_result) {
                            case CONSTR_NOTHING:
                                RETURN;
                            default: {
                                JUVIX_INT_SUB(juvix_result, juvix_result,
                                              TMP(0));
                                RETURN;
                            }
                        }
                    }
                }
            });
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        ARG(0) = make_smallint(20);
        CALL(0, gen, juvix_label_1);
        ARG(1) = juvix_result;
        ARG(0) = make_smallint(101);
        TAIL_CALL(0, run);
    }

    JUVIX_EPILOGUE;
    return 0;
}
