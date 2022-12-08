/* Compute the Nth prime number */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2)

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 2)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {
    BUILTIN_UIDS_INFO, {"nil"}, {"cons"}};

#define UID_NIL FIRST_USER_UID
#define UID_CONS (FIRST_USER_UID + 1)

#define CONSTR_NIL MAKE_HEADER(UID_NIL, 0)

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(3);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_NS(check_divisible);
    {
        switch (ARG(1)) {
            case CONSTR_NIL:
                juvix_result = BOOL_FALSE;
                RETURN_NS;
            default: {
                DECL_TMP(0);
                DECL_TMP(1);
                JUVIX_INT_MOD(TMP(0), ARG(0), FST(ARG(1)));
                JUVIX_INT_EQ(TMP(1), TMP(0), make_smallint(0));
                JUVIX_BRANCH(
                    TMP(1),
                    {
                        juvix_result = BOOL_TRUE;
                        RETURN_NS;
                    },
                    {
                        ARG(1) = SND(ARG(1));
                        TAIL_CALL_NS(0, check_divisible);
                    });
            }
        }
    }

    JUVIX_FUNCTION(go, 4);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = FST(ARG(2));
                RETURN_NS;
            },
            {
                STACK_PUSH(ARG(0));
                STACK_PUSH(ARG(1));
                STACK_PUSH(ARG(2));
                ARG(0) = ARG(1);
                ARG(1) = ARG(2);
                CALL(0, check_divisible, juvix_label_go_1);
                STACK_POP(ARG(2));
                STACK_POP(ARG(1));
                STACK_POP(ARG(0));
                JUVIX_BRANCH(
                    juvix_result,
                    {
                        JUVIX_INT_ADD(ARG(1), ARG(1), make_smallint(1));
                        TAIL_CALL(0, go);
                    },
                    {
                        PREALLOC(
                            2,
                            {
                                STACK_PUSH(ARG(0));
                                STACK_PUSH(ARG(1));
                                STACK_PUSH(ARG(2));
                            },
                            {
                                STACK_POP(ARG(2));
                                STACK_POP(ARG(1));
                                STACK_POP(ARG(0));
                            });
                        ALLOC_CONSTR_PAIR(TMP(0));
                        FST(TMP(0)) = ARG(1);
                        SND(TMP(0)) = ARG(2);
                        ARG(2) = TMP(0);
                        JUVIX_INT_ADD(ARG(1), ARG(1), make_smallint(1));
                        JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                        TAIL_CALL(0, go);
                    });
            });
    }

    JUVIX_FUNCTION_NS(prime);
    {
        ARG(1) = make_smallint(2);
        ARG(2) = CONSTR_NIL;
        TAIL_CALL_NS(0, go);
    }

    JUVIX_FUNCTION_NS(juvix_function_main);
    {
        ARG(0) = make_smallint(1024 * 16);
        TAIL_CALL_NS(0, prime);
    }

    JUVIX_EPILOGUE;
    return 0;
}
