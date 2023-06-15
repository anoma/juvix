/* Merge sort a list of N integers */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2)

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 3)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {
    BUILTIN_UIDS_INFO,
    {"nil", 0, APP_FIXITY},
    {"cons", 0, APP_FIXITY},
    {"pair", 0, APP_FIXITY}};

#define UID_NIL FIRST_USER_UID
#define UID_CONS (FIRST_USER_UID + 1)
#define UID_PAIR (FIRST_USER_UID + 2)

#define CONSTR_NIL MAKE_HEADER(UID_NIL, 0)

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(3);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION(split_go, 3);
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
        switch (ARG(0)) {
            case CONSTR_NIL: {
                ALLOC_CONSTR_PAIR(juvix_result);
                FST(juvix_result) = ARG(1);
                SND(juvix_result) = ARG(2);
                RETURN;
            }
            default: {
                DECL_TMP(0);
                ALLOC_CONSTR_PAIR(TMP(0));
                FST(TMP(0)) = FST(ARG(0));
                SND(TMP(0)) = ARG(1);
                ARG(0) = SND(ARG(0));
                ARG(1) = ARG(2);
                ARG(2) = TMP(0);
                TAIL_CALL(0, split_go);
            }
        }
    }

    JUVIX_FUNCTION_NS(split);
    {
        ARG(1) = CONSTR_NIL;
        ARG(2) = CONSTR_NIL;
        TAIL_CALL_NS(0, split_go);
    }

    JUVIX_FUNCTION(merge, 2);
    {
        switch (ARG(0)) {
            case CONSTR_NIL:
                juvix_result = ARG(1);
                RETURN;
            default:
                switch (ARG(1)) {
                    case CONSTR_NIL:
                        juvix_result = ARG(0);
                        RETURN;
                    default: {
                        DECL_TMP(0);
                        DECL_TMP(1);
                        DECL_TMP(2);
                        TMP(0) = FST(ARG(0));
                        TMP(1) = FST(ARG(1));
                        JUVIX_INT_LE(TMP(2), TMP(0), TMP(1));
                        JUVIX_BRANCH(
                            TMP(2),
                            {
                                STACK_PUSH(TMP(0));
                                ARG(0) = SND(ARG(0));
                                CALL(0, merge, juvix_label_merge_1);
                                STACK_POP(TMP(0));
                                PREALLOC(
                                    2,
                                    {
                                        STACK_PUSH(TMP(0));
                                        STACK_PUSH(juvix_result);
                                    },
                                    {
                                        STACK_POP(juvix_result);
                                        STACK_POP(TMP(0));
                                    })
                                ALLOC_CONSTR_PAIR(TMP(2));
                                FST(TMP(2)) = TMP(0);
                                SND(TMP(2)) = juvix_result;
                                juvix_result = TMP(2);
                                RETURN;
                            },
                            {
                                STACK_PUSH(TMP(1));
                                ARG(1) = SND(ARG(1));
                                CALL(0, merge, juvix_label_merge_2);
                                STACK_POP(TMP(1));
                                PREALLOC(
                                    2,
                                    {
                                        STACK_PUSH(TMP(1));
                                        STACK_PUSH(juvix_result);
                                    },
                                    {
                                        STACK_POP(juvix_result);
                                        STACK_POP(TMP(1));
                                    })
                                ALLOC_CONSTR_PAIR(TMP(2));
                                FST(TMP(2)) = TMP(1);
                                SND(TMP(2)) = juvix_result;
                                juvix_result = TMP(2);
                                RETURN;
                            });
                    }
                }
        }
    }

    JUVIX_FUNCTION(sort, 2);
    {
        switch (ARG(0)) {
            case CONSTR_NIL:
                juvix_result = CONSTR_NIL;
                RETURN;
            default:
                switch (SND(ARG(0))) {
                    case CONSTR_NIL:
                        juvix_result = ARG(0);
                        RETURN;
                    default: {
                        DECL_TMP(0);
                        DECL_TMP(1);
                        CALL(0, split, juvix_label_sort_1);
                        TMP(0) = FST(juvix_result);
                        TMP(1) = SND(juvix_result);
                        STACK_PUSH(TMP(1));
                        ARG(0) = TMP(0);
                        CALL(0, sort, juvix_label_sort_2);
                        STACK_POP(TMP(1));
                        TMP(0) = juvix_result;
                        STACK_PUSH(TMP(0));
                        ARG(0) = TMP(1);
                        CALL(0, sort, juvix_label_sort_3);
                        STACK_POP(TMP(0));
                        ARG(0) = TMP(0);
                        ARG(1) = juvix_result;
                        TAIL_CALL(0, merge);
                    }
                }
        }
    }

    JUVIX_FUNCTION_NS(sorted);
    {
        switch (ARG(0)) {
            case CONSTR_NIL:
                juvix_result = BOOL_TRUE;
                RETURN_NS;
            default:
                switch (SND(ARG(0))) {
                    case CONSTR_NIL:
                        juvix_result = BOOL_TRUE;
                        RETURN_NS;
                    default: {
                        DECL_TMP(0);
                        JUVIX_INT_LE(TMP(0), FST(ARG(0)), FST(SND(ARG(0))));
                        JUVIX_BRANCH(
                            TMP(0),
                            {
                                ARG(0) = SND(ARG(0));
                                TAIL_CALL_NS(0, sorted);
                            },
                            {
                                juvix_result = BOOL_FALSE;
                                RETURN_NS;
                            })
                    }
                }
        }
    }

    JUVIX_FUNCTION(gen, 2);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = ARG(1);
                RETURN;
            },
            {
                PREALLOC(
                    2,
                    {
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(ARG(1));
                    },
                    {
                        STACK_POP(ARG(1));
                        STACK_POP(ARG(0));
                    });
                ALLOC_CONSTR_PAIR(TMP(0));
                FST(TMP(0)) = ARG(0);
                SND(TMP(0)) = ARG(1);
                ARG(1) = TMP(0);
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                TAIL_CALL(0, gen);
            });
    }

    JUVIX_FUNCTION(juvix_function_main, 1);
    {
        ARG(1) = CONSTR_NIL;
        ARG(0) = make_smallint(2000000);
        CALL(0, gen, juvix_label_main_1);
        ARG(0) = juvix_result;
        CALL(0, sort, juvix_label_2);
        ARG(0) = juvix_result;
        TAIL_CALL(0, sorted);
    }

    JUVIX_EPILOGUE;
    return 0;
}
