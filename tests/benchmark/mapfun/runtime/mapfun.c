/* Succesively map K functions to a list of N integers */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2);

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 2)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {
    BUILTIN_UIDS_INFO, {"nil", 0, APP_FIXITY}, {"cons", 0, APP_FIXITY}};

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

    DECL_CURRY(1);

    JUVIX_FUNCTION(map, 3);
    {
        switch (ARG(1)) {
            case CONSTR_NIL:
                juvix_result = CONSTR_NIL;
                RETURN;
            default: {
                DECL_TMP(0);
                DECL_TMP(1);
                TMP(0) = ARG(0);
                TMP(1) = ARG(1);
                STACK_PUSH(TMP(0));
                STACK_PUSH(TMP(1));
                ARG(0) = TMP(0);
                ARG(1) = SND(TMP(1));
                CALL(0, map, juvix_label_map_1);
                STACK_POP(TMP(1));
                STACK_POP(TMP(0));
                STACK_PUSH(juvix_result);
                ASSIGN_CARGS(TMP(0),
                             { CARG(juvix_closure_nargs) = FST(TMP(1)); });
                CALL_CLOSURE(TMP(0), juvix_label_map_2);
                STACK_POP(TMP(0));
                TMP(1) = juvix_result;
                PREALLOC(
                    2,
                    {
                        STACK_PUSH(TMP(0));
                        STACK_PUSH(TMP(1));
                    },
                    {
                        STACK_POP(TMP(1));
                        STACK_POP(TMP(0));
                    });
                ALLOC_CONSTR_PAIR(juvix_result);
                FST(juvix_result) = TMP(1);
                SND(juvix_result) = TMP(0);
                RETURN;
            }
        }
    }

    JUVIX_FUNCTION(mapfun, 3);
    {
        switch (ARG(0)) {
            case CONSTR_NIL:
                juvix_result = ARG(1);
                RETURN;
            default: {
                DECL_TMP(0);
                PREALLOC(
                    CURRY_CLOSURE_ALLOC_SIZE,
                    {
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(ARG(1));
                    },
                    {
                        STACK_POP(ARG(1));
                        STACK_POP(ARG(0));
                    });
                STACK_PUSH(ARG(0));
                STACK_PUSH(ARG(1));
                ARG(0) = FST(ARG(0));
                CURRY_CLOSURE(TMP(0), ARG(0), 1);
                STACK_POP(ARG(1));
                ARG(0) = TMP(0);
                CALL(0, map, juvix_label_mapfun_1);
                STACK_POP(ARG(0));
                ARG(0) = SND(ARG(0));
                ARG(1) = juvix_result;
                TAIL_CALL(0, mapfun);
            }
        }
    }

juvix_closure_minus:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(minus);
    {
        JUVIX_INT_SUB(juvix_result, ARG(0), ARG(1));
        RETURN_NS;
    }

juvix_closure_app_plus:
    PREALLOC(
        CURRY_CLOSURE_ALLOC_SIZE,
        {
            STACK_ENTER(3);
            STACK_PUSH(CARG(0));
            STACK_PUSH(CARG(1));
            STACK_PUSH(CARG(2));
        },
        {
            STACK_POP(CARG(2));
            STACK_POP(CARG(1));
            STACK_POP(CARG(0));
            STACK_LEAVE;
        });
    ARG(0) = CARG(0);
    CURRY_CLOSURE(ARG(1), CARG(1), 1);
    ARG(2) = CARG(2);
    JUVIX_FUNCTION_NS(plus);
    {
        DECL_TMP(0);
        JUVIX_INT_ADD(TMP(0), ARG(0), ARG(2));
        ASSIGN_CARGS(ARG(1), { CARG(juvix_closure_nargs) = TMP(0); });
        TAIL_CALL_CLOSURE_NS(ARG(1));
    }

    JUVIX_FUNCTION(genfs, 2);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = CONSTR_NIL;
                RETURN;
            },
            {
                DECL_TMP(1);
                STACK_PUSH(ARG(0));
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                CALL(0, genfs, juvix_label_genfs_1);
                STACK_POP(ARG(0));
                PREALLOC(
                    CLOSURE_HEAD_SIZE + 1 + 2,
                    {
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(juvix_result);
                    },
                    {
                        STACK_POP(juvix_result);
                        STACK_POP(ARG(0));
                    });
                ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_minus), 1, 1);
                CLOSURE_ARG(TMP(0), 0) = ARG(0);
                ALLOC_CONSTR_PAIR(TMP(1));
                FST(TMP(1)) = TMP(0);
                SND(TMP(1)) = juvix_result;
                juvix_result = TMP(1);
                RETURN;
            });
    }

    JUVIX_FUNCTION(genffs, 2);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = CONSTR_NIL;
                RETURN;
            },
            {
                DECL_TMP(1);
                STACK_PUSH(ARG(0));
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                CALL(0, genffs, juvix_label_genfs_2);
                STACK_POP(ARG(0));
                PREALLOC(
                    CLOSURE_HEAD_SIZE + 1 + 2,
                    {
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(juvix_result);
                    },
                    {
                        STACK_POP(juvix_result);
                        STACK_POP(ARG(0));
                    });
                ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_app_plus), 1,
                              2);
                CLOSURE_ARG(TMP(0), 0) = ARG(0);
                ALLOC_CONSTR_PAIR(TMP(1));
                FST(TMP(1)) = TMP(0);
                SND(TMP(1)) = juvix_result;
                juvix_result = TMP(1);
                RETURN;
            });
    }

    JUVIX_FUNCTION(gen, 2);
    {
        DECL_TMP(0);
        JUVIX_VAL_EQ(TMP(0), ARG(0), ARG(1));
        JUVIX_BRANCH(
            TMP(0),
            {
                PREALLOC(
                    2, { STACK_PUSH(ARG(0)); }, { STACK_POP(ARG(0)); });
                ALLOC_CONSTR_PAIR(juvix_result);
                FST(juvix_result) = ARG(0);
                SND(juvix_result) = CONSTR_NIL;
                RETURN;
            },
            {
                JUVIX_INT_ADD(TMP(0), ARG(0), make_smallint(1));
                STACK_PUSH(ARG(0));
                ARG(0) = TMP(0);
                CALL(0, gen, juvix_label_gen_1);
                STACK_POP(ARG(0));
                PREALLOC(
                    2,
                    {
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(juvix_result);
                    },
                    {
                        STACK_POP(juvix_result);
                        STACK_POP(ARG(0));
                    });
                ALLOC_CONSTR_PAIR(TMP(0));
                FST(TMP(0)) = ARG(0);
                SND(TMP(0)) = juvix_result;
                juvix_result = TMP(0);
                RETURN;
            });
    }

    JUVIX_FUNCTION_NS(sum_go);
    {
        switch (ARG(0)) {
            case CONSTR_NIL:
                juvix_result = ARG(1);
                RETURN_NS;
            default: {
                JUVIX_INT_ADD(ARG(1), ARG(1), FST(ARG(0)));
                ARG(0) = SND(ARG(0));
                TAIL_CALL_NS(0, sum_go);
            }
        }
    }

    JUVIX_FUNCTION_NS(sum);
    {
        ARG(1) = make_smallint(0);
        TAIL_CALL_NS(0, sum_go);
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        ARG(1) = make_smallint(10000);
        ARG(0) = make_smallint(1);
        CALL(0, gen, juvix_label_1);
        STACK_PUSH(juvix_result);
        ARG(0) = make_smallint(100);
        CALL(0, genfs, juvix_label_2);
        STACK_PUSH(juvix_result);
        ARG(0) = make_smallint(100);
        CALL(0, genffs, juvix_label_3);
        ARG(0) = juvix_result;
        STACK_POP(ARG(1));
        CALL(0, mapfun, juvix_label_4);
        ARG(0) = juvix_result;
        STACK_POP(ARG(1));
        CALL(0, mapfun, juvix_label_5);
        ARG(0) = juvix_result;
        TAIL_CALL(0, sum);
    }

    JUVIX_EPILOGUE;
    return 0;
}
