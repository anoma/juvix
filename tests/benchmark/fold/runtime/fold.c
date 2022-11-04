/* Successively map K functions to a list of N integers */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2);

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 2)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {
    BUILTIN_UIDS_INFO, {"nil"}, {"cons"}};

#define UID_NIL FIRST_USER_UID
#define UID_CONS (FIRST_USER_UID + 1)

#define CONSTR_NIL MAKE_HEADER(UID_NIL, 0)

int main() {
    JUVIX_PROLOGUE(3, JUVIX_DECL_ARGS);

    DECL_ZEROARG_CLOSURE(plus, 0, 2);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

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

    JUVIX_FUNCTION(foldl, 3);
    {
        switch (ARG(2)) {
            case CONSTR_NIL:
                juvix_result = ARG(1);
                RETURN;
            default: {
                STACK_PUSH(ARG(0));
                STACK_PUSH(ARG(2));
                ASSIGN_CARGS(ARG(0), {
                    CARG(juvix_closure_nargs) = ARG(1);
                    CARG(juvix_closure_nargs + 1) = FST(ARG(2));
                });
                CALL_CLOSURE(ARG(0), juvix_label_foldl_1);
                STACK_POP(ARG(2));
                STACK_POP(ARG(0));
                ARG(1) = juvix_result;
                ARG(2) = SND(ARG(2));
                TAIL_CALL(0, foldl);
            }
        }
    }

juvix_closure_plus:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(plus);
    {
        DECL_TMP(0);
        JUVIX_INT_ADD(TMP(0), ARG(0), ARG(1));
        JUVIX_INT_MOD(TMP(0), TMP(0), make_smallint(1 << 28));
        juvix_result = TMP(0);
        RETURN_NS;
    }

    JUVIX_FUNCTION_NS(gen);
    {
        DECL_TMP(0);
        JUVIX_VAL_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = ARG(1);
                RETURN_NS;
            },
            {
                PREALLOC(
                    2,
                    {
                        STACK_ENTER(2);
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(ARG(1));
                    },
                    {
                        STACK_POP(ARG(1));
                        STACK_POP(ARG(0));
                        STACK_LEAVE;
                    });
                ALLOC_CONSTR_PAIR(TMP(0));
                FST(TMP(0)) = ARG(0);
                SND(TMP(0)) = ARG(1);
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                ARG(1) = TMP(0);
                TAIL_CALL_NS(0, gen);
            });
    }

    JUVIX_FUNCTION(run, 3);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(0), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                ARG(0) = juvix_zeroarg_closure_plus;
                TAIL_CALL(0, foldl);
            },
            {
                STACK_PUSH(ARG(0));
                STACK_PUSH(ARG(2));
                JUVIX_INT_SUB(ARG(1), make_smallint(0), ARG(1));
                ARG(0) = juvix_zeroarg_closure_plus;
                CALL(0, foldl, juvix_label_run_1);
                STACK_POP(ARG(2));
                STACK_POP(ARG(0));
                ARG(1) = juvix_result;
                JUVIX_INT_SUB(ARG(0), ARG(0), make_smallint(1));
                TAIL_CALL(0, run);
            });
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        ARG(0) = make_smallint(100000);
        ARG(1) = CONSTR_NIL;
        CALL(0, gen, juvix_label_1);
        ARG(2) = juvix_result;
        ARG(1) = make_smallint(0);
        ARG(0) = make_smallint(1000);
        TAIL_CALL(0, run);
    }

    JUVIX_EPILOGUE;
    return 0;
}
