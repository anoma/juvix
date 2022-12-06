/* Higher-order function composition */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2)

static constr_info_t juvix_constr_info_array[BUILTIN_UIDS_NUM] = {
    BUILTIN_UIDS_INFO};

#define FUIDS_NUM 5
#define FUID_UNKNOWN 0
#define FUID_COMPOSE 1
#define FUID_PLUS 2
#define FUID_INC 3
#define FUID_ID 4

static function_info_t juvix_function_info_array[FUIDS_NUM] = {
    {"<closure>"}, {"compose"}, {"plus"}, {"inc"}, {"id"}};

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(3);

    DECL_ZEROARG_CLOSURE(id, FUID_ID, 1);
    DECL_ZEROARG_CLOSURE(inc, FUID_INC, 1);

    juvix_constrs_num = BUILTIN_UIDS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    juvix_functions_num = FUIDS_NUM;
    juvix_function_info = juvix_function_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

juvix_closure_compose:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    ARG(2) = CARG(2);
    JUVIX_FUNCTION(juvix_function_compose, 2);
    {
        STACK_PUSH(ARG(0));
        ASSIGN_CARGS(ARG(1), { CARG(juvix_closure_nargs) = ARG(2); });
        CALL_CLOSURE(ARG(1), juvix_label_compose_1);
        STACK_POP(ARG(0));
        ASSIGN_CARGS(ARG(0), { CARG(juvix_closure_nargs) = juvix_result; });
        TAIL_CALL_CLOSURE(ARG(0));
    }

juvix_closure_id:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION_NS(juvix_function_id);
    {
        juvix_result = ARG(0);
        RETURN_NS;
    }

    JUVIX_FUNCTION(juvix_function_iterate, 2);
    {
        DECL_TMP(0);
        JUVIX_VAL_EQ(TMP(0), ARG(1), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = juvix_zeroarg_closure_id;
                RETURN;
            },
            {
                STACK_PUSH(ARG(0));
                JUVIX_INT_SUB(TMP(0), ARG(1), make_smallint(1));
                ARG(1) = TMP(0);
                CALL(0, juvix_function_iterate, juvix_label_iterate_1);
                STACK_POP(ARG(0));
                PREALLOC(
                    CLOSURE_HEAD_SIZE + 2,
                    {
                        STACK_PUSH(ARG(0));
                        STACK_PUSH(juvix_result);
                    },
                    {
                        STACK_POP(juvix_result);
                        STACK_POP(ARG(0));
                    });
                ALLOC_CLOSURE(TMP(0), FUID_COMPOSE,
                              LABEL_ADDR(juvix_closure_compose), 2, 1);
                CLOSURE_ARG(TMP(0), 0) = ARG(0);
                CLOSURE_ARG(TMP(0), 1) = juvix_result;
                juvix_result = TMP(0);
                RETURN;
            });
    }

juvix_closure_inc:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION_NS(juvix_function_inc);
    {
        JUVIX_INT_ADD(juvix_result, ARG(0), make_smallint(1));
        RETURN_NS;
    }

juvix_closure_plus:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION(juvix_function_plus, 2);
    {
        STACK_PUSH(ARG(1));
        ARG(1) = ARG(0);
        ARG(0) = juvix_zeroarg_closure_inc;
        CALL(0, juvix_function_iterate, juvix_label_plus_1);
        STACK_POP(ARG(1));
        ASSIGN_CARGS(juvix_result, { CARG(juvix_closure_nargs) = ARG(1); });
        TAIL_CALL_CLOSURE(juvix_result);
    }

juvix_closure_mult:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION(juvix_function_mult, 1);
    {
        DECL_TMP(0);
        STACK_PUSH(ARG(1));
        PREALLOC(
            CLOSURE_HEAD_SIZE + 1, { STACK_PUSH(ARG(0)); },
            { STACK_POP(ARG(0)); });
        ALLOC_CLOSURE(TMP(0), FUID_PLUS, LABEL_ADDR(juvix_closure_plus), 1, 1);
        CLOSURE_ARG(TMP(0), 0) = ARG(0);
        STACK_POP(ARG(1));
        ARG(0) = TMP(0);
        CALL(0, juvix_function_iterate, juvix_label_mult_1);
        ASSIGN_CARGS(juvix_result,
                     { CARG(juvix_closure_nargs) = make_smallint(0); });
        TAIL_CALL_CLOSURE(juvix_result);
    }

    JUVIX_FUNCTION(juvix_function_exp, 1);
    {
        DECL_TMP(0);
        STACK_PUSH(ARG(1));
        PREALLOC(
            CLOSURE_HEAD_SIZE + 1, { STACK_PUSH(ARG(0)); },
            { STACK_POP(ARG(0)); });
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_mult), 1, 1);
        CLOSURE_ARG(TMP(0), 0) = ARG(0);
        STACK_POP(ARG(1));
        ARG(0) = TMP(0);
        CALL(0, juvix_function_iterate, juvix_label_exp_1);
        ASSIGN_CARGS(juvix_result,
                     { CARG(juvix_closure_nargs) = make_smallint(1); });
        TAIL_CALL_CLOSURE(juvix_result);
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        ARG(1) = make_smallint(7);
        ARG(0) = make_smallint(3);
        CALL(0, juvix_function_plus, juvix_label_1);
        JUVIX_TRACE(juvix_result);

        ARG(1) = make_smallint(7);
        ARG(0) = make_smallint(3);
        CALL(0, juvix_function_mult, juvix_label_2);
        JUVIX_TRACE(juvix_result);

        ARG(1) = make_smallint(7);
        ARG(0) = make_smallint(3);
        CALL(0, juvix_function_exp, juvix_label_3);
        JUVIX_TRACE(juvix_result);

        ARG(1) = make_smallint(10);
        ARG(0) = make_smallint(3);
        CALL(0, juvix_function_exp, juvix_label_4);
        JUVIX_TRACE(juvix_result);

        juvix_result = OBJ_VOID;
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
