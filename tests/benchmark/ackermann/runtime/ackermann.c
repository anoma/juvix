/* Ackermann function (higher-order definition) */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1);    \
    DECL_REG_ARG(2)

static constr_info_t juvix_constr_info_array[BUILTIN_UIDS_NUM] = {
    BUILTIN_UIDS_INFO};

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(3);

    juvix_constrs_num = BUILTIN_UIDS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    DECL_CURRY(1);

    JUVIX_FUNCTION(iter, 2);
    {
        DECL_TMP(0);
        JUVIX_INT_EQ(TMP(0), ARG(1), make_smallint(0));
        JUVIX_BRANCH(
            TMP(0),
            {
                juvix_result = ARG(2);
                RETURN;
            },
            {
                STACK_PUSH(ARG(0));
                JUVIX_INT_SUB(ARG(1), ARG(1), make_smallint(1));
                CALL(0, iter, juvix_label_iter_1);
                STACK_POP(ARG(0));
                ASSIGN_CARGS(ARG(0),
                             { CARG(juvix_closure_nargs) = juvix_result; });
                TAIL_CALL_CLOSURE(ARG(0));
            });
    }

juvix_closure_step_2:
    PREALLOC(
        CURRY_CLOSURE_ALLOC_SIZE,
        {
            STACK_ENTER(2);
            STACK_PUSH(CARG(0));
            STACK_PUSH(CARG(1));
        },
        {
            STACK_POP(CARG(1));
            STACK_POP(CARG(0));
            STACK_LEAVE;
        });
    CURRY_CLOSURE(ARG(0), CARG(0), 1);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(step_2);
    {
        ARG(2) = make_smallint(1);
        JUVIX_INT_ADD(ARG(1), ARG(1), make_smallint(1));
        TAIL_CALL_NS(0, iter);
    }

juvix_closure_step:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION(step, 1);
    {
        PREALLOC(
            CLOSURE_HEAD_SIZE + 1, { STACK_PUSH(ARG(0)); },
            { STACK_POP(ARG(0)); });
        ALLOC_CLOSURE(juvix_result, 0, LABEL_ADDR(juvix_closure_step_2), 1, 1);
        CLOSURE_ARG(juvix_result, 0) = ARG(0);
        RETURN;
    }

juvix_closure_plus:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(plus);
    {
        JUVIX_INT_ADD(juvix_result, ARG(0), ARG(1));
        RETURN_NS;
    }

    JUVIX_FUNCTION(ackermann, 2);
    {
        DECL_TMP(0);  // step
        DECL_TMP(1);  // plus 1
        PREALLOC(
            2 * CLOSURE_HEAD_SIZE + 1,
            {
                STACK_PUSH(ARG(0));
                STACK_PUSH(ARG(1));
            },
            {
                STACK_POP(ARG(1));
                STACK_POP(ARG(0));
            });
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_step), 0, 1);
        ALLOC_CLOSURE(TMP(1), 0, LABEL_ADDR(juvix_closure_plus), 1, 1);
        CLOSURE_ARG(TMP(1), 0) = make_smallint(1);
        STACK_PUSH(ARG(1));
        ARG(2) = TMP(1);
        ARG(1) = ARG(0);
        ARG(0) = TMP(0);
        CALL(0, iter, juvix_label_ackermann_1);
        STACK_POP(ARG(1));
        ASSIGN_CARGS(juvix_result, { CARG(juvix_closure_nargs) = ARG(1); });
        TAIL_CALL_CLOSURE(juvix_result);
    }

    JUVIX_FUNCTION_NS(juvix_function_main);
    {
        ARG(1) = make_smallint(11);
        ARG(0) = make_smallint(3);
        TAIL_CALL_NS(0, ackermann);
    }

    JUVIX_EPILOGUE;
    return 0;
}
