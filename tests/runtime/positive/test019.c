/* Dynamic closure extension */

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

juvix_closure_f:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    ARG(2) = CARG(2);
    JUVIX_FUNCTION_NS(juvix_function_f);
    {
        DECL_TMP(0);
        JUVIX_INT_ADD(TMP(0), ARG(1), ARG(2));
        JUVIX_INT_MUL(juvix_result, ARG(0), TMP(0));
        RETURN_NS;
    }

juvix_closure_app:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(juvix_function_app);
    {
        ASSIGN_CARGS(ARG(0), { CARG(juvix_closure_nargs) = ARG(1); });
        TAIL_CALL_CLOSURE_NS(ARG(0));
    }

juvix_closure_g:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION(juvix_function_g, 2 + DISPATCH_STACK_SIZE);
    {
        STACK_PUSH(make_smallint(1));
        STACK_PUSH(make_smallint(2));
        TAIL_CALL_CLOSURES(ARG(0), 2);
    }

    JUVIX_FUNCTION(juvix_function_h, 1 + DISPATCH_STACK_SIZE);
    {
        STACK_PUSH(ARG(1));
        TAIL_CALL_CLOSURES(ARG(0), 1);
    }

juvix_closure_inc:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION_NS(juvix_function_inc);
    {
        JUVIX_INT_ADD(juvix_result, ARG(0), make_smallint(1));
        RETURN_NS;
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        DECL_TMP(0);
        STACK_PUSH(make_smallint(10));
        STACK_PUSH(make_smallint(2));
        STACK_PUSH(make_smallint(3));
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_f), 0, 3);
        CALL_CLOSURES(TMP(0), 2, juvix_label_1);
        STACK_PUSH(juvix_result);
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_app), 0, 2);
        CALL_CLOSURES(TMP(0), 2, juvix_label_2);
        JUVIX_TRACE(juvix_result);

        STACK_PUSH(make_smallint(10));
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_f), 0, 3);
        STACK_PUSH(TMP(0));
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_g), 0, 1);
        CALL_CLOSURES(TMP(0), 2, juvix_label_3);
        JUVIX_TRACE(juvix_result);

        STACK_PUSH(make_smallint(7));
        ALLOC_CLOSURE(ARG(1), 0, LABEL_ADDR(juvix_closure_inc), 0, 1);
        ALLOC_CLOSURE(ARG(0), 0, LABEL_ADDR(juvix_closure_app), 0, 2);
        CALL(0, juvix_function_h, juvix_label_4);
        CALL_CLOSURES(juvix_result, 1, juvix_label_5);
        JUVIX_TRACE(juvix_result);

        juvix_result = OBJ_VOID;
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
