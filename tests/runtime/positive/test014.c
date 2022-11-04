/* Higher-order functions */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1);        \
    DECL_ARG(2);

static constr_info_t juvix_constr_info_array[BUILTIN_UIDS_NUM] = {
    BUILTIN_UIDS_INFO};

int main() {
    JUVIX_PROLOGUE(3, JUVIX_DECL_ARGS);

    juvix_constrs_num = BUILTIN_UIDS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION(juvix_function_S, 3 + DISPATCH_STACK_SIZE);
    {
        STACK_PUSH(ARG(0));
        STACK_PUSH(ARG(2));
        STACK_PUSH(ARG(2));
        CALL_CLOSURES(ARG(1), 1, juvix_label_1);
        STACK_POP(ARG(2));
        STACK_POP(ARG(0));
        STACK_PUSH(juvix_result);
        STACK_PUSH(ARG(2));
        TAIL_CALL_CLOSURES(ARG(0), 2);
    }

juvix_closure_K:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(juvix_function_K);
    {
        juvix_result = ARG(0);
        RETURN_NS;
    }

juvix_closure_I:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION(juvix_function_I, 2);
    {
        STACK_PUSH(ARG(0));
        PREALLOC(1 + CLOSURE_SKIP, {}, {});
        ALLOC_CLOSURE(ARG(0), 0, LABEL_ADDR(juvix_closure_K), 0, 2);
        ARG(1) = ARG(0);
        STACK_POP(ARG(2));
        TAIL_CALL(0, juvix_function_S);
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        DECL_TMP(0);  // holds 1
        DECL_TMP(1);  // holds calloc I 0
        DECL_TMP(2);  // holds 2
        ARG(0) = make_smallint(1);
        CALL(0, juvix_function_I, juvix_label_2);
        TMP(0) = juvix_result;
        ALLOC_CLOSURE(TMP(1), 0, LABEL_ADDR(juvix_closure_I), 0, 1);
        STACK_PUSH(TMP(0));
        STACK_PUSH(TMP(1));
        ARG(0) = TMP(1);
        CALL(0, juvix_function_I, juvix_label_3);
        TMP(1) = STACK_TOP;
        ASSIGN_CARGS(TMP(1), { CARG(juvix_closure_nargs) = juvix_result; });
        CALL_CLOSURE(TMP(1), juvix_label_4);
        STACK_POP(TMP(1));
        STACK_POP(TMP(0));
        STACK_PUSH(TMP(1));
        ASSIGN_CARGS(juvix_result, { CARG(juvix_closure_nargs) = TMP(0); });
        CALL_CLOSURE(juvix_result, juvix_label_5);
        STACK_POP(TMP(1));
        JUVIX_INT_ADD(TMP(2), TMP(0), juvix_result);
        STACK_PUSH(TMP(2));
        STACK_PUSH(TMP(2));
        STACK_PUSH(TMP(1));
        STACK_PUSH(TMP(1));
        STACK_PUSH(TMP(1));
        STACK_PUSH(TMP(1));
        STACK_PUSH(TMP(1));
        STACK_PUSH(TMP(1));
        STACK_PUSH(TMP(1));
        STACK_PUSH(TMP(1));
        CALL_CLOSURES(TMP(1), 9, juvix_label_6);
        STACK_POP(TMP(2));
        JUVIX_INT_ADD(TMP(1), juvix_result, TMP(2));
        juvix_result = TMP(1);
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
