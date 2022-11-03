/* Closure extension */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1)

static constr_info_t juvix_constr_info_array[BUILTIN_UIDS_NUM] = {
    BUILTIN_UIDS_INFO};

int main() {
    JUVIX_PROLOGUE(2, JUVIX_DECL_ARGS);

    juvix_constrs_num = BUILTIN_UIDS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION(juvix_function_ext_10, 2);
    {
        PREALLOC(
            get_nfields(ARG(0)) + 2, { STACK_PUSH(ARG(0)); },
            { STACK_POP(ARG(0)); });
        EXTEND_CLOSURE(juvix_result, ARG(0), 1, {
            CLOSURE_ARG(juvix_result, juvix_closure_nargs) = make_smallint(10);
        });
        RETURN;
    }

    JUVIX_FUNCTION_NS(juvix_function_app_0);
    {
        DECL_TMP(0);
        TMP(0) = ARG(0);
        ASSIGN_CARGS(TMP(0), { CARG(juvix_closure_nargs) = make_smallint(1); });
        TAIL_CALL_CLOSURE_NS(TMP(0));
    }

    JUVIX_FUNCTION(juvix_function_f, 1);
    {
        CALL(0, juvix_function_ext_10, juvix_label_f_1);
        ARG(0) = juvix_result;
        TAIL_CALL(0, juvix_function_app_0);
    }

juvix_closure_plus:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(juvix_function_plus);
    {
        JUVIX_INT_ADD(juvix_result, ARG(0), ARG(1));
        RETURN_NS;
    }

juvix_closure_minus:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(juvix_function_minus);
    {
        JUVIX_INT_SUB(juvix_result, ARG(0), ARG(1));
        RETURN_NS;
    }

juvix_closure_mult:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_NS(juvix_function_mult);
    {
        JUVIX_INT_MUL(juvix_result, ARG(0), ARG(1));
        RETURN_NS;
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA);
    {
        PREALLOC(1 + CLOSURE_SKIP, {}, {});
        ALLOC_CLOSURE(ARG(0), 0, LABEL_ADDR(juvix_closure_plus), 0, 2);
        CALL(0, juvix_function_f, juvix_label_1);
        JUVIX_TRACE(juvix_result);
        PREALLOC(1 + CLOSURE_SKIP, {}, {});
        ALLOC_CLOSURE(ARG(0), 0, LABEL_ADDR(juvix_closure_minus), 0, 2);
        CALL(0, juvix_function_f, juvix_label_2);
        JUVIX_TRACE(juvix_result);
        PREALLOC(1 + CLOSURE_SKIP, {}, {});
        ALLOC_CLOSURE(ARG(0), 0, LABEL_ADDR(juvix_closure_mult), 0, 2);
        CALL(0, juvix_function_f, juvix_label_3);
        JUVIX_TRACE(juvix_result);
        juvix_result = OBJ_VOID;
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
