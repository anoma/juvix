/* Dynamic closure extension with apply */

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
        ASSIGN_CARGS(ARG(0), {
            CARG(juvix_closure_nargs) = make_smallint(2);
            CARG(juvix_closure_nargs + 1) = make_smallint(1);
        });
        TAIL_APPLY_2(ARG(0));
    }

    JUVIX_FUNCTION(juvix_function_h, 1 + DISPATCH_STACK_SIZE);
    {
        ASSIGN_CARGS(ARG(0), { CARG(juvix_closure_nargs) = ARG(1); });
        TAIL_APPLY_1(ARG(0));
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
        DECL_TMP(1);
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_f), 0, 3);
        ASSIGN_CARGS(TMP(0), {
            CARG(juvix_closure_nargs) = make_smallint(3);
            CARG(juvix_closure_nargs + 1) = make_smallint(2);
        });
        APPLY_2(TMP(0), juvix_label_1);
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_app), 0, 2);
        ASSIGN_CARGS(TMP(0), {
            CARG(juvix_closure_nargs) = juvix_result;
            CARG(juvix_closure_nargs + 1) = make_smallint(10);
        });
        APPLY_2(TMP(0), juvix_label_2);
        JUVIX_TRACE(juvix_result);

        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_f), 0, 3);
        ALLOC_CLOSURE(TMP(1), 0, LABEL_ADDR(juvix_closure_g), 0, 1);
        ASSIGN_CARGS(TMP(1), {
            CARG(juvix_closure_nargs) = TMP(0);
            CARG(juvix_closure_nargs + 1) = make_smallint(10);
        });
        APPLY_2(TMP(1), juvix_label_3);
        JUVIX_TRACE(juvix_result);

        ALLOC_CLOSURE(ARG(1), 0, LABEL_ADDR(juvix_closure_inc), 0, 1);
        ALLOC_CLOSURE(ARG(0), 0, LABEL_ADDR(juvix_closure_app), 0, 2);
        CALL(0, juvix_function_h, juvix_label_4);
        ASSIGN_CARGS(juvix_result,
                     { CARG(juvix_closure_nargs) = make_smallint(7); });
        APPLY_1(juvix_result, juvix_label_5);
        JUVIX_TRACE(juvix_result);

        juvix_result = OBJ_VOID;
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
