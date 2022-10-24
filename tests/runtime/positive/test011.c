/* Tail calls */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1);        \
    DECL_ARG(2)

int main() {
    JUVIX_PROLOGUE(3, JUVIX_DECL_ARGS);

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_NS(juvix_function_multiply);
    {
        JUVIX_INT_MUL(juvix_result, ARG(0), ARG(1));
        RETURN_NS;
    }

    JUVIX_FUNCTION_NS(juvix_function_plus);
    {
        JUVIX_INT_ADD(juvix_result, ARG(0), ARG(1));
        RETURN_NS;
    }

juvix_closure_calculate:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    ARG(2) = CARG(2);

    JUVIX_FUNCTION(juvix_function_calculate, 2);
    {
        STACK_PUSH(ARG(0));
        ARG(0) = ARG(2);
        CALL(0, juvix_function_multiply, juvix_label_1);
        STACK_POP(ARG(0));
        ARG(1) = juvix_result;
        TAIL_CALL(0, juvix_function_plus);
    }

    JUVIX_FUNCTION(juvix_function_main, 0);
    {
        DECL_STMP(0);
        ALLOC_CLOSURE(STMP(0), 1, LABEL_ADDR(juvix_closure_calculate), 2, 1);
        CLOSURE_ARG(STMP(0), 0) = make_smallint(5);
        CLOSURE_ARG(STMP(0), 1) = make_smallint(3);
        ASSIGN_CARGS(STMP(0),
                     { CARG(juvix_closure_nargs) = make_smallint(2); });
        TAIL_CALL_CLOSURE(STMP(0));
    }

    JUVIX_EPILOGUE;
    return 0;
}
