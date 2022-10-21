/* Indirect call */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1);        \
    DECL_ARG(2)

int main() {
    JUVIX_PROLOGUE(3, JUVIX_DECL_ARGS);

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

juvix_closure_calculate:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    ARG(2) = CARG(2);

    JUVIX_FUNCTION_LEAF(juvix_function_calculate);
    {
        DECL_STMP(0);
        JUVIX_INT_MUL(STMP(0), ARG(2), ARG(1));
        JUVIX_INT_ADD(STMP(0), STMP(0), ARG(0));
        juvix_result = STMP(0);
        RETURN_LEAF;
    }

    JUVIX_FUNCTION(juvix_function_main, 1, 3 + CLOSURE_SKIP, {}, {});
    {
        DECL_STMP(0);
        ALLOC_CLOSURE(STMP(0), 1, LABEL_ADDR(juvix_closure_calculate), 2, 1);
        CLOSURE_ARG(STMP(0), 0) = make_smallint(5);
        CLOSURE_ARG(STMP(0), 1) = make_smallint(3);
        ASSIGN_CARGS(STMP(0),
                     { CARG(juvix_closure_nargs) = make_smallint(2); });
        CALL_CLOSURE(STMP(0), juvix_label_1);
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
