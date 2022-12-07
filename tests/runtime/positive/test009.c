/* Direct call */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1);        \
    DECL_ARG(2)

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(3);

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_NS(juvix_function_calculate);
    {
        DECL_STMP(0);
        JUVIX_INT_MUL(STMP(0), ARG(2), ARG(1));
        JUVIX_INT_ADD(STMP(0), STMP(0), ARG(0));
        juvix_result = STMP(0);
        RETURN_NS;
    }

    JUVIX_FUNCTION(juvix_function_main, 1);
    {
        ARG(2) = make_smallint(2);
        ARG(1) = make_smallint(3);
        ARG(0) = make_smallint(5);
        CALL(1, juvix_function_calculate, juvix_label_1);
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
