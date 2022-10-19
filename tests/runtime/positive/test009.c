/* Direct call */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1);        \
    DECL_ARG(2)

#define JUVIX_DECL_DISPATCH   \
    juvix_dispatch_label_2:   \
    ARG(2) = *juvix_ccl_sp++; \
    juvix_dispatch_label_1:   \
    ARG(1) = *juvix_ccl_sp++; \
    juvix_dispatch_label_0:   \
    ARG(0) = *juvix_ccl_sp;   \
    DISPATCH(juvix_dispatch_label)

#define JUVIX_INIT_DISPATCH                   \
    INIT_DISPATCH(0, juvix_dispatch_label_0); \
    INIT_DISPATCH(1, juvix_dispatch_label_1); \
    INIT_DISPATCH(2, juvix_dispatch_label_2)

int main() {
    JUVIX_PROLOGUE(3, JUVIX_DECL_ARGS, JUVIX_DECL_DISPATCH,
                   JUVIX_INIT_DISPATCH);

    STACK_ENTER(1);
    CALL(0, juvix_function_main, juvix_label_0);
    STACK_LEAVE;
    goto juvix_program_end;

    JUVIX_FUNCTION_NOALLOC(juvix_function_calculate);
    {
        DECL_STMP(0);
        JUVIX_INT_MUL(STMP(0), ARG(2), ARG(1));
        JUVIX_INT_ADD(STMP(0), STMP(0), ARG(0));
        juvix_result = STMP(0);
        RETURN;
    }

    JUVIX_FUNCTION_NOALLOC(juvix_function_main);
    {
        STACK_ENTER(1);
        ARG(2) = make_smallint(2);
        ARG(1) = make_smallint(3);
        ARG(0) = make_smallint(5);
        CALL(1, juvix_function_calculate, juvix_label_1);
        STACK_LEAVE;
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
