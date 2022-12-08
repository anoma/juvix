/* Basic arithmetic */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS UNUSED DECL_ARG(0)

int main() {
    JUVIX_DECL_ARGS;
    JUVIX_PROLOGUE(1);
    DECL_TMP(0);
    DECL_TMP(1);
    DECL_TMP(2);
    JUVIX_INT_MUL(TMP(0), make_smallint(2), make_smallint(3));
    JUVIX_INT_ADD(TMP(1), TMP(0), make_smallint(5));
    JUVIX_INT_SUB(TMP(2), make_smallint(15), TMP(1));
    JUVIX_INT_DIV(TMP(0), make_smallint(9), TMP(2));
    JUVIX_INT_MUL(TMP(0), make_smallint(7), TMP(0));
    JUVIX_INT_SUB(TMP(0), make_smallint(25), TMP(0));
    JUVIX_INT_MOD(TMP(0), TMP(0), make_smallint(12));
    juvix_result = TMP(0);
    JUVIX_EPILOGUE;
    return 0;
}
