/* Prologue and epilogue */

#include <juvix/api.h>

int main() {
    DECL_ARG(0);
    DECL_ARG(1);
    DECL_ARG(2);
    JUVIX_PROLOGUE(3);
    juvix_result = make_smallint(789);
    JUVIX_EPILOGUE;
    return 0;
}
