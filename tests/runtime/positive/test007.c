/* Prologue and epilogue */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS UNUSED DECL_ARG(0)

int main() {
    JUVIX_PROLOGUE(1, JUVIX_DECL_ARGS);
    juvix_result = make_smallint(789);
    JUVIX_EPILOGUE;
    return 0;
}
