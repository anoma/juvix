/* Prologue and epilogue */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS __attribute__((unused)) DECL_ARG(0)

#define JUVIX_DECL_DISPATCH \
    juvix_dispatch_label_0: \
    ARG(0) = *juvix_ccl_sp; \
    DISPATCH(juvix_dispatch_label)

#define JUVIX_INIT_DISPATCH INIT_DISPATCH(0, juvix_dispatch_label_0)

int main() {
    JUVIX_PROLOGUE(1, JUVIX_DECL_ARGS, JUVIX_DECL_DISPATCH,
                   JUVIX_INIT_DISPATCH);
    juvix_result = make_smallint(789);
    JUVIX_EPILOGUE;
    return 0;
}
