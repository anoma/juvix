/* Tracing & strings */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS UNUSED DECL_ARG(0)

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 1)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {BUILTIN_UIDS_INFO,
                                                             {"box"}};

int main() {
    JUVIX_PROLOGUE(1, JUVIX_DECL_ARGS);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    DECL_TMP(0);
    DECL_TMP(1);
    JUVIX_TRACE(make_smallint(987));
    ALLOC_CSTRING(TMP(0), "seven");
    JUVIX_TRACE(TMP(0));
    ALLOC_CONSTR_BOXED(TMP(1), FIRST_USER_UID, 1);
    CONSTR_ARG(TMP(1), 0) = TMP(0);
    JUVIX_TRACE(TMP(1));
    juvix_result = OBJ_VOID;

    JUVIX_EPILOGUE;
    return 0;
}
