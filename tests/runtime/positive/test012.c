/* Tracing & strings */

#include <juvix/api.h>

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 1)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {
    BUILTIN_UIDS_INFO, {"box", 0, APP_FIXITY}};

int main() {
    DECL_ARG(0);
    DECL_ARG(1);
    DECL_ARG(2);
    JUVIX_PROLOGUE(3);

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
