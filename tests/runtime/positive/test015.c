/* Branching & matching */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_REG_ARG(0);    \
    DECL_REG_ARG(1)

#define CONSTRS_NUM (BUILTIN_UIDS_NUM + 2)

static constr_info_t juvix_constr_info_array[CONSTRS_NUM] = {
    BUILTIN_UIDS_INFO, {"nil"}, {"cons"}};

#define UID_NIL FIRST_USER_UID
#define UID_CONS (FIRST_USER_UID + 1)

#define CONSTR_NIL MAKE_HEADER(UID_NIL, 0)

int main() {
    JUVIX_PROLOGUE(2, JUVIX_DECL_ARGS);

    juvix_constrs_num = CONSTRS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

    JUVIX_FUNCTION_LEAF(juvix_function_hd);
    {
        juvix_result = CONSTR_ARG(ARG(0), 0);
        RETURN_LEAF;
    }

    JUVIX_FUNCTION_LEAF(juvix_function_tl);
    {
        juvix_result = CONSTR_ARG(ARG(0), 1);
        RETURN_LEAF;
    }

    JUVIX_FUNCTION_LEAF(juvix_function_null);
    {
        switch (ARG(0)) {
            case CONSTR_NIL:
                juvix_result = BOOL_TRUE;
                break;
            default:  // cons
                juvix_result = BOOL_FALSE;
                break;
        }
        RETURN_LEAF;
    }

    JUVIX_FUNCTION(
        juvix_function_map, 3, 3,
        {
            STACK_PUSH(ARG(0));
            STACK_PUSH(ARG(1));
        },
        {
            STACK_POP(ARG(1));
            STACK_POP(ARG(0));
        });
    {
        switch (ARG(1)) {
            case CONSTR_NIL:
                juvix_result = CONSTR_NIL;
                RETURN;
            default: {
                DECL_TMP(0);
                DECL_TMP(1);
                TMP(0) = ARG(0);
                TMP(1) = ARG(1);
                STACK_PUSH(TMP(0));
                STACK_PUSH(TMP(1));
                ARG(0) = TMP(0);
                ARG(1) = CONSTR_ARG(TMP(1), 1);
                CALL(0, juvix_function_map, juvix_label_1);
                STACK_POP(TMP(1));
                STACK_POP(TMP(0));
                STACK_PUSH(juvix_result);
                ASSIGN_CARGS(TMP(0), {
                    CARG(juvix_closure_nargs) = CONSTR_ARG(TMP(1), 0);
                });
                CALL_CLOSURE(TMP(0), juvix_label_2);
                STACK_POP(TMP(0));
                TMP(1) = juvix_result;
                ALLOC_CONSTR_BOXED(juvix_result, UID_CONS, 2);
                CONSTR_ARG(juvix_result, 0) = TMP(1);
                CONSTR_ARG(juvix_result, 1) = TMP(0);
                RETURN;
            }
        }
    }

    JUVIX_FUNCTION(
        juvix_function_map_1, 3, 3,
        {
            STACK_PUSH(ARG(0));
            STACK_PUSH(ARG(1));
        },
        {
            STACK_POP(ARG(1));
            STACK_POP(ARG(0));
        });
    {
        STACK_PUSH(ARG(0));
        STACK_PUSH(ARG(1));
        ARG(0) = ARG(1);
        CALL(0, juvix_function_null, juvix_label_3);
        STACK_POP(ARG(1));
        STACK_POP(ARG(0));
        JUVIX_BRANCH(
            juvix_result, { juvix_result = CONSTR_NIL; },
            {
                DECL_TMP(0);
                DECL_TMP(1);
                STACK_PUSH(ARG(0));
                ARG(0) = ARG(1);
                CALL(0, juvix_function_tl, juvix_label_4);
                ARG(0) = STACK_TOP;
                STACK_PUSH(ARG(1));
                ARG(1) = juvix_result;
                CALL(0, juvix_function_map_1, juvix_label_5);
                STACK_POP(TMP(1));
                STACK_POP(TMP(0));
                STACK_PUSH(TMP(0));
                STACK_PUSH(juvix_result);
                ARG(0) = TMP(1);
                CALL(0, juvix_function_hd, juvix_label_6);
                STACK_POP(TMP(1));  // TMP(1) = map tail
                STACK_POP(TMP(0));  // TMP(0) = function
                STACK_PUSH(TMP(1));
                ASSIGN_CARGS(TMP(0),
                             { CARG(juvix_closure_nargs) = juvix_result; });
                CALL_CLOSURE(TMP(0), juvix_label_7);
                STACK_POP(TMP(1));
                ALLOC_CONSTR_BOXED(TMP(0), UID_CONS, 2);
                CONSTR_ARG(TMP(0), 0) = juvix_result;
                CONSTR_ARG(TMP(0), 1) = TMP(1);
                juvix_result = TMP(0);
            });
        RETURN;
    }

juvix_closure_add_one:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION_LEAF(juvix_function_add_one);
    {
        JUVIX_INT_ADD(juvix_result, ARG(0), make_smallint(1));
        RETURN_LEAF;
    }

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA, 6 + 1 + CLOSURE_SKIP,
                   {}, {});
    {
        DECL_TMP(0);  // cons 0 (cons 1 nil)
        DECL_TMP(1);  // cons 1 nil
        DECL_TMP(2);  // calloc add_one 0
        ALLOC_CONSTR_BOXED(TMP(1), UID_CONS, 2);
        CONSTR_ARG(TMP(1), 0) = make_smallint(1);
        CONSTR_ARG(TMP(1), 1) = CONSTR_NIL;
        ALLOC_CONSTR_BOXED(TMP(0), UID_CONS, 2);
        CONSTR_ARG(TMP(0), 0) = make_smallint(0);
        CONSTR_ARG(TMP(0), 1) = TMP(1);
        ALLOC_CLOSURE(TMP(2), 0, LABEL_ADDR(juvix_closure_add_one), 0, 1);
        ARG(0) = TMP(2);
        ARG(1) = TMP(0);
        CALL(0, juvix_function_map, juvix_label_8);
        JUVIX_TRACE(juvix_result);
        ARG(0) = TMP(2);
        ARG(1) = TMP(0);
        CALL(0, juvix_function_map_1, juvix_label_9);
        RETURN;
    }

    JUVIX_EPILOGUE;
    return 0;
}
