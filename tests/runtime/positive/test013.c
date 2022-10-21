/* Tail calls */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS \
    DECL_ARG(0);        \
    DECL_ARG(1)

static constr_info_t juvix_constr_info_array[BUILTIN_UIDS_NUM] = {
    BUILTIN_UIDS_INFO};

int main() {
    JUVIX_PROLOGUE(2, JUVIX_DECL_ARGS);

    juvix_constrs_num = BUILTIN_UIDS_NUM;
    juvix_constr_info = juvix_constr_info_array;

    CALL(0, juvix_function_main, juvix_label_0);
    goto juvix_program_end;

juvix_closure_const:
    ARG(0) = CARG(0);
    ARG(1) = CARG(1);
    JUVIX_FUNCTION_LEAF(juvix_function_const);
    {
        juvix_result = ARG(0);
        RETURN;
    }

juvix_closure_print:
    ARG(0) = CARG(0);
    JUVIX_FUNCTION(
        juvix_function_print, 1, 2, { STACK_PUSH(ARG(0)); },
        { STACK_POP(ARG(0)); });
    {
        ALLOC_CONSTR_BOXED(juvix_result, UID_WRITE, 1);
        CONSTR_ARG(juvix_result, 0) = ARG(0);
        RETURN;
    }

    JUVIX_FUNCTION(
        juvix_function_sequence, 2, 2 + CLOSURE_SKIP + 3,
        {
            STACK_PUSH(ARG(0));
            STACK_PUSH(ARG(1));
        },
        {
            STACK_POP(ARG(1));
            STACK_POP(ARG(0));
        });
    {
        DECL_TMP(0);
        ALLOC_CLOSURE(TMP(0), 0, LABEL_ADDR(juvix_closure_const), 1, 1);
        CLOSURE_ARG(TMP(0), 0) = ARG(1);
        ALLOC_CONSTR_BOXED(juvix_result, UID_BIND, 2);
        CONSTR_ARG(juvix_result, 0) = ARG(0);
        CONSTR_ARG(juvix_result, 1) = TMP(0);
        RETURN;
    }

#define PRINT_CLOSURE_SIZE (1 + CLOSURE_SKIP)
#define WRITE_SIZE 2
#define RETURN_SIZE 2
#define BIND_SIZE 3

    JUVIX_FUNCTION(juvix_function_main, MAX_STACK_DELTA,
                   3 * PRINT_CLOSURE_SIZE + 6 * WRITE_SIZE + 3 * RETURN_SIZE +
                       3 * BIND_SIZE + 2,
                   {}, {});
    {
        DECL_TMP(0);
        DECL_TMP(1);
        DECL_TMP(2);
        ALLOC_CONSTR_BOXED(TMP(0), UID_WRITE, 1);
        CONSTR_ARG(TMP(0), 0) = make_smallint(1);
        ALLOC_CONSTR_BOXED(TMP(1), UID_WRITE, 1);
        CONSTR_ARG(TMP(1), 0) = make_smallint(2);
        ARG(0) = TMP(0);
        ARG(1) = TMP(1);
        CALL(0, juvix_function_sequence, juvix_label_1);
        TMP(0) = juvix_result;
        ALLOC_CONSTR_BOXED(TMP(1), UID_WRITE, 1);
        CONSTR_ARG(TMP(1), 0) = make_smallint(3);
        ARG(0) = TMP(0);
        ARG(1) = TMP(1);
        CALL(0, juvix_function_sequence, juvix_label_2);
        TMP(0) = juvix_result;
        ALLOC_CONSTR_BOXED(TMP(1), UID_WRITE, 1);
        CONSTR_ARG(TMP(1), 0) = make_smallint(4);
        ARG(0) = TMP(0);
        ARG(1) = TMP(1);
        CALL(0, juvix_function_sequence, juvix_label_3);
        TMP(0) = juvix_result;
        ALLOC_CONSTR_BOXED(TMP(1), UID_WRITE, 1);
        CONSTR_ARG(TMP(1), 0) = make_smallint(5);
        ARG(0) = TMP(0);
        ARG(1) = TMP(1);
        CALL(0, juvix_function_sequence, juvix_label_4);
        TMP(0) = juvix_result;
        ALLOC_CSTRING(TMP(2), "\n");
        ALLOC_CONSTR_BOXED(TMP(1), UID_WRITE, 1);
        CONSTR_ARG(TMP(1), 0) = TMP(2);
        ARG(0) = TMP(0);
        ARG(1) = TMP(1);
        CALL(0, juvix_function_sequence, juvix_label_5);
        TMP(0) = juvix_result;

        // TMP(0) is printing "12345\n"
        // TMP(1) is free
        // TMP(2) is the cstring "\n"
        DECL_TMP(3);  // calloc print 0
        DECL_TMP(4);  // return X
        DECL_TMP(5);  // bind print "\n"
        DECL_TMP(6);  // bind print 2
        DECL_TMP(7);  // bind print 1
        ALLOC_CLOSURE(TMP(3), 0, LABEL_ADDR(juvix_closure_print), 0, 1);
        ALLOC_CONSTR_BOXED(TMP(4), UID_RETURN, 1);
        CONSTR_ARG(TMP(4), 0) = TMP(2);
        ALLOC_CONSTR_BOXED(TMP(5), UID_BIND, 2);
        CONSTR_ARG(TMP(5), 0) = TMP(4);
        CONSTR_ARG(TMP(5), 1) = TMP(3);
        ALLOC_CONSTR_BOXED(TMP(4), UID_RETURN, 1);
        CONSTR_ARG(TMP(4), 0) = make_smallint(2);
        ALLOC_CONSTR_BOXED(TMP(6), UID_BIND, 2);
        CONSTR_ARG(TMP(6), 0) = TMP(4);
        CONSTR_ARG(TMP(6), 1) = TMP(3);
        ALLOC_CONSTR_BOXED(TMP(4), UID_RETURN, 1);
        CONSTR_ARG(TMP(4), 0) = make_smallint(1);
        ALLOC_CONSTR_BOXED(TMP(7), UID_BIND, 2);
        CONSTR_ARG(TMP(7), 0) = TMP(4);
        CONSTR_ARG(TMP(7), 1) = TMP(3);
        ARG(0) = TMP(6);
        ARG(1) = TMP(5);
        CALL(0, juvix_function_sequence, juvix_label_6);
        ARG(0) = TMP(7);
        ARG(1) = juvix_result;
        CALL(0, juvix_function_sequence, juvix_label_7);
        ARG(0) = juvix_result;
        ARG(1) = TMP(0);
        TAIL_CALL(0, juvix_function_sequence);
    }

    JUVIX_EPILOGUE;
    return 0;
}
