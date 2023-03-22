#ifndef JUVIX_IO_H
#define JUVIX_IO_H

#include <juvix/funcall/funcall.h>

void io_init();
void io_flush();

void io_trace(word_t x);
void io_print_toplevel(word_t x);

// If the returned value is true, `ret` constains a closure and `arg` an
// argument that should be supplied to this closure. If the returned value is
// false, `ret` contains the value in the monad.
bool io_interpret(word_t x, word_t *ret, word_t *arg);

// IO interprets a function result stored in `juvix_result`.
#define IO_INTERPRET                                                     \
    STACK_PUSH_ADDR(LABEL_ADDR(juvix_io_interpret_end));                 \
    juvix_io_interpret:                                                  \
    while (1) {                                                          \
        word_t juvix_io_ret, juvix_io_arg;                               \
        SAVE_MEMORY_POINTERS;                                            \
        if (io_interpret(juvix_result, &juvix_io_ret, &juvix_io_arg)) {  \
            ASSERT(is_closure(juvix_io_ret));                            \
            ASSERT(get_closure_largs(juvix_io_ret) == 1);                \
            RESTORE_MEMORY_POINTERS;                                     \
            STACK_ENTER(2);                                              \
            STACK_PUSH(juvix_io_ret);                                    \
            juvix_result = juvix_io_arg;                                 \
            CALL(0, juvix_io_interpret, juvix_io_interpret_label_0);     \
            STACK_POP(juvix_io_ret);                                     \
            ASSIGN_CARGS(juvix_io_ret,                                   \
                         { CARG(juvix_closure_nargs) = juvix_result; }); \
            CALL_CLOSURE(juvix_io_ret, juvix_io_interpret_label_1);      \
            STACK_LEAVE;                                                 \
        } else {                                                         \
            RESTORE_MEMORY_POINTERS;                                     \
            juvix_result = juvix_io_ret;                                 \
            RETURN_NS;                                                   \
        }                                                                \
    }                                                                    \
    juvix_io_interpret_end:                                              \
    io_flush();

#endif
