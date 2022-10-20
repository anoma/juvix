#ifndef JUVIX_IO_H
#define JUVIX_IO_H

#include <juvix/funcall/funcall.h>

void io_init();
void io_flush();

void io_print_toplevel(word_t x);

// If the returned value is true, `ret` constains a closure and `arg` an
// argument that should be supplied to this closure. If the returned value is
// false, `ret` contains the value in the monad.
bool io_interpret(word_t x, word_t *ret, word_t *arg);

// IO interprets a function result stored in `juvix_result`
#define IO_INTERPRET(label)                                              \
    while (1) {                                                          \
        word_t juvix_io_ret, juvix_io_arg;                               \
        SAVE_MEMORY_POINTERS;                                            \
        if (io_interpret(juvix_result, &juvix_io_ret, &juvix_io_arg)) {  \
            ASSERT(is_closure(juvix_io_ret));                            \
            ASSERT(get_closure_largs(juvix_io_ret) == 1);                \
            RESTORE_MEMORY_POINTERS;                                     \
            STACK_ENTER(1);                                              \
            ASSIGN_CARGS(juvix_io_ret,                                   \
                         { CARG(juvix_closure_nargs) = juvix_io_arg; }); \
            CALL_CLOSURE(juvix_io_ret, label);                           \
            STACK_LEAVE;                                                 \
        } else {                                                         \
            break;                                                       \
        }                                                                \
    }

#endif
