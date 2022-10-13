#ifndef JUVIX_IO_H
#define JUVIX_IO_H

#include <juvix/funcall.h>

void io_init();
void io_flush();

// If the returned value is true, `ret` constains a closure and `arg` an
// argument that should be supplied to this closure. If the returned value is
// false, `ret` contains the value in the monad.
bool io_interpret(word_t x, word_t *ret, word_t *arg);

// IO interprets a function result stored in `juvix_result`
#define IO_INTERPRET(sp, label)                                         \
    while (1) {                                                         \
        word_t juvix_io_ret, juvix_io_arg;                              \
        SAVE_MEMORY_POINTERS;                                           \
        if (io_interpret(juvix_result, &juvix_io_ret, &juvix_io_arg)) { \
            ASSERT(is_closure(juvix_io_ret));                           \
            ASSERT(get_closure_largs(juvix_io_ret) == 1);               \
            RESTORE_MEMORY_POINTERS;                                    \
            STACK_ENTER(sp, 1);                                         \
            ARG(0) = juvix_io_arg;                                      \
            CALL_CLOSURE(juvix_io_ret, sp, label);                      \
            STACK_LEAVE(sp);                                            \
        } else {                                                        \
            break;                                                      \
        }                                                               \
    }

#endif
