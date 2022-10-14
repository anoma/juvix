#ifndef JUVIX_API_H
#define JUVIX_API_H

#include <juvix/funcall.h>
#include <juvix/io.h>
#include <juvix/mem.h>
#include <juvix/object.h>

static inline juvix_init() {
    mem_init();
    funcall_init();
}

#define JUVIX_INIT                                 \
    juvix_init();                                  \
    juvix_memory_pointer = alloc_memory_pointer(); \
    juvix_stack_pointer = stack_pointer()

#define JUVIX_PROLOGUE(MAX_ARGS, ARG_DECLS, DISPATCH_DECLS) \
    MEM_DECLS;                                              \
    ARG_DECLS;                                              \
    FUNCALL_DECLS;                                          \
    JUVIX_INIT;                                             \
    STACK_PUSH_LABEL(LABEL_ADDR(juvix_program_end));        \
    goto juvix_program_start;                               \
    DECL_CALL_CLOSURES(MAX_ARGS);                           \
    DISPATCH_DECLS;                                         \
    juvix_program_start:

#define JUVIX_EPILOGUE                  \
    juvix_program_end:                  \
    --juvix_stack_pointer;              \
    IO_INTERPRET(juvix_io_interpreter); \
    io_print_toplevel(juvix_result);

#define DECL_TMP(k) word_t juvix_tmp_##k
#define TMP(k) juvix_tmp_##k

// Begin a function definition. `n` is the maximum number of words allocated in
// the function. `SAVE` and `RESTORE` should save and restore function arguments
// on the global stack.
#define FUNCTION_BEGIN(label, n, SAVE, RESTORE) \
    label:                                      \
    PREALLOC(n, SAVE, RESTORE)

#endif
