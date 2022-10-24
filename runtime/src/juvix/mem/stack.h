#ifndef JUVIX_MEM_STACK_H
#define JUVIX_MEM_STACK_H

#include <juvix/defs.h>
#include <juvix/limits.h>

#define DECL_STACK_POINTER register word_t *juvix_stack_pointer

// The following functions return the stack pointer
word_t *stack_init();
word_t *stack_grow(word_t *sp);
word_t *stack_shrink(word_t *sp);

#define STACK_ENTER(n)                                             \
    do {                                                           \
        ASSERT(n <= MAX_STACK_DELTA);                              \
        if (unlikely(is_next_page(juvix_stack_pointer, n))) {      \
            juvix_stack_pointer = stack_grow(juvix_stack_pointer); \
        }                                                          \
    } while (0)

#define STACK_LEAVE                                                  \
    do {                                                             \
        if (unlikely(is_page_start(juvix_stack_pointer))) {          \
            juvix_stack_pointer = stack_shrink(juvix_stack_pointer); \
        }                                                            \
    } while (0)

#define STACK_PUSH(val) (*juvix_stack_pointer++ = (word_t)(val))
#define STACK_POP(var) (var = *--juvix_stack_pointer)
#define STACK_TOP (*(juvix_stack_pointer - 1))
#define STACK_POPT --juvix_stack_pointer

#endif
