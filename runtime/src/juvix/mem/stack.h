#ifndef JUVIX_MEM_STACK_H
#define JUVIX_MEM_STACK_H

#include <juvix/defs.h>
#include <juvix/limits.h>

#define DECL_STACK_POINTER register word_t *juvix_stack_pointer

typedef struct Stack_segment {
    struct Stack_segment *prev;
    // beginning of allocated space
    word_t *begin;
    // stack pointer (grows upwards)
    word_t *pointer;
    // end of allocated space
    word_t *end;
} stack_segment_t;

extern stack_segment_t *juvix_global_stack;

// The current stack pointer.
static inline word_t *stack_pointer() { return juvix_global_stack->pointer; }

void stack_init();
void stack_grow(word_t *sp);
void stack_shrink(word_t *sp);

#define STACK_ENTER(n)                                          \
    do {                                                        \
        ASSERT(n <= MAX_STACK_DELTA);                           \
        if (unlikely(!is_same_page(juvix_stack_pointer,         \
                                   juvix_stack_pointer + n))) { \
            stack_grow(juvix_stack_pointer);                    \
            juvix_stack_pointer = stack_pointer();              \
        }                                                       \
    } while (0)

#define STACK_LEAVE                                         \
    do {                                                    \
        if (unlikely(is_page_start(juvix_stack_pointer))) { \
            stack_shrink(juvix_stack_pointer);              \
            juvix_stack_pointer = stack_pointer();          \
        }                                                   \
    } while (0)

#define STACK_PUSH(val) (*juvix_stack_pointer++ = (word_t)(val))
#define STACK_POP(var) (var = *--juvix_stack_pointer)

#define STACK_SAVE (juvix_global_stack->pointer = juvix_stack_pointer)

#endif
