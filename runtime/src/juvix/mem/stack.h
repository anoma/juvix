#ifndef JUVIX_MEM_STACK_H
#define JUVIX_MEM_STACK_H

#include <juvix/defs.h>
#include <juvix/limits.h>

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

#define STACK_ENTER(sp, n)                      \
    do {                                        \
        ASSERT(n <= MAX_STACK_DELTA);           \
        if (unlikely(!SAME_PAGE(sp, sp + n))) { \
            stack_grow(sp);                     \
            sp = stack_pointer();               \
        }                                       \
    } while (0)

#define STACK_LEAVE(sp)                    \
    do {                                   \
        if (unlikely(IS_PAGE_START(sp))) { \
            stack_shrink(sp);              \
            sp = stack_pointer();          \
        }                                  \
    } while (0)

#define STACK_PUSH(sp, val) (*sp++ = (word_t)(val))
#define STACK_POP(sp, var) (var = *--sp)

#define STACK_SAVE(sp) (juvix_global_stack->pointer = sp)

#endif
