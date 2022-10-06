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
// The beginning of the current stack segment.
static inline word_t *stack_begin() { return juvix_global_stack->begin; }
// The end of the current stack segment.
static inline word_t *stack_end() { return juvix_global_stack->end; }

void stack_init();
void stack_grow(word_t *sp, size_t n);
void stack_shrink(word_t *sp, size_t n);

#define STACK_PUSH(sp, n, beg, end)   \
    do {                              \
        ASSERT(n <= MAX_STACK_DELTA); \
        sp += n;                      \
        if (unlikely(sp > end)) {     \
            stack_grow(sp - n, n);    \
            sp = stack_pointer();     \
            beg = stack_begin();      \
            end = stack_end();        \
        }                             \
    } while (0)

#define STACK_POP(sp, n, beg, end)    \
    do {                              \
        ASSERT(n <= MAX_STACK_DELTA); \
        sp -= n;                      \
        if (unlikely(sp < beg)) {     \
            stack_shrink(sp + n, n);  \
            sp = stack_pointer();     \
            beg = stack_begin();      \
            end = stack_end();        \
        }                             \
    } while (0)

#endif
