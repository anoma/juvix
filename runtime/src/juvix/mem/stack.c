
#include <juvix/mem/pages.h>
#include <juvix/mem/stack.h>

stack_segment_t *juvix_global_stack;

static void seg_init(stack_segment_t *seg) {
    seg->prev = NULL;
    seg->begin = palign((char *)seg + sizeof(stack_segment_t), sizeof(word_t));
    seg->end = (char *)seg + PAGE_SIZE;
    seg->pointer = seg->begin;
}

void stack_init() {
    juvix_global_stack = palloc(1);
    seg_init(juvix_global_stack);
}

void stack_grow(word_t *sp, size_t n) {
    ASSERT(juvix_global_stack != NULL);
    ASSERT(sp >= juvix_global_stack->pointer);
    ASSERT(sp <= juvix_global_stack->end);
    ASSERT(n <= MAX_STACK_DELTA);
    juvix_global_stack->pointer = sp;
    stack_segment_t *seg = palloc(1);
    seg_init(seg);
    seg->prev = juvix_global_stack;
    seg->pointer += n;
    juvix_global_stack = seg;
}

void stack_shrink(word_t *sp, size_t n) {
    ASSERT(juvix_global_stack != NULL);
    ASSERT(sp >= juvix_global_stack->pointer);
    ASSERT(sp <= juvix_global_stack->end);
    ASSERT(n <= MAX_STACK_DELTA);
    stack_segment_t *seg = juvix_global_stack->prev;
    ASSERT(seg != NULL);
    ASSERT(sp < juvix_global_stack->begin + n);
    n -= sp - juvix_global_stack->begin;
    seg->pointer -= n;
    ASSERT(seg->pointer >= seg->begin);
    pfree(juvix_global_stack, 1);
    juvix_global_stack = seg;
}
