
#include <juvix/mem/pages.h>
#include <juvix/mem/stack.h>

#define STACK_PREV(seg) *(word_t **)(seg + PAGE_SIZE / sizeof(word_t) - 1)

word_t *stack_init() {
    word_t *seg = palloc(1);
    STACK_PREV(seg) = NULL;
    return seg + 1;
}

word_t *stack_grow(word_t *sp) {
    word_t *seg = palloc(1);
    STACK_PREV(seg) = sp;
    return seg;
}

word_t *stack_shrink(word_t *sp) {
    ASSERT(is_page_start(sp));
    ASSERT(page_start(sp) == sp);
    word_t *seg = sp;
    ASSERT(seg != NULL);
    word_t *prev = STACK_PREV(seg);
    ASSERT(prev != NULL);
    pfree(seg, 1);
    return prev;
}
