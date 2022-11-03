/* Stack */

#include <juvix/mem/pages.h>
#include <juvix/mem/stack.h>

static unsigned long seed = 123456789UL;

static uint myrandom() {
    return ((seed = seed * 214013L + 2531011L) >> 16) & 0x7fff;
}

#define N 1024
#define M (PAGE_SIZE / sizeof(word_t) + 16)

int main() {
    DECL_STACK_POINTER;
    juvix_stack_pointer = stack_init();
    word_t *ptr = juvix_stack_pointer;
    STACK_ENTER(1);
    STACK_PUSH(3);
    ASSERT(STACK_TOP == 3);
    STACK_POPT;
    STACK_LEAVE;
    ASSERT(juvix_stack_pointer == ptr);
    ASSERT(juvix_allocated_pages_num == 1);
    for (uint i = 0; i < M; ++i) {
        STACK_ENTER(3);
        STACK_PUSH(1);
        STACK_PUSH(2);
        STACK_PUSH(3);
    }
    ASSERT(juvix_allocated_pages_num == juvix_max_allocated_pages_num);
    ASSERT(juvix_allocated_pages_num == 4);
    for (uint i = 0; i < M; ++i) {
        ASSERT(STACK_TOP == 3);
        STACK_POPT;
        ASSERT(STACK_TOP == 2);
        STACK_POPT;
        ASSERT(STACK_TOP == 1);
        STACK_POPT;
        STACK_LEAVE;
    }
    ASSERT(juvix_stack_pointer == ptr);
    ASSERT(juvix_allocated_pages_num == 1);
    for (uint i = 0; i < M; ++i) {
        STACK_ENTER(2);
        STACK_PUSH(1);
        STACK_PUSH(2);
    }
    ASSERT(juvix_max_allocated_pages_num == 4);
    ASSERT(juvix_allocated_pages_num == 3);
    for (uint i = 0; i < M; ++i) {
        ASSERT(STACK_TOP == 2);
        STACK_POPT;
        ASSERT(STACK_TOP == 1);
        STACK_POPT;
        STACK_LEAVE;
    }
    ASSERT(juvix_stack_pointer == ptr);
    ASSERT(juvix_allocated_pages_num == 1);
    for (uint i = 0; i < N; ++i) {
        uint n = myrandom() % MAX_STACK_DELTA + 1;
        STACK_ENTER(n);
        for (uint j = 0; j < n; ++j) {
            STACK_PUSH(j);
        }
        STACK_PUSH(n);
    }
    ASSERT(juvix_allocated_pages_num == juvix_max_allocated_pages_num);
    for (uint i = 0; i < N; ++i) {
        uint n;
        STACK_POP(n);
        for (uint j = 0; j < n; ++j) {
            uint k;
            STACK_POP(k);
            ASSERT(k == n - j - 1);
        }
        STACK_LEAVE;
    }
    ASSERT(juvix_stack_pointer == ptr);
    ASSERT(juvix_allocated_pages_num == 1);
    return 0;
}
