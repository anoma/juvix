/* Allocator for unstructured objects (macro interface) */

#include <juvix/api.h>

static unsigned long seed = 123456789UL;

static uint myrandom() {
    return ((seed = seed * 214013L + 2531011L) >> 16) & 0x7fff;
}

#define M 64
#define N \
    (M *  \
     ((PAGE_SIZE - align(sizeof(pool_t), sizeof(dword_t))) / sizeof(word_t)))

int main() {
    uint s;
    word_t *ptr = NULL;
    DECL_MEMORY_POINTER;
    alloc_init();
    juvix_memory_pointer = alloc_memory_pointer();
    for (uint i = 0; i < PAGE_SIZE / sizeof(word_t); ++i) {
        PREALLOC(1, {}, {});
        word_t *next;
        ALLOC(next, 1);
        *next = (word_t)ptr;
        ptr = next;
    }
    ASSERT(juvix_max_allocated_pages_num == juvix_allocated_pages_num);
    ASSERT(juvix_max_allocated_pages_num == 2);
    s = 0;
    while (ptr != NULL) {
        ptr = (word_t *)*ptr;
        ++s;
    }
    ASSERT(s == PAGE_SIZE / sizeof(word_t));
    for (uint i = 0; i < N; ++i) {
        uint n = myrandom() % (MAX_FIELDS + 1) + 1;
        word_t *next;
        PREALLOC(n, {}, {});
        ALLOC(next, n);
        *next = (word_t)ptr;
        ptr = next;
    }
    s = 0;
    while (ptr != NULL) {
        ptr = (word_t *)*ptr;
        ++s;
    }
    ASSERT(s == N);
    ASSERT(juvix_max_allocated_pages_num == juvix_allocated_pages_num);
    alloc_cleanup();
    ASSERT(juvix_max_allocated_pages_num >= M + 2);
    ASSERT(juvix_max_allocated_pages_num <= 2 + M * (MAX_FIELDS + 1));
    ASSERT(juvix_allocated_pages_num == 0);
    return s != N;
}
