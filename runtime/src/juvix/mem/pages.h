#ifndef JUVIX_MEM_PAGES_H
#define JUVIX_MEM_PAGES_H

#include <juvix/defs.h>

extern size_t juvix_max_allocated_pages_num;
extern size_t juvix_allocated_pages_num;

// Allocate a continuous block of memory of `n` pages (`n * PAGE_SIZE` bytes).
// The memory is PAGE_SIZE-aligned.
void *palloc(size_t n);
// Free a previously allocated block of `n` pages.
void pfree(void *ptr, size_t n);

static inline bool is_same_page(void *p1, void *p2) {
    return ((((uintptr_t)p1) ^ ((uintptr_t)p2)) & ~PAGE_MASK) == 0;
}

static inline bool is_page_start(void *p) {
    return (((uintptr_t)(p)) & PAGE_MASK) == 0;
}

#endif
