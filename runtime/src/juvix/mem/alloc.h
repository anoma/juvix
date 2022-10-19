#ifndef JUVIX_MEM_ALLOC_H
#define JUVIX_MEM_ALLOC_H

/*
 This module provides an allocator for unstructured objects. When using the
 allocated memory one needs to be mindful of C alignment and aliasing rules:
 https://en.cppreference.com/w/c/language/object
 https://stefansf.de/post/type-based-alias-analysis/
*/

#include <juvix/mem/gens.h>
#include <juvix/mem/pages.h>

#define DECL_MEMORY_POINTER register word_t *juvix_memory_pointer

extern generation_t *alloc_youngest_generation;

// Initialise the allocator module.
void alloc_init();
void alloc_cleanup();
// Allocate more memory pages.
void alloc_pages(bool can_gc);

static inline word_t *alloc_memory_pointer() {
    return alloc_youngest_generation->memory->free_begin;
}

static inline void alloc_save_memory_pointer(word_t *ptr) {
    alloc_youngest_generation->memory->free_begin = ptr;
}

#define SAVE_MEMORY_POINTERS alloc_save_memory_pointer(juvix_memory_pointer);
#define RESTORE_MEMORY_POINTERS juvix_memory_pointer = alloc_memory_pointer();

// Preallocate n words. `SAVE` and `RESTORE` should save and restore live local
// variables on the global stack (can lauch GC which needs access to these
// variables).
#define PREALLOC(n, SAVE, RESTORE)                                            \
    if (unlikely(                                                             \
            !is_same_page(juvix_memory_pointer, juvix_memory_pointer + n))) { \
        ASSERT(n < MAX_FUNCTION_MEMORY);                                      \
        SAVE;                                                                 \
        SAVE_MEMORY_POINTERS;                                                 \
        alloc_pages(true);                                                    \
        RESTORE_MEMORY_POINTERS;                                              \
        RESTORE;                                                              \
        ASSERT(is_same_page(juvix_memory_pointer, juvix_memory_pointer + n)); \
    }

// Allocate an n-word object and assign it to `ptr`. Assumes n words have been
// previously preallocated with PREALLOC.
#define ALLOC(ptr, n)                                                     \
    ASSERT(is_same_page(juvix_memory_pointer, juvix_memory_pointer + n)); \
    ptr = juvix_memory_pointer;                                           \
    juvix_memory_pointer += n;

// Allocate n words. Cannot call GC. This function doesn't require preallocation
// and cancels all preallocations made. One needs to save the memory pointers
// beforehand.
word_t *alloc(size_t n);

#endif
