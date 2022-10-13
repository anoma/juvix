#ifndef JUVIX_MEM_ALLOC_H
#define JUVIX_MEM_ALLOC_H

/*
 This module provides an allocator for unstructured objects. When using the
 allocated memory one needs to be mindful of C alignment and aliasing rules:
 https://en.cppreference.com/w/c/language/object.
*/

#include <juvix/mem/gens.h>
#include <juvix/mem/pages.h>

extern generation_t *alloc_youngest_generation;

// Initialise the allocator module.
void alloc_init();
// Allocate more memory for words. The argument points to the beginning of free
// pool memory.
void alloc_words(bool can_gc);
// Allocate more memory for dwords.
void alloc_dwords(bool can_gc);

static inline word_t *alloc_words_memory_pointer() {
    if (alloc_youngest_generation->words != NULL) {
        return alloc_youngest_generation->words->free_begin;
    } else {
        return NULL;
    }
}

static inline dword_t *alloc_dwords_memory_pointer() {
    if (alloc_youngest_generation->dwords != NULL) {
        return alloc_youngest_generation->dwords->free_begin;
    } else {
        return NULL;
    }
}

static inline void alloc_save_words_memory_pointer(word_t *ptr) {
    pool_t *pool = alloc_youngest_generation->words;
    if (pool != NULL) {
        pool->free_begin = ptr;
    }
}

static inline void alloc_save_dwords_memory_pointer(dword_t *ptr) {
    pool_t *pool = alloc_youngest_generation->dwords;
    if (pool != NULL) {
        pool->free_begin = ptr;
    }
}

#define XALLOC(ptr, n, mp, ALLOCATE)            \
    do {                                        \
        ptr = mp;                               \
        mp += n;                                \
        if (unlikely(!is_same_page(ptr, mp))) { \
            ALLOCATE;                           \
            ptr = mp;                           \
            mp += n;                            \
        };                                      \
    } while (0)

#define DECL_ALLOC_VARS                                                        \
    register word_t *juvix_words_ptr = (word_t *)(PAGE_SIZE - sizeof(word_t)); \
    dword_t *juvix_dwords_ptr = (dword_t *)(PAGE_SIZE - sizeof(dword_t));

#define SAVE_MEMORY_POINTERS                          \
    alloc_save_words_memory_pointer(juvix_words_ptr); \
    alloc_save_dwords_memory_pointer(juvix_dwords_ptr);

#define RESTORE_MEMORY_POINTERS                     \
    juvix_words_ptr = alloc_words_memory_pointer(); \
    juvix_dwords_ptr = alloc_dwords_memory_pointer();

// Allocate an n-word object and assign it to `ptr`. `SAVE` and `RESTORE` should
// save and restore live local variables on the global stack (alloc_words can
// lauch GC which needs access to these variables).
#define ALLOC(ptr, n, SAVE, RESTORE)  \
    XALLOC(ptr, n, juvix_words_ptr, { \
        SAVE;                         \
        SAVE_MEMORY_POINTERS;         \
        alloc_words(true);            \
        RESTORE_MEMORY_POINTERS;      \
        RESTORE;                      \
    })

#define DALLOC(ptr, n, SAVE, RESTORE)  \
    XALLOC(ptr, n, juvix_dwords_ptr, { \
        SAVE;                          \
        SAVE_MEMORY_POINTERS;          \
        alloc_dwords(true);            \
        RESTORE_MEMORY_POINTERS;       \
        RESTORE;                       \
    })

// One needs to save the memory pointers before calling the following functions
word_t *alloc(size_t n);
dword_t *dalloc(size_t n);

#endif
