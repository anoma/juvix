#ifndef JUVIX_MEM_ALLOC_H
#define JUVIX_MEM_ALLOC_H

/*
 This module provides an allocator for unstructured objects. When using the
 allocated memory one needs to be mindful of C alignment and aliasing rules:
 https://en.cppreference.com/w/c/language/object.
*/

#include <juvix/mem/gens.h>

extern generation_t *alloc_youngest_generation;

// Initialise the allocator module.
void alloc_init();
// Allocate more memory for words. The argument points to the beginning of free
// pool memory.
void alloc_words(word_t *beg);
// Allocate more memory for dwords.
void alloc_dwords(dword_t *beg);

static inline word_t *alloc_words_memory_pointer() {
    return alloc_youngest_generation->words->free_begin;
}

static inline word_t *alloc_words_memory_end() {
    return alloc_youngest_generation->words->free_end;
}

static inline dword_t *alloc_dwords_memory_pointer() {
    return alloc_youngest_generation->dwords->free_begin;
}

static inline dword_t *alloc_dwords_memory_end() {
    return alloc_youngest_generation->dwords->free_end;
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

// Allocate an n-word object and assign it to `ptr`. `SAVE` and `RESTORE` should
// save and restore live local variables on the global stack (alloc_words can
// lauch GC which needs access to these variables).
#define ALLOC(ptr, n, SAVE, RESTORE)                    \
    XALLOC(ptr, n, juvix_words_ptr, {                   \
        SAVE;                                           \
        alloc_words(juvix_words_ptr - n);               \
        juvix_words_ptr = alloc_words_memory_pointer(); \
        RESTORE;                                        \
    })

#define DALLOC(ptr, n, SAVE, RESTORE)                     \
    XALLOC(ptr, n, juvix_dwords_ptr, {                    \
        SAVE;                                             \
        alloc_dwords(mp - n);                             \
        juvix_dwords_ptr = alloc_dwords_memory_pointer(); \
        RESTORE;                                          \
    })

#define DECL_ALLOC_VARS               \
    register word_t *juvix_words_ptr; \
    dword_t *juvix_dwords_ptr;

#endif
