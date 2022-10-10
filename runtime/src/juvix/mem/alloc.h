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
// Allocate more memory for words. The argument points to the begging of free
// pool memory.
void alloc_words(word_t *beg);
// Allocate more memory for dwords.
void alloc_dwords(dword_t *beg);

static inline word_t *alloc_words_memory_pointer() {
    return alloc_youngest_generation->words->free_begin;
}

static inline dword_t *alloc_longs_memory_pointer() {
    return alloc_youngest_generation->dwords->free_begin;
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

// Allocate an n-word object and assign it to `ptr`. `mp` should be a word_t*
// pointer variable reserved for managing allocator data. `SAVE` and `RESTORE`
// should save and restore live local variables on the global stack (alloc_words
// can lauch GC which needs access to these variables).
#define ALLOC(ptr, n, mp, SAVE, RESTORE)    \
    XALLOC(ptr, n, mp, {                    \
        SAVE;                               \
        alloc_words(mp - n);                \
        beg = alloc_words_memory_pointer(); \
        RESTORE;                            \
    })

#define DALLOC(ptr, n, beg, SAVE, RESTORE)   \
    XALLOC(ptr, n, beg, {                    \
        SAVE;                                \
        alloc_dwords(beg - n);               \
        beg = alloc_dwords_memory_pointer(); \
        RESTORE;                             \
    })

#endif
