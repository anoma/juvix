#ifndef JUVIX_MEM_ALLOC_H
#define JUVIX_MEM_ALLOC_H

#include <juvix/mem/gens.h>

extern generation_t *alloc_youngest_generation;

// Initialise the allocator module.
void alloc_init();
// Allocate more memory for words. The argument points to the begging of free
// pool memory.
void alloc_words(word_t *beg);
// Allocate more memory for dwords.
void alloc_dwords(dword_t *beg);

static inline word_t *alloc_free_words_begin() {
    return alloc_youngest_generation->words->free_begin;
}

static inline word_t *alloc_free_words_end() {
    return alloc_youngest_generation->words->free_end;
}

static inline word_t *alloc_free_dwords_begin() {
    return alloc_youngest_generation->dwords->free_begin;
}

static inline word_t *alloc_free_dwords_end() {
    return alloc_youngest_generation->dwords->free_end;
}

#define XALLOC(ptr, n, beg, end, ALLOCATE) \
    do {                                   \
        ptr = beg;                         \
        beg += n;                          \
        if (unlikely(beg > end)) {         \
            ALLOCATE;                      \
            ptr = beg;                     \
            beg += n;                      \
        };                                 \
    } while (0)

// Allocate an n-word object and assign it to `ptr`. `beg` and `end` should be
// word_t* pointer variables reserved for managing allocator data. `SAVE` and
// `RESTORE` should save and restore live local variables on the global stack
// (gen_alloc can lauch GC which needs access to these variables).
#define ALLOC(ptr, n, beg, end, SAVE, RESTORE) \
    XALLOC(ptr, n, beg, end, {                 \
        SAVE;                                  \
        alloc_words(beg - n);                  \
        beg = alloc_free_words_begin();        \
        end = alloc_free_words_end();          \
        RESTORE;                               \
    })

#define DALLOC(ptr, n, beg, end, SAVE, RESTORE) \
    XALLOC(ptr, n, beg, end, {                  \
        SAVE;                                   \
        alloc_dwords(beg - n);                  \
        beg = alloc_free_dwords_begin();        \
        end = alloc_free_dwords_end();          \
        RESTORE;                                \
    })

#endif
