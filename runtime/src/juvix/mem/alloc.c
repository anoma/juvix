
#include <juvix/mem/alloc.h>
#include <juvix/mem/gens.h>
#include <juvix/opts.h>

generation_t *alloc_youngest_generation;

void alloc_init() {
    gens_init();
    alloc_youngest_generation = gen_alloc(NULL, opt_generation_min_pages);
}

static void alloc_prepare(bool can_gc) {
    if (can_gc && alloc_youngest_generation->pages_max <=
                      alloc_youngest_generation->pages_num) {
        // TODO: decide on GC collection here
        alloc_youngest_generation =
            gen_alloc(alloc_youngest_generation, opt_generation_min_pages);
    }
    ++alloc_youngest_generation->pages_num;
}

void alloc_words(bool can_gc) {
    alloc_prepare(can_gc);
    pool_t *pool = pool_alloc(alloc_youngest_generation->words);
    alloc_youngest_generation->words = pool;
}

void alloc_dwords(bool can_gc) {
    alloc_prepare(can_gc);
    pool_t *pool = pool_alloc(alloc_youngest_generation->dwords);
    alloc_youngest_generation->dwords = pool;
}

word_t *alloc(size_t n) {
    if (alloc_youngest_generation->words == NULL) {
        alloc_words(false);
    }
    word_t *ptr;
    XALLOC(ptr, n, alloc_youngest_generation->words->free_begin,
           { alloc_words(false); });
    return ptr;
}

dword_t *dalloc(size_t n) {
    if (alloc_youngest_generation->dwords == NULL) {
        alloc_dwords(false);
    }
    dword_t *ptr;
    XALLOC(ptr, n, alloc_youngest_generation->dwords->free_begin,
           { alloc_dwords(false); });
    return ptr;
}
