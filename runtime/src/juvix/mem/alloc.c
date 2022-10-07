
#include <juvix/mem/alloc.h>
#include <juvix/mem/gens.h>
#include <juvix/opts.h>

generation_t *alloc_youngest_generation;

void alloc_init() {
    gens_init();
    alloc_youngest_generation = gen_alloc(NULL, opt_generation_min_pages);
}

static void alloc_prepare() {
    if (alloc_youngest_generation->pages_max <=
        alloc_youngest_generation->pages_num) {
        // TODO: decide on GC collection here
        alloc_youngest_generation =
            gen_alloc(alloc_youngest_generation, opt_generation_min_pages);
    }
    ++alloc_youngest_generation->pages_num;
}

void alloc_words(word_t *beg) {
    pool_t *pool = alloc_youngest_generation->words;
    if (pool != NULL) {
        pool->free_begin = beg;
    }
    alloc_prepare();
    pool = pool_alloc(alloc_youngest_generation->words);
    alloc_youngest_generation->words = pool;
}

void alloc_dwords(dword_t *beg) {
    pool_t *pool = alloc_youngest_generation->dwords;
    if (pool != NULL) {
        pool->free_begin = beg;
    }
    alloc_prepare();
    pool = pool_alloc(alloc_youngest_generation->dwords);
    alloc_youngest_generation->dwords = pool;
}
