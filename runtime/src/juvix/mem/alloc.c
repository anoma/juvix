
#include <juvix/mem/alloc.h>
#include <juvix/mem/gens.h>
#include <juvix/opts.h>

generation_t *alloc_youngest_generation;

void alloc_init() {
    gens_init();
    alloc_youngest_generation = gen_alloc(NULL, opt_generation_min_pages);
}

void alloc_cleanup() {
    generation_t *gen = alloc_youngest_generation;
    while (gen != NULL) {
        generation_t *prev = gen->prev;
        gen_free(gen);
        gen = prev;
    }
}

void alloc_pages(bool can_gc) {
    if (can_gc && alloc_youngest_generation->pages_max <=
                      alloc_youngest_generation->pages_num) {
        // TODO: decide on GC collection here
        alloc_youngest_generation =
            gen_alloc(alloc_youngest_generation, opt_generation_min_pages);
    }
    ++alloc_youngest_generation->pages_num;
    alloc_youngest_generation->memory =
        pool_alloc(alloc_youngest_generation->memory);
}

word_t *alloc(size_t n) {
    ASSERT(alloc_youngest_generation->memory != NULL);
    ASSERT(n > 0);
    word_t *ptr;
    ptr = alloc_youngest_generation->memory->free_begin;
    if (unlikely(!is_same_page(ptr, ptr + n))) {
        alloc_pages(false);
        ptr = alloc_youngest_generation->memory->free_begin;
        ASSERT(is_same_page(ptr, ptr + n));
    }
    alloc_youngest_generation->memory->free_begin =
        (word_t *)alloc_youngest_generation->memory->free_begin + n;
    ASSERT(alloc_youngest_generation->memory->free_begin <
           alloc_youngest_generation->memory->free_end);
    return ptr;
}
