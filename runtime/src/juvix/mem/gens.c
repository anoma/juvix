
#include <juvix/mem/gens.h>
#include <juvix/mem/pages.h>

#define MAX_GENERATIONS 32

static generation_t gens_pool[MAX_GENERATIONS];
static generation_t *oldest_gen = NULL;
static generation_t *next_free_gen = NULL;

void gens_init() {
    for (int i = 0; i < MAX_GENERATIONS - 1; ++i) {
        gens_pool[i].next = gens_pool + i + 1;
    }
    gens_pool[MAX_GENERATIONS - 1].next = NULL;
    next_free_gen = gens_pool;
}

pool_t *pool_alloc(pool_t *next) {
    pool_t *pool = palloc(1);
    pool->next = next;
    pool->data = palign((uint8_t *)pool + sizeof(pool_t), sizeof(dword_t));
    pool->free_begin = pool->data;
    pool->free_end = (uint8_t *)pool + PAGE_SIZE;
    ASSERT_ALIGNED(pool, PAGE_SIZE);
    ASSERT_ALIGNED(pool->free_end, PAGE_SIZE);
    return pool;
}

static void pool_free(pool_t *pool) {
    while (pool != NULL) {
        pool_t *next = pool->next;
        pfree(pool, 1);
        pool = next;
    }
}

generation_t *gen_alloc(generation_t *prev, size_t pages_max) {
    ASSERT(prev != next_free_gen);
    if (next_free_gen == NULL) {
        ASSERT(oldest_gen != NULL);
        ASSERT(oldest_gen->next != NULL);
        ASSERT(oldest_gen->next->prev == oldest_gen);
        gen_merge(oldest_gen->next);
        ASSERT(next_free_gen != NULL);
    }
    generation_t *gen = next_free_gen;
    next_free_gen = gen->next;
    gen->next = NULL;
    gen->prev = prev;
    if (prev != NULL) {
        prev->next = gen;
    }
    gen->pages_max = pages_max;
    gen->pages_num = 0;
    gen->memory = pool_alloc(NULL);
    if (prev == NULL) {
        oldest_gen = gen;
    }
    return gen;
}

void gen_free(generation_t *gen) {
    if (gen == oldest_gen) {
        ASSERT(gen->prev == NULL);
        oldest_gen = oldest_gen->next;
    }
    if (gen->prev != NULL) {
        gen->prev->next = gen->next;
    }
    if (gen->next != NULL) {
        gen->next->prev = gen->prev;
    }
    gen->next = next_free_gen;
    next_free_gen = gen;
    pool_free(gen->memory);
}

static void pool_prepend(pool_t **ppool, pool_t *pool) {
    ASSERT(*ppool != NULL);
    ASSERT(pool != NULL);
    pool_t *next = *ppool;
    *ppool = pool;
    while (pool->next != NULL) {
        pool = pool->next;
    }
    pool->next = next;
}

void gen_merge(generation_t *gen) {
    ASSERT(gen->prev != NULL);
    generation_t *prev = gen->prev;
    prev->next = gen->next;
    if (gen->next != NULL) {
        gen->next->prev = prev;
    }
    pool_prepend(&prev->memory, gen->memory);
    prev->pages_num += gen->pages_num;
    gen->next = next_free_gen;
    next_free_gen = gen;
}
