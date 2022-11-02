#ifndef JUVIX_MEM_GENS_H
#define JUVIX_MEM_GENS_H

#include <juvix/defs.h>
#include <juvix/limits.h>

typedef struct Pool {
    struct Pool *next;
    void *data;
    // the first free allocation unit in `data`
    void *free_begin;
    void *free_end;
} pool_t;

STATIC_ASSERT(sizeof(pool_t) <= MAX_MEM_STRUCT_SIZE);

typedef struct Generation {
    // Pointer to the previous (older) generation.
    struct Generation *prev;
    // Pointer to the next (younger) generation.
    struct Generation *next;
    pool_t *memory;
    // The total number of pages allocated in all pools.
    size_t pages_num;
    // The threshold number of pages before a new generation is created.
    size_t pages_max;
} generation_t;

void gens_init();

pool_t *pool_alloc(pool_t *next);

// Allocate a new generation. May merge old generations if generation limit
// reached. If `prev` is NULL, then the newly allocated generation is assumed to
// be the oldest generation.
generation_t *gen_alloc(generation_t *prev, size_t pages_max);
void gen_free(generation_t *gen);

// Merge the given generation with the previous one.
void gen_merge(generation_t *gen);

#endif
