#ifndef JUVIX_MEM_GENS_H
#define JUVIX_MEM_GENS_H

#include <juvix/defs.h>

typedef struct Pool {
    struct Pool *next;
    void *data;
    // the first free allocation unit in `data`
    void *free_begin;
    void *free_end;
} pool_t;

typedef struct Generation {
    // Pointer to the previous (older) generation.
    struct Generation *prev;
    // Pointer to the next (younger) generation.
    struct Generation *next;
    size_t pages_num;
    size_t pages_max;
    pool_t *words;
    pool_t *dwords;
} generation_t;

pool_t *pool_alloc(pool_t *next);

// Allocate a new generation. May merge old generations if generation limit
// reached. If `prev` is NULL, then the newly allocated generation is assumed to
// be the oldest generation.
generation_t *gen_alloc(generation_t *prev, size_t pages_max);
void gen_free(generation_t *gen);

// Merge the given generation with the previous one.
void gen_merge(generation_t *gen);

#endif
