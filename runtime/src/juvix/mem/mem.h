#ifndef JUVIX_MEM_MEM_H
#define JUVIX_MEM_MEM_H

#include <juvix/defs.h>

// use MEMCOPY *only* if `N` is constant, `dest` and `src` are pointer variables
#define MEMCOPY(dest, src, N)                                   \
    do {                                                        \
        for (size_t juvix_idx = 0; juvix_idx != N; ++juvix_idx) \
            (dest)[juvix_idx] = (src)[juvix_idx];               \
    } while (0)

// use MEMFILL *only* if `N` is constant and `dest` is a pointer variable
#define MEMFILL(dest, val, N)                                   \
    do {                                                        \
        for (size_t juvix_idx = 0; juvix_idx != N; ++juvix_idx) \
            (dest)[juvix_idx] = (val);                          \
    } while (0)

// Copy `n` words from `src` to `dest`.
void memcopy(word_t *restrict dest, const word_t *restrict src, size_t n);
// Fill `n` words at `dest` with `val`
void memfill(word_t *dest, word_t val, size_t n);

#endif
