
#include <juvix/mem/mem.h>

#ifdef API_LIBC
#include <string.h>
#endif

void memcopy(word_t *restrict dest, const word_t *restrict src, size_t n) {
#if defined(API_LIBC)
    memcpy(dest, src, n * sizeof(word_t))
#else
    // TODO: optimise this
    MEMCOPY(dest, src, n);
#endif
}

void memfill(word_t *dest, word_t val, size_t n) {
#if defined(API_LIBC) && defined(BITS32)
    memset(dest, val, n * sizeof(word_t))
#else
    // TODO: optimise this
    MEMFILL(dest, val, n);
#endif
}
