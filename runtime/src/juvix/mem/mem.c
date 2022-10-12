
#include <juvix/mem/mem.h>

#ifndef API_LIBC
void memcopy(word_t *restrict dest, const word_t *restrict src, size_t n) {
    // TODO: optimise this
    MEMCOPY(dest, src, n);
}
#endif

void memfill(word_t *dest, word_t val, size_t n) {
    // TODO: optimise this
    MEMFILL(dest, val, n);
}
