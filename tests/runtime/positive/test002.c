/* Page allocation */

#include <juvix/mem/mem.h>
#include <juvix/mem/pages.h>

static unsigned long seed = 123456789UL;

static uint myrandom() {
    return ((seed = seed * 214013L + 2531011L) >> 16) & 0x7fff;
}

#define N 256
#define MAX_SIZE 16

int main() {
    void *data[N];
    size_t size[N];
    for (int i = 0; i < N; ++i) {
        data[i] = NULL;
    }
    for (int i = 0; i < 10 * N; ++i) {
        uint idx = myrandom() % N;
        if (data[idx] != NULL) {
            pfree(data[idx], size[idx]);
            data[idx] = 0;
        } else {
            size[idx] = myrandom() % MAX_SIZE + 1;
            data[idx] = palloc(size[idx]);
            memfill(data[idx], myrandom(),
                    size[idx] * PAGE_SIZE / sizeof(word_t));
        }
    }
    for (int i = 0; i < N; ++i) {
        if (data[i] != NULL) {
            pfree(data[i], size[i]);
        }
    }
    ASSERT(juvix_allocated_pages_num == 0);
    return 0;
}
