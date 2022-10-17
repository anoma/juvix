/* Page allocation */

#include <juvix/mem/mem.h>
#include <juvix/mem/pages.h>

static uint seed = 123456789;

static uint myrandom() {
    const uint a = 1103515245;
    const uint c = 12345;
    seed = (a * seed + c) >> 31;
    return seed;
}

#define N 1024
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
