// Compute the Nth prime

#include <stdio.h>
#include <stdbool.h>

#define N (1024 * 16)

static int primes[N];

int main() {
    int i = 0;
    int p = 2;
    while (i < N) {
        bool b = false;
        for (int j = 0; j < i; ++j) {
            if (p % primes[j] == 0) {
                b = true;
                break;
            }
        }
        if (b) {
            ++p;
        } else {
            primes[i] = p;
            ++p;
            ++i;
        }
    }
    printf("%d\n", primes[N-1]);
    return 0;
}
