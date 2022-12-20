// Compute the Nth Fibonacci number modulo 2 ^ 28

#include <stdio.h>

static int fib(int k) {
    int n = 0;
    int m = 1;
    for (int i = 0; i < k; ++i) {
        int x = (n + m) % (1 << 28);
        n = m;
        m = x;
    }
    return n;
}

int main() {
    printf("%d\n", fib(1000000));
    return 0;
}
