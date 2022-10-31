// Compute the Nth Fibonacci number modulo 2 ^ 28

#include <stdio.h>

typedef int (*fun_t)(int, int, int);

static int step(int k, int n, int m, fun_t cont) {
    if (k == 0) {
        return n;
    } else {
        return cont(k - 1, m, (n + m) % (1 << 28));
    }
}

static int fib_go(int k, int n, int m) {
    return step(k, n, m, fib_go);
}

int main() {
    printf("%d\n", fib_go(100000000, 0, 1));
    return 0;
}
