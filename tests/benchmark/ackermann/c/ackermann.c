// Ackermann function (higher-order definition)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef uintptr_t (*fun_t)(uintptr_t,uintptr_t);

typedef struct{
    uintptr_t arg;
    fun_t f;
} closure_t;

static uintptr_t iter(closure_t *cl, int n, uintptr_t x) {
    while (n > 0) {
        x = cl->f(cl->arg, x);
        --n;
    };
    return x;
}

static uintptr_t step_2(uintptr_t f, uintptr_t n) {
    return iter((closure_t*)f, n + 1, 1);
}

static uintptr_t step(uintptr_t dummy, uintptr_t x) {
    closure_t *cl = malloc(sizeof(closure_t));
    cl->arg = x;
    cl->f = (fun_t)step_2;
    return (uintptr_t)cl;
}

static uintptr_t plus(uintptr_t x, uintptr_t y) {
    return x + y;
}

static int ackermann(int m, int n) {
    closure_t *plus_one = malloc(sizeof(closure_t));
    plus_one->arg = 1;
    plus_one->f = plus;
    closure_t *mystep = malloc(sizeof(closure_t));
    mystep->arg = 0;
    mystep->f = step;
    closure_t *cl = (closure_t *)iter(mystep, m, (uintptr_t)plus_one);
    return cl->f(cl->arg, n);
}

int main() {
    printf("%d\n", ackermann(3, 11));
    return 0;
}
