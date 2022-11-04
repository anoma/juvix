// Ackermann function (higher-order definition)

#include <stdio.h>
#include <stdlib.h>

typedef uintptr_t uint;

typedef uint (*fun_t)(uint,uint);

typedef struct{
    uint arg;
    fun_t f;
} closure_t;

static uint iter(closure_t *cl, int n, uint x) {
    while (n > 0) {
        x = cl->f(cl->arg, x);
        --n;
    };
    return x;
}

static uint step_2(uint f, uint n) {
    return iter((closure_t*)f, n + 1, 1);
}

static uint step(uint dummy, uint x) {
    closure_t *cl = malloc(sizeof(closure_t));
    cl->arg = x;
    cl->f = (fun_t)step_2;
    return (uint)cl;
}

static uint plus(uint x, uint y) {
    return x + y;
}

static int ackermann(int m, int n) {
    closure_t *plus_one = malloc(sizeof(closure_t));
    plus_one->arg = 1;
    plus_one->f = plus;
    closure_t *mystep = malloc(sizeof(closure_t));
    mystep->arg = 0;
    mystep->f = step;
    closure_t *cl = (closure_t *)iter(mystep, m, (uint)plus_one);
    return cl->f(cl->arg, n);
}

int main() {
    printf("%d\n", ackermann(3, 11));
    return 0;
}
