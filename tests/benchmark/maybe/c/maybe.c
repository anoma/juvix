// optionally sum N integers from a binary tree K times

#include <stdio.h>
#include <stdlib.h>

typedef struct Tree {
    int value;
    struct Tree *left;
    struct Tree *right;
} tree_t;

static tree_t *gen(int n) {
    if (n == 0) {
        return NULL;
    } else {
        tree_t *t = malloc(sizeof(tree_t));
        t->value = n;
        t->left = gen(n - 1);
        t->right = t->left;
        return t;
    }
}

static int sum(int n, tree_t *t) {
    if (t == NULL) {
        return 0;
    } else {
        if (t->value == n) {
            return -1;
        } else {
            int s1 = sum(n, t->left);
            if (s1 < 0) {
                return -1;
            } else {
                int s2 = sum(n, t->right);
                if (s2 < 0) {
                    return -1;
                } else {
                    return s1 + s2 + t->value;
                }
            }
        }
    }
}

static int run(int n, tree_t *t) {
    if (n == 0) {
        return sum(0, t);
    } else {
        int x = run(n - 1, t);
        if (x < 0) {
            return sum(n, t);
        } else {
            int y = sum(n, t);
            if (y < 0) {
                return -1;
            } else {
                return y - x;
            }
        }
    }
}

int main() {
    printf("%d\n", run(101, gen(20)));
    return 0;
}
