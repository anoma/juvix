// count combinations of numbers 1 to N having sum N

#include <stdio.h>

int combinations(int n, int s) {
    if (s == 0) {
        return 1;
    } else if (n == 0) {
        return 0;
    } else if (s < 0) {
        return 0;
    } else {
        return combinations(n, s - n) + combinations(n - 1, s);
    }
}

int main() {
    printf("%d\n", combinations(100, 100));
    return 0;
}
