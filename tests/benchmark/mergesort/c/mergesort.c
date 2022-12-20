// Merge sort an array of N integers

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

static void merge(int *dest, int *tab1, int *tab2, int size1, int size2) {
    int *end1 = tab1 + size1;
    int *end2 = tab2 + size2;
    while (tab1 != end1 && tab2 != end2) {
        if (*tab1 <= *tab2) {
            *dest++ = *tab1++;
        } else {
            *dest++ = *tab2++;
        }
    }
    while (tab1 != end1) {
        *dest++ = *tab1++;
    }
    while (tab2 != end2) {
        *dest++ = *tab2++;
    }
}

static void sort(int *tab, int size) {
    if (size > 1) {
        int *tmp = malloc(sizeof(int) * size);
        memcpy(tmp, tab, size * sizeof(int));
        int size2 = size / 2;
        sort(tmp, size2);
        sort(tmp + size2, size - size2);
        merge(tab, tmp, tmp + size2, size2, size - size2);
        free(tmp);
    }
}

static bool sorted(int *tab, int size) {
    for (int i = 0; i < size - 1; ++i) {
        if (tab[i] > tab[i + 1]) {
            return false;
        }
    }
    return true;
}

#define N 100000

int main() {
    int *tab = malloc(N * sizeof(int));
    for (int i = 0; i < N; ++i) {
        tab[i] = i;
    }
    sort(tab, N);
    if (sorted(tab, N)) {
        printf("true\n");
    } else {
        printf("false\n");
    }
    return 0;
}
