#ifndef NAT_H_
#define NAT_H_

#include <stdbool.h>

typedef int prim_nat;

#define prim_zero 0

prim_nat prim_suc(prim_nat n) {
    return n + 1;
}

bool is_prim_zero(prim_nat n) {
    return n == 0;
}

bool is_prim_suc(prim_nat n) {
    return n != 0;
}

prim_nat proj_ca0_prim_suc(prim_nat n) {
    return n - 1;
}

prim_nat prim_natplus(prim_nat a, prim_nat b) {
    return a + b;
}


#endif // NAT_H_
