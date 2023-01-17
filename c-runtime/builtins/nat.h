#ifndef NAT_H_
#define NAT_H_

#include <stdbool.h>

typedef int prim_nat;

#define prim_zero 0

prim_nat prim_suc(prim_nat n) { return n + 1; }

bool is_prim_zero(prim_nat n) { return n == 0; }

bool is_prim_suc(prim_nat n) { return n != 0; }

prim_nat proj_ca0_prim_suc(prim_nat n) { return n - 1; }

prim_nat prim_natplus(prim_nat a, prim_nat b) { return a + b; }

prim_nat prim_natsub(prim_nat a, prim_nat b) { return a - b; }

prim_nat prim_natmul(prim_nat a, prim_nat b) { return a * b; }

prim_nat prim_natudiv(prim_nat a, prim_nat b) { return (a + b - 1) / b; }

prim_nat prim_natdiv(prim_nat a, prim_nat b) { return a / b; }

prim_nat prim_natmod(prim_nat a, prim_nat b) { return a % b; }

bool prim_natle(prim_nat a, prim_nat b) { return a <= b; }

bool prim_natlt(prim_nat a, prim_nat b) { return a < b; }

bool prim_nateq(prim_nat a, prim_nat b) { return a == b; }

#endif  // NAT_H_
