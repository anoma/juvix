#ifndef ANOMA_H_
#define ANOMA_H_

#include "minic-runtime.h"

int readPre(const char *key) {
    if (strcmp("change1-key", key)) {
        return 100;
    } else if (strcmp("change2-key", key)) {
        return 90;
    } else {
        return -1;
    }
}

int readPost(const char *key) {
    if (strcmp("change1-key", key)) {
        return 90;
    } else if (strcmp("change2-key", key)) {
        return 100;
    } else {
        return -1;
    }
}

const char* isBalanceKey(const char* s1, const char* s2) {
    return "owner-address";
}

#endif // ANOMA_H_
