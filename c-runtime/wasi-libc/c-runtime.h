#ifndef C_RUNTIME_H_
#define C_RUNTIME_H_

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bool.h"
#include "io.h"
#include "juvix_string.h"
#include "nat.h"

typedef struct juvix_function {
    uintptr_t fun;
} juvix_function_t;

char* intToStr(int i) {
    int length = snprintf(NULL, 0, "%d", i);
    char* str = (char*)malloc(length + 1);
    snprintf(str, length + 1, "%d", i);
    return str;
}

char* concat(const char* s1, const char* s2) {
    const size_t len1 = strlen(s1);
    const size_t len2 = strlen(s2);
    int i, j = 0, count = 0;
    char* result = (char*)malloc(len1 + len2 + 1);

    for (i = 0; i < len1; i++) {
        result[i] = s1[i];
        count++;
    }

    for (i = count; j < len2; i++) {
        result[i] = s2[j];
        j++;
    }

    result[len1 + len2] = '\0';
    return result;
}

// Tries to parse str as a positive integer.
// Returns -1 if parsing fails.
int parsePositiveInt(char* str) {
    int result = 0;
    char* p = str;
    size_t len = strlen(str);
    while ((*str >= '0') && (*str <= '9')) {
        result = (result * 10) + ((*str) - '0');
        str++;
    }
    if (str - p != len) return -1;
    return result;
}

char* readline() {
    int i = 0, bytesAlloced = 64;
    char* buffer = (char*)malloc(bytesAlloced);
    int bufferSize = bytesAlloced;

    for (;; ++i) {
        int ch;

        if (i == bufferSize - 1) {
            bytesAlloced += bytesAlloced;
            buffer = (char*)realloc(buffer, bytesAlloced);
            bufferSize = bytesAlloced;
        }

        ch = getchar();
        if (ch == -1) {
            free(buffer);
            return 0;
        }
        if (ch == '\n') break;
        buffer[i] = ch;
    }

    buffer[i] = '\0';
    return buffer;
}

int putStr(const char* str) {
    fputs(str, stdout);
    return fflush(stdout);
}

int putStrLn(const char* str) {
    puts(str);
    return fflush(stdout);
}

int debug(const char* str) { return putStrLn(str); }

prim_io prim_printNat(prim_nat n) { return putStr(intToStr(n)); }

prim_io prim_printString(prim_string s) { return putStr(s); }

prim_io prim_printBool(prim_bool b) { return putStr(b ? "true" : "false"); }

prim_io prim_readline(juvix_function_t* f) {
    return ((prim_io(*)(juvix_function_t*, prim_string))f->fun)(f, readline());
}

prim_string prim_natToString(prim_nat n) { return intToStr(n); }

prim_nat prim_stringToNat(prim_string s) { return parsePositiveInt(s); }

prim_string prim_stringConcat(prim_string s1, prim_string s2) {
    return concat(s1, s2);
}

prim_bool prim_stringEq(prim_string s1, prim_string s2) {
    return strcmp(s1, s2) == 0;
}

#endif  // C_RUNTIME_H_
