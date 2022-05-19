#ifndef MINIC_RUNTIME_H_
#define MINIC_RUNTIME_H_

#include <stdlib.h>
#include <stdio.h>

char* intToStr(int i) {
    int length = snprintf(NULL, 0, "%d", i);
    char* str = (char*)malloc(length + 1);
    snprintf(str, length + 1, "%d", i);
    return str;
}

int putStr(const char* str) {
    return fputs(str, stdout);
}

int putStrLn(const char* str) {
    return puts(str);
}

#endif // MINIC_RUNTIME_H_
