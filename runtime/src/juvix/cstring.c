
#include <juvix/cstring.h>

#ifndef API_LIBC
size_t strlen(const char *str) {
    const char *str0 = str;
    while (*str) {
        ++str;
    }
    return str - str0;
}

char *strcpy(char *restrict dest, const char *restrict src) {
    char *dest0 = dest;
    while (*src) {
        *dest = *src;
        ++dest;
        ++src;
    }
    return dest0;
}
#endif
