
#include <juvix/object/cstring.h>

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

char *strcat(char *restrict dest, const char *restrict src) {
    strcpy(dest + strlen(dest), src);
    return dest;
}

#endif

word_t alloc_cstring(const char *str) {
    size_t n = (strlen(str) + sizeof(word_t)) / sizeof(word_t);
    word_t var = (word_t)alloc(n + 1);
    INIT_CSTRING(var, str, n);
    return var;
}

int_t strtoint(const char *str) {
    int_t result;
    int_t puiss;

    errno = 0;

    result = 0;
    puiss = 1;
    while (*str == '-' || *str == '+') {
        if (*str == '-') {
            puiss = puiss * -1;
        }
        str++;
    }
    while (*str >= '0' && *str <= '9') {
        result = result * 10 + (*str - '0');
        str++;
        if (result > (MAX_SMALLINT - 10) / 10) {
            errno = EINVAL;
            return result;
        }
    }
    if (*str != 0) {
        errno = EINVAL;
    }
    return (result * puiss);
}
