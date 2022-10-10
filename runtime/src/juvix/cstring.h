#ifndef JUVIX_CSTRING_H
#define JUVIX_CSTRING_H

#include <juvix/mem/alloc.h>
#include <juvix/mem/mem.h>
#include <juvix/object.h>

#ifdef API_LIBC
#include <string.h>
#else
size_t strlen(const char *str);
char *strcpy(char *restrict dest, const char *restrict src);
#endif

#define ALLOC_CSTRING(var, str, beg, end, SAVE, RESTORE)                \
    do {                                                                \
        size_t juvix_nfields =                                          \
            (strlen(str) + sizeof(word_t)) / sizeof(word_t);            \
        ALLOC((word_t *)(var), juvix_nfields, beg, end, SAVE, RESTORE); \
        strcpy((char *)(var) + sizeof(word_t), str);                    \
    } while (0)

#endif
