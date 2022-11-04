#ifndef JUVIX_OBJECT_CSTRING_H
#define JUVIX_OBJECT_CSTRING_H

#include <juvix/mem/alloc.h>
#include <juvix/mem/mem.h>
#include <juvix/object/object.h>

#ifdef API_LIBC
#include <string.h>
#else
size_t strlen(const char *str);
char *strcpy(char *restrict dest, const char *restrict src);
#endif

static inline char *get_cstring(word_t x) {
    ASSERT(is_cstring(x));
    return (char *)x + sizeof(word_t);
}

#define INIT_CSTRING(var, str, nfields)                                      \
    FIELD(var, 0) = make_special_header(SUID_CSTRING, nfields, SKIP_ALL, 0); \
    FIELD(var, nfields) = 0;                                                 \
    strcpy((char *)(var) + sizeof(word_t), str);

#define ALLOC_CSTRING(var, str)                              \
    do {                                                     \
        size_t juvix_nfields =                               \
            (strlen(str) + sizeof(word_t)) / sizeof(word_t); \
        void *tmp;                                           \
        ALLOC(tmp, juvix_nfields + 1);                       \
        var = (word_t)tmp;                                   \
        INIT_CSTRING(var, str, juvix_nfields);               \
    } while (0)

// Memory pointers (see alloc.h) need to be saved before calling this
// function.
word_t alloc_cstring(const char *str);

#endif
