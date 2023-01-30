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
char *strcat(char *restrict dest, const char *restrict src);
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

#define CONCAT_CSTRINGS(var, str1, str2)                              \
    do {                                                              \
        size_t juvix_nfields = get_nfields(str1) + get_nfields(str2); \
        if (juvix_nfields > MAX_FIELDS) {                             \
            error_exit_msg("concat: string too long");                \
        }                                                             \
        void *tmp;                                                    \
        ALLOC(tmp, juvix_nfields + 1);                                \
        INIT_CSTRING((word_t)tmp, get_cstring(str1), juvix_nfields);  \
        strcat((char *)tmp + sizeof(word_t), get_cstring(str2));      \
        var = (word_t)tmp;                                            \
    } while (0)

// Memory pointers (see alloc.h) need to be saved before calling this
// function.
word_t alloc_cstring(const char *str);

#define GET_CONST_CSTRING(n) juvix_strings[n]
#define MAKE_CONST_CSTRING(n, str) juvix_strings[n] = alloc_cstring(str)

#define DECL_CONST_CSTRINGS(n) static word_t juvix_strings[n]

// Converts a C string to an integer. Sets 'errno' to 'EINVAL' if the conversion
// could not be performed.
int strtoint(const char *str);

#endif
