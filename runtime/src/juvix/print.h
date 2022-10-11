#ifndef JUVIX_PRINT_H
#define JUVIX_PRINT_H

#include <juvix/defs.h>

// Returns the number of characters printed. If `buf` was big enough then the
// return value is strictly less than `n` and the string in `buf` is
// 0-terminated. Otherwise, the return value is equal to `n`.
size_t print_to_buf(char *buf, size_t n, word_t x);
// The returned pointer should be freed with `free_strbuf`.
char *print(word_t x);
void free_strbuf(char *buf);

// Prints using print_msg
void printout(word_t x);

#endif
