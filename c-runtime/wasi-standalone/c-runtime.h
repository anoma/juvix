#ifndef C_RUNTIME_H_
#define C_RUNTIME_H_

#include "bool.h"
#include "nat.h"
#include "io.h"
#include "juvix_string.h"

typedef __SIZE_TYPE__ size_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __INT64_TYPE__ int64_t;

typedef struct juvix_function {
    uintptr_t fun;
} juvix_function_t;

/**
 * Allocator
 */

void *malloc(size_t size);
void free(void *p);

void* memcpy(void *dest, const void *src, size_t n) {
    char *csrc = (char*) src;
    char *cdest = (char*) dest;

    for (int i=0; i < n; i++) {
        cdest[i] = csrc[i];
    }
    return dest;
}

void* realloc(void *ptr, size_t n) {
    void* newptr;
    newptr = malloc(n);
    memcpy(newptr, ptr, n);
    free(ptr);
    return newptr;
}

/**
 * WASI Definitions
 */

typedef struct Ciovec {
    uint8_t *buf;
    uint32_t buf_len;
} Ciovec_t;

uint16_t fd_write(uint32_t fd, Ciovec_t *iovs_ptr, uint32_t iovs_len, uint32_t *nwritten)
    __attribute__((
                __import_module__("wasi_snapshot_preview1"),
                __import_name__("fd_write"),
                   ));

uint16_t fd_read(uint32_t fd, Ciovec_t *iovs_ptr, uint32_t iovs_len, uint32_t *read)
    __attribute__((
                __import_module__("wasi_snapshot_preview1"),
                __import_name__("fd_read"),
                   ));

_Noreturn void proc_exit(uint32_t rval)
    __attribute__((
                __import_module__("wasi_snapshot_preview1"),
                __import_name__("proc_exit"),
                   ));

/**
 * stdlib.h
 */

#define EXIT_FAILURE 1

_Noreturn void exit(int rval) {
    proc_exit(rval);
}

/**
 * string.h
 */

int strcmp(const char *s1, const char *s2) {
    while (*s1) {
        if (*s1 != *s2) {
            break;
        }
        s1++;
        s2++;
    }
    return *(const unsigned char*)s1 - *(const unsigned char*)s2;
}

size_t strlen(const char *s) {
    const char* p = s;
    while(*p!='\0') {
        ++p;
    }
    return p - s;
}

/**
 * Utilities
 */

/**
    * C++ version 0.4 char* style "itoa":
    * Written by Lukás Chmela
    * Released under GPLv3.

    */
char* itoa(int value, char* result, int base) {
    // check that the base if valid
    if (base < 2 || base > 36) { *result = '\0'; return result; }

    char* ptr = result, *ptr1 = result, tmp_char;
    int tmp_value;

    do {
        tmp_value = value;
        value /= base;
        *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz" [35 + (tmp_value - value * base)];
    } while ( value );

    // Apply negative sign
    if (tmp_value < 0) *ptr++ = '-';
    *ptr-- = '\0';
    while(ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr--= *ptr1;
        *ptr1++ = tmp_char;
    }
    return result;
}

char* intToStr(int i) {
    // int will occupy at least one char
    size_t len = 1;

    // calculate the size of the digits
    int n = i < 0 ? -i : i;
    while (n >= 10) {
        n /= 10;
        len += 1;
    }

    // add an extra char for the negative sign
    if (i < 0) {
        len += 1;
    }

    char* buf = (char*)malloc(len + 1);
    itoa(i, buf, 10);
    return buf;
}

int strToInt(char *str)
{
  int result;
  int puiss;

  result = 0;
  puiss = 1;
  while (('-' == (*str)) || ((*str) == '+'))
  {
      if (*str == '-')
        puiss = puiss * -1;
      str++;
  }
  while ((*str >= '0') && (*str <= '9'))
  {
      result = (result * 10) + ((*str) - '0');
      str++;
  }
  return (result * puiss);
}

int putStr(const char* str) {
    size_t n = strlen(str);
    Ciovec_t vec = {.buf = (uint8_t*)str, .buf_len=(uint32_t)n};
    return fd_write(1, &vec, 1, 0);
}

int putStrLn(const char* str) {
    return putStr(str) || putStr("\n");
}

int debug(const char* str) {
    return putStrLn(str);
}

prim_io prim_printNat(prim_nat n) {
    return putStrLn(intToStr(n));
}

// Tries to parse str as a positive integer.
// Returns -1 if parsing fails.
int parsePositiveInt(char *str) {
    int result = 0;
    char* p = str;
    size_t len = strlen(str);
    while ((*str >= '0') && (*str <= '9'))
    {
        result = (result * 10) + ((*str) - '0');
        str++;
    }
    if (str - p != len) return -1;
    return result;
}


char* concat(const char* s1, const char* s2) {
    const size_t len1 = strlen(s1);
    const size_t len2 = strlen(s2);
    int i,j=0,count=0;
    char* result = (char*)malloc(len1 + len2 + 1);

    for(i=0; i < len1; i++) {
        result[i] = s1[i];
        count++;
    }

    for(i=count; j < len2; i++) {
        result[i] = s2[j];
        j++;
    }

    result[len1 + len2] = '\0';
    return result;
}

int getchar() {
    int ch;
    Ciovec_t v = {.buf = (uint8_t*) &ch, .buf_len=1};
    if (fd_read(0, &v, 1, 0) != 0) return -1;
    return ch;
}

char* readline() {
    int i = 0, bytesAlloced = 64;
    char* buffer = (char*) malloc(bytesAlloced);
    int bufferSize = bytesAlloced;

    for (;;++i) {
        int ch;

        if (i == bufferSize - 1) {
            bytesAlloced += bytesAlloced;
            buffer = (char*) realloc(buffer, bytesAlloced);
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

#endif // C_RUNTIME_H_
