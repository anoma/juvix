#ifndef MINIC_RUNTIME_H_
#define MINIC_RUNTIME_H_

typedef __SIZE_TYPE__ size_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINTPTR_TYPE__ uintptr_t;

typedef struct minijuvix_function {
    uintptr_t fun;
} minijuvix_function_t;

/**
 * Allocator
 */

void *malloc(size_t size);
void free(void *p);

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

int putStr(const char* str) {
    size_t n = strlen(str);
    Ciovec_t vec = {.buf = (uint8_t*)str, .buf_len=n};
    return fd_write(1, &vec, 1, 0);
}

int putStrLn(const char* str) {
    return putStr(str) || putStr("\n");
}


#endif // MINIC_RUNTIME_H_
