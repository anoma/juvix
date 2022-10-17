
#include <juvix/mem/mem.h>

#ifndef API_LIBC
void memcopy(word_t *restrict dest, const word_t *restrict src, size_t n) {
    MEMCOPY(dest, src, n);
}
#endif

void memfill(word_t *dest, word_t val, size_t n) { MEMFILL(dest, val, n); }

#ifndef API_LIBC

#define BULK_MEMORY_THRESHOLD 32

// Copied from WASI libc sources, originally part of musl libc:
// https://github.com/WebAssembly/wasi-libc/blob/main/libc-top-half/musl/src/string/memset.c
void *memset(void *dest, int c, size_t n) {
#if defined(__wasm_bulk_memory__)
    if (n > BULK_MEMORY_THRESHOLD) return __builtin_memset(dest, c, n);
#endif
    unsigned char *s = dest;
    size_t k;

    /* Fill head and tail with minimal branching. Each
     * conditional ensures that all the subsequently used
     * offsets are well-defined and in the dest region. */

    if (!n) return dest;
    s[0] = c;
    s[n - 1] = c;
    if (n <= 2) return dest;
    s[1] = c;
    s[2] = c;
    s[n - 2] = c;
    s[n - 3] = c;
    if (n <= 6) return dest;
    s[3] = c;
    s[n - 4] = c;
    if (n <= 8) return dest;

    /* Advance pointer to align it at a 4-byte boundary,
     * and truncate n to a multiple of 4. The previous code
     * already took care of any head/tail that get cut off
     * by the alignment. */

    k = -(uintptr_t)s & 3;
    s += k;
    n -= k;
    n &= -4;

#ifdef __GNUC__
    typedef uint32_t __attribute__((__may_alias__)) u32;
    typedef uint64_t __attribute__((__may_alias__)) u64;

    u32 c32 = ((u32)-1) / 255 * (unsigned char)c;

    /* In preparation to copy 32 bytes at a time, aligned on
     * an 8-byte bounary, fill head/tail up to 28 bytes each.
     * As in the initial byte-based head/tail fill, each
     * conditional below ensures that the subsequent offsets
     * are valid (e.g. !(n<=24) implies n>=28). */

    *(u32 *)(s + 0) = c32;
    *(u32 *)(s + n - 4) = c32;
    if (n <= 8) return dest;
    *(u32 *)(s + 4) = c32;
    *(u32 *)(s + 8) = c32;
    *(u32 *)(s + n - 12) = c32;
    *(u32 *)(s + n - 8) = c32;
    if (n <= 24) return dest;
    *(u32 *)(s + 12) = c32;
    *(u32 *)(s + 16) = c32;
    *(u32 *)(s + 20) = c32;
    *(u32 *)(s + 24) = c32;
    *(u32 *)(s + n - 28) = c32;
    *(u32 *)(s + n - 24) = c32;
    *(u32 *)(s + n - 20) = c32;
    *(u32 *)(s + n - 16) = c32;

    /* Align to a multiple of 8 so we can fill 64 bits at a time,
     * and avoid writing the same bytes twice as much as is
     * practical without introducing additional branching. */

    k = 24 + ((uintptr_t)s & 4);
    s += k;
    n -= k;

    /* If this loop is reached, 28 tail bytes have already been
     * filled, so any remainder when n drops below 32 can be
     * safely ignored. */

    u64 c64 = c32 | ((u64)c32 << 32);
    for (; n >= 32; n -= 32, s += 32) {
        *(u64 *)(s + 0) = c64;
        *(u64 *)(s + 8) = c64;
        *(u64 *)(s + 16) = c64;
        *(u64 *)(s + 24) = c64;
    }
#else
    /* Pure C fallback with no aliasing violations. */
    for (; n; n--, s++) *s = c;
#endif

    return dest;
}

// Copied from WASI libc sources, originally part of musl libc:
// https://github.com/WebAssembly/wasi-libc/blob/main/libc-top-half/musl/src/string/memcpy.c
void *memcpy(void *restrict dest, const void *restrict src, size_t n) {
#if defined(__wasm_bulk_memory__)
    if (n > BULK_MEMORY_THRESHOLD) return __builtin_memcpy(dest, src, n);
#endif
    unsigned char *d = dest;
    const unsigned char *s = src;

#ifdef __GNUC__

#if __BYTE_ORDER == __LITTLE_ENDIAN
#define LS >>
#define RS <<
#else
#define LS <<
#define RS >>
#endif

    typedef uint32_t __attribute__((__may_alias__)) u32;
    uint32_t w, x;

    for (; (uintptr_t)s % 4 && n; n--) *d++ = *s++;

    if ((uintptr_t)d % 4 == 0) {
        for (; n >= 16; s += 16, d += 16, n -= 16) {
            *(u32 *)(d + 0) = *(u32 *)(s + 0);
            *(u32 *)(d + 4) = *(u32 *)(s + 4);
            *(u32 *)(d + 8) = *(u32 *)(s + 8);
            *(u32 *)(d + 12) = *(u32 *)(s + 12);
        }
        if (n & 8) {
            *(u32 *)(d + 0) = *(u32 *)(s + 0);
            *(u32 *)(d + 4) = *(u32 *)(s + 4);
            d += 8;
            s += 8;
        }
        if (n & 4) {
            *(u32 *)(d + 0) = *(u32 *)(s + 0);
            d += 4;
            s += 4;
        }
        if (n & 2) {
            *d++ = *s++;
            *d++ = *s++;
        }
        if (n & 1) {
            *d = *s;
        }
        return dest;
    }

    if (n >= 32) switch ((uintptr_t)d % 4) {
            case 1:
                w = *(u32 *)s;
                *d++ = *s++;
                *d++ = *s++;
                *d++ = *s++;
                n -= 3;
                for (; n >= 17; s += 16, d += 16, n -= 16) {
                    x = *(u32 *)(s + 1);
                    *(u32 *)(d + 0) = (w LS 24) | (x RS 8);
                    w = *(u32 *)(s + 5);
                    *(u32 *)(d + 4) = (x LS 24) | (w RS 8);
                    x = *(u32 *)(s + 9);
                    *(u32 *)(d + 8) = (w LS 24) | (x RS 8);
                    w = *(u32 *)(s + 13);
                    *(u32 *)(d + 12) = (x LS 24) | (w RS 8);
                }
                break;
            case 2:
                w = *(u32 *)s;
                *d++ = *s++;
                *d++ = *s++;
                n -= 2;
                for (; n >= 18; s += 16, d += 16, n -= 16) {
                    x = *(u32 *)(s + 2);
                    *(u32 *)(d + 0) = (w LS 16) | (x RS 16);
                    w = *(u32 *)(s + 6);
                    *(u32 *)(d + 4) = (x LS 16) | (w RS 16);
                    x = *(u32 *)(s + 10);
                    *(u32 *)(d + 8) = (w LS 16) | (x RS 16);
                    w = *(u32 *)(s + 14);
                    *(u32 *)(d + 12) = (x LS 16) | (w RS 16);
                }
                break;
            case 3:
                w = *(u32 *)s;
                *d++ = *s++;
                n -= 1;
                for (; n >= 19; s += 16, d += 16, n -= 16) {
                    x = *(u32 *)(s + 3);
                    *(u32 *)(d + 0) = (w LS 8) | (x RS 24);
                    w = *(u32 *)(s + 7);
                    *(u32 *)(d + 4) = (x LS 8) | (w RS 24);
                    x = *(u32 *)(s + 11);
                    *(u32 *)(d + 8) = (w LS 8) | (x RS 24);
                    w = *(u32 *)(s + 15);
                    *(u32 *)(d + 12) = (x LS 8) | (w RS 24);
                }
                break;
        }
    if (n & 16) {
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
    }
    if (n & 8) {
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
    }
    if (n & 4) {
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
        *d++ = *s++;
    }
    if (n & 2) {
        *d++ = *s++;
        *d++ = *s++;
    }
    if (n & 1) {
        *d = *s;
    }
    return dest;
#endif

    for (; n; n--) *d++ = *s++;
    return dest;
}

#ifdef __GNUC__
typedef __attribute__((__may_alias__)) size_t WT;
#define WS (sizeof(WT))
#endif

// Copied from WASI libc sources, originally part of musl libc:
// https://github.com/WebAssembly/wasi-libc/blob/main/libc-top-half/musl/src/string/memmove.c
void *memmove(void *dest, const void *src, size_t n) {
#if defined(__wasm_bulk_memory__)
    if (n > BULK_MEMORY_THRESHOLD) return __builtin_memmove(dest, src, n);
#endif
    char *d = dest;
    const char *s = src;

    if (d == s) return d;
    if ((uintptr_t)s - (uintptr_t)d - n <= -2 * n) return memcpy(d, s, n);

    if (d < s) {
#ifdef __GNUC__
        if ((uintptr_t)s % WS == (uintptr_t)d % WS) {
            while ((uintptr_t)d % WS) {
                if (!n--) return dest;
                *d++ = *s++;
            }
            for (; n >= WS; n -= WS, d += WS, s += WS) *(WT *)d = *(WT *)s;
        }
#endif
        for (; n; n--) *d++ = *s++;
    } else {
#ifdef __GNUC__
        if ((uintptr_t)s % WS == (uintptr_t)d % WS) {
            while ((uintptr_t)(d + n) % WS) {
                if (!n--) return dest;
                d[n] = s[n];
            }
            while (n >= WS) n -= WS, *(WT *)(d + n) = *(WT *)(s + n);
        }
#endif
        while (n) n--, d[n] = s[n];
    }

    return dest;
}

#endif
