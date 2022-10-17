#ifndef JUVIX_DEFS_H
#define JUVIX_DEFS_H

#include <juvix/arch/defs.h>

#ifdef API_LIBC
#include <stdio.h>
#include <stdlib.h>
#endif

#ifdef API_WASI
#include <juvix/arch/wasi.h>
#endif

/**********************************************/
/* Basic primitive functions and macros */

static void print_msg(const char *msg) { puts(msg); }

_Noreturn static inline void error_exit() {
#if defined(API_LIBC)
    abort();
#elif defined(ARCH_WASM32)
    __builtin_trap();
#endif
}

_Noreturn static inline void error_exit_msg(const char *msg) {
    print_msg(msg);
    error_exit();
}

// Debug assertions
#ifdef DEBUG
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define ASSERT(x)                                                      \
    do {                                                               \
        if (!(x))                                                      \
            error_exit_msg(__FILE__ ":" TOSTRING(                      \
                __LINE__) ": Juvix assertion failed. Please report."); \
    } while (0)
#else
#define ASSERT(x) \
    do {          \
    } while (0)
#endif
#define ASSERT_EQ(a, b) ASSERT((a) == (b))

// Static assertions (requires C11)
#define STATIC_ASSERT(x) _Static_assert((x), "assertion failed")
#define STATIC_ASSERT_EQ(a, b) STATIC_ASSERT((a) == (b))

static inline size_t max(size_t a, size_t b) { return a < b ? b : a; }
// `alignment` must be a power of 2
static inline uintptr_t align(uintptr_t val, uintptr_t alignment) {
    return (val + alignment - 1) & ~(alignment - 1);
}
static inline void *palign(void *ptr, uintptr_t alignment) {
    return (void *)align((uintptr_t)ptr, alignment);
}
#define ASSERT_ALIGNED(x, y) \
    ASSERT((uintptr_t)(x) == align((uintptr_t)(x), (y)))

/*************************************************************************/
/* Static assertions */

#if defined(BITS32)
STATIC_ASSERT_EQ(sizeof(void *), 4);
#elif defined(BITS64)
STATIC_ASSERT_EQ(sizeof(void *), 8);
#else
#error "Unsupported configuration"
#endif

#endif
