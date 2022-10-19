#ifndef JUVIX_ARCH_DEFS_H
#define JUVIX_ARCH_DEFS_H

#include <juvix/config.h>

// Number of bits in a word
#if defined(ARCH_WASM32) || defined(ARCH_NATIVE32)
#define BITS32
#elif defined(ARCH_NATIVE64)
#define BITS64
#else
#error "Unsupported configuration"
#endif

// Detect compiler
#ifdef __GNUC__
#ifdef __clang__
#define COMPILER_CLANG
#else
#define COMPILER_GCC
#endif
#endif

#if defined(COMPILER_CLANG) || defined(COMPILER_GCC)
#define EXT_LABELS_AS_VALUES
#endif

#if defined(COMPILER_GCC) || defined(COMPILER_CLANG)
#define likely(exp) (__builtin_expect((exp), 1))
#define unlikely(exp) (__builtin_expect((exp), 0))
#else
#define likely(exp) (exp)
#define unlikely(exp) (exp)
#endif

#if defined(COMPILER_CLANG) || defined(COMPILER_GCC)
#define UNREACHABLE __builtin_unreachable()
#else
#define UNREACHABLE \
    do {            \
    } while (0)
#endif

// typedefs for basic integer types
#if (defined(COMPILER_CLANG) || defined(COMPILER_GCC)) && !defined(API_LIBC)

typedef __SIZE_TYPE__ size_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINTPTR_TYPE__ uintptr_t;

#elif defined(API_LIBC)

#include <stdint.h>

#else
#error "Unsupported configuration"
#endif

typedef unsigned uint;

// typedefs for word_t, dword_t, int_t, long_t
#if defined(BITS32)

typedef uint32_t word_t;
typedef uint64_t dword_t;
typedef int32_t int_t;
typedef int64_t long_t;

#elif defined(BITS64)

typedef uint64_t word_t;
typedef int64_t int_t;

#ifdef __SIZEOF_INT128__
typedef unsigned __int128 dword_t;
typedef __int128 long_t;
#else
#error "Unsupported configuration"
#endif

#else
#error "Unsupported configuration"
#endif

// PAGE_SIZE_LOG2 must be at least 16
#define PAGE_SIZE_LOG2 16U
#define PAGE_SIZE (1U << PAGE_SIZE_LOG2)
#define PAGE_MASK (PAGE_SIZE - 1)

// NULL
#ifndef NULL
#define NULL ((void *)0)
#endif

#ifdef API_LIBC
#include <stdbool.h>
#else
typedef int bool;
#define true 1
#define false 0
#endif

#ifdef EXT_LABELS_AS_VALUES
#define LABEL_ADDR(label) &&label
#define STORED_GOTO(ptr) goto *(ptr)
typedef void *label_addr_t;
#else
#error \
    "The \"labels as values\" compiler extension is required (use GCC or clang)."
#endif

#endif
