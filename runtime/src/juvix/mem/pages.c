
#include <juvix/mem/pages.h>
#include <juvix/opts.h>

size_t juvix_max_allocated_pages_num = 0;
size_t juvix_allocated_pages_num = 0;

#if defined(API_LIBC)

#include <stdlib.h>

void *palloc(size_t n) {
    // `aligned_alloc` requires C11
    void *ptr = aligned_alloc(PAGE_SIZE, n * PAGE_SIZE);
    if (ptr == NULL) {
        error_exit_msg("error: out of memory");
    }
    ASSERT_ALIGNED(ptr, PAGE_SIZE);
    juvix_allocated_pages_num += n;
    if (juvix_allocated_pages_num > juvix_max_allocated_pages_num) {
        juvix_max_allocated_pages_num = juvix_allocated_pages_num;
    }
    return ptr;
}

void pfree(void *ptr, size_t n) {
    juvix_allocated_pages_num -= n;
    free(ptr);
}

#elif defined(ARCH_WASM32)

#define WASM_PAGE_SIZE_LOG2 16U
#define WASM_PAGE_SIZE (1U << WASM_PAGE_SIZE_LOG2)

STATIC_ASSERT(PAGE_SIZE_LOG2 >= WASM_PAGE_SIZE_LOG2);

typedef struct Page {
    struct Page *next;
    size_t size;  // the number of WASM pages
} page_t;

extern void __heap_base;
static void *heap_end = NULL;
static size_t heap_pages_num;
static page_t *free_page = NULL;

void *palloc(size_t n) {
    ASSERT(n > 0);
    n = n << (PAGE_SIZE_LOG2 - WASM_PAGE_SIZE_LOG2);
    // now `n` is the number of WASM pages to allocate
    page_t *prev = NULL;
    page_t *page = free_page;
    while (page && page->size < n) {
        prev = page;
        page = page->next;
    }
    if (page) {
        page_t *next;
        if (page->size > n) {
            next = (page_t *)((char *)page + (n << WASM_PAGE_SIZE_LOG2));
            next->size = page->size - n;
            next->next = page->next;
        } else {
            next = page->next;
        }
        if (prev) {
            prev->next = next;
        } else {
            free_page = next;
        }
        ASSERT_ALIGNED(page, PAGE_SIZE);
        juvix_allocated_pages_num += n;
        if (juvix_allocated_pages_num > juvix_max_allocated_pages_num) {
            juvix_max_allocated_pages_num = juvix_allocated_pages_num;
        }
        return page;
    }

    // Allocate `n` pages from WebAssembly
    uintptr_t heap_size = __builtin_wasm_memory_size(0) << WASM_PAGE_SIZE_LOG2;
    if (heap_end == NULL) {
        // first-time allocation
        heap_end = palign(&__heap_base, PAGE_SIZE);
        if (heap_size < (uintptr_t)heap_end) {
            size_t delta = ((uintptr_t)heap_end - heap_size + WASM_PAGE_SIZE) >>
                           WASM_PAGE_SIZE_LOG2;
            if (__builtin_wasm_memory_grow(0, delta) == (size_t)-1) {
                error_exit_msg("error: out of memory");
            }
            heap_size += delta << WASM_PAGE_SIZE_LOG2;
            ASSERT((uintptr_t)heap_end <= heap_size);
        }
        heap_pages_num =
            (heap_size - (uintptr_t)heap_end) >> WASM_PAGE_SIZE_LOG2;
    }
    if (heap_size - (uintptr_t)heap_end < (n << WASM_PAGE_SIZE_LOG2)) {
        size_t delta =
            max(opt_heap_grow_pages << (PAGE_SIZE_LOG2 - WASM_PAGE_SIZE_LOG2),
                max(n, heap_pages_num / 2));
        ASSERT(delta != 0);
        if (__builtin_wasm_memory_grow(0, delta) == (size_t)-1) {
            error_exit_msg("error: out of memory");
        }
        heap_pages_num += delta;
    }
    void *ptr = heap_end;
    heap_end = (char *)heap_end + (n << WASM_PAGE_SIZE_LOG2);
    ASSERT_ALIGNED(ptr, PAGE_SIZE);
    juvix_allocated_pages_num += n >> (PAGE_SIZE_LOG2 - WASM_PAGE_SIZE_LOG2);
    if (juvix_allocated_pages_num > juvix_max_allocated_pages_num) {
        juvix_max_allocated_pages_num = juvix_allocated_pages_num;
    }
    return ptr;
}

void pfree(void *ptr, size_t n) {
    n = n << (PAGE_SIZE_LOG2 - WASM_PAGE_SIZE_LOG2);
    page_t *page = ptr;
    page->size = n;
    page->next = free_page;
    free_page = page;
    juvix_allocated_pages_num -= n >> (PAGE_SIZE_LOG2 - WASM_PAGE_SIZE_LOG2);
}

#else
#error "Unsupported configuration"
#endif
