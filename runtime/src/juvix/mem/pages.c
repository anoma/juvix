
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
    page_t *prev = NULL;
    page_t *page = free_page;
    while (page && page->size < n) {
        prev = page;
        page = page->next;
    }
    if (page) {
        page_t *next;
        if (page->size > n) {
            next = (page_t *)((char *)page + (n << PAGE_SIZE_LOG2));
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
    uintptr_t heap_size = __builtin_wasm_memory_size(0) << PAGE_SIZE_LOG2;
    if (heap_end == NULL) {
        // first-time allocation
        heap_end = palign(&__heap_base, PAGE_SIZE);
        heap_pages_num = (heap_size - (uintptr_t)heap_end) >> PAGE_SIZE_LOG2;
    }
    if (heap_size - (uintptr_t)heap_end < (n << PAGE_SIZE_LOG2)) {
        size_t delta = max(opt_heap_grow_pages, max(n, heap_pages_num / 2));
        ASSERT(delta != 0);
        if (__builtin_wasm_memory_grow(0, delta) == (size_t)-1) {
            error_exit_msg("error: out of memory");
        }
        heap_pages_num += delta;
    }
    void *ptr = heap_end;
    heap_end = (char *)heap_end + (n << PAGE_SIZE_LOG2);
    ASSERT_ALIGNED(ptr, PAGE_SIZE);
    juvix_allocated_pages_num += n;
    if (juvix_allocated_pages_num > juvix_max_allocated_pages_num) {
        juvix_max_allocated_pages_num = juvix_allocated_pages_num;
    }
    return ptr;
}

void pfree(void *ptr, size_t n) {
    page_t *page = ptr;
    page->size = n;
    page->next = free_page;
    free_page = page;
    juvix_allocated_pages_num -= n;
}

#else
#error "Unsupported configuration"
#endif
