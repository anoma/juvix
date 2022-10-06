#ifndef JUVIX_MEM_PAGES_H
#define JUVIX_MEM_PAGES_H

#include <juvix/defs.h>

// Allocate a continuous block of memory of `n` pages (`n * PAGE_SIZE` bytes).
// The memory is dword-aligned.
void *palloc(size_t n);
// Free a previously allocated block of `n` pages.
void pfree(void *ptr, size_t n);

#endif
