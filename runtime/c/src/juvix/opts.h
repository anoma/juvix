#ifndef JUVIX_OPTS_H
#define JUVIX_OPTS_H

#include <juvix/defs.h>

// How many pages should be requested from the OS at once when we've run out of
// memory?
extern size_t opt_heap_grow_pages;
// The minimum number of pages in a generation.
extern size_t opt_generation_min_pages;

/*******************************/
/* Default values */

#ifndef OPT_HEAP_GROW_PAGES
#define OPT_HEAP_GROW_PAGES 24
#endif

#ifndef OPT_GENERATION_MIN_PAGES
#define OPT_GENERATION_MIN_PAGES 4
#endif

#endif
