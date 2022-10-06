#ifndef JUVIX_LIMITS
#define JUVIX_LIMITS

#include <juvix/defs.h>

// The maximum number of words by which the global stack can be pushed or popped
// at once
#define MAX_STACK_DELTA ((PAGE_SIZE - 64) / sizeof(word_t))

#endif
