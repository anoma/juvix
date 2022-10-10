#ifndef JUVIX_LIMITS
#define JUVIX_LIMITS

#include <juvix/defs.h>

// The maximum number of words by which the global stack can be pushed or popped
// at once
#define MAX_STACK_DELTA ((PAGE_SIZE - 64) / sizeof(word_t))

// Maximum number of different UIDs (unique global object identifiers). Make
// sure this corresponds to the number of bits for the UID field in a header
// word (see juvix/object.h). Note that since UIDs start from 0, an UID cannot
// have the value MAX_UIDS.
#define MAX_UIDS 1048576

// Maximum special UIDs which don't correspond to constructor tags. UIDs
// corresponding to constructor tags have values at equal or greater.
#define MAX_SPECIAL_UIDS 16

// Maximum number of different constructors (globally).
#define MAX_CONSTR_TAGS (MAX_UIDS - MAX_SPECIAL_UIDS)

// Maximum number of functions (globally). Make sure this corresponds to the
// number of bits in the FUID field in juvix/closure.h.
#define MAX_FUNCTIONS 16777216

// Maximum number of fields. Make sure this corresponds to the number of bits
// for the NFIELDS field in a header word (see juvix/object.h).
#define MAX_FIELDS 255

#define MAX_CONSTR_ARGS MAX_FIELDS
#define MAX_FUNCTION_ARGS MAX_FIELDS

#define MAX_LOCAL_VARS 8192

/*****************************************/
/* Static asserts */

STATIC_ASSERT(MAX_FUNCTION_ARGS + MAX_LOCAL_VARS + 2 < MAX_STACK_DELTA);

#endif
