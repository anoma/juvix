#ifndef JUVIX_LIMITS
#define JUVIX_LIMITS

#include <juvix/defs.h>

#define MAX_MEM_STRUCT_SIZE 64U

// The maximum number of words by which the global stack can be pushed or popped
// at once.
#define MAX_STACK_DELTA ((PAGE_SIZE - MAX_MEM_STRUCT_SIZE) / sizeof(word_t))

// The maximum number of words allocated in a single function.
#define MAX_FUNCTION_MEMORY ((PAGE_SIZE - MAX_MEM_STRUCT_SIZE) / sizeof(word_t))

// Maximum number of different UIDs (unique global object identifiers). Make
// sure this corresponds to the number of bits for the UID field in a header
// word (see juvix/object.h). Note that since UIDs start from 0, an UID cannot
// have the value MAX_UIDS.
#define MAX_UIDS 1048576U

// Maximum number of builtin constructor uids
#define MAX_BUILTIN_UIDS 64U

#define FIRST_USER_UID MAX_BUILTIN_UIDS

// Maximum number of different constructors (globally).
#define MAX_CONSTR_TAGS MAX_UIDS

// Maximum number of special UIDs. Make sure this corresponds to the number of
// bits in the SUID field in a special header word (see: juvix/object.h).
#define MAX_SUIDS 4U

// Maximum number of fields. Make sure this corresponds to the number of bits
// for the NFIELDS field in a header word (see juvix/object.h).
#define MAX_FIELDS 255U

#define MAX_CONSTR_ARGS MAX_FIELDS
// Max number of fields minus the extra field in a closure.
#define MAX_FUNCTION_ARGS (MAX_FIELDS - 1)

#define MAX_CSTRING_LENGTH (MAX_FIELDS * sizeof(word_t) - 1)

#define MAX_LOCAL_VARS (PAGE_SIZE / 2 / sizeof(word_t))

/*****************************************/
/* Static asserts */

STATIC_ASSERT(MAX_FUNCTION_ARGS + MAX_LOCAL_VARS + 2 < MAX_STACK_DELTA);

#endif
