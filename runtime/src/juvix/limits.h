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

#define BUILTIN_UIDS_NUM 8U

// Maximum number of different user constructors (globally).
#define MAX_CONSTR_TAGS (MAX_UIDS - BUILTIN_UIDS_NUM)

// Maximum number of special UIDs. Make sure this corresponds to the number of
// bits in the SUID field in a special header word (see: juvix/object.h).
#define MAX_SUIDS 4U

// Maximum number of fields. Make sure this corresponds to the number of bits
// for the NFIELDS field in a header word (see juvix/object.h).
#define MAX_FIELDS 255U

#define MAX_CONSTR_ARGS MAX_FIELDS
// Max number of fields minus the extra fields in a closure.
#define MAX_FUNCTION_ARGS (MAX_FIELDS - 2)

#define MAX_CSTRING_LENGTH (MAX_FIELDS * sizeof(word_t) - 1)

#define MAX_LOCAL_VARS (PAGE_SIZE / 2 / sizeof(word_t))

// The value that should be added to the stack height for the CallClosures
// invocation.
#define DISPATCH_STACK_SIZE 4

// The size of a closure is CLOSURE_HEAD_SIZE + arguments number
#ifdef DEBUG
#define CLOSURE_HEAD_SIZE 3
#else
#define CLOSURE_HEAD_SIZE 2
#endif

#define MAX_CLOSURE_SIZE (MAX_FUNCTION_ARGS + CLOSURE_HEAD_SIZE)

/*****************************************/
/* Static asserts */

STATIC_ASSERT(MAX_FUNCTION_ARGS + MAX_LOCAL_VARS + 2 < MAX_STACK_DELTA);

#endif
