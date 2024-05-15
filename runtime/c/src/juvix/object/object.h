#ifndef JUVIX_OBJECT_OBJECT_H
#define JUVIX_OBJECT_OBJECT_H

#include <juvix/defs.h>
#include <juvix/limits.h>

// The least significant two bits encode object kind
#define KIND_MASK 3
#define GET_KIND(x) ((word_t)(x)&KIND_MASK)

#define KIND_PTR 0
#define KIND_UNBOXED0 1
#define KIND_UNBOXED1 3
#define KIND_HEADER 2

#define KIND3_MASK 7
#define GET_KIND3(x) ((word_t)(x)&KIND3_MASK)

static inline bool is_unboxed(word_t x) { return x & 1; }
static inline bool is_ptr(word_t x) { return GET_KIND(x) == KIND_PTR; }
static inline bool is_header(word_t x) { return GET_KIND(x) == KIND_HEADER; }

/*
An ordinary header word consists of (field:bits in lower 32 bits from most to
least significant):

|NFIELDS:8|UID:20|MARK:1|SPECIAL:1|KIND:2|

- NFIELDS specifies the number of fields following the header.
- UID contains a unique object identifier.
- MARK is a mark bit used by the GC. It is always 0 when not in GC collection
  phase.
- SPECIAL is set to 0.
- KIND specifies the object kind. Contains the KIND_HEADER bit-pattern.


A special header word consists of:

|NFIELDS:8|RESERVED:16|SKIP:2|SUID:2|MARK:1|SPECIAL:1|KIND:2|

- NFIELDS specifies the number of fields following the header.
- RESERVED contains bits reserved for the special object (it depends on the SUID
  what they signify)
- SKIP indicates how may fields directly following the header contain
  non-garbage-collected raw bit patterns. SKIP = SKIP_ALL indicates that all
  fields contain raw bit patterns. SKIP < SKIP_ALL indicates the actual number
  of non-garbage collected raw bit-pattern fields.
- SUID contains the special object identifier.
- MARK is a mark bit used by the GC. It is always 0 when not in GC collection
  phase.
- SPECIAL is set to 1.
- KIND specifies the object kind. Contains the KIND_HEADER bit-pattern.
*/

#define MARK_MASK ((word_t)0x10)

static inline bool is_marked(word_t x) { return x & MARK_MASK; }
static inline word_t set_mark(word_t x) { return x | MARK_MASK; }
static inline word_t clear_mark(word_t x) { return x & ~MARK_MASK; }

#define UID_MASK ((word_t)0x00FFFFF0)
#define UID_SHIFT 4U
#define GET_UID(x) (((word_t)(x)&UID_MASK) >> UID_SHIFT)

#define NFIELDS_MASK 0xFF000000
#define NFIELDS_SHIFT 24U
#define GET_NFIELDS(x) ((word_t)(x) >> NFIELDS_SHIFT)

#define MAKE_HEADER(uid, nfields) \
    (((nfields) << NFIELDS_SHIFT) | ((uid) << UID_SHIFT) | KIND_HEADER)

static inline word_t make_header(word_t uid, word_t nfields) {
    ASSERT(uid < MAX_UIDS);
    ASSERT(nfields <= MAX_FIELDS);
    return MAKE_HEADER(uid, nfields);
}

#define MAKE_UNBOXED(x) (((x) << 1U) | 1U)

static inline word_t make_unboxed(word_t x) { return MAKE_UNBOXED(x); }
static inline word_t get_unboxed(word_t x) { return x >> 1U; }
static inline int_t get_unboxed_int(word_t x) { return (int_t)x >> 1; }

#define FIELD(var, n) (((word_t *)(var))[n])

static inline word_t get_header(word_t ptr) { return FIELD(ptr, 0); }
static inline bool has_header(word_t ptr) { return is_header(get_header(ptr)); }

static inline size_t get_nfields(word_t ptr) {
    return GET_NFIELDS(FIELD(ptr, 0));
}

static inline word_t get_uid(word_t ptr) { return GET_UID(FIELD(ptr, 0)); }

#define SPECIAL_MASK ((word_t)0x4)
#define SPECIAL_HEADER_MASK ((word_t)0x7)

#define KIND_SPECIAL_HEADER (SPECIAL_MASK | KIND_HEADER)

static inline bool is_special(word_t x) { return x & SPECIAL_MASK; }

static inline bool is_special_header(word_t x) {
    return (x & SPECIAL_HEADER_MASK) == KIND_SPECIAL_HEADER;
}

#define SUID_MASK ((word_t)0x30)
#define SUID_SHIFT 4U
#define GET_SUID(x) (((word_t)(x)&SUID_MASK) >> SUID_SHIFT)

#define SKIP_MASK ((word_t)0xC0)
#define SKIP_SHIFT 6U
#define GET_SKIP(x) (((word_t)(x)&SKIP_MASK) >> SKIP_SHIFT)
#define SKIP_ALL 3U

#define RESERVED_MASK 0x00FFFF00
#define RESERVED_SHIFT 8U
#define GET_RESERVED(x) (((word_t)(x)&RESERVED_MASK) >> RESERVED_SHIFT)

static inline word_t make_special_header(word_t suid, word_t nfields,
                                         word_t skip, word_t reserved) {
    ASSERT(suid < MAX_SUIDS);
    ASSERT(nfields <= MAX_FIELDS);
    ASSERT(skip <= SKIP_ALL);
    ASSERT(reserved <= (RESERVED_MASK >> RESERVED_SHIFT));
    return (nfields << NFIELDS_SHIFT) | (reserved << RESERVED_SHIFT) |
           (skip << SKIP_SHIFT) | (suid << SUID_SHIFT) | KIND_SPECIAL_HEADER;
}

static inline bool has_special_header(word_t ptr) {
    return is_special_header(get_header(ptr));
}

static inline size_t get_skip(word_t ptr) { return GET_SKIP(FIELD(ptr, 0)); }

static inline size_t get_reserved(word_t ptr) {
    return GET_RESERVED(FIELD(ptr, 0));
}

static inline word_t get_suid(word_t ptr) { return GET_SUID(FIELD(ptr, 0)); }

/*************************************************/
/* Builtin constructor UIDs */

#define UID_FALSE 0
#define UID_TRUE 1
#define UID_UNIT 2
#define UID_VOID 3
#define UID_RETURN 4
#define UID_BIND 5
#define UID_WRITE 6
#define UID_READLN 7

STATIC_ASSERT(BUILTIN_UIDS_NUM > UID_READLN);

#define FIRST_USER_UID BUILTIN_UIDS_NUM

#define BUILTIN_UIDS_INFO                                   \
    {"false", 0, APP_FIXITY}, {"true", 0, APP_FIXITY},      \
        {"unit", 0, APP_FIXITY}, {"void", 0, APP_FIXITY},   \
        {"return", 0, APP_FIXITY}, {"bind", 0, APP_FIXITY}, \
        {"write", 0, APP_FIXITY}, {                         \
        "readln", 0, APP_FIXITY                             \
    }

/*************************************************/
/* Special UIDs */

// A closure has a non-garbage-collected function
// address field immediately after the header. The other fields are closure
// arguments. The RESERVED bits indicate the number of arguments remaining. In
// debug mode, there is one more non-garbage-collected field after the header
// which contains the FUID (function id).
#define SUID_CLOSURE 0
// The header is followed by a zero-terminated string. NFIELDS contains the
// length of the string rounded up to a multiple of word size.
#define SUID_CSTRING 1

static inline bool is_closure(word_t x) {
    return is_ptr(x) && has_special_header(x) && get_suid(x) == SUID_CLOSURE;
}

static inline bool is_cstring(word_t x) {
    return is_ptr(x) && has_special_header(x) && get_suid(x) == SUID_CSTRING;
}

#define OBJ_UNIT MAKE_HEADER(UID_UNIT, 0)
#define OBJ_VOID MAKE_HEADER(UID_VOID, 0)

#endif
