
#include <juvix/constr.h>
#include <juvix/equality.h>

bool juvix_do_equal(word_t x, word_t y) {
    ASSERT(!is_unboxed(x));
    ASSERT(!is_unboxed(y));
    ASSERT(GET_KIND(x) == GET_KIND(y));

    switch (GET_KIND(x)) {
    case KIND_PTR:
        ASSERT(is_ptr(y));
        word_t hx = get_header(x);
        word_t hy = get_header(y);
        if (hx != hy) {
            return false;
        }
        if (is_header(hx)) {
            size_t n = GET_NFIELDS(hx);
            for (size_t i = 1; i <= n; ++i) {
                if (!juvix_equal(FIELD(x, i), FIELD(y, i))) {
                    return false;
                }
            }
            return true;
        } else {
            return SND(x) == SND(y);
        }
    case KIND_HEADER_OR_DWORDPTR:
        if (is_dword_ptr(x)) {
            ASSERT(is_dword_ptr(y));
            return *get_dword_ptr(x) == *get_dword_ptr(y);
        } else {
            ASSERT(is_header(x));
            ASSERT(is_header(y));
            return x == y;
        }
    default:
        UNREACHABLE;
    }
}
