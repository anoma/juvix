
#include <juvix/object/constr.h>
#include <juvix/object/equality.h>

bool juvix_constr_equal(word_t x, word_t y) {
    ASSERT(!is_ptr(x));
    ASSERT(!is_ptr(y));
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
        return juvix_equal(FST(x), FST(y)) && juvix_equal(SND(x), SND(y));
    }
}
