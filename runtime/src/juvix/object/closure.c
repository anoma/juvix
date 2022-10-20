
#include <juvix/object/closure.h>

word_t alloc_closure(uint fuid, label_addr_t addr, uint nargs, uint largs) {
    word_t var = (word_t)alloc(nargs + CLOSURE_SKIP + 1);
    INIT_CLOSURE(var, fuid, (word_t)addr, nargs, largs);
    return var;
}

word_t extend_closure(word_t closure, uint n) {
    size_t nfields = get_nfields(closure) + n;
    word_t var = (word_t)alloc(nfields + 1);
    COPY_EXTEND_CLOSURE(var, closure, n, nfields);
    return var;
}
