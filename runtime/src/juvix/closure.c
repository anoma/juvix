
#include <juvix/closure.h>

word_t alloc_closure(uint fuid, label_addr_t addr, uint nfields, uint nargs) {
    word_t var = (word_t)alloc(nfields + 1);
    INIT_CLOSURE(var, fuid, (word_t)addr, nfields, nargs);
    return var;
}

word_t extend_closure(word_t closure, uint n) {
    word_t var = (word_t)alloc(get_nfields(closure) + 1);
    COPY_EXTEND_CLOSURE(var, closure, n);
    return var;
}
