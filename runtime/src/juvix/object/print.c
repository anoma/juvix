
#include <juvix/info.h>
#include <juvix/mem/pages.h>
#include <juvix/object/closure.h>
#include <juvix/object/constr.h>
#include <juvix/object/cstring.h>
#include <juvix/object/object.h>
#include <juvix/object/print.h>

static size_t print_long(char *buf, size_t n, long_t x) {
    ASSERT(n >= 1);
    if (x == 0) {
        *buf++ = '0';
        return 1;
    }
    size_t sign = 0;
    if (x < 0) {
        x = -x;
        *buf++ = '-';
        --n;
        sign = 1;
    }
    char *buf0 = buf;
    while (x != 0 && n > 0) {
        long_t y = x % 10;
        *buf++ = '0' + y;
        x = x / 10;
        --n;
    }
    size_t k = buf - buf0;
    for (size_t i = 0; i < k / 2; ++i) {
        char c = buf0[i];
        buf0[i] = *(buf - i - 1);
        *(buf - i - 1) = c;
    }
    return k + sign;
}

#define PUTC(c)                          \
    do {                                 \
        *buf++ = c;                      \
        if (--n == 0) return buf - buf0; \
    } while (0)

static size_t print_object(bool is_top, char *buf, size_t n, word_t x);

static size_t print_args(bool is_top, char *restrict buf, size_t n,
                         const char *restrict str, word_t *args, size_t nargs) {
    ASSERT(n >= 1);
    char *buf0 = buf;
    if (!is_top && nargs > 0) {
        PUTC('(');
    }
    while (*str) {
        PUTC(*str++);
    }
    for (size_t i = 0; i < nargs; ++i) {
        PUTC(' ');
        size_t delta = print_object(false, buf, n, args[i]);
        buf += delta;
        n -= delta;
        if (n == 0) {
            return buf - buf0;
        }
    }
    if (!is_top && nargs > 0) {
        PUTC(')');
    }
    return buf - buf0;
}

static size_t print_object(bool is_top, char *buf, size_t n, word_t x) {
    ASSERT(n >= 1);
    switch (GET_KIND(x)) {
        case KIND_UNBOXED0:
        case KIND_UNBOXED1:
            return print_long(buf, n, get_unboxed_int(x));
        case KIND_PTR: {
            char *buf0 = buf;
            word_t h = get_header(x);
            size_t delta;
            if (is_header(h)) {
                if (is_special(h)) {
                    switch (GET_SUID(h)) {
                        case SUID_CLOSURE: {
                            size_t nargs = get_closure_nargs(x);
#ifdef DEBUG
                            ASSERT(get_closure_fuid(x) < juvix_functions_num);
                            const char *str =
                                juvix_function_info[get_closure_fuid(x)].name;
#else
                            const char *str = "<closure>";
#endif
                            buf += print_args(is_top, buf, n, str,
                                              get_closure_args(x), nargs);
                            break;
                        }
                        case SUID_CSTRING: {
                            const char *str = get_cstring(x);
                            while (*str) {
                                PUTC(*str++);
                            }
                            break;
                        }
                    }
                } else {
                    size_t nargs = GET_NFIELDS(h);
                    ASSERT(GET_UID(x) < juvix_constrs_num);
                    const char *str = juvix_constr_info[GET_UID(h)].name;
                    buf += print_args(is_top, buf, n, str, get_constr_args(x),
                                      nargs);
                }
            } else {
                PUTC('(');
                delta = print_object(true, buf, n, FST(x));
                n -= delta;
                buf += delta;
                if (n == 0) {
                    return buf - buf0;
                }
                PUTC(',');
                PUTC(' ');
                delta = print_object(true, buf, n, SND(x));
                n -= delta;
                buf += delta;
                if (n == 0) {
                    return buf - buf0;
                }
                PUTC(')');
            }
            return buf - buf0;
        }
        case KIND_HEADER:
            ASSERT(is_header(x));
            char *buf0 = buf;
            ASSERT(GET_UID(x) < juvix_constrs_num);
            const char *str = juvix_constr_info[GET_UID(x)].name;
            while (*str) {
                PUTC(*str++);
            }
            return buf - buf0;
        default:
            UNREACHABLE;
    }
}

#undef PUTC

size_t print_to_buf(char *buf, size_t n, word_t x) {
    ASSERT(n >= 1);
    size_t k = print_object(true, buf, n, x);
    if (k < n) {
        buf[k] = 0;
    }
    return k;
}

char *print(word_t x) {
    // TODO: replace this with malloc when we have it for all APIs
    char *buf = palloc(1);
    size_t n = print_to_buf(buf, PAGE_SIZE, x);
    if (n == PAGE_SIZE) {
        buf[PAGE_SIZE - 1] = 0;
        buf[PAGE_SIZE - 2] = '.';
        buf[PAGE_SIZE - 3] = '.';
        buf[PAGE_SIZE - 4] = '.';
    }
    return buf;
}

void free_strbuf(char *buf) { pfree(buf, 1); }

// Prints using print_msg
void printout(word_t x) {
    char *buf = print(x);
    print_msg(buf);
    free_strbuf(buf);
}
