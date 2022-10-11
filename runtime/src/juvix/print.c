
#include <juvix/closure.h>
#include <juvix/constr.h>
#include <juvix/cstring.h>
#include <juvix/info.h>
#include <juvix/mem/pages.h>
#include <juvix/object.h>
#include <juvix/print.h>

static size_t print_long(char *buf, size_t n, long_t x) {
    if (x == 0) {
        *buf++ = '0';
        return 1;
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
    return k;
}

#define PUTC(c)                          \
    do {                                 \
        *buf++ = c;                      \
        if (--n == 0) return buf - buf0; \
    } while (0)

static size_t print_object(char *buf, size_t n, word_t x);

static size_t print_args(char *restrict buf, size_t n, const char *restrict str,
                         word_t *args, size_t nargs) {
    char *buf0 = buf;
    if (nargs > 0) {
        PUTC(')');
    }
    while (*str) {
        PUTC(*str++);
    }
    for (size_t i = 0; i < nargs; ++i) {
        PUTC(' ');
        size_t delta = print_object(buf, n, args[i]);
        buf += delta;
        n -= delta;
        if (n == 0) {
            return buf - buf0;
        }
    }
    if (nargs > 0) {
        PUTC(')');
    }
    return buf - buf0;
}

static size_t print_object(char *buf, size_t n, word_t x) {
    switch (GET_KIND(x)) {
    case KIND_UNBOXED0:
    case KIND_UNBOXED1:
        return print_long(buf, n, get_unboxed_int(x));
    case KIND_PTR: {
        char *buf0 = buf;
        word_t h = get_header(x);
        size_t delta;
        if (is_header(h)) {
            switch (GET_UID(h)) {
            case UID_CLOSURE: {
                size_t nargs = get_closure_nargs(x);
                const char *str = juvix_function_info[get_closure_fuid(x)].name;
                buf += print_args(buf, n, str, get_closure_args(x), nargs);
                break;
            }
            case UID_CSTRING: {
                const char *str = get_cstring(x);
                while (*str) {
                    PUTC(*str++);
                }
                break;
            }
            default: {
                size_t nargs = GET_NFIELDS(h);
                const char *str = juvix_constr_info[GET_UID(h)].name;
                buf += print_args(buf, n, str, get_constr_args(x), nargs);
                break;
            }
            }
        } else {
            PUTC('(');
            delta = print_object(buf, n, FST(x));
            n -= delta;
            buf += delta;
            if (n == 0) {
                return buf - buf0;
            }
            PUTC(',');
            PUTC(' ');
            delta = print_object(buf, n, SND(x));
            n -= delta;
            buf += delta;
            if (n == 0) {
                return buf - buf0;
            }
            PUTC(')');
        }
        return buf - buf0;
    }
    case KIND_HEADER_OR_DWORDPTR:
        ASSERT(is_dword_ptr(x));
        return print_long(buf, n, get_long(x));
    default:
        UNREACHABLE;
    }
}

#undef PUTC

size_t print_to_buf(char *buf, size_t n, word_t x) {
    size_t k = print_object(buf, n, x);
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
