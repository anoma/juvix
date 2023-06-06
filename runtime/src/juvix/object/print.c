
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

static bool check_parens(assoc_t assoc, fixity_t *op_fixity,
                         fixity_t *arg_fixity) {
    if (op_fixity == NULL) {
        return false;
    } else if (arg_fixity->precedence > op_fixity->precedence) {
        return false;
    } else if (arg_fixity->precedence < op_fixity->precedence) {
        return true;
    } else if (op_fixity->assoc == assoc) {
        return false;
    } else {
        return true;
    }
}

#define PUTC(c)                          \
    do {                                 \
        *buf++ = c;                      \
        if (--n == 0) return buf - buf0; \
    } while (0)

static size_t print_object(assoc_t assoc, fixity_t *fixity, char *buf, size_t n,
                           word_t x);

static size_t print_app(bool needs_parens, bool is_binop, char *restrict buf,
                        size_t n, const char *restrict str, word_t *args,
                        size_t nargs) {
    ASSERT(n >= 1);
    char *buf0 = buf;
    if (needs_parens && nargs > 0) {
        PUTC('(');
    }
    if (is_binop) {
        PUTC('(');
    }
    while (*str) {
        PUTC(*str++);
    }
    if (is_binop) {
        PUTC(')');
    }
    for (size_t i = 0; i < nargs; ++i) {
        PUTC(' ');
        size_t delta = print_object(assoc_right, &app_fixity, buf, n, args[i]);
        buf += delta;
        n -= delta;
        if (n == 0) {
            return buf - buf0;
        }
    }
    if (needs_parens && nargs > 0) {
        PUTC(')');
    }
    return buf - buf0;
}

static size_t print_binop(bool needs_parens, fixity_t *op_fixity,
                          char *restrict buf, size_t n,
                          const char *restrict str, word_t *args) {
    ASSERT(n >= 1);
    char *buf0 = buf;
    if (needs_parens) {
        PUTC('(');
    }
    // print left arg
    size_t delta = print_object(assoc_left, op_fixity, buf, n, args[0]);
    buf += delta;
    n -= delta;
    if (n == 0) {
        return buf - buf0;
    }
    // print constructor name
    if (str[0] != ',') {
        PUTC(' ');
    }
    while (*str) {
        PUTC(*str++);
    }
    PUTC(' ');
    // print right arg
    delta = print_object(assoc_right, op_fixity, buf, n, args[1]);
    buf += delta;
    n -= delta;
    if (n == 0) {
        return buf - buf0;
    }
    if (needs_parens) {
        PUTC(')');
    }
    return buf - buf0;
}

static size_t print_object(assoc_t assoc, fixity_t *fixity, char *buf, size_t n,
                           word_t x) {
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
                            const char *str =
                                get_closure_fuid(x) < juvix_functions_num
                                    ? juvix_function_info[get_closure_fuid(x)]
                                          .name
                                    : "<function>";
#else
                            const char *str = "<function>";
#endif
                            bool needs_parens =
                                check_parens(assoc, fixity, &app_fixity);
                            buf += print_app(needs_parens, false, buf, n, str,
                                             get_closure_args(x), nargs);
                            break;
                        }
                        case SUID_CSTRING: {
                            const char *str = get_cstring(x);
                            PUTC('"');
                            while (*str) {
                                switch (*str) {
                                    case '\a':
                                        PUTC('\\');
                                        PUTC('a');
                                        ++str;
                                        break;
                                    case '\b':
                                        PUTC('\\');
                                        PUTC('b');
                                        ++str;
                                        break;
                                    case '\f':
                                        PUTC('\\');
                                        PUTC('f');
                                        ++str;
                                        break;
                                    case '\n':
                                        PUTC('\\');
                                        PUTC('n');
                                        ++str;
                                        break;
                                    case '\r':
                                        PUTC('\\');
                                        PUTC('r');
                                        ++str;
                                        break;
                                    case '\t':
                                        PUTC('\\');
                                        PUTC('t');
                                        ++str;
                                        break;
                                    case '\v':
                                        PUTC('\\');
                                        PUTC('v');
                                        ++str;
                                        break;
                                    case '\\':
                                        PUTC('\\');
                                        PUTC('\\');
                                        ++str;
                                        break;
                                    case '"':
                                        PUTC('\\');
                                        PUTC('"');
                                        ++str;
                                        break;
                                    default:
                                        PUTC(*str++);
                                }
                            }
                            PUTC('"');
                            break;
                        }
                    }
                } else {
                    size_t nargs = GET_NFIELDS(h);
                    ASSERT(GET_UID(h) < juvix_constrs_num);
                    constr_info_t *ci = &juvix_constr_info[GET_UID(h)];
                    const char *str = ci->name;
                    if (ci->is_infix && nargs == 2) {
                        bool needs_parens =
                            check_parens(assoc, fixity, &ci->fixity);
                        buf += print_binop(needs_parens, &ci->fixity, buf, n,
                                           str, get_constr_args(x));
                    } else {
                        bool needs_parens =
                            check_parens(assoc, fixity, &app_fixity);
                        buf += print_app(needs_parens, ci->is_infix, buf, n,
                                         str, get_constr_args(x), nargs);
                    }
                }
            } else {
                PUTC('(');
                delta = print_object(assoc_none, &app_fixity, buf, n, FST(x));
                n -= delta;
                buf += delta;
                if (n == 0) {
                    return buf - buf0;
                }
                PUTC(',');
                PUTC(' ');
                delta = print_object(assoc_none, &app_fixity, buf, n, SND(x));
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
    size_t k = print_object(assoc_none, NULL, buf, n, x);
    if (k < n) {
        buf[k] = 0;
    }
    return k;
}

// The returned pointer should be freed with `free_strbuf`
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
