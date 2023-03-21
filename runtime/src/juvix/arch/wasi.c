
#include <juvix/arch/wasi.h>

#ifdef API_WASI

_Noreturn void exit(int code) { proc_exit(code); }

void puts_nonl(const char *msg) {
    size_t n = 0;
    while (msg[n]) ++n;
    ciovec_t vec = {.buf = (uint8_t *)msg, .buf_len = n};
    fd_write(1, &vec, 1, 0);
}

void puts(const char *msg) {
    puts_nonl(msg);
    uint8_t c = '\n';
    ciovec_t vec1 = {.buf = &c, .buf_len = 1};
    fd_write(1, &vec1, 1, 0);
}
#endif
