#ifndef JUVIX_ARCH_WASI_H
#define JUVIX_ARCH_WASI_H

#include <juvix/arch/defs.h>

#ifdef API_WASI

_Noreturn void exit(int code);

void puts_nonl(const char *str);
void puts(const char *str);

typedef struct Ciovec {
    uint8_t *buf;
    uint32_t buf_len;
} ciovec_t;

uint16_t fd_write(uint32_t fd, ciovec_t *iovs_ptr, uint32_t iovs_len,
                  uint32_t *nwritten)
    __attribute__((__import_module__("wasi_snapshot_preview1"),
                   __import_name__("fd_write"), ));

uint16_t fd_read(uint32_t fd, ciovec_t *iovs_ptr, uint32_t iovs_len,
                 uint32_t *read)
    __attribute__((__import_module__("wasi_snapshot_preview1"),
                   __import_name__("fd_read"), ));

_Noreturn void proc_exit(uint32_t rval)
    __attribute__((__import_module__("wasi_snapshot_preview1"),
                   __import_name__("proc_exit"), ));

#endif

#endif
