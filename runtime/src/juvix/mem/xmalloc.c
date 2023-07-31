
#include <juvix/mem/xmalloc.h>

#if defined(ARCH_ZKLLVM) || defined(API_LIBC)

#include <stdlib.h>

void *xmalloc(size_t n) { return malloc(n); }

#else

void *xmalloc(size_t n) { error_exit_msg("malloc not available"); }

#endif
