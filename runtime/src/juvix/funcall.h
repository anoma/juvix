#ifndef JUVIX_FUNCALL_H
#define JUVIX_FUNCALL_H

#include <juvix/funcall/funcall.h>
#include <juvix/funcall/stacktrace.h>

#define FUNCALL_DECLS(MAX_ARGS) \
    DECL_RESULT;                \
    DECL_CARGS(MAX_ARGS)

static inline void funcall_init() { stacktrace_init(); }

#endif
