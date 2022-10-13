#ifndef JUVIX_API_H
#define JUVIX_API_H

#include <juvix/funcall.h>
#include <juvix/mem.h>
#include <juvix/object.h>

static inline juvix_init() {
    mem_init();
    funcall_init();
}

#endif
