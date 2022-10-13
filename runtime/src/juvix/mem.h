#ifndef JUVIX_MEM_H
#define JUVIX_MEM_H

#include <juvix/mem/alloc.h>
#include <juvix/mem/gens.h>
#include <juvix/mem/mem.h>
#include <juvix/mem/pages.h>
#include <juvix/mem/stack.h>

static inline void mem_init() {
    alloc_init();
    stack_init();
}

#endif
