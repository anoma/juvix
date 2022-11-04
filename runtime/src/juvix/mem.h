#ifndef JUVIX_MEM_H
#define JUVIX_MEM_H

#include <juvix/mem/alloc.h>
#include <juvix/mem/gens.h>
#include <juvix/mem/mem.h>
#include <juvix/mem/pages.h>
#include <juvix/mem/stack.h>

#define MEM_DECLS        \
    DECL_MEMORY_POINTER; \
    DECL_STACK_POINTER

#define MEM_INIT                                   \
    alloc_init();                                  \
    juvix_memory_pointer = alloc_memory_pointer(); \
    juvix_stack_pointer = stack_init()

#endif
