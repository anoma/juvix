#ifndef JUVIX_FUNCALL_STACKTRACE_H
#define JUVIX_FUNCALL_STACKTRACE_H

#include <juvix/defs.h>

#ifdef DEBUG

#define STACKTRACE_PUSH(fuid) stacktrace_push(fuid)
#define STACKTRACE_REPLACE(fuid) stacktrace_replace(fuid)
#define STACKTRACE_POP stacktrace_pop()

#else

#define STACKTRACE_PUSH(fuid) \
    do {                      \
    } while (0)

#define STACKTRACE_REPLACE(fuid) \
    do {                         \
    } while (0)

#define STACKTRACE_POP \
    do {               \
    } while (0)

#endif

void stacktrace_init();
void stacktrace_push(uint fuid);
void stacktrace_replace(uint fuid);
void stacktrace_pop();
void stacktrace_dump();

#endif
