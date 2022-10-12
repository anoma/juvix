
#include <juvix/info.h>
#include <juvix/mem/pages.h>
#include <juvix/stacktrace.h>

#ifdef DEBUG

#define BUFFER_SIZE (PAGE_SIZE / sizeof(uint))

static uint *buffer;
static uint index;
static uint size;

void stacktrace_init() {
    buffer = palloc(1);
    index = 0;
    size = 0;
}

void stacktrace_push(uint fuid) {
    buffer[index++] = fuid;
    ++size;
    index = index % BUFFER_SIZE;
}

void stacktrace_replace(uint fuid) {
    if (index > 0) {
        buffer[index - 1] = fuid;
    } else {
        buffer[BUFFER_SIZE - 1] = fuid;
    }
}

#define DEC(i)                   \
    do {                         \
        if (i > 0) {             \
            --i;                 \
        } else {                 \
            i = BUFFER_SIZE - 1; \
        }                        \
    } while (0)

void stacktrace_pop() {
    DEC(index);
    --size;
}

void stacktrace_dump() {
    uint i = index;
    uint s = size;
    print_msg("Stacktrace:\n");
    while (s > 0) {
        DEC(i);
        --s;
        ASSERT(buffer[i] < juvix_functions_num);
        print_msg(juvix_function_info[buffer[i]].name);
    }
}

#else

void stacktrace_init() {}
void stacktrace_push(uint fuid) {}
void stacktrace_replace(uint fuid) {}
void stacktrace_pop() {}
void stacktrace_dump() { print_msg("<stacktrace not available>"); }

#endif
