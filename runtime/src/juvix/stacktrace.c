
#include <juvix/info.h>
#include <juvix/mem/pages.h>
#include <juvix/stacktrace.h>

#ifdef DEBUG

#define BUFFER_SIZE (PAGE_SIZE / sizeof(uint))

static uint *st_buffer;
static uint st_index;
static uint st_size;

void stacktrace_init() {
    st_buffer = palloc(1);
    st_index = 0;
    st_size = 0;
}

void stacktrace_push(uint fuid) {
    st_buffer[st_index++] = fuid;
    ++st_size;
    st_index = st_index % BUFFER_SIZE;
}

void stacktrace_replace(uint fuid) {
    if (st_index > 0) {
        st_buffer[st_index - 1] = fuid;
    } else {
        st_buffer[BUFFER_SIZE - 1] = fuid;
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
    DEC(st_index);
    --st_size;
}

void stacktrace_dump() {
    uint i = st_index;
    uint s = st_size;
    print_msg("Stacktrace:\n");
    while (s > 0) {
        DEC(i);
        --s;
        ASSERT(st_buffer[i] < juvix_functions_num);
        print_msg(juvix_function_info[st_buffer[i]].name);
    }
}

#else

void stacktrace_init() {}
void stacktrace_push(uint fuid) {}
void stacktrace_replace(uint fuid) {}
void stacktrace_pop() {}
void stacktrace_dump() { print_msg("<stacktrace not available>"); }

#endif
