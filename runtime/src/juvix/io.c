
#include <juvix/io.h>
#include <juvix/mem/pages.h>
#include <juvix/object/constr.h>
#include <juvix/object/cstring.h>
#include <juvix/object/print.h>

static char *io_buffer = NULL;
static uint io_index = 0;

void io_init() {
    // TODO: use malloc and realloc once they're available for all APIs
    io_buffer = palloc(1);
    io_index = 0;
}

void io_flush() {
    ASSERT(io_index < PAGE_SIZE);
    if (io_index > 0) {
#ifdef API_LIBC
        io_buffer[io_index] = 0;
        if (fputs(io_buffer, stdout) == EOF) {
            error_exit_msg("write error");
        }
        fflush(stdout);
        io_index = 0;
#else
        io_buffer[io_index] = 0;
        print_msg(io_buffer);
        io_index = 0;
#endif
    }
}

static void io_write(word_t x) {
    if (io_buffer == NULL) {
        io_init();
    }
    ASSERT(io_index < PAGE_SIZE);
    if (is_cstring(x)) {
        const char *str = get_cstring(x);
        while (*str) {
            io_buffer[io_index++] = *str++;
            if (io_index == PAGE_SIZE) {
                io_buffer[PAGE_SIZE - 1] = 0;
                print_msg(io_buffer);
                error_exit_msg("error: IO buffer overflow");
            }
            if (io_buffer[io_index - 1] == '\n') {
                io_buffer[io_index - 1] = 0;
                print_msg(io_buffer);
                io_index = 0;
            }
        }
    } else {
        io_index += print_to_buf(io_buffer + io_index, PAGE_SIZE - io_index, x);
        if (io_index >= PAGE_SIZE) {
            io_buffer[PAGE_SIZE - 1] = 0;
            print_msg(io_buffer);
            error_exit_msg("error: IO buffer overflow");
        }
    }
}

#ifdef API_WASI
static char getchar() {
    uint8_t ch;
    ciovec_t v = {.buf = &ch, .buf_len = 1};
    if (fd_read(0, &v, 1, 0) != 0) {
        error_exit_msg("read error");
    }
    return ch;
}
#endif

static word_t io_readln() {
    if (io_buffer == NULL) {
        io_init();
    }
    ASSERT(io_index < PAGE_SIZE);
#if defined(API_LIBC)
    io_flush();
    if (fgets(io_buffer, MAX_CSTRING_LENGTH - 1, stdin) == NULL) {
        error_exit_msg("read error");
    }
    io_buffer[MAX_CSTRING_LENGTH - 1] = 0;
    return alloc_cstring(io_buffer);
#elif defined(API_WASI)
    io_flush();
    char c = getchar();
    uint i = 0;
    while (c != '\n') {
        if (i == MAX_CSTRING_LENGTH) {
            error_exit_msg("error: string too long");
        }
        io_buffer[i++] = c;
        c = getchar();
    }
    io_buffer[i] = 0;
    return alloc_cstring(io_buffer);
#else
    error_exit_msg("error: IO read not available");
#endif
}

void io_print_toplevel(word_t x) {
    if (x != OBJ_VOID) {
        io_write(x);
        ASSERT(io_index < PAGE_SIZE);
        io_buffer[io_index] = 0;
        print_msg(io_buffer);
    }
}

bool io_interpret(word_t x, word_t *ret, word_t *arg) {
    if (is_ptr(x) && has_header(x)) {
        switch (get_uid(x)) {
            case UID_RETURN:
                ASSERT(get_constr_nargs(x) == 1);
                *ret = CONSTR_ARG(x, 0);
                return false;
            case UID_BIND:
                ASSERT(get_constr_nargs(x) == 2);
                *ret = CONSTR_ARG(x, 1);
                *arg = CONSTR_ARG(x, 0);
                return true;
            case UID_WRITE:
                ASSERT(get_constr_nargs(x) == 1);
                io_write(CONSTR_ARG(x, 0));
                *ret = OBJ_VOID;
                return false;
            case UID_READLN:
                *ret = io_readln();
                return false;
            default:
                *ret = x;
                return false;
        }
    } else {
        *ret = x;
        return false;
    }
}
