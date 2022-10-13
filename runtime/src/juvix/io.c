
#include <juvix/constr.h>
#include <juvix/cstring.h>
#include <juvix/io.h>
#include <juvix/mem/pages.h>
#include <juvix/print.h>

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
    ASSERT(io_index < PAGE_SIZE);
    if (is_cstring(x)) {
        const char *str = get_cstring(x);
        while (*str) {
            io_buffer[io_index++] = *str++;
            if (io_index == PAGE_SIZE) {
                io_buffer[PAGE_SIZE - 1] = 0;
                print_msg(io_buffer);
                error_exit_msg("error: IO io_buffer overflow");
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
            error_exit_msg("error: IO io_buffer overflow");
        }
    }
}

static word_t io_readln() {
    ASSERT(io_index < PAGE_SIZE);
#ifdef API_LIBC
    io_flush();
    if (fgets(io_buffer, MAX_CSTRING_LENGTH - 1, stdin) == NULL) {
        error_exit_msg("read error");
    }
    io_buffer[MAX_CSTRING_LENGTH - 1] = 0;
    return alloc_cstring(io_buffer);
#else
    error_exit_msg("error: IO read not available");
#endif
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
