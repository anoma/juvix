/* Basic compilation with Juvix runtime */

#include <juvix/api.h>

int main() {
    juvix_init();
    print_msg("Hello world!");
    return 0;
}
