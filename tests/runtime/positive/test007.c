/* Prologue and epilogue */

#include <juvix/api.h>

int main() {
    JUVIX_PROLOGUE(3);
    juvix_result = make_smallint(789);
    JUVIX_EPILOGUE;
    return 0;
}
