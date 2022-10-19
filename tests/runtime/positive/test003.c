/* Printing of integers */

#include <juvix/object/integer.h>
#include <juvix/object/print.h>

int main() {
    printout(make_smallint(10));
    printout(make_smallint(2));
    printout(make_smallint(7));
    printout(make_smallint(123456789));
    printout(make_smallint(123456));
    printout(make_smallint(-123456789));
    return 0;
}
