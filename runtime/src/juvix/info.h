#ifndef JUVIX_INFO_H
#define JUVIX_INFO_H

#include <juvix/defs.h>
#include <juvix/limits.h>

typedef enum { assoc_left, assoc_right, assoc_none } assoc_t;

typedef struct {
    int64_t precedence;
    assoc_t assoc;
} fixity_t;

#define PREC_UPDATE (-(1LL << 63))
#define PREC_ARROW (PREC_UPDATE + 1)
#define PREC_APP (9223372036854775807LL)

#define APP_FIXITY \
    { PREC_APP, assoc_left }

typedef struct {
    const char *name;
} function_info_t;

typedef struct {
    const char *name;
    bool is_infix;
    fixity_t fixity;
} constr_info_t;

extern function_info_t *juvix_function_info;
extern constr_info_t *juvix_constr_info;
extern size_t juvix_functions_num;
extern size_t juvix_constrs_num;

extern fixity_t app_fixity;

#endif
