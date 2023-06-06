#ifndef JUVIX_INFO_H
#define JUVIX_INFO_H

#include <juvix/defs.h>

typedef enum { assoc_left, assoc_right, assoc_none } assoc_t;

typedef struct {
    int precedence;
    assoc_t assoc;
} fixity_t;

#define PREC_MINUS_OMEGA (-1)
#define PREC_OMEGA 100000

#define APP_FIXITY \
    { PREC_OMEGA, assoc_left }

typedef struct {
    const char *name;
} function_info_t;

typedef struct {
    const char *name;
    bool is_binary;
    fixity_t fixity;
} constr_info_t;

extern function_info_t *juvix_function_info;
extern constr_info_t *juvix_constr_info;
extern size_t juvix_functions_num;
extern size_t juvix_constrs_num;

extern fixity_t app_fixity;

#endif
