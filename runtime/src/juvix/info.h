#ifndef JUVIX_INFO_H
#define JUVIX_INFO_H

#include <juvix/defs.h>

typedef struct {
    const char *name;
} function_info_t;

typedef struct {
    const char *name;
} constr_info_t;

extern function_info_t *juvix_function_info;
extern constr_info_t *juvix_constr_info;

#endif
