#ifndef Z_PARAM_H_
#define Z_PARAM_H_

#include <flint/flint.h>
#include <flint/dirichlet.h>

typedef struct
{
    dirichlet_group_t *G;
    dirichlet_char_t *chi;
}
z_param_struct;

typedef z_param_struct z_param_t[1];

#endif //  Z_PARAM_H_
