/*
    Copyright 2018 Joel Svensson        svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ENV_H_
#define ENV_H_

#include "lispbm_types.h"

extern int env_init(void);
extern VALUE *env_get_global_ptr(void);
extern VALUE env_copy_shallow(VALUE env);
extern VALUE env_lookup(VALUE sym, VALUE env);
extern VALUE env_set(VALUE env, VALUE key, VALUE val);
extern VALUE env_modify_binding(VALUE env, VALUE key, VALUE val);
extern VALUE env_build_params_args(VALUE params,
                                   VALUE args,
                                   VALUE env0);

#endif
