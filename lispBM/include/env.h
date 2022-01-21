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

//environment interface
extern int lbm_init_env(void);
extern lbm_value *lbm_get_env_ptr(void);
extern lbm_value lbm_env_copy_shallow(lbm_value env);
extern lbm_value lbm_env_lookup(lbm_value sym, lbm_value env);
extern lbm_value lbm_env_set(lbm_value env, lbm_value key, lbm_value val);
extern lbm_value lbm_env_modify_binding(lbm_value env, lbm_value key, lbm_value val);

// Internal use
extern lbm_value lbm_env_build_params_args(lbm_value params,
                                       lbm_value args,
                                       lbm_value env0);

#endif
