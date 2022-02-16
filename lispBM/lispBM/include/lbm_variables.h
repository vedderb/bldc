/*
    Copyright 2022 Joel Svensson   svenssonjoel@yahoo.se
    Copyright 2022 Benjamin Vedder

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

#ifndef LBM_VARIABLES_H_
#define LBM_VARIABLES_H_

#include "lbm_types.h"

extern int lbm_variables_init(lbm_value *variable_storage, int variable_storage_size);
extern lbm_value *lbm_get_variable_table(void);
extern lbm_value lbm_get_variable_by_index(int i);
extern const char *lbm_get_variable_name_by_index(int index);


/* internal use  by evaluator (mostly)*/
extern lbm_value lbm_get_var(lbm_uint index);
extern lbm_value lbm_set_var(lbm_uint index, lbm_value value);

#endif 
