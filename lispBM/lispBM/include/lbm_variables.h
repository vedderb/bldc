/** \file lbm_variables.h */
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

#ifdef __cplusplus
extern "C" {
#endif

/** Initialize the variable storage area
 *
 * \param variable_storage Pointer to array where variables are stored.
 * \param variable_storage_size Number of variables that can be stored in the array.
 * \return 1 on success and 0 on failure.
 */
int lbm_variables_init(lbm_value *variable_storage, int variable_storage_size);
/** Get a pointer to the variable storage area.
 *
 * \return Pointer to storage area or NULL on failure.
 */
lbm_value *lbm_get_variable_table(void);
/** Get the value of a variable index.
 *
 * \param i Index of variable to access.
 * \return Value of variable at index.
 */
lbm_value lbm_get_variable_by_index(int i);
/** Lookup what the name of the variable associated with a specific
 *  index in the variable storage is.
 *
 * \param index Index of variable of interes.
 * \return Pointer to a string on success or null if no variable is associated with that index.
 */
const char *lbm_get_variable_name_by_index(int index);


/* internal use  by evaluator (mostly)*/

/** Get value of variable at index.
 *
 * \param index variable index to access.
 * \return Value of variable at index. This value if NIL if there is no binding.
 */
lbm_value lbm_get_var(lbm_uint index);
/** Set value of a variable
 *
 * \param index Index of variable to set.
 * \paran value Value to set the variable to.
 * \return Value of variable or NIL if index is out of range.
 */
lbm_value lbm_set_var(lbm_uint index, lbm_value value);

#ifdef __cplusplus
}
#endif
#endif 
