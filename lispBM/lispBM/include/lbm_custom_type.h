/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se

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
/** \file lbm_custom_type.h */

#ifndef LBM_CUSTOM_TYPE_H_
#define LBM_CUSTOM_TYPE_H_

#include <stdbool.h>
#include <stddef.h>
#include <lbm_types.h>
#include <lbm_defines.h>
#include <heap.h>

#ifdef __cplusplus
extern "C" {
#endif

// Custom type lbm_memory footprint
#define CUSTOM_TYPE_VALUE        0
#define CUSTOM_TYPE_DESCRIPTOR   1
#define CUSTOM_TYPE_DESTRUCTOR   2
#define CUSTOM_TYPE_LBM_MEM_SIZE 3

// encoding
//
// ( lbm-mem-ptr . LBM_TYPE_CUSTOM_SYM )
//     |
//     pointer to lbm_memory
//

typedef bool (*custom_type_destructor)(lbm_uint);

/** Create a value of a custom type with a destructor and a description
 *  
 * \param value The custom value. This can be a pointer to memory allocated 
 *              on the C-side. 
 * \param fptr The destructor function. This function should free any memory 
 *             allocated on the C-side. 
 * \param desc A description of the type that will be used for printing.
 * \param result Pointer to lbm_value that will hold the value of the custom type.
 * \return true on success or false otherwise.
 */
bool lbm_custom_type_create(lbm_uint value, custom_type_destructor fptr, const char *desc, lbm_value *result);  

/** Called by garbage collector and invokes the destructor 
 * on the custom value.  
 * 
 * /return true on success or false otherwise.
 */
bool lbm_custom_type_destroy(lbm_uint *lbm_mem_ptr);

static inline const char *lbm_get_custom_descriptor(lbm_value value) {
  if (lbm_type_of(value) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint*)lbm_dec_custom(value);
    return (const char*)m[CUSTOM_TYPE_DESCRIPTOR];
  }
  return NULL;
}

static inline lbm_uint lbm_get_custom_value(lbm_value value) {
  lbm_uint *m = (lbm_uint*)lbm_dec_custom(value);
  return m[CUSTOM_TYPE_VALUE];
}

#ifdef __cplusplus
}
#endif
#endif
