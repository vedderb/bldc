/** \file extensions.h */
/*
    Copyright 2019, 2022 Joel Svensson        svenssonjoel@yahoo.se
                    2022 Benjamin Vedder

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

#ifndef EXTENSIONS_H_
#define EXTENSIONS_H_

#include "symrepr.h"
#include "heap.h"
#include "lbm_types.h"

#define LBM_EXTENSION(name, argv, argn)                                 \
  __attribute__((aligned(LBM_STORABLE_ADDRESS_ALIGNMENT))) lbm_value name(lbm_value *(argv), lbm_uint (argn)) 

/** Type representing an extension function.
 * \param Pointer to array of lbm_values.
 * \param Number of arguments.
 * \return Result value.
 */
typedef lbm_value (*extension_fptr)(lbm_value*,lbm_uint);

/** Initialize the extensions subsystem.
 *
 * \param extension_storage Pointer to array of extension_fptr.
 * \param extension_storage_size Size of function pointer array.
 * \return 1 on success and 0 for failure
 */
extern int lbm_extensions_init(extension_fptr *extension_storage, int extension_storage_size);
/** Look up an extension associated with a key symbol.
 *
 * \param sym Symbol bound to the extension to look for.
 * \return extension_fptr on success or NULL on failure.
 */
extern extension_fptr lbm_get_extension(lbm_uint sym);
/** Adds a symbol-extension mapping.
 * \param sym_str String representation of symbol to use as key.
 * \param ext The extension function pointer.
 * \return true on success and false on failure.
 */
extern bool lbm_add_extension(char *sym_str, extension_fptr ext);

/** Check if an lbm_value is a symbol that is bound to an extension.
 * \param exp Key to look up.
 * \return true if the lbm_value respresents an extension otherwise false.
 */
static inline bool lbm_is_extension(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_TYPE_SYMBOL) &&
          (lbm_get_extension(lbm_dec_sym(exp)) != NULL));
}
#endif
