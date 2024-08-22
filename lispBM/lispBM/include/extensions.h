/** \file extensions.h */
/*
    Copyright 2019, 2022, 2024 Joel Svensson        svenssonjoel@yahoo.se
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

#include "heap.h"
#include "lbm_types.h"
#include "lbm_constants.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Type representing an extension function.
 * \param Pointer to array of lbm_values.
 * \param Number of arguments.
 * \return Result value.
 */
typedef lbm_value (*extension_fptr)(lbm_value*,lbm_uint);

/** Type representing an entry in the extension table
 */
typedef struct {
  extension_fptr fptr;
  char *name;
} lbm_extension_t;


extern lbm_extension_t *extension_table;

#define LBM_EXTENSION(name, argv, argn)                                 \
  __attribute__((aligned(LBM_STORABLE_ADDRESS_ALIGNMENT))) lbm_value name(lbm_value *(argv), lbm_uint (argn))

/** Initialize the extensions subsystem. Extension storage is allocated on lbm_memory.
 *
 * \param extension_storage_size Size of function pointer array.
 * \return 1 on success and 0 for failure
 */
  int lbm_extensions_init(lbm_extension_t *extension_storage, lbm_uint extension_storage_size);
/** The number of extensions that can be allocated.
 * \return The maximum number of extensions that can be added.
 */
lbm_uint lbm_get_max_extensions(void);
/** Get the number of allocated extensions
 * \return The number of extensions that have been added.
 */
lbm_uint lbm_get_num_extensions(void);
/** Lookup an extensions index.
 * \param sym_str Extension name to look up.
 * \param ix Pointer used to store result.
 * \return true on success, false otherwise.
 */
bool lbm_lookup_extension_id(char *sym_str, lbm_uint *ix);
/** Look up an extension associated with a key symbol.
 *
 * \param sym Symbol bound to the extension to look for.
 * \return extension_fptr on success or NULL on failure.
 */
extension_fptr lbm_get_extension(lbm_uint sym);
/** Reset an extension back to the default value.
 *  Trying to apply the extension after clearing it will result
 *  in an eval error.
 *
 * \param sym_id Symbol id of the extension to clear.
 * \return true if successfully cleared an extension otherwise false.
 */
bool lbm_clr_extension(lbm_uint sym_id);
/** Adds a symbol-extension mapping.
 * \param sym_str String representation of symbol to use as key.
 * \param ext The extension function pointer.
 * \return true on success and false on failure.
 */
bool lbm_add_extension(char *sym_str, extension_fptr ext);

/** Check if an lbm_value is a symbol that is bound to an extension.
 * \param exp Key to look up.
 * \return true if the lbm_value respresents an extension otherwise false.
 */
static inline bool lbm_is_extension(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_TYPE_SYMBOL) &&
          (lbm_get_extension(lbm_dec_sym(exp)) != NULL));
}


/** Check if a value is the symbol t or the symbol nil
 * \param v The value.
 * \return true if the value is t or nil otherwise false.
 */
bool lbm_check_true_false(lbm_value v);
/** Check if all arguments are numbers. Sets error-reason if result is false.
 * \param args The argument array.
 * \param argn The number of arguments.
 * \return true if all arguments are numbers, false otherwise.
 */
bool lbm_check_number_all(lbm_value *args, lbm_uint argn);
/** Check if the number of arguments is n. Sets error-reason if result is false.
 * \param argn number of arguments.
 * \param n number of expected arguments.
 * \return true if the number of arguments is correct. false otherwise
 */
bool lbm_check_argn(lbm_uint argn, lbm_uint n);
/** Check if all arguments are numbers and that there is n of them. Sets error-reason if result is false.
 * \param args The argument array.
 * \param argn The number of arguments.
 * \param n The expected number of arguments.
 * \return true or false.
 */
bool lbm_check_argn_number(lbm_value *args, lbm_uint argn, lbm_uint n);

#define LBM_CHECK_NUMBER_ALL() if (!lbm_check_number_all(args, argn)) {return ENC_SYM_EERROR;}
#define LBM_CHECK_ARGN(n) if (!lbm_check_argn(argn, n)) {return ENC_SYM_EERROR;}
#define LBM_CHECK_ARGN_NUMBER(n) if (!lbm_check_argn_number(args, argn, n)) {return ENC_SYM_EERROR;}

lbm_value lbm_extensions_default(lbm_value *args, lbm_uint argn);

#ifdef __cplusplus
}
#endif
#endif
