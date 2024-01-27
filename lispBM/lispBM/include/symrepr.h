 /*
    Copyright 2018, 2021, 2022 Joel Svensson   svenssonjoel@yahoo.se
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

/** \file symrepr.h
 *  symrepr.h implements the symbol table and symbol creation and lookup functions.
 *
 *  The symbol table is implemented as a linked list in the arrays and symbols
 *  memory defined in lispbm_memory.h. So lbm_memory_init must be run before
 *  the symbol table is initialized and used.
 *
 */

#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h>
#include <stdbool.h>

#include "lbm_types.h"
#include "lbm_defines.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*symrepr_name_iterator_fun)(const char *);


/** Initialize the symbol table.
 *
 * \return 1
 */
int lbm_symrepr_init(void);
/** Iterate over all symbol names as strings
 *
 * \param symrepr_name_iterator_fun function taking a string
 */
void lbm_symrepr_name_iterator(symrepr_name_iterator_fun f);
/** Add a symbol to the symbol table. The symbol name string is copied to arrays and symbols memory.
 *
 * \param name String representation of the symbol.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
int lbm_add_symbol(char *name, lbm_uint *id);
/** Add a symbol to the symbol table. The symbol name string is copied to flash.
 *
 * \param name String representation of the symbol.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
int lbm_add_symbol_flash(char *name, lbm_uint* id);
/** Name of symbol to symbol. If the symbol exists the ID of the symbol is returned.
    If the name does not match any existing symbol, one is created and that ID is returned.
    \param name String name of symbol.
    \param id Resulting ID is returned through this argument.
    \return 1 for success and 0 for failure.
*/
int lbm_str_to_symbol(char *name, lbm_uint *sym_id);
/** Add a symbol to the symbol table. The name is assumed to be a statically allocated string.
 *
 * \param name Statically allocated name string.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
int lbm_add_symbol_const(char *name, lbm_uint *id);
/** Get a pointer to the list entry holding a symbol name, id mapping.
 *
 * \param name Name string to look up.
 * \return Pointer to list entry or NULL.
 */
lbm_uint *lbm_get_symbol_list_entry_by_name(char *name);
/** Look up an id from the symbol table given a name.
 *
 * \param name Name string to look up.
 * \param id Resulting id is returned through this argument.
 * \return 1 on success (name was found) and 0 for failure.
 */
int lbm_get_symbol_by_name(char *name, lbm_uint *id);
/** Look up a symbol name from the symbol table given an id.
 *
 * \param id The id to look up in the symbol table.
 * \return pointer to the name string if success otherwise NULL.
 */
const char* lbm_get_name_by_symbol(lbm_uint id);

/**
 *
 * \return The total amount of lbm_memory space occupied by the symbol table in bytes.
 */
lbm_uint lbm_get_symbol_table_size(void);
/**
 *
 * \return The total amount of flash space occupied by the symbol table in bytes.
 */
lbm_uint lbm_get_symbol_table_size_flash(void);
/**
 * \return The size in bytes of all symbol strings stored in the symbol table.
 */
lbm_uint lbm_get_symbol_table_size_names(void);
/**
 * \return The size in bytes of all symbol strings stored in flash from the symbol table.
 */
lbm_uint lbm_get_symbol_table_size_names_flash(void);

/** Check if a symbol name is stored in flash.
 * \param str Symbol name string.
 * \return True if symbol name is stored in flash, otherwise False.
 */
bool lbm_symbol_in_flash(char *str);
/** Check if a symbol name id mapping list entry is stored in flash.
 * \param str Symbol name string.
 * \return True if symbol name/id mapping list entry is stored in flash, otherwise False.
 */
bool lbm_symbol_list_entry_in_flash(char *str);


extern lbm_value symbol_x;
extern lbm_value symbol_y;

#ifdef __cplusplus
}
#endif
#endif
