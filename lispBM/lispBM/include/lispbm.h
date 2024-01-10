/*
    Copyright 2022, 2024 Joel Svensson  svenssonjoel@yahoo.se

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

/** \file lispbm.h */

#ifndef LISPBM_H_
#define LISPBM_H_

#include "heap.h"
#include "symrepr.h"
#include "extensions.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "env.h"
#include "lbm_memory.h"
#include "lbm_types.h"
#include "lbm_c_interop.h"
#include "lbm_custom_type.h"
#include "lbm_channel.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Initialize lispBM. This function initials all subsystems by calling:
 *  - \ref lbm_print_init
 *  - \ref lbm_memory_init
 *  - \ref lbm_symrepr_init
 *  - \ref lbm_heap_init
 *  - \ref lbm_init_env
 *  - \ref lbm_eval_init
 *  - \ref lbm_extensions_init
 *
 * \param heap_storage Pointer to array of lbm_cons_t to use as heap. This array must be aligned 4 at least.
 * \param heap_size Size of heap storage array in number of lm_cons_t.
 * \param memory Pointer to lbm_uint array to use for the arrays and symbols memory. This array must be aligned 4 at least.
 * \param memory_size  Size of the memory array.
 * \param memory_bitmap Pointer to lbm_uint array to use for the memory subsystem meta-data.
 * \param bitmap_size Size of the memory meta-data array.
 * \param gc_stack_size Size in number of lbm_uint values to use for the GC stack.
 * \param print_stack_size Size in number of lbm_uint values of the print stack.
 * \param extension_storage Array of lbm_extension_t pointers.
 * \param extension_storage_size Size of extension array.
 * \return 1 on success and 0 on failure.
 */

int lbm_init(lbm_cons_t *heap_storage, lbm_uint heap_size,
             lbm_uint *memory, lbm_uint memory_size,
             lbm_uint *memory_bitmap, lbm_uint bitmap_size,
             lbm_uint gc_stack_size,
             lbm_uint print_stack_size,
             lbm_extension_t *extension_storage,
             lbm_uint extension_storage_size);

#ifdef __cplusplus
}
#endif
#endif
