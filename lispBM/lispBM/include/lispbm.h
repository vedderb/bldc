/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

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
#include "prelude.h"
#include "env.h"
#include "compression.h"
#include "lbm_memory.h"
#include "lbm_types.h"
#include "lbm_c_interop.h"

/** Initialize lispBM. This function initials all subsystems by calling:
 *  - \ref lbm_memory_init
 *  - \ref lbm_symrepr_init
 *  - \ref lbm_heap_init
 *  - \ref lbm_init_env
 *  - \ref lbm_eval_init
 *  - \ref lbm_extensions_init
 *
 * \param heap_storage Pointer to array of lbm_cons_t to use as heap. This array must be aligned 4 at least.
 * \param heap_size Size of heap storage array in number of lm_cons_t.
 * \param memory Pointer to uint32_t array to use for the arrays and symbols memory. This array must be aligned 4 at least.
 * \param memory_size  Size of the memory array.
 * \param memory_bitmap Pointer to uint32_t array to use for the memory subsystem meta-data.
 * \param bitmap_size Size of the memory meta-data array.
 * \return 1 on success and 0 on failure.
 */
extern int lbm_init(lbm_cons_t *heap_storage, uint32_t heap_size,
                    uint32_t *memory, uint32_t memory_size,
                    uint32_t *memory_bitmap, uint32_t bitmap_size);

#endif
