/*
    Copyright 2018, 2020, 2024 Joel Svensson  svenssonjoel@yahoo.se

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

#include "lispbm.h"

bool lbm_init(lbm_cons_t *heap_storage, lbm_uint heap_size,
             lbm_uint *memory, lbm_uint memory_size,
             lbm_uint *memory_bitmap, lbm_uint bitmap_size,
             lbm_uint gc_stack_size,
             lbm_uint print_stack_size,
             lbm_extension_t *extension_storage,
             lbm_uint extension_storage_size) {
  return
    lbm_memory_init(memory, memory_size,
                    memory_bitmap, bitmap_size) &&
    lbm_symrepr_init() &&
    lbm_heap_init(heap_storage, heap_size, gc_stack_size) &&
    lbm_print_init(print_stack_size) &&
    lbm_extensions_init(extension_storage, extension_storage_size) &&
    lbm_eval_init();
}

