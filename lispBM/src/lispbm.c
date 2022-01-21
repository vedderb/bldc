/*
    Copyright 2018, 2020 Joel Svensson  svenssonjoel@yahoo.se

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

int lbm_init(lbm_cons_t *heap_storage, uint32_t heap_size,
                uint32_t *memory, uint32_t memory_size,
                uint32_t *memory_bitmap, uint32_t bitmap_size) {


  if (lbm_memory_init(memory, memory_size,
                  memory_bitmap, bitmap_size) == 0)
    return 0;

  if (lbm_symrepr_init() == 0)
    return 0;

  if (lbm_heap_init(heap_storage, heap_size) == 0)
    return 0;

  if (lbm_init_env() == 0)
    return 0;

  if (lbm_eval_init() == 0)
    return 0;

  if (lbm_extensions_init() == 0)
    return 0;

  return 1;
}

