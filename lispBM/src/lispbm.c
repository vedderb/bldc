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

int lispbm_init(cons_t *heap_storage, uint32_t heap_size,
                uint32_t *memory, uint32_t memory_size,
                uint32_t *memory_bitmap, uint32_t bitmap_size) {


  if (memory_init(memory, memory_size,
                  memory_bitmap, bitmap_size) == 0)
    return -1;

  if (symrepr_init() == 0)
    return -2;

  if (heap_init(heap_storage, heap_size) == 0)
    return -3;

  if (env_init() == 0)
    return -4;

  if (eval_cps_init() == 0)
    return -5;

  if (extensions_init() == 0)
    return -6;

  return 1;
}

