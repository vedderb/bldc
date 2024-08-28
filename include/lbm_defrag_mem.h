/*
    Copyright 2024 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef LBM_DEFRAG_MEM_H_
#define LBM_DEFRAG_MEM_H_

#include <heap.h>
#include <stdint.h>

extern lbm_value lbm_defrag_mem_create(lbm_uint nbytes);
extern void lbm_defrag_mem_destroy(lbm_uint *ptr);
extern lbm_value lbm_defrag_mem_alloc(lbm_uint *defrag_mem, lbm_uint nbytes);
extern void lbm_defrag_mem_free(lbm_uint* data);

#endif
