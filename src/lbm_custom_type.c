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

#include <lbm_custom_type.h>
#include <heap.h>
#include <lbm_memory.h>


bool lbm_custom_type_create(lbm_uint value, custom_type_destructor fptr, const char *desc, lbm_value *result) {

  lbm_uint *t = lbm_memory_allocate(3);

  if (t == NULL) return false;

  t[CUSTOM_TYPE_VALUE] = value;
  t[CUSTOM_TYPE_DESCRIPTOR] = (lbm_uint)desc;
  t[CUSTOM_TYPE_DESTRUCTOR] = (lbm_uint)fptr;

  lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_CUSTOM, (lbm_uint) t, ENC_SYM_CUSTOM_TYPE);
  if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) {
    *result = cell;
    lbm_memory_free(t);
    return false;
  }
  //  lbm_set_car(cell, (lbm_uint)t);
  //lbm_set_cdr(cell, lbm_enc_sym(SYM_CUSTOM_TYPE));
  //cell = lbm_set_ptr_type(cell, LBM_TYPE_CUSTOM);
  *result = cell;
  
  return true;
}

bool lbm_custom_type_destroy(lbm_uint *lbm_mem_ptr) {

  lbm_uint value = lbm_mem_ptr[CUSTOM_TYPE_VALUE];
  custom_type_destructor destruct = (custom_type_destructor)lbm_mem_ptr[CUSTOM_TYPE_DESTRUCTOR];
  return destruct(value);   
}
