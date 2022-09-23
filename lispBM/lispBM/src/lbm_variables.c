/*
    Copyright 2022 Joel Svensson   svenssonjoel@yahoo.se
    Copyright 2022 Benjamin Vedder

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

#include "lbm_variables.h"
#include "symrepr.h"
#include "heap.h"

lbm_value *variable_table = NULL;
int variable_table_size = 0;

int lbm_variables_init(lbm_value *variable_storage, int variable_storage_size) {

  if (variable_storage == NULL || variable_storage_size <= 0)
    return 0;

  variable_table = variable_storage;
  variable_table_size = variable_storage_size;
  for (int i = 0; i < variable_table_size; i ++) {
    variable_table[i] = ENC_SYM_NIL;
  }
  return 1;
}

lbm_value *lbm_get_variable_table(void) {
  return variable_table;
}

lbm_value lbm_get_var(lbm_uint sym_val) {

  int i = (int)sym_val - VARIABLE_SYMBOLS_START;
  return lbm_get_variable_by_index(i);
}

lbm_value lbm_get_variable_by_index(int i) {
  if (variable_table &&
      i >= 0 &&
      i < variable_table_size) {
    return variable_table[i];
  } else {
    return ENC_SYM_NIL;
  }
}

const char *lbm_get_variable_name_by_index(int index) {
  if (index < 0 ||
      index >= lbm_get_num_variables()) return NULL;

  lbm_uint sym_id = (lbm_uint)index + VARIABLE_SYMBOLS_START;
  return lbm_get_name_by_symbol(sym_id);
}

lbm_value lbm_set_var(lbm_uint index, lbm_value value) {

  int i = (int)index - VARIABLE_SYMBOLS_START;

  if (variable_table &&
      i >= 0 &&
      i < variable_table_size) {
    variable_table[i] = value;
  } else {
    return ENC_SYM_NIL;
  }
  return value;
}
