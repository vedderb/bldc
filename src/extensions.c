/*
    Copyright 2019, 2021, 2022 Joel Svensson  svenssonjoel@yahoo.se
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

#include <lbm_memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "extensions.h"

static lbm_uint ext_offset = EXTENSION_SYMBOLS_START;
static lbm_uint ext_max    = 0;
static extension_fptr *extension_table = NULL;

lbm_value lbm_extensions_default(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  return lbm_enc_sym(SYM_EERROR);
}

int lbm_extensions_init(extension_fptr *extension_storage, int extension_storage_size) {
  if (extension_storage == NULL || extension_storage_size <= 0) return 0;

  extension_table = extension_storage;
  memset(extension_table, 0, sizeof(extension_fptr) * (unsigned int)extension_storage_size);

  for (int i = 0; i < extension_storage_size; i ++) {
    extension_storage[i] = lbm_extensions_default;
  }

  ext_max = (lbm_uint)extension_storage_size;

  return 1;
}

extension_fptr lbm_get_extension(lbm_uint sym) {
  lbm_uint ext_next = sym - ext_offset;
  if (ext_next >= ext_max) {
    return NULL;
  }
  return extension_table[ext_next];
}

bool lbm_clr_extension(lbm_uint sym_id) {
  lbm_uint ext_id = sym_id - ext_offset;
  if (ext_id >= ext_max) {
    return false;
  }
  extension_table[ext_id] = lbm_extensions_default;
  return true;
}

bool lbm_add_extension(char *sym_str, extension_fptr ext) {
  lbm_value symbol;
  lbm_uint ext_ix = 0;

  if (lbm_get_symbol_by_name(sym_str, &symbol)) {
    // symbol already exists and may or may not be an extension.
    if (lbm_is_extension(lbm_enc_sym(symbol))) {
      ext_ix = symbol - ext_offset;
    } else return false;
  } else {
    int res = lbm_add_extension_symbol_const(sym_str, &symbol);
    if (!res) return false;
    ext_ix = symbol - ext_offset;
  }

  if (ext_ix >= ext_max) {
      return false;
  }
  extension_table[ext_ix] = ext;
  return true;
}

