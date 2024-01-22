/*
    Copyright 2019, 2021, 2022, 2024 Joel Svensson  svenssonjoel@yahoo.se
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
#include <eval_cps.h>

#include "extensions.h"

static lbm_uint ext_max    = 0;
static lbm_uint ext_num    = 0;
static lbm_uint next_extension_ix = 0;

lbm_extension_t *extension_table = NULL;

lbm_value lbm_extensions_default(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;
  return ENC_SYM_EERROR;
}

int lbm_extensions_init(lbm_extension_t *extension_storage, lbm_uint extension_storage_size) {
  if (extension_storage == NULL || extension_storage_size == 0) return 0;

  extension_table = extension_storage;
  memset(extension_table, 0, sizeof(lbm_extension_t) * extension_storage_size);

  for (lbm_uint i = 0; i < extension_storage_size; i ++) {
    extension_storage[i].fptr = lbm_extensions_default;
  }

  ext_num = 0;
  next_extension_ix = 0;
  ext_max = (lbm_uint)extension_storage_size;

  return 1;
}

lbm_uint lbm_get_max_extensions(void) {
  return ext_max;
}

lbm_uint lbm_get_num_extensions(void) {
  return ext_num;
}

extension_fptr lbm_get_extension(lbm_uint sym) {
  lbm_uint ext_next = sym - EXTENSION_SYMBOLS_START;
  if (ext_next >= ext_max) {
    return NULL;
  }
  return extension_table[ext_next].fptr;
}

bool lbm_clr_extension(lbm_uint sym_id) {
  lbm_uint ext_id = sym_id - EXTENSION_SYMBOLS_START;
  if (ext_id >= ext_max) {
    return false;
  }
  extension_table[ext_id].name = NULL;
  extension_table[ext_id].fptr = lbm_extensions_default;
  return true;
}

bool lbm_lookup_extension_id(char *sym_str, lbm_uint *ix) {
  for (lbm_uint i = 0; i < ext_max; i ++) {
    if(extension_table[i].name) {
      if (strcmp(extension_table[i].name, sym_str) == 0) {
        *ix = i + EXTENSION_SYMBOLS_START;
        return true;
      }
    }
  }
  return false;
}

bool lbm_add_extension(char *sym_str, extension_fptr ext) {
  lbm_value symbol;

  // Check if symbol already exists.
  if (lbm_get_symbol_by_name(sym_str, &symbol)) {
    if (lbm_is_extension(lbm_enc_sym(symbol))) {
      // update the extension entry.
      if (strcmp(extension_table[symbol - EXTENSION_SYMBOLS_START].name, sym_str) == 0) {
        // Do not replace name ptr.
        extension_table[symbol - EXTENSION_SYMBOLS_START].fptr = ext;
        return true;
      }
    }
    return false;
  }

  if (next_extension_ix < ext_max) {
    lbm_uint sym_ix = next_extension_ix ++;
    extension_table[sym_ix].name = sym_str;
    extension_table[sym_ix].fptr = ext;
    ext_num ++;
    return true;
  }
  return false;
}

// Helpers for extension developers:

static bool lbm_is_number_all(lbm_value *args, lbm_uint argn) {
  for (lbm_uint i = 0;i < argn;i++) {
    if (!lbm_is_number(args[i])) {
      return false;
    }
  }
  return true;
}

bool lbm_check_true_false(lbm_value v) {
  bool res = lbm_is_symbol_true(v) || lbm_is_symbol_nil(v);
  lbm_set_error_reason((char*)lbm_error_str_not_a_boolean);
  return res;
}

bool lbm_check_number_all(lbm_value *args, lbm_uint argn) {
  if (!lbm_is_number_all(args, argn)) {
    lbm_set_error_reason((char*)lbm_error_str_no_number);
    return false;
  }
  return true;
}

bool lbm_check_argn(lbm_uint argn, lbm_uint n) {
  if (argn != n) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return false;
  } else {
    return true;
  }
}

bool lbm_check_argn_number(lbm_value *args, lbm_uint argn, lbm_uint n) {
  if (!lbm_is_number_all(args, argn)) {
    lbm_set_error_reason((char*)lbm_error_str_no_number);
    return false;
  } else if (argn != n) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return false;
  } else {
    return true;
  }
}
