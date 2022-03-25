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

static int ext_offset = EXTENSION_SYMBOLS_START;
static int ext_max    = -1;
static extension_fptr *extension_table = NULL;

int lbm_extensions_init(extension_fptr *extension_storage, int extension_storage_size) {
  if (extension_storage == NULL || extension_storage_size <= 0) return 0;

  extension_table = extension_storage;
  memset(extension_table, 0, sizeof(extension_fptr) * (unsigned int)extension_storage_size);

  ext_max = extension_storage_size;

  return 1;
}

extension_fptr lbm_get_extension(lbm_uint sym) {
  int ext_next = (int)sym - ext_offset;

  if (ext_next < 0 || ext_next > ext_max) {
    return NULL;
  }

  return extension_table[ext_next];
}

bool lbm_add_extension(char *sym_str, extension_fptr ext) {
  lbm_value symbol;
  int res = lbm_add_extension_symbol_const(sym_str, &symbol);

  if (!res) return false;

  int ext_next = (int)symbol - ext_offset;

  if (ext_next < 0 || ext_next > ext_max) {
    return false;
  }

  extension_table[ext_next] = ext;

  return true;
}

