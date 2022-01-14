/*
    Copyright 2019 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef EXTENSIONS_H_
#define EXTENSIONS_H_

#include "symrepr.h"
#include "heap.h"
#include "lispbm_types.h"

typedef VALUE (*extension_fptr)(VALUE*,UINT);

extern int extensions_init(void);
extern extension_fptr extensions_lookup(UINT sym);
extern bool extensions_add(char *sym_str, extension_fptr ext);

static inline bool is_extension(VALUE exp) {
  return ((type_of(exp) == VAL_TYPE_SYMBOL) &&
          (extensions_lookup(dec_sym(exp)) != NULL));
}
#endif
