/*
    Copyright 2019, 2022 Joel Svensson        svenssonjoel@yahoo.se
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

#ifndef EXTENSIONS_H_
#define EXTENSIONS_H_

#include "symrepr.h"
#include "heap.h"
#include "lispbm_types.h"

typedef lbm_value (*extension_fptr)(lbm_value*,lbm_uint);

extern int lbm_extensions_init(void);
extern extension_fptr lbm_get_extension(lbm_uint sym);
extern bool lbm_add_extension(char *sym_str, extension_fptr ext);

static inline bool lbm_is_extension(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_VAL_TYPE_SYMBOL) &&
          (lbm_get_extension(lbm_dec_sym(exp)) != NULL));
}
#endif
