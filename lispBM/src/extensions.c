/*
    Copyright 2019, 2021 Joel Svensson  svenssonjoel@yahoo.se

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

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "lispbm_memory.h"
#include "extensions.h"

#define SYM 0
#define FPTR 1
#define NEXT 2

/* typedef struct s_extension_function{ */
/*   VALUE sym; */
/*   extension_fptr ext_fun; */
/*   struct s_extension_function* next; */
/* } extension_function_t; */

uint32_t* extensions = NULL;

int extensions_init(void) {
  extensions = NULL;
  return 1;
}

extension_fptr extensions_lookup(UINT sym) {
  uint32_t *t = extensions;
  while (t != NULL) {
    if (t[SYM] == sym) {
      return (extension_fptr)t[FPTR];
    }
    t = (uint32_t*)t[NEXT];
  }
  return NULL;
}

bool extensions_add(char *sym_str, extension_fptr ext) {
  VALUE symbol;
  int res = symrepr_addsym_const(sym_str, &symbol);

  if (!res) return false;

  uint32_t *m = memory_allocate(3); /* 3 words */

  if (!m) return false;

  m[SYM] = symbol;
  m[FPTR] = (uint32_t) ext;
  m[NEXT] = (uint32_t) extensions;
  extensions = m;
  return true;
}
