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

#include "extensions/set_extensions.h"

#include "extensions.h"
#include "fundamental.h"

#define ABORT_ON_MERROR(X) if ((X) == ENC_SYM_MERROR) return ENC_SYM_MERROR;

static lbm_value ext_member(lbm_value *args, lbm_uint argn);
static lbm_value ext_set_insert(lbm_value *args, lbm_uint argn);
static lbm_value ext_set_union(lbm_value *args, lbm_uint argn);

void lbm_set_extensions_init(void) {
  lbm_add_extension("member", ext_member);
  lbm_add_extension("set-insert", ext_set_insert);
  lbm_add_extension("set-union", ext_set_union);
}

static lbm_value ext_member(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_list(args[0])) {
    res = ENC_SYM_NIL;
    lbm_value curr = args[0];

    while (lbm_is_cons(curr)) {
      if (struct_eq(lbm_car(curr), args[1])) {
        res = args[0];
        break;
      }
      curr = lbm_cdr(curr);
    }
  }
  return res;
}

static lbm_value set_insert(lbm_value set, lbm_value val) {

  lbm_value end = ENC_SYM_NIL;
  lbm_value start = ENC_SYM_NIL;
    
  lbm_value curr = set;
  while (lbm_is_cons(curr)) {
    lbm_value h = lbm_car(curr);
    if (struct_eq(lbm_car(curr), val)) {
      return set;
    }
    lbm_value cell = lbm_cons(h, ENC_SYM_NIL);
    ABORT_ON_MERROR(cell);
    if (end == ENC_SYM_NIL) {
      end = cell;
      start = cell;
    } else {
      lbm_set_cdr(end, cell);
      end = cell;
    }
    curr = lbm_cdr(curr);
  }
  lbm_value v = lbm_cons(val, ENC_SYM_NIL);
  ABORT_ON_MERROR(v);
  if (end == ENC_SYM_NIL) {
    start = v;
  } else {
    lbm_set_cdr(end, v);
  }
  return start;
}

/* extends a copy of the input set with the new element. */
static lbm_value ext_set_insert(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_list(args[0])) {
    res = set_insert(args[0], args[1]);
  }
  return res;  
}


static lbm_value ext_set_union(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_list(args[0]) && lbm_is_list(args[1])) {
    lbm_value curr = args[0];
    lbm_value set  = args[1];

    while (lbm_is_cons(curr)) {
      set = set_insert(set, lbm_car(curr));
      ABORT_ON_MERROR(set);
      curr = lbm_cdr(curr);
    }
    return set;
  }
  return res;
}
