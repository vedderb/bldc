/*
    Copyright 2018, 2020, 2021, 2024 Joel Svensson    svenssonjoel@yahoo.se

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

#include <lbm_types.h>
#include <stdio.h>

#include "symrepr.h"
#include "heap.h"
#include "print.h"
#include "env.h"
#include "lbm_memory.h"

static lbm_value env_global[GLOBAL_ENV_ROOTS];

int lbm_init_env(void) {
  for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
    env_global[i] = ENC_SYM_NIL;
  }
  return 1;
}

lbm_value *lbm_get_global_env(void) {
  return env_global;
}

// Copy the list structure of an environment.
lbm_value lbm_env_copy_spine(lbm_value env) {

  lbm_value r = ENC_SYM_MERROR;
  lbm_uint len = lbm_list_length(env);

  lbm_value new_env = lbm_heap_allocate_list(len);
  if (new_env != ENC_SYM_MERROR) {
    lbm_value curr_tgt = new_env;
    lbm_value curr_src = env;
    while (lbm_type_of(curr_tgt) == LBM_TYPE_CONS) {
      lbm_set_car(curr_tgt, lbm_car(curr_src));
      curr_tgt = lbm_cdr(curr_tgt);
      curr_src = lbm_cdr(curr_src);
    }
    r = new_env;
  }
  return r;
}

// A less safe version of lookup. It should be fine unless env is corrupted.
bool lbm_env_lookup_b(lbm_value *res, lbm_value sym, lbm_value env) {
  lbm_value curr = env;

  while (lbm_is_ptr(curr)) {
    lbm_cons_t *pair = lbm_ref_cell(lbm_ref_cell(curr)->car);
    if ((pair->car == sym)
        && (pair->cdr != ENC_SYM_PLACEHOLDER)) {
      *res = pair->cdr;
      return true;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return false;
}

bool lbm_global_env_lookup(lbm_value *res, lbm_value sym) {
  lbm_uint dec_sym = lbm_dec_sym(sym);
  lbm_uint ix = dec_sym & GLOBAL_ENV_MASK;
  lbm_value curr = env_global[ix];

  while (lbm_is_ptr(curr)) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if ((lbm_ref_cell(c)->car) == sym) {
      *res = lbm_ref_cell(c)->cdr;
      return true;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return false;
}

// TODO: env set should ideally copy environment if it has to update
// in place. This has never come up as an issue, the rest of the code
// must be very well behaved.
lbm_value lbm_env_set(lbm_value env, lbm_value key, lbm_value val) {

  lbm_value curr = env;
  lbm_value new_env;
  lbm_value keyval;

  while(lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value car_val = lbm_car(curr);
    if (lbm_car(car_val) == key) {
      lbm_set_cdr(car_val,val);
      return env;
    }
    curr = lbm_cdr(curr);
  }

  keyval = lbm_cons(key,val);
  if (lbm_type_of(keyval) == LBM_TYPE_SYMBOL) {
    return keyval;
  }

  new_env = lbm_cons(keyval, env);
  return new_env;
}

// TODO: same remark as lbm_set_env
lbm_value lbm_env_set_functional(lbm_value env, lbm_value key, lbm_value val) {

  lbm_value keyval = lbm_cons(key, val);
  if (lbm_type_of(keyval) == LBM_TYPE_SYMBOL) {
    return keyval;
  }

  lbm_value curr = env;

  while(lbm_type_of(curr) == LBM_TYPE_CONS) {
    if (lbm_caar(curr) == key) {
      lbm_set_car(curr,keyval);
      return env;
    }
    curr = lbm_cdr(curr);
  }

  lbm_value new_env = lbm_cons(keyval, env);
  return new_env;
}

lbm_value lbm_env_modify_binding(lbm_value env, lbm_value key, lbm_value val) {

  lbm_value curr = env;

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value car_val = lbm_car(curr);
    if (lbm_car(car_val) == key) {
      lbm_set_cdr(car_val, val);
      return env;
    }
    curr = lbm_cdr(curr);

  }
  return ENC_SYM_NOT_FOUND;
}

lbm_value lbm_env_drop_binding(lbm_value env, lbm_value key) {

  lbm_value curr = env;
  // If key is first in env
  if (lbm_car(lbm_car(curr)) == key) {
    return lbm_cdr(curr);
  }

  lbm_value prev = env;
  curr = lbm_cdr(curr);

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    if (lbm_caar(curr) == key) {
      lbm_set_cdr(prev, lbm_cdr(curr));
      return env;
    }
    prev = curr;
    curr = lbm_cdr(curr);
  }
  return ENC_SYM_NOT_FOUND;
}
