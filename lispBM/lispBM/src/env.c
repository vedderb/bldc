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
    lbm_value c = lbm_ref_cell(curr)->car;
    if ((lbm_ref_cell(c)->car) == sym) {
      *res = lbm_ref_cell(c)->cdr;
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

lbm_value lbm_env_lookup(lbm_value sym, lbm_value env) {
  lbm_value curr = env;

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    if (lbm_car(lbm_car(curr)) == sym) {
      return lbm_cdr(lbm_car(curr));
    }
    curr = lbm_cdr(curr);
  }
  return ENC_SYM_NOT_FOUND;
}

// TODO: env set should ideally copy environment if it has to update
// in place. This has never come up as an issue, the rest of the code
// must be very well behaved.
lbm_value lbm_env_set(lbm_value env, lbm_value key, lbm_value val) {

  lbm_value curr = env;
  lbm_value new_env;
  lbm_value keyval;

  while(lbm_type_of(curr) == LBM_TYPE_CONS) {
    if (lbm_car(lbm_car(curr)) == key) {
      lbm_set_cdr(lbm_car(curr),val);
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
    if (lbm_car(lbm_car(curr)) == key) {
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
    if (lbm_car(lbm_car(curr)) == key) {
      lbm_set_cdr(lbm_car(curr), val);
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
    if (lbm_car(lbm_car(curr)) == key) {
      lbm_set_cdr(prev, lbm_cdr(curr));
      return env;
    }
    prev = curr;
    curr = lbm_cdr(curr);
  }
  return ENC_SYM_NOT_FOUND;
}

lbm_value lbm_env_build_params_args(lbm_value params,
                            lbm_value args,
                            lbm_value env0) {
  lbm_value curr_param = params;
  lbm_value curr_arg = args;

  // TODO: This should be checked outside of this function.
  //
  if (lbm_list_length(params) != lbm_list_length(args)) { // programmer error
    return ENC_SYM_FATAL_ERROR;
  }

  lbm_value env = env0;
  while (lbm_type_of(curr_param) == LBM_TYPE_CONS) {

    lbm_value entry = lbm_cons(lbm_car(curr_param), lbm_car(curr_arg));
    if (lbm_type_of(entry) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(entry) == SYM_MERROR)
      return ENC_SYM_MERROR;

    env = lbm_cons(entry,env);

    if (lbm_type_of(env) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(env) == SYM_MERROR)
      return ENC_SYM_MERROR;

    curr_param = lbm_cdr(curr_param);
    curr_arg   = lbm_cdr(curr_arg);
  }
  return env;
}
