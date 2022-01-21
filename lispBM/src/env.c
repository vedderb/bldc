/*
    Copyright 2018, 2020, 2021 Joel Svensson    svenssonjoel@yahoo.se

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

#include "symrepr.h"
#include "heap.h"
#include "print.h"
#include "lispbm_types.h"

lbm_value env_global;

int lbm_init_env(void) {
  env_global = lbm_enc_sym(SYM_NIL);
  return 1;
}

lbm_value *lbm_get_env_ptr(void) {
  return &env_global;
}

// Copies just the skeleton structure of an environment
// The new "copy" will have pointers to the original key-val bindings.
lbm_value lbm_env_copy_shallow(lbm_value env) {

  lbm_value res = lbm_enc_sym(SYM_NIL);
  lbm_value curr = env;

  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    lbm_value key = lbm_car(lbm_car(curr));
    if (lbm_dec_sym(key) != SYM_NIL) {
      res = lbm_cons(lbm_car(curr), res);

      // Check for "out of memory"
      if (lbm_type_of(res) == LBM_VAL_TYPE_SYMBOL &&
          lbm_dec_sym(res) == SYM_MERROR) {
        return res;
      }
    }
    curr = lbm_cdr(curr);
  }
  return  res;
}

lbm_value lbm_env_lookup(lbm_value sym, lbm_value env) {
  lbm_value curr = env;

  if(lbm_dec_sym(sym) == SYM_NIL) {
    return sym;
  }

  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    if (lbm_car(lbm_car(curr)) == sym) {
      return lbm_cdr(lbm_car(curr));
    }
    curr = lbm_cdr(curr);
  }
  return lbm_enc_sym(SYM_NOT_FOUND);
}

lbm_value lbm_env_set(lbm_value env, lbm_value key, lbm_value val) {

  lbm_value curr = env;
  lbm_value new_env;
  lbm_value keyval;

  while(lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    if (lbm_car(lbm_car(curr)) == key) {
      lbm_set_cdr(lbm_car(curr),val);
      return env;
    }
    curr = lbm_cdr(curr);
  }

  keyval = lbm_cons(key,val);
  if (lbm_type_of(keyval) == LBM_VAL_TYPE_SYMBOL) {
    return keyval;
  }

  new_env = lbm_cons(keyval, env);
  if (lbm_type_of(new_env) == LBM_VAL_TYPE_SYMBOL) {
    return keyval;
  }

  return new_env;
}


lbm_value lbm_env_modify_binding(lbm_value env, lbm_value key, lbm_value val) {

  lbm_value curr = env;

  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    if (lbm_car(lbm_car(curr)) == key) {
      lbm_set_cdr(lbm_car(curr), val);
      return env;
    }
    curr = lbm_cdr(curr);

  }
  return lbm_enc_sym(SYM_NOT_FOUND);
}


lbm_value lbm_env_build_params_args(lbm_value params,
                            lbm_value args,
                            lbm_value env0) {
  lbm_value curr_param = params;
  lbm_value curr_arg = args;

  // TODO: This should be checked outside of this function.
  //
  if (lbm_list_length(params) != lbm_list_length(args)) { // programmer error
    return lbm_enc_sym(SYM_FATAL_ERROR);
  }

  lbm_value env = env0;
  while (lbm_type_of(curr_param) == LBM_PTR_TYPE_CONS) {

    lbm_value entry = lbm_cons(lbm_car(curr_param), lbm_car(curr_arg));
    if (lbm_type_of(entry) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(entry) == SYM_MERROR)
      return lbm_enc_sym(SYM_MERROR);

    env = lbm_cons(entry,env);

    if (lbm_type_of(env) == LBM_VAL_TYPE_SYMBOL &&
        lbm_dec_sym(env) == SYM_MERROR)
      return lbm_enc_sym(SYM_MERROR);

    curr_param = lbm_cdr(curr_param);
    curr_arg   = lbm_cdr(curr_arg);
  }
  return env;
}
