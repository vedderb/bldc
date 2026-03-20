/*
    Copyright 2018, 2020, 2021, 2024, 2025 Joel Svensson    svenssonjoel@yahoo.se

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

bool lbm_init_env(void) {
  for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
    env_global[i] = ENC_SYM_NIL;
  }
  return true;
}

lbm_uint lbm_get_global_env_size(void) {
  lbm_uint n = 0;
  for (int i = 0; i < GLOBAL_ENV_ROOTS; i ++) {
    lbm_value curr = env_global[i];
    while (lbm_is_cons(curr)) {
      n++;
      curr = lbm_ref_cell(curr)->cdr;
    }
  }
  return n;
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
    // if src environment is invalid, so is tgt.
    while (lbm_is_cons_rw(curr_tgt) && lbm_is_cons(curr_src)) {
      lbm_cons_t *tgt_cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(curr_tgt)];
      lbm_cons_t *src_cell = lbm_ref_cell(curr_src);
      tgt_cell->car = src_cell->car;
      curr_tgt = tgt_cell->cdr;
      curr_src = src_cell->cdr;
    }
    r = new_env;
  }
  return r;
}

// env_lookup that should be safe even in the presence of incorrectly
// structured env. Could be the case when user manually creates closure.
bool lbm_env_lookup_b(lbm_value *res, lbm_value sym, lbm_value env) {
  lbm_value curr = env;

  while (lbm_is_cons(curr)) {
    lbm_cons_t *cr = lbm_ref_cell(curr);
    if (lbm_is_cons(cr->car)) {
      lbm_cons_t *pair = lbm_ref_cell(cr->car);
      if ((pair->car == sym)
          && (pair->cdr != ENC_SYM_PLACEHOLDER)) {
        *res = pair->cdr;
        return true;
      }
    }
    curr = cr->cdr;
  }
  return false;
}

// Global environment lookup
// Assumes that environment is structurally correct.
// Assumes that global environment structures are never constant.
bool lbm_global_env_lookup(lbm_value *res, lbm_value sym) {
  lbm_uint dec_sym = lbm_dec_sym(sym);
  lbm_uint ix = dec_sym & GLOBAL_ENV_MASK;
  lbm_value curr = env_global[ix];

  while (curr) { // Uses the fact that nil is 0 and the assumption
                 // that global environments are structurally correct.
    lbm_uint curr_ix = lbm_dec_ptr(curr);
    lbm_cons_t *curr_cell = &lbm_heaps[LBM_RAM_HEAP][curr_ix];
    lbm_value c = curr_cell->car;
    lbm_uint c_ix = lbm_dec_ptr(c); // Assumes environment is correctly shaped.
    lbm_cons_t *c_cell = &lbm_heaps[LBM_RAM_HEAP][c_ix];
    if (c_cell->car == sym) {
      *res = c_cell->cdr;
      return true;
    }
    curr = curr_cell->cdr;
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

  while(lbm_is_cons_rw(curr)) {
    lbm_cons_t *cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(curr)];
    lbm_value car_val = cell->car;
    if (lbm_is_cons_rw(car_val)) { // else corrupt environment.
      lbm_cons_t *car_cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(car_val)];
      if (car_cell->car == key) {
        car_cell->cdr = val;
        return env;
      }
    } // Possibly add a else here to handle corrupt environment.
    curr = cell->cdr;
  }

  keyval = lbm_cons(key,val);
  if (lbm_is_symbol(keyval)){
    return keyval;
  }

  new_env = lbm_cons(keyval, env);
  return new_env;
}

lbm_value lbm_env_modify_binding(lbm_value env, lbm_value key, lbm_value val) {

  lbm_value curr = env;

  while (lbm_is_cons_rw(curr)) {
    lbm_cons_t *curr_cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(curr)];
    lbm_value car_val = curr_cell->car;
    if (lbm_is_cons_rw(car_val)) {
      lbm_cons_t *car_cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(car_val)];
      if (car_cell->car == key) {
        car_cell->cdr = val;
        return env;
      }
    } // Else environment is invalid.
    curr = curr_cell->cdr;
  }
  return ENC_SYM_NOT_FOUND;
}


// TODO: Drop binding should really return a new environment
//       where the drop key/val is missing.
//
//       The internal use of drop_binding in fundamental undefine
//       is probably fine as we do not generally treat environments
//       as first order values. If we did, drop_binding is too destructive!
lbm_value lbm_env_drop_binding(lbm_value env, lbm_value key) {

  if (lbm_is_cons_rw(env)) {
    lbm_cons_t *cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(env)];

    // If key is first in env
    if (lbm_car(cell->car) == key) {
      return cell->cdr; // New head of env
    }

    lbm_cons_t *prev = cell;
    lbm_value curr = cell->cdr;

    while (lbm_is_cons_rw(curr)) {
      cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(curr)];
      if (lbm_is_cons_rw(cell->car)) {
        lbm_cons_t *car_cell = &lbm_heaps[LBM_RAM_HEAP][lbm_dec_ptr(cell->car)];
        if (car_cell->car == key) {
          prev->cdr = cell->cdr; // removes "cell" from list
          return env;
        }
      } // the unhandled else here would be an invalid environment.
      prev = cell;
      curr = cell->cdr;
    }
  }
  return ENC_SYM_NOT_FOUND;
}
