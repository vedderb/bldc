/*
    Copyright 2025 Joel Svensson        svenssonjoel@yahoo.se

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

#include "extensions/mutex_extensions.h"

#include "extensions.h"
#include "eval_cps.h"

#ifdef LBM_OPT_MUTEX_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_MUTEX_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif


// This file provides a mutual exclusion feature based on the block_from_extension
// mechanism present in LBM.
//
// Three operations are provided:
// - mutex-create
// - mutex-lock
// - mutex-unlock
//
// It is strongly adviced to use these as a low-level interface and then to wrap
// them up in a (with-mutex-do mutex expr) that implements the locking and unlocking
// so that no mutex is left dangling.
//
// with-mutex-do can be implemented as
//
// (define with-mutex-do (lambda (mutex quoted-expr)
//   (progn (mutex-lock mutex)
//          (eval quoted-expr)
//          (mutex-unlock mutex)
//
//
// The mutex object is a dotted pair (ls . last) which contains
// two references into a single list, implementing a O(1)-insert-last O(1)-remove-first
// queue. At the surface though, it is a regular lisp dotted pair that
// can be destroyed with standard lisp functionality, no protection!
// When a good replacement for "Custom types" is invented this will be improved. 

bool is_mutex(lbm_value v) {
  // true if it is somewhat likely that v is a mutex.
  bool res = (lbm_is_cons(v) &&
              (!lbm_is_symbol_nil(lbm_car(v)) ||  lbm_is_symbol_nil(lbm_cdr(v)))); // car == nil -> cdr == nil
  // potentially add a clause
  //      car == (cons a b) -> cdr == (cons c nil) 
  return res; 
}

bool is_mutex_unlocked(lbm_value v) {
  return (lbm_is_symbol_nil(lbm_car(v)));
}

void enqueue_cid(lbm_value mutex, lbm_value cid_pair) {
  if (lbm_is_symbol_nil(lbm_car(mutex))) {
    lbm_set_car(mutex, cid_pair);
    lbm_set_cdr(mutex, cid_pair);
  } else {
    lbm_value last = lbm_cdr(mutex);
    lbm_set_cdr(last, cid_pair);
    lbm_set_cdr(mutex, cid_pair);
  }
}

bool dequeue_cid(lbm_value mutex, lbm_value cid) {
  bool res = false;
  if (lbm_is_cons(lbm_car(mutex))) {
    lbm_value locked_cid = lbm_car(lbm_car(mutex));
    if (locked_cid == cid) { // no decoding
      res = true;
      lbm_value head = lbm_car(mutex);
      lbm_value last = lbm_cdr(mutex);
      if (head == last) { // one element
        lbm_set_car(mutex, ENC_SYM_NIL);
        lbm_set_cdr(mutex, ENC_SYM_NIL);
      } else {
        lbm_set_car(mutex, lbm_cdr(head));
      }
    }
  }
  return res;
}

lbm_value head_of_queue(lbm_value mutex) {
  lbm_value res = ENC_SYM_NIL;
  if (lbm_is_cons(lbm_car(mutex))) {
    res = lbm_car(lbm_car(mutex));
  }
  return res;
}

static lbm_value ext_mutex_create(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_cons(ENC_SYM_NIL, ENC_SYM_NIL);
}

static lbm_value ext_mutex_lock(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && is_mutex(args[0])) {
    lbm_cid cid = lbm_get_current_cid();
    lbm_value cid_pair = lbm_cons(lbm_enc_i(cid), ENC_SYM_NIL);
    res = cid_pair; // Return the error from cons if failed.
    if (lbm_is_cons(cid_pair)) {
      res = ENC_SYM_TRUE;
      if (is_mutex_unlocked(args[0])) {
        enqueue_cid(args[0], cid_pair);
      } else {
        enqueue_cid(args[0], cid_pair);
        lbm_block_ctx_from_extension(); 
      }
    }
  }
  return res;
}

static lbm_value ext_mutex_unlock(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && is_mutex(args[0])) {
    res = ENC_SYM_EERROR; // no mutex is locked!
    if (!is_mutex_unlocked(args[0])) {
      lbm_cid cid = lbm_get_current_cid(); // this cid should be top of queue!
                                           // otherwise error
      if (dequeue_cid(args[0], lbm_enc_i(cid))) {
        lbm_value h = head_of_queue(args[0]);
        res = ENC_SYM_TRUE;
        if (!lbm_is_symbol_nil(h)) {
          lbm_cid unblock = lbm_dec_i(h);
          lbm_unblock_ctx_unboxed(unblock, ENC_SYM_TRUE);
        }
      } 
    }
  }
  return res;
}


void lbm_mutex_extensions_init(void) {
  lbm_add_extension("mutex-create", ext_mutex_create);
  lbm_add_extension("mutex-lock", ext_mutex_lock);
  lbm_add_extension("mutex-unlock", ext_mutex_unlock);
}
