/*
    Copyright 2019, 2021, 2022, 2023 Joel Svensson   svenssonjoel@yahoo.se
                          2022       Benjamin Vedder

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
#include "symrepr.h"
#include "stack.h"
#include "heap.h"
#include "eval_cps.h"
#include "env.h"
#include "lbm_utils.h"
#include "lbm_custom_type.h"

#include <stdio.h>
#include <math.h>

// TODO: Check for correctnes
#define IS_NUMBER(X) \
  ( (((X) & 1) && ((X) & LBM_NUMBER_MASK)) ||   \
    ((X) & 0xC))
// if (x & 1)
//   (x & LBM_NUMBER_MASK)
//   (x & 0xC))


// Todo: It may be possible perform some of these operations
//       on encoded values followed by a "correction" of the result values
//       type bits.
//       But the checks required to figure out if it is possible to apply the
//       operation in this way has a cost too...

static lbm_uint add2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = ENC_SYM_TERROR;

  if (!(IS_NUMBER(a) && IS_NUMBER(b))) {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
    return retval;
  }

  lbm_uint t = (lbm_type_of_functional(a) < lbm_type_of_functional(b)) ? lbm_type_of_functional(b) : lbm_type_of_functional(a);
  switch (t) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_as_i32(a) + lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_as_u32(a) + lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_as_u32(a) + lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_as_i32(a) + lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: retval = lbm_enc_float(lbm_dec_as_float(a) + lbm_dec_as_float(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_as_u64(a) + lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_as_i64(a) + lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: retval = lbm_enc_double(lbm_dec_as_double(a) + lbm_dec_as_double(b)); break;
  }
  return retval;
}

static lbm_uint mul2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = ENC_SYM_TERROR;

  if (!(IS_NUMBER(a) && IS_NUMBER(b))) {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
    return retval;
  }

  lbm_uint t = (lbm_type_of_functional(a) < lbm_type_of_functional(b)) ? lbm_type_of_functional(b) : lbm_type_of_functional(a);
  switch (t) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_as_i32(a) * lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_as_u32(a) * lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_as_u32(a) * lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_as_i32(a) * lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: retval = lbm_enc_float(lbm_dec_as_float(a) * lbm_dec_as_float(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_as_u64(a) * lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_as_i64(a) * lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: retval = lbm_enc_double(lbm_dec_as_double(a) * lbm_dec_as_double(b)); break;
  }
  return retval;
}

static lbm_uint div2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = ENC_SYM_TERROR;

  if (!(IS_NUMBER(a) && IS_NUMBER(b))) {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
    return retval;
  }

  lbm_uint t = (lbm_type_of_functional(a) < lbm_type_of_functional(b)) ? lbm_type_of_functional(b) : lbm_type_of_functional(a);
  switch (t) {
  case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u32(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: if (lbm_dec_as_i32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i32(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: if (lbm_dec_as_float(b) == 0.0f || lbm_dec_as_float(b) == -0.0f) {return ENC_SYM_DIVZERO;} retval = lbm_enc_float(lbm_dec_as_float(a) / lbm_dec_as_float(b)); break;
  case LBM_TYPE_U64: if (lbm_dec_as_u64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u64(lbm_dec_as_u32(a) / lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: if (lbm_dec_as_i64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i64(lbm_dec_as_i32(a) / lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: if (lbm_dec_as_double(b) == (double)0.0 || lbm_dec_as_double(b) == (double)-0.0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_double(lbm_dec_as_double(a) / lbm_dec_as_double(b)); break;
  }
  return retval;
}

static lbm_uint mod2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = ENC_SYM_TERROR;

  if (!(IS_NUMBER(a) && IS_NUMBER(b))) {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
    return retval;
  }

  lbm_uint t = (lbm_type_of_functional(a) < lbm_type_of_functional(b)) ? lbm_type_of_functional(b) : lbm_type_of_functional(a);
  switch (t) {
  case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i32(a) % lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u32(a) % lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u32(lbm_dec_as_u32(a) % lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: if (lbm_dec_as_i32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i32(lbm_dec_as_i32(a) % lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: if (lbm_dec_as_float(b) == 0.0f || lbm_dec_as_float(b) == -0.0f) {return ENC_SYM_DIVZERO;} retval = lbm_enc_float(fmodf(lbm_dec_as_float(a), lbm_dec_as_float(b))); break;
  case LBM_TYPE_U64: if (lbm_dec_as_u64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u64(lbm_dec_as_u64(a) % lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: if (lbm_dec_as_i64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i64(lbm_dec_as_i64(a) % lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: if (lbm_dec_as_double(b) == (double)0.0 || lbm_dec_as_double(b) == (double)-0.0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_double(fmod(lbm_dec_as_double(a), lbm_dec_as_double(b))); break;
  }
  return retval;
}

static lbm_uint negate(lbm_uint a) {

  lbm_uint retval = ENC_SYM_TERROR;

  if (!IS_NUMBER(a)) {
    lbm_set_error_suspect(a);
    return retval;
  }

  if (lbm_type_of_functional(a) > LBM_TYPE_CHAR) {
    switch (lbm_type_of_functional(a)) {
    case LBM_TYPE_I: retval = lbm_enc_i(- lbm_dec_i(a)); break;
    case LBM_TYPE_U: retval = lbm_enc_u(- lbm_dec_u(a)); break;
    case LBM_TYPE_U32: retval = lbm_enc_u32(- lbm_dec_u32(a)); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(- lbm_dec_i32(a)); break;
    case LBM_TYPE_FLOAT: retval = lbm_enc_float(- lbm_dec_float(a)); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(- lbm_dec_u64(a)); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(- lbm_dec_i64(a)); break;
    case LBM_TYPE_DOUBLE: retval = lbm_enc_double(- lbm_dec_double(a)); break;
    }
  }
  return retval;
}

static lbm_uint sub2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = ENC_SYM_TERROR;

  if (!(IS_NUMBER(a) && IS_NUMBER(b))) {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
    return retval;
  }

  lbm_uint t = (lbm_type_of_functional(a) < lbm_type_of_functional(b)) ? lbm_type_of_functional(b) : lbm_type_of_functional(a);
  switch (t) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_as_i32(a) - lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_as_u32(a) - lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_as_u32(a) - lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_as_i32(a) - lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: retval = lbm_enc_float(lbm_dec_as_float(a) - lbm_dec_as_float(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_as_u64(a) - lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_as_i64(a) - lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: retval = lbm_enc_double(lbm_dec_as_double(a) - lbm_dec_as_double(b)); break;
  }
  return retval;
}

static bool array_equality(lbm_value a, lbm_value b) {
  if (lbm_is_array_r(a) && lbm_is_array_r(b)) {
    lbm_array_header_t *a_ = (lbm_array_header_t*)lbm_car(a);
    lbm_array_header_t *b_ = (lbm_array_header_t*)lbm_car(b);

    if (a_ == NULL || b_ == NULL) return false; // Not possible to properly report error from here.

    if (a_->size == b_->size) {
      return (memcmp((char*)a_->data, (char*)b_->data, a_->size) == 0);
    }
  }
  return false;
}

bool struct_eq(lbm_value a, lbm_value b) {

  bool res = false;
  lbm_type ta = lbm_type_of_functional(a);
  lbm_type tb = lbm_type_of_functional(b);

  if (ta == tb) {
    switch(ta){
    case LBM_TYPE_SYMBOL:
      return (lbm_dec_sym(a) == lbm_dec_sym(b));
    case LBM_TYPE_I:
      return (lbm_dec_i(a) == lbm_dec_i(b));
    case LBM_TYPE_U:
      return (lbm_dec_u(a) == lbm_dec_u(b));
    case LBM_TYPE_CHAR:
      return (lbm_dec_char(a) == lbm_dec_char(b));
    case LBM_TYPE_CONS:
      return ( struct_eq(lbm_car(a),lbm_car(b)) &&
               struct_eq(lbm_cdr(a),lbm_cdr(b)) );
    case LBM_TYPE_I32:
      return (lbm_dec_i32(a) == lbm_dec_i32(b));
    case LBM_TYPE_U32:
      return (lbm_dec_u32(a) == lbm_dec_u32(b));
    case LBM_TYPE_FLOAT:
      return (lbm_dec_float(a) == lbm_dec_float(b));
    case LBM_TYPE_I64:
      return (lbm_dec_i64(a) == lbm_dec_i64(b));
    case LBM_TYPE_U64:
      return (lbm_dec_u64(a) == lbm_dec_u64(b));
    case LBM_TYPE_DOUBLE:
      return (lbm_dec_double(a) == lbm_dec_double(b));
    case LBM_TYPE_ARRAY:
      return array_equality(a, b);
    }
  }
  return res;
}

/* returns -1 if a < b; 0 if a = b; 1 if a > b */
static int compare(lbm_uint a, lbm_uint b) {

  int retval = 0;

  if (!(IS_NUMBER(a) && IS_NUMBER(b))) {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
    return ENC_SYM_TERROR;
  }

  lbm_uint t = (lbm_type_of_functional(a) < lbm_type_of_functional(b)) ? lbm_type_of_functional(b) : lbm_type_of_functional(a);
  switch (t) {
  case LBM_TYPE_I: retval = CMP(lbm_dec_as_i32(a), lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = CMP(lbm_dec_as_u32(a), lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = CMP(lbm_dec_as_u32(a), lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = CMP(lbm_dec_as_i32(a), lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: retval = CMP(lbm_dec_as_float(a), lbm_dec_as_float(b)); break;
  case LBM_TYPE_U64: retval = CMP(lbm_dec_as_u64(a), lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: retval = CMP(lbm_dec_as_i64(a), lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: retval = CMP(lbm_dec_as_double(a), lbm_dec_as_double(b)); break;
  }
  return retval;
}

/* (array-create type size) */
static void array_create(lbm_value *args, lbm_uint nargs, lbm_value *result) {
  *result = ENC_SYM_EERROR;
  if (nargs == 1 && IS_NUMBER(args[0])) {
    lbm_heap_allocate_array(result, lbm_dec_as_u32(args[0]));
  }
}

static lbm_value index_list(lbm_value l, int32_t n) {
  lbm_value curr = l;

  if (n < 0) {
    int32_t len = (int32_t)lbm_list_length(l);
    n = len + n;
    if (n < 0) return ENC_SYM_NIL;
  }

  while (lbm_is_cons(curr) &&
          n > 0) {
    curr = lbm_cdr(curr);
    n --;
  }
  if (lbm_is_cons(curr)) {
    return lbm_car(curr);
  } else {
    return ENC_SYM_NIL;
  }
}

static lbm_value assoc_lookup(lbm_value key, lbm_value assoc) {
  lbm_value curr = assoc;
  while (lbm_is_cons(curr)) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (lbm_is_cons(c)) {
      if (struct_eq(lbm_ref_cell(c)->car, key)) {
        return lbm_ref_cell(c)->cdr;
      }
    } else {
      return ENC_SYM_EERROR;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return ENC_SYM_NO_MATCH;
}

static lbm_value cossa_lookup(lbm_value key, lbm_value assoc) {
  lbm_value curr = assoc;
  while (lbm_is_cons(curr)) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (lbm_is_cons(c)) {
      if (struct_eq(lbm_ref_cell(c)->cdr, key)) {
        return lbm_ref_cell(c)->car;
      }
    } else {
      return ENC_SYM_EERROR;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return ENC_SYM_NO_MATCH;
}



/***************************************************/
/* Fundamental operations                          */

static lbm_value fundamental_add(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_uint sum = lbm_enc_u(0);
  for (lbm_uint i = 0; i < nargs; i ++) {
    sum = add2(sum, args[i]);
    if (lbm_type_of(sum) == LBM_TYPE_SYMBOL) {
      break;
    }
  }
  return sum;
}

static lbm_value fundamental_sub(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint res;

  switch (nargs) {
  case 0:
      res = lbm_enc_u(0);
      break;

  case 1:
      res = negate(args[0]);
      break;

  case 2:
      res = sub2(args[0], args[1]);
      break;

  default:
      res = args[0];
      for (lbm_uint i = 1; i < nargs; i ++) {
          res = sub2(res, args[i]);
          if (lbm_type_of(res) == LBM_TYPE_SYMBOL)
              break;
      }
      break;
  }
  return res;
}

static lbm_value fundamental_mul(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint prod = lbm_enc_u(1);
  for (lbm_uint i = 0; i < nargs; i ++) {
    prod = mul2(prod, args[i]);
    if (lbm_type_of(prod) == LBM_TYPE_SYMBOL) {
      break;
    }
  }
  return prod;
}

static lbm_value fundamental_div(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint res = args[0];

  if (nargs >= 1) {
    for (lbm_uint i = 1; i < nargs; i ++) {
      res = div2(res, args[i]);
      if (lbm_type_of(res) == LBM_TYPE_SYMBOL) {
        break;
      }
    }
  } else {
    res = ENC_SYM_EERROR;
  }
  return res;
}

static lbm_value fundamental_mod(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint res = args[0];
  for (lbm_uint i = 1; i < nargs; i ++) {
    res = mod2(res, args[i]);
    if (lbm_type_of(res) == LBM_TYPE_SYMBOL) {
      break;
    }
  }
  return res;
}

static lbm_value fundamental_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;

  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    r = r && struct_eq(a, b);
    if (!r) break;
  }
  if (r) {
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_NIL;
}

static lbm_value fundamental_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
   lbm_value r = fundamental_eq(args, nargs, ctx);
  if (r == ENC_SYM_NIL) {
    return ENC_SYM_TRUE;
  } else if (r == ENC_SYM_TERROR) {
    return ENC_SYM_TERROR;
  }
  return ENC_SYM_NIL;
}


static lbm_value fundamental_numeq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!IS_NUMBER(a)) {
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!IS_NUMBER(b)) {
      ok = false;
      break;
    }
    r = r && (compare(a, b) == 0);
    if (!r) break;
  }
  if (ok) {
    if (r) {
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_num_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value r = fundamental_numeq(args, nargs, ctx);
  if (r == ENC_SYM_NIL) {
    return ENC_SYM_TRUE;
  } else if (r == ENC_SYM_TERROR) {
    return ENC_SYM_TERROR;
  }
  return ENC_SYM_NIL;
}


static lbm_value fundamental_lt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!IS_NUMBER(a)) {
    lbm_set_error_suspect(a);
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!IS_NUMBER(b)) {
      ok = false;
      break;
    }
    r = r && (compare(a, b) == -1);
  }
  if (ok) {
    if (r) {
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  lbm_set_error_suspect(b);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_gt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!IS_NUMBER(a)) {
    lbm_set_error_suspect(a);
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!IS_NUMBER(b)) {
      ok = false;
      break;
    }
    r = r && (compare(a, b) == 1);
  }
  if (ok) {
    if (r) {
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  lbm_set_error_suspect(b);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_leq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!IS_NUMBER(a)) {
    lbm_set_error_suspect(a);
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!IS_NUMBER(b)) {
      ok = false;
      break;
    }
    r = r && (compare(a, b) <= 0);
  }
  if (ok) {
    if (r) {
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  lbm_set_error_suspect(b);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_geq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!IS_NUMBER(a)) {
    lbm_set_error_suspect(a);
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!IS_NUMBER(b)) {
      ok = false;
      break;
    }
    r = r && (compare(a, b) >= 0);
  }
  if (ok) {
    if (r) {
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  lbm_set_error_suspect(b);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  if (nargs == 0) {
    return ENC_SYM_NIL;
  }
  lbm_uint a = args[0];
  if (lbm_type_of_functional(a) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(a) == SYM_NIL) {
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_NIL;
}

static lbm_value fundamental_gc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) args;
  (void) nargs;
  (void) ctx;
  lbm_perform_gc();
  return ENC_SYM_TRUE;
}

static lbm_value fundamental_self(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) args;
  (void) nargs;
  (void) ctx;
  return lbm_enc_i(ctx->id);
}

static lbm_value fundamental_set_mailbox_size(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {

  if (nargs == 1 && IS_NUMBER(args[0])) {
    uint32_t s = lbm_dec_as_u32(args[0]);
    if (lbm_mailbox_change_size(ctx, s)) {
      return ENC_SYM_TRUE;
    }
  }
  return ENC_SYM_NIL;
}

static lbm_value fundamental_cons(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs < 2) return ENC_SYM_EERROR;
  lbm_uint a = args[0];
  lbm_uint b = args[1];
  return lbm_cons(a,b);
}

static lbm_value fundamental_car(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs == 1) {
    if (lbm_is_cons(args[0])) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      return cell->car;
    }
  }
  return ENC_SYM_NIL;
}

static lbm_value fundamental_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs == 1) {
    if (lbm_is_cons(args[0])) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      return cell->cdr;
    }
  }
  return ENC_SYM_NIL;
}

static lbm_value fundamental_list(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_NIL;
  for (lbm_uint i = 1; i <= nargs; i ++) {
    result = lbm_cons(args[nargs-i], result);
    if (lbm_type_of(result) == LBM_TYPE_SYMBOL)
      break;
  }
  return result;
}

static lbm_value fundamental_append(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs == 0) return ENC_SYM_NIL;
  if (nargs == 1 && !lbm_is_list(args[0])) {
      lbm_set_error_suspect(args[0]);
    return ENC_SYM_TERROR;
  }
  lbm_value res = args[nargs-1];
  for (int i = (int)nargs -2; i >= 0; i --) {
    lbm_value curr = args[i];
    if (!lbm_is_list(curr)) {
      lbm_set_error_suspect(curr);
      return ENC_SYM_TERROR;
    }
    int n = 0;
    while (lbm_type_of_functional(curr) == LBM_TYPE_CONS) {
      n++;
      curr = lbm_cdr(curr);
    }
    curr = args[i];
    for (int j = n-1; j >= 0; j --) {
      res = lbm_cons(index_list(curr,j),res);
    }
  }
  return(res);
}

static lbm_value fundamental_undefine(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value *global_env = lbm_get_global_env();
  if (nargs == 1 && lbm_is_symbol(args[0])) {
    lbm_value key = args[0];
    lbm_uint ix_key = lbm_dec_sym(key) & GLOBAL_ENV_MASK;
    lbm_value env = global_env[ix_key];
    lbm_value res = lbm_env_drop_binding(env, key);
    if (res == ENC_SYM_NOT_FOUND) {
      return ENC_SYM_NIL;
    }
    global_env[ix_key] = res;
    return ENC_SYM_TRUE;
  } else if (nargs == 1 && lbm_is_cons(args[0])) {
    lbm_value curr = args[0];
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      lbm_value key = lbm_car(curr);
      lbm_uint ix_key = lbm_dec_sym(key) & GLOBAL_ENV_MASK;
      lbm_value env = global_env[ix_key];
      lbm_value res = lbm_env_drop_binding(env, key);
      if (res != ENC_SYM_NOT_FOUND) {
        global_env[ix_key] = res;
      }
      curr = lbm_cdr(curr);
    }
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_buf_create(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  array_create(args, nargs, &result);
  return result;
}

static lbm_value fundamental_symbol_to_string(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs < 1 ||
      lbm_type_of_functional(args[0]) != LBM_TYPE_SYMBOL)
    return ENC_SYM_NIL;
  lbm_value sym = args[0];
  const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(sym));
  if (sym_str == NULL) return ENC_SYM_NIL;
  size_t len = strlen(sym_str);

  lbm_value v;
  if (lbm_heap_allocate_array(&v, len+1)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(v);
    if (!arr) return ENC_SYM_MERROR;
    memset(arr->data,0,len+1);
    memcpy(arr->data,sym_str,len);
  } else {
    return ENC_SYM_MERROR;
  }
  return v;
}

static lbm_value fundamental_string_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs < 1 ||
      lbm_is_array_r(args[0]))
    return result;
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
  if (!arr) return ENC_SYM_FATAL_ERROR;
  char *str = (char *)arr->data;
  lbm_uint sym;
  if (lbm_get_symbol_by_name(str, &sym)) {
    result = lbm_enc_sym(sym);
  } else if (lbm_add_symbol(str, &sym)) {
    result = lbm_enc_sym(sym);
  }
  return result;
}

static lbm_value fundamental_symbol_to_uint(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs < 1) return ENC_SYM_EERROR;
  lbm_value s = args[0];
  if (lbm_type_of_functional(s) == LBM_TYPE_SYMBOL)
    return lbm_enc_u(lbm_dec_sym(s));

  lbm_set_error_suspect(s);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_uint_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs < 1) return ENC_SYM_EERROR;
  lbm_value s = args[0];
  if (lbm_type_of_functional(s) == LBM_TYPE_U)
    return lbm_enc_sym(lbm_dec_u(s));

  lbm_set_error_suspect(s);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_set_car(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs == 2) {
    if (lbm_set_car(args[0],args[1])) {
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  return ENC_SYM_EERROR;
}

static lbm_value fundamental_set_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs == 2) {
    if (lbm_set_cdr(args[0],args[1])) {
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  return ENC_SYM_EERROR;
}

static lbm_value fundamental_set_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 3) {
    if (lbm_is_cons(args[0]) &&
        IS_NUMBER(args[1])) {
      lbm_value curr = args[0];
      lbm_uint i = 0;
      lbm_uint ix = lbm_dec_as_u32(args[1]);
      result = ENC_SYM_NIL;
      while (lbm_is_ptr(curr)) {
        if (i == ix) {
          lbm_set_car(curr, args[2]);
          result = args[0];
          break;
        } else if (i > ix) {
          break;
        }
        curr = lbm_cdr(curr);
        i++;
      }
    }
  }
  return result;
}

static lbm_value fundamental_assoc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 2) {
    if (lbm_is_cons(args[0])) {
      lbm_value r = assoc_lookup(args[1], args[0]);
      if (lbm_is_symbol(r) &&
          lbm_dec_sym(r) == SYM_NO_MATCH) {
        result = ENC_SYM_NIL;
      } else {
        result = r;
      }
    } else if (lbm_is_symbol(args[0]) &&
               lbm_dec_sym(args[0]) == SYM_NIL) {
      result = args[0]; /* nil */
    } /* else error */
  }
  return result;
}

static lbm_value fundamental_acons(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 3) {
    lbm_value keyval = lbm_cons(args[0], args[1]);
    lbm_value new_alist = lbm_cons(keyval, args[2]);

    if (lbm_is_symbol(keyval) ||
        lbm_is_symbol(new_alist) )
      result = ENC_SYM_MERROR;
    else
      result = new_alist;
  } else if (nargs == 2) {
    result = lbm_cons(args[0], args[1]);
  }
  return result;
}

static lbm_value fundamental_set_assoc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 3) {
    result = lbm_env_set(args[0], args[1], args[2]);
  } else if (nargs == 2 && lbm_is_cons(args[1])) {
    lbm_value x = lbm_car(args[1]);
    lbm_value xs = lbm_cdr(args[1]);
    result = lbm_env_set(args[0], x, xs);
  }
  return result;
}

static lbm_value fundamental_cossa(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 2) {
    if (lbm_is_cons(args[0])) {
      lbm_value r = cossa_lookup(args[1], args[0]);
      if (lbm_is_symbol(r) &&
          lbm_dec_sym(r) == SYM_NO_MATCH) {
        result = ENC_SYM_NIL;
      } else {
        result = r;
      }
    } else if (lbm_is_symbol(args[0]) &&
               lbm_dec_sym(args[0]) == SYM_NIL) {
      result = args[0]; /* nil */
    } /* else error */
  }
  return result;
}

static lbm_value fundamental_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 2 && IS_NUMBER(args[1])) {
    result = index_list(args[0], lbm_dec_as_i32(args[1]));
  }
  return result;
}

static lbm_value fundamental_to_i(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_i((lbm_int)lbm_dec_as_i64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_i32(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_i32(lbm_dec_as_i32(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_u(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_u((lbm_uint)lbm_dec_as_u64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_u32(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_u32(lbm_dec_as_u32(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_float(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_float(lbm_dec_as_float(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_i64(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_i64(lbm_dec_as_i64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_u64(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_u64(lbm_dec_as_u64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_double(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_double(lbm_dec_as_double(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_byte(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_char(lbm_dec_as_char(args[0]));
  }
  return result;
}

static lbm_value fundamental_shl(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (!(IS_NUMBER(args[0]) && IS_NUMBER(args[1]))) {
      return retval;
    }
    switch (lbm_type_of_functional(args[0])) {
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(args[0]) << lbm_dec_as_u32(args[1])); break;
    }
  }
  return retval;
}

static lbm_value fundamental_shr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (!(IS_NUMBER(args[0]) && IS_NUMBER(args[1]))) {
      return retval;
    }
    switch (lbm_type_of_functional(args[0])) {
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(args[0]) >> lbm_dec_as_u32(args[1])); break;
    }
  }
  return retval;
}

static lbm_value fundamental_bitwise_and(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (!(IS_NUMBER(args[0]) && IS_NUMBER(args[1]))) {
      return retval;
    }
    switch (lbm_type_of_functional(args[0])) {
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) & lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) & lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(args[0]) & lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(args[0]) & lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(args[0]) & lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(args[0]) & lbm_dec_as_u64(args[1])); break;
    }
  }
  return retval;
}

static lbm_value fundamental_bitwise_or(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (!(IS_NUMBER(args[0]) && IS_NUMBER(args[1]))) {
      return retval;
    }
    switch (lbm_type_of_functional(args[0])) {
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) | lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) | lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(args[0]) | lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(args[0]) | lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(args[0]) | lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(args[0]) | lbm_dec_as_u64(args[1])); break;
    }
  }
  return retval;
}

static lbm_value fundamental_bitwise_xor(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (!(IS_NUMBER(args[0]) && IS_NUMBER(args[1]))) {
      return retval;
    }
    switch (lbm_type_of_functional(args[0])) {
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) ^ lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) ^ lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(args[0]) ^ lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(args[0]) ^ lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(args[0]) ^ lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(args[0]) ^ lbm_dec_as_u64(args[1])); break;
    }
  }
  return retval;
}

static lbm_value fundamental_bitwise_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 1) {
    retval = ENC_SYM_TERROR;
    if (!(IS_NUMBER(args[0]))) {
      return retval;
    }
    switch (lbm_type_of_functional(args[0])) {
    case LBM_TYPE_I: retval = lbm_enc_i(~lbm_dec_i(args[0])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(~lbm_dec_u(args[0])); break;
    case LBM_TYPE_U32: retval = lbm_enc_u32(~lbm_dec_u32(args[0])); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(~lbm_dec_i32(args[0])); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(~lbm_dec_i64(args[0])); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(~lbm_dec_u64(args[0])); break;
    }
  }
  return retval;
}

static lbm_value fundamental_custom_destruct(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1 && (lbm_type_of(args[0]) == LBM_TYPE_CUSTOM)) {
    lbm_uint *mem_ptr = (lbm_uint*)lbm_dec_custom(args[0]);
    if(!mem_ptr) return ENC_SYM_FATAL_ERROR;
    lbm_custom_type_destroy(mem_ptr);
    lbm_value tmp = lbm_set_ptr_type(args[0], LBM_TYPE_CONS);
    lbm_set_car(tmp, ENC_SYM_NIL);
    lbm_set_cdr(tmp, ENC_SYM_NIL);
      /* The original value will still be of type custom_ptr */
    result = ENC_SYM_TRUE;
  }
  return result;
}

static lbm_value fundamental_type_of(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs != 1) return ENC_SYM_NIL;
  lbm_value val = args[0];
  lbm_type t = lbm_type_of(val);

  if (lbm_is_ptr(val)) {
    // Ignore constant or not constant.
    t &= LBM_PTR_TO_CONSTANT_MASK;
  }
  switch(t) {
  case LBM_TYPE_CONS: return ENC_SYM_TYPE_LIST;
  case LBM_TYPE_ARRAY: return ENC_SYM_TYPE_ARRAY;
  case LBM_TYPE_I32: return ENC_SYM_TYPE_I32;
  case LBM_TYPE_U32: return ENC_SYM_TYPE_U32;
  case LBM_TYPE_FLOAT: return ENC_SYM_TYPE_FLOAT;
  case LBM_TYPE_I64: return ENC_SYM_TYPE_I64;
  case LBM_TYPE_U64: return ENC_SYM_TYPE_U64;
  case LBM_TYPE_DOUBLE: return ENC_SYM_TYPE_DOUBLE;
  case LBM_TYPE_I: return ENC_SYM_TYPE_I;
  case LBM_TYPE_U: return ENC_SYM_TYPE_U;
  case LBM_TYPE_CHAR: return ENC_SYM_TYPE_CHAR;
  case LBM_TYPE_SYMBOL: return ENC_SYM_TYPE_SYMBOL;
  }
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_list_length(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1 && lbm_is_list(args[0])) {
    int32_t len = (int32_t)lbm_list_length(args[0]);
    result = lbm_enc_i(len);
  }
  return result;
}

static lbm_value fundamental_range(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;

  int32_t start;
  int32_t end;
  bool rev = false;

  if (nargs == 1 && IS_NUMBER(args[0])) {
    start = 0;
    end = lbm_dec_as_i32(args[0]);
  } else if (nargs == 2 &&
             IS_NUMBER(args[0]) &&
             IS_NUMBER(args[1])) {
    start = lbm_dec_as_i32(args[0]);
    end = lbm_dec_as_i32(args[1]);
  } else {
    return result;
  }

  if (end == start) return ENC_SYM_NIL;
  else if (end < start) {
    int32_t tmp = end;
    end = start;
    start = tmp;
    rev = true;
  }

  int num = end - start;

  if ((unsigned int)num > lbm_heap_num_free()) {
    return ENC_SYM_MERROR;
  }

  lbm_value r_list = ENC_SYM_NIL;
  for (int i = end - 1; i >= start; i --) {
    r_list = lbm_cons(lbm_enc_i(i), r_list);
  }
  return rev ? lbm_list_destructive_reverse(r_list) : r_list;
}

static lbm_value fundamental_reg_event_handler(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs != 1 || !IS_NUMBER(args[0])) {
    return ENC_SYM_TERROR;
  }

  lbm_set_event_handler_pid((lbm_cid)lbm_dec_i(args[0]));
  return(ENC_SYM_TRUE);
}

static lbm_value fundamental_take(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs != 2 || !IS_NUMBER(args[1]) || !lbm_is_list(args[0]))
    return ENC_SYM_TERROR;

  int len = lbm_dec_as_i32(args[1]);
  return lbm_list_copy(&len, args[0]);
}

static lbm_value fundamental_drop(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs != 2 || !IS_NUMBER(args[1]) || !lbm_is_list(args[0]))
    return ENC_SYM_TERROR;
  return lbm_list_drop(lbm_dec_as_u32(args[1]), args[0]);
}

const fundamental_fun fundamental_table[] =
  {fundamental_add,
   fundamental_sub,
   fundamental_mul,
   fundamental_div,
   fundamental_mod,
   fundamental_eq,
   fundamental_not_eq,
   fundamental_numeq,
   fundamental_num_not_eq,
   fundamental_lt,
   fundamental_gt,
   fundamental_leq,
   fundamental_geq,
   fundamental_not,
   fundamental_gc,
   fundamental_self,
   fundamental_set_mailbox_size,
   fundamental_cons,
   fundamental_car,
   fundamental_cdr,
   fundamental_list,
   fundamental_append,
   fundamental_undefine,
   fundamental_buf_create,
   fundamental_symbol_to_string,
   fundamental_string_to_symbol,
   fundamental_symbol_to_uint,
   fundamental_uint_to_symbol,
   fundamental_set_car,
   fundamental_set_cdr,
   fundamental_set_ix,
   fundamental_assoc,
   fundamental_acons,
   fundamental_set_assoc,
   fundamental_cossa,
   fundamental_ix,
   fundamental_to_i,
   fundamental_to_i32,
   fundamental_to_u,
   fundamental_to_u32,
   fundamental_to_float,
   fundamental_to_i64,
   fundamental_to_u64,
   fundamental_to_double,
   fundamental_to_byte,
   fundamental_shl,
   fundamental_shr,
   fundamental_bitwise_and,
   fundamental_bitwise_or,
   fundamental_bitwise_xor,
   fundamental_bitwise_not,
   fundamental_custom_destruct,
   fundamental_type_of,
   fundamental_list_length,
   fundamental_range,
   fundamental_reg_event_handler,
   fundamental_take,
   fundamental_drop,
  };
