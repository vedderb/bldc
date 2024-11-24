/*
    Copyright 2019, 2021 - 2024      Joel Svensson   svenssonjoel@yahoo.se
                           2022      Benjamin Vedder

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
#include "lbm_constants.h"
#include "fundamental.h"
#include "lbm_defrag_mem.h"

#include <stdio.h>
#include <math.h>

/* Type promotion ranks

   32bit LBM:
   byte < i < u < i32 < u32 < i64 < u64 < float < double

   64bit LBM:
   byte < i32 < u32 < i < u < i64 < u64 < float < double
 */

// PROMOTE_SWAP is for commutative operations
// PROMOTE is for non-commutative operations

#ifndef LBM64
#define PROMOTE_SWAP(t, a, b)                                  \
  if (lbm_type_of_functional(a) < lbm_type_of_functional(b)) { \
    lbm_value tmp = a;                                         \
    a = b;                                                     \
    b = tmp;                                                   \
  }                                                            \
  t = lbm_type_of_functional(a);
#else
#define PROMOTE_SWAP(t, a, b)                                           \
  if (lbm_type_of_functional(b) == LBM_TYPE_FLOAT && (lbm_type_of_functional(a) < LBM_TYPE_DOUBLE)) { \
      lbm_value tmp = a;                                                \
      a = b;                                                            \
      b = tmp;                                                          \
  } if (lbm_type_of_functional(a) == LBM_TYPE_FLOAT && (lbm_type_of_functional(b) < LBM_TYPE_DOUBLE)) { \
    /* DO NOTHING */                                                    \
  } else if (lbm_type_of_functional(a) < lbm_type_of_functional(b)) {   \
    lbm_value tmp = a;                                                  \
    a = b;                                                              \
    b = tmp;                                                            \
  }                                                                     \
  t = lbm_type_of_functional(a);
#endif

#ifndef LBM64
#define PROMOTE(t, a, b)                                                \
  t = lbm_type_of_functional(a);                                        \
  lbm_uint t_b = lbm_type_of_functional(b);                             \
  if (t < t_b) {                                                        \
    t  = t_b;                                                           \
  }

#else
#define PROMOTE(t, a, b)                                                \
  if (lbm_type_of_functional(b) == LBM_TYPE_FLOAT) {                    \
    if (lbm_type_of_functional(a) < LBM_TYPE_DOUBLE) {                  \
      t = LBM_TYPE_FLOAT;                                               \
    } else {                                                            \
      t = lbm_type_of_functional(a);                                    \
    }                                                                   \
  }  else if (lbm_type_of_functional(a) < lbm_type_of_functional(b)) {  \
    t = lbm_type_of_functional(b);                                      \
  } else {                                                              \
    t = lbm_type_of_functional(a);                                      \
  }
#endif


#define IS_NUMBER lbm_is_number

// Todo: It may be possible perform some of these operations
//       on encoded values followed by a "correction" of the result values
//       type bits.
//       But the checks required to figure out if it is possible to apply the
//       operation in this way has a cost too...

static lbm_uint mul2(lbm_uint a, lbm_uint b) {
  lbm_uint retval = ENC_SYM_TERROR;
  if (IS_NUMBER(a) && IS_NUMBER(b)) {
    lbm_type t;
    PROMOTE_SWAP(t, a, b);
    switch (t) {
    case LBM_TYPE_CHAR: retval = lbm_enc_char((uint8_t)(lbm_dec_char(a) * lbm_dec_char(b))); break;
#ifdef LBM64
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(a) * lbm_dec_as_i64(b)); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(a) * lbm_dec_as_u64(b)); break;
#else
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(a) * lbm_dec_as_i32(b)); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(a) * lbm_dec_as_u32(b)); break;
#endif
    case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(a) * lbm_dec_as_u32(b)); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(a) * lbm_dec_as_i32(b)); break;
    case LBM_TYPE_FLOAT: retval = lbm_enc_float(lbm_dec_float(a) * lbm_dec_as_float(b)); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(a) * lbm_dec_as_u64(b)); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(a) * lbm_dec_as_i64(b)); break;
    case LBM_TYPE_DOUBLE: retval = lbm_enc_double(lbm_dec_double(a) * lbm_dec_as_double(b)); break;
    }
  } else {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
  }
  return retval;
}

static lbm_uint div2(lbm_uint a, lbm_uint b) {
  lbm_uint retval = ENC_SYM_TERROR;
  if (IS_NUMBER(a) && IS_NUMBER(b)) {
    lbm_type t;
    PROMOTE(t, a, b);
    switch (t) {
    case LBM_TYPE_CHAR: if (lbm_dec_char(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_char((uint8_t)(lbm_dec_char(a) / lbm_dec_char(b))); break;
#ifdef LBM64
    case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i64(a) / lbm_dec_as_i64(b)); break;
    case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u64(a) / lbm_dec_as_u64(b)); break;
#else
    case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
    case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
#endif
    case LBM_TYPE_U32: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u32(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
    case LBM_TYPE_I32: if (lbm_dec_as_i32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i32(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
    case LBM_TYPE_FLOAT: if (lbm_dec_as_float(b) == 0.0f || lbm_dec_as_float(b) == -0.0f) {return ENC_SYM_DIVZERO;} retval = lbm_enc_float(lbm_dec_as_float(a) / lbm_dec_as_float(b)); break;
    case LBM_TYPE_U64: if (lbm_dec_as_u64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u64(lbm_dec_as_u32(a) / lbm_dec_as_u64(b)); break;
    case LBM_TYPE_I64: if (lbm_dec_as_i64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i64(lbm_dec_as_i32(a) / lbm_dec_as_i64(b)); break;
    case LBM_TYPE_DOUBLE: if (lbm_dec_as_double(b) == (double)0.0 || lbm_dec_as_double(b) == (double)-0.0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_double(lbm_dec_as_double(a) / lbm_dec_as_double(b)); break;
    }
  } else {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
  }
  return retval;
}

static lbm_uint mod2(lbm_uint a, lbm_uint b) {
  lbm_uint retval = ENC_SYM_TERROR;
  if (IS_NUMBER(a) && IS_NUMBER(b)) {
    lbm_type t;
    PROMOTE(t, a, b);
    switch (t) {
    case LBM_TYPE_CHAR: if (lbm_dec_char(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_char((uint8_t)(lbm_dec_char(a) % lbm_dec_as_i32(b))); break;
#ifdef LBM64
    case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i64(a) % lbm_dec_as_i64(b)); break;
    case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u64(a) % lbm_dec_as_u64(b)); break;
#else
    case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i32(a) % lbm_dec_as_i32(b)); break;
    case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u32(a) % lbm_dec_as_u32(b)); break;
#endif
    case LBM_TYPE_U32: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u32(lbm_dec_as_u32(a) % lbm_dec_as_u32(b)); break;
    case LBM_TYPE_I32: if (lbm_dec_as_i32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i32(lbm_dec_as_i32(a) % lbm_dec_as_i32(b)); break;
    case LBM_TYPE_FLOAT: if (lbm_dec_as_float(b) == 0.0f || lbm_dec_as_float(b) == -0.0f) {return ENC_SYM_DIVZERO;} retval = lbm_enc_float(fmodf(lbm_dec_as_float(a), lbm_dec_as_float(b))); break;
    case LBM_TYPE_U64: if (lbm_dec_as_u64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u64(lbm_dec_as_u64(a) % lbm_dec_as_u64(b)); break;
    case LBM_TYPE_I64: if (lbm_dec_as_i64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i64(lbm_dec_as_i64(a) % lbm_dec_as_i64(b)); break;
    case LBM_TYPE_DOUBLE: if (lbm_dec_as_double(b) == (double)0.0 || lbm_dec_as_double(b) == (double)-0.0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_double(fmod(lbm_dec_as_double(a), lbm_dec_as_double(b))); break;
    }
  } else {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
  }
  return retval;
}

static lbm_uint sub2(lbm_uint a, lbm_uint b) {
  lbm_uint retval = ENC_SYM_TERROR;
  if (IS_NUMBER(a) && IS_NUMBER(b)) {
    lbm_uint t;
    PROMOTE(t, a, b);
    switch (t) {
    case LBM_TYPE_BYTE: retval = lbm_enc_char((uint8_t)(lbm_dec_char(a) - lbm_dec_char(b))); break;
#ifdef LBM64
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_as_i64(a) - lbm_dec_as_i64(b)); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_as_u64(a) - lbm_dec_as_u64(b)); break;
#else
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_as_i32(a) - lbm_dec_as_i32(b)); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_as_u32(a) - lbm_dec_as_u32(b)); break;
#endif
    case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_as_u32(a) - lbm_dec_as_u32(b)); break;
    case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_as_i32(a) - lbm_dec_as_i32(b)); break;
    case LBM_TYPE_FLOAT: retval = lbm_enc_float(lbm_dec_as_float(a) - lbm_dec_as_float(b)); break;
    case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_as_u64(a) - lbm_dec_as_u64(b)); break;
    case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_as_i64(a) - lbm_dec_as_i64(b)); break;
    case LBM_TYPE_DOUBLE: retval = lbm_enc_double(lbm_dec_as_double(a) - lbm_dec_as_double(b)); break;
    }
  } else {
    lbm_set_error_suspect(IS_NUMBER(a) ? b : a);
  }
  return retval;
}

// a and b must be bytearrays!
static bool bytearray_equality(lbm_value a, lbm_value b) {
  lbm_array_header_t *a_ = (lbm_array_header_t*)lbm_car(a);
  lbm_array_header_t *b_ = (lbm_array_header_t*)lbm_car(b);

  // A NULL array arriving here should be impossible.
  // if the a and b are not valid arrays at this point, the data
  // is most likely nonsense - corrupted by cosmic radiation.
  bool res = a_->size == b_->size;
  if (res) {
    res = (memcmp((char*)a_->data, (char*)b_->data, a_->size) == 0);
  }
  return res;
}

// a and b must be arrays!
static bool array_struct_equality(lbm_value a, lbm_value b) {
  lbm_array_header_t *a_ = (lbm_array_header_t*)lbm_car(a);
  lbm_array_header_t *b_ = (lbm_array_header_t*)lbm_car(b);
  lbm_value *adata = (lbm_value*)a_->data;
  lbm_value *bdata = (lbm_value*)b_->data;
  bool res =  a_->size == b_->size;
  if (res) {
    lbm_uint size = (lbm_uint)a_->size / (lbm_uint)sizeof(lbm_value);
    for (lbm_uint i = 0; i < size; i ++ ) {
      res = struct_eq(adata[i], bdata[i]);
      if (!res) break;
    }
  }
  return res;
}

bool struct_eq(lbm_value a, lbm_value b) {

  bool res = false;
  lbm_type ta = lbm_type_of_functional(a);
  lbm_type tb = lbm_type_of_functional(b);

  if (ta == tb) {
    switch(ta){
    case LBM_TYPE_SYMBOL:
      res = (lbm_dec_sym(a) == lbm_dec_sym(b)); break;
    case LBM_TYPE_I:
      res =  (lbm_dec_i(a) == lbm_dec_i(b)); break;
    case LBM_TYPE_U:
      res = (lbm_dec_u(a) == lbm_dec_u(b)); break;
    case LBM_TYPE_CHAR:
      res = (lbm_dec_char(a) == lbm_dec_char(b)); break;
    case LBM_TYPE_CONS:
      res = ( struct_eq(lbm_car(a),lbm_car(b)) &&
              struct_eq(lbm_cdr(a),lbm_cdr(b)) ); break;
    case LBM_TYPE_I32:
      res = (lbm_dec_i32(a) == lbm_dec_i32(b)); break;
    case LBM_TYPE_U32:
      res = (lbm_dec_u32(a) == lbm_dec_u32(b)); break;
    case LBM_TYPE_FLOAT:
      res = (lbm_dec_float(a) == lbm_dec_float(b)); break;
    case LBM_TYPE_I64:
      res =  (lbm_dec_i64(a) == lbm_dec_i64(b)); break;
    case LBM_TYPE_U64:
      res = (lbm_dec_u64(a) == lbm_dec_u64(b)); break;
    case LBM_TYPE_DOUBLE:
      res = (lbm_dec_double(a) == lbm_dec_double(b)); break;
    case LBM_TYPE_ARRAY:
      res =  bytearray_equality(a, b); break;
    case LBM_TYPE_LISPARRAY:
      res =  array_struct_equality(a, b); break;
    }
  }
  return res;
}


/* returns -1 if a < b; 0 if a = b; 1 if a > b
   args must be numbers
*/
static int compare_num(lbm_uint a, lbm_uint b) {

  int retval = 0;

  lbm_uint t;
  PROMOTE(t, a, b);
  switch (t) {
  case LBM_TYPE_CHAR: retval = CMP(lbm_dec_char(a), lbm_dec_char(b)); break;
#ifdef LBM64
  case LBM_TYPE_I: retval = CMP(lbm_dec_as_i64(a), lbm_dec_as_i64(b)); break;
  case LBM_TYPE_U: retval = CMP(lbm_dec_as_u64(a), lbm_dec_as_u64(b)); break;
#else
  case LBM_TYPE_I: retval = CMP(lbm_dec_as_i32(a), lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = CMP(lbm_dec_as_u32(a), lbm_dec_as_u32(b)); break;
#endif
  case LBM_TYPE_U32: retval = CMP(lbm_dec_as_u32(a), lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = CMP(lbm_dec_as_i32(a), lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: retval = CMP(lbm_dec_as_float(a), lbm_dec_as_float(b)); break;
  case LBM_TYPE_U64: retval = CMP(lbm_dec_as_u64(a), lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: retval = CMP(lbm_dec_as_i64(a), lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: retval = CMP(lbm_dec_as_double(a), lbm_dec_as_double(b)); break;
  }
  return retval;
}

/* (array-create size) */
static void array_create(lbm_value *args, lbm_uint nargs, lbm_value *result) {
  *result = ENC_SYM_EERROR;
  if (nargs == 1 && IS_NUMBER(args[0])) {
    lbm_heap_allocate_array(result, lbm_dec_as_u32(args[0]));
  }
}

static lbm_value assoc_lookup(lbm_value key, lbm_value assoc) {
  lbm_value curr = assoc;
  lbm_value res = ENC_SYM_NO_MATCH;
  while (lbm_is_cons(curr)) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (lbm_is_cons(c)) {
      if (struct_eq(lbm_ref_cell(c)->car, key)) {
        res = lbm_ref_cell(c)->cdr;
        break;
      }
    } else {
      res = ENC_SYM_EERROR;
      break;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return res;
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
  lbm_uint sum = lbm_enc_char(0);
  for (lbm_uint i = 0; i < nargs; i ++) {
    lbm_value v = args[i];
    if (IS_NUMBER(v)) { // inlining add2 explicitly removes one condition.
        lbm_type t;
        PROMOTE_SWAP(t, sum, v);
        switch (t) {
        case LBM_TYPE_BYTE: sum = lbm_enc_char((uint8_t)(lbm_dec_char(sum) + lbm_dec_char(v))); break;
#ifdef LBM64
        case LBM_TYPE_I: sum = lbm_enc_i(lbm_dec_i(sum) + lbm_dec_as_i64(v)); break;
        case LBM_TYPE_U: sum = lbm_enc_u(lbm_dec_u(sum) + lbm_dec_as_u64(v)); break;
#else
        case LBM_TYPE_I: sum = lbm_enc_i(lbm_dec_i(sum) + lbm_dec_as_i32(v)); break;
        case LBM_TYPE_U: sum = lbm_enc_u(lbm_dec_u(sum) + lbm_dec_as_u32(v)); break;
#endif
        case LBM_TYPE_U32: sum = lbm_enc_u32(lbm_dec_u32(sum) + lbm_dec_as_u32(v)); break;
        case LBM_TYPE_I32: sum = lbm_enc_i32(lbm_dec_i32(sum) + lbm_dec_as_i32(v)); break;
        case LBM_TYPE_FLOAT: sum = lbm_enc_float(lbm_dec_float(sum) + lbm_dec_as_float(v)); break;
          // extra check only in the cases that require it. (on 32bit, some wasted cycles on 64 bit)
        case LBM_TYPE_U64:
          sum = lbm_enc_u64(lbm_dec_u64(sum) + lbm_dec_as_u64(v));
          if (lbm_is_symbol_merror(sum)) goto add_end;
          break;
        case LBM_TYPE_I64:
          sum = lbm_enc_i64(lbm_dec_i64(sum) + lbm_dec_as_i64(v));
          if (lbm_is_symbol_merror(sum)) goto add_end;
          break;
        case LBM_TYPE_DOUBLE:
          sum = lbm_enc_double(lbm_dec_double(sum) + lbm_dec_as_double(v));
          if (lbm_is_symbol_merror(sum)) goto add_end;
          break;
        }
    } else {
      lbm_set_error_suspect(v);
      sum = ENC_SYM_TERROR;
      break; // out of loop
    }
  }
 add_end:
  return sum;
}

static lbm_value fundamental_sub(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint res;

  switch (nargs) {
  case 0:
    res = lbm_enc_char(0);
    break;

  case 1:
    res = sub2(lbm_enc_char(0),args[0]);
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

  lbm_uint prod = lbm_enc_char(1);
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

  if (nargs >= 2) {
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
  if (nargs != 2) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_EERROR;
  }
  lbm_value res = args[0];
  lbm_value arg2 = args[1];
  res = mod2(res, arg2);
  return res;
}

static lbm_value fundamental_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  bool r = true;

  for (lbm_uint i = 1; i < nargs; i ++) {
    lbm_uint b = args[i];
    r = r && struct_eq(a, b);
    if (!r) break;
  }
  return r ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value fundamental_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value r = fundamental_eq(args, nargs, ctx);
  return r ? ENC_SYM_NIL : ENC_SYM_TRUE; // Works because ENC_SYM_NIL == 0 and ENC_SYM_TRUE is != 0
}


static lbm_value fundamental_numeq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_value res = ENC_SYM_TERROR;

  if (IS_NUMBER(a)) {
    res = ENC_SYM_TRUE;
    for (lbm_uint i = 1; i < nargs; i ++) {
      lbm_uint b = args[i];
      if (!IS_NUMBER(b)) {
        res = ENC_SYM_TERROR;
        break;
      }
      if (!compare_num(a, b) == 0) {
        res = ENC_SYM_NIL;
        break;
      }
    }
  }
  return res;
}

static lbm_value fundamental_num_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value r = fundamental_numeq(args, nargs, ctx);
  if (r == ENC_SYM_NIL) {
    r = ENC_SYM_TRUE;
  } else if (r == ENC_SYM_TRUE) {
    r = ENC_SYM_NIL;
  }
  return r;
}

static lbm_value fundamental_leq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b = ENC_SYM_NIL;
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
    r = r && (compare_num(a, b) <= 0);
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
  lbm_uint b = ENC_SYM_NIL;
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
    r = r && (compare_num(a, b) >= 0);
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

static lbm_value fundamental_lt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value r = fundamental_geq(args, nargs, ctx);
  if (r == ENC_SYM_NIL) r = ENC_SYM_TRUE;
  else if (r == ENC_SYM_TRUE) r = ENC_SYM_NIL;
  return r;
}

static lbm_value fundamental_gt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value r = fundamental_leq(args, nargs, ctx);
  if (r == ENC_SYM_NIL) r = ENC_SYM_TRUE;
  else if (r == ENC_SYM_TRUE) r = ENC_SYM_NIL;
  return r;
}

static lbm_value fundamental_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value r = ENC_SYM_EERROR;
  if (nargs == 1) {
    r = args[0] ? ENC_SYM_NIL : ENC_SYM_TRUE;
  }
  return r;
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
  lbm_value r = ENC_SYM_EERROR;
  if (nargs == 1) {
    if (IS_NUMBER(args[0])) {
      uint32_t s = lbm_dec_as_u32(args[0]);
      if (lbm_mailbox_change_size(ctx, s)) {
        r = ENC_SYM_TRUE;
      } else {
        r = ENC_SYM_NIL;
      }
    } else {
      r = ENC_SYM_TERROR;
    }
  }
  return r;
}

static lbm_value fundamental_cons(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value r = ENC_SYM_EERROR;
  if (nargs == 2) {
    lbm_uint a = args[0];
    lbm_uint b = args[1];
    r = lbm_cons(a,b);
  }
  return r;
}

static lbm_value fundamental_car(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value r = ENC_SYM_EERROR;
  if (nargs == 1) {
    if (lbm_is_cons(args[0])) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      r =  cell->car;
    } else if (lbm_is_symbol_nil(args[0])) {
      r = ENC_SYM_NIL;
    } else {
      r = ENC_SYM_TERROR;
    }
  }
  return r;
}

static lbm_value fundamental_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value r = ENC_SYM_EERROR;
  if (nargs == 1) {
    if (lbm_is_cons(args[0])) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      r = cell->cdr;
    } else if (lbm_is_symbol_nil(args[0])) {
      r = ENC_SYM_NIL;
    } else {
      r = ENC_SYM_TERROR;
    }
  }
  return r;
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
      res = lbm_cons(lbm_index_list(curr,j),res);
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
  lbm_value res = ENC_SYM_EERROR;
  if (nargs == 1) {
    if (lbm_type_of_functional(args[0]) == LBM_TYPE_SYMBOL) {
      lbm_value sym = args[0];
      const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(sym));
      if (sym_str == NULL) return ENC_SYM_NIL;
      size_t len = strlen(sym_str);
      lbm_value v;
      if (lbm_heap_allocate_array(&v, len+1)) {
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(v);
        memset(arr->data,0,len+1);
        memcpy(arr->data,sym_str,len);
        res = v;
      } else {
        res = ENC_SYM_MERROR;
      }
    } else {
      res = ENC_SYM_TERROR;
    }
  }
  return res;
}

static lbm_value fundamental_string_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = ENC_SYM_TERROR;
    if (lbm_is_array_r(args[0])) {
      lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
      // TODO: String to symbol, string should be in LBM_memory..
      // Some better sanity check is possible here.
      // Check that array points into lbm_memory.
      // Additionally check that it is a zero-terminated string.
      char *str = (char *)arr->data;
      lbm_uint sym = ENC_SYM_NIL;
      lbm_str_to_symbol(str,&sym);
      result = lbm_enc_sym(sym);
    }
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
  lbm_value res = ENC_SYM_EERROR;
  if (nargs == 2) {
    res = lbm_set_car(args[0], args[1]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_set_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_EERROR;
  if (nargs == 2) {
    res = lbm_set_cdr(args[0],args[1]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_set_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_TERROR;
  if (nargs == 3 && IS_NUMBER(args[1])) {
    if (lbm_is_list_rw(args[0])) {
      lbm_value curr = args[0];
      lbm_uint i = 0;
      lbm_int ix_pre = lbm_dec_as_i32(args[1]);
      if (ix_pre < 0) {
        lbm_int len = (lbm_int)lbm_list_length(args[0]);
        ix_pre = len + ix_pre;
      }
      lbm_uint ix = (lbm_uint)ix_pre;
      while (lbm_is_cons_rw(curr)) { // rw as we are going to modify
        lbm_value next = lbm_cdr(curr);
        if (i == ix) {
          lbm_set_car(curr, args[2]);
          result = args[0]; // Acts as true and as itself.
          break;
        } else if (lbm_is_symbol_nil(next)) {
          result = ENC_SYM_NIL; // index out of bounds, no update.
          break;
        }
        curr = next;
        i++;
      }
    } else if (lbm_is_lisp_array_rw(args[0])) {
      lbm_value index = lbm_dec_as_u32(args[1]);
      lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(args[0]);
      lbm_value *arrdata = (lbm_value*)header->data;
      lbm_uint size = header->size / sizeof(lbm_value);
      if (index < size) {
        arrdata[index] = args[2]; // value
        result = args[0];
      }  // index out of range will be eval error.
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
          r == ENC_SYM_NO_MATCH) {
        result = ENC_SYM_NIL;
      } else {
        result = r;
      }
    } else if (lbm_is_symbol(args[0]) &&
               args[0] == ENC_SYM_NIL) {
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
    result = lbm_env_set_functional(args[0], args[1], args[2]);
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
          r == ENC_SYM_NO_MATCH) {
        result = ENC_SYM_NIL;
      } else {
        result = r;
      }
    } else if (lbm_is_symbol(args[0]) &&
               args[0] == ENC_SYM_NIL) {
      result = args[0]; /* nil */
    } /* else error */
  }
  return result;
}

static lbm_value fundamental_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 2 && IS_NUMBER(args[1])) {
    if (lbm_is_list(args[0])) {
      result = lbm_index_list(args[0], lbm_dec_as_i32(args[1]));
    } else if (lbm_is_lisp_array_r(args[0])) {
      lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(args[0]);
      lbm_value *arrdata = (lbm_value*)header->data;
      lbm_uint size = header->size / sizeof(lbm_value);
      lbm_uint index = lbm_dec_as_u32(args[1]);
      if (index < size) {
        result = arrdata[index];
      }  // index out of range will be eval error.
    }
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
#ifdef LBM64
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) & lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) & lbm_dec_as_u64(args[1])); break;
#else
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) & lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) & lbm_dec_as_u32(args[1])); break;
#endif
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
#ifdef LBM64
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) | lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) | lbm_dec_as_u64(args[1])); break;
#else
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) | lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) | lbm_dec_as_u32(args[1])); break;
#endif
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
#ifdef LBM64
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) ^ lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) ^ lbm_dec_as_u64(args[1])); break;
#else
    case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) ^ lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) ^ lbm_dec_as_u32(args[1])); break;
#endif
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
  lbm_value res = ENC_SYM_EERROR;
  if (nargs == 1) {
    lbm_value val = args[0];
    lbm_type t = lbm_type_of(val);

    if (lbm_is_ptr(val)) {
      // Ignore constant or not constant.
      t &= LBM_PTR_TO_CONSTANT_MASK;
    }
    switch(t) {
    case LBM_TYPE_CONS: res = ENC_SYM_TYPE_LIST; break;
    case LBM_TYPE_ARRAY: res = ENC_SYM_TYPE_ARRAY; break;
    case LBM_TYPE_I32: res = ENC_SYM_TYPE_I32; break;
    case LBM_TYPE_U32: res = ENC_SYM_TYPE_U32; break;
    case LBM_TYPE_FLOAT: res = ENC_SYM_TYPE_FLOAT; break;
    case LBM_TYPE_I64: res = ENC_SYM_TYPE_I64; break;
    case LBM_TYPE_U64: res = ENC_SYM_TYPE_U64; break;
    case LBM_TYPE_DOUBLE: res = ENC_SYM_TYPE_DOUBLE; break;
    case LBM_TYPE_I: res = ENC_SYM_TYPE_I; break;
    case LBM_TYPE_U: res = ENC_SYM_TYPE_U; break;
    case LBM_TYPE_CHAR: res = ENC_SYM_TYPE_CHAR; break;
    case LBM_TYPE_SYMBOL: res = ENC_SYM_TYPE_SYMBOL; break;
    case LBM_TYPE_LISPARRAY: res = ENC_SYM_TYPE_LISPARRAY; break;
    case LBM_TYPE_DEFRAG_MEM: res = ENC_SYM_TYPE_DEFRAG_MEM; break;
    case LBM_TYPE_CUSTOM: res = ENC_SYM_TYPE_CUSTOM; break;
    }
  }
  return res;
}

static lbm_value fundamental_list_length(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = ENC_SYM_TERROR;
    if (lbm_is_list(args[0])) {
      int32_t len = (int32_t)lbm_list_length(args[0]);
      result = lbm_enc_i(len);
    } else if (lbm_is_array_r(args[0])) {
      lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(args[0]);
      result = lbm_enc_i((int)(header->size));
    } else if (lbm_is_lisp_array_r(args[0])) {
      lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(args[0]);
      result = lbm_enc_i((int)(header->size / (sizeof(lbm_uint))));
    }
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
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 1 && IS_NUMBER(args[0])) {
    lbm_set_event_handler_pid((lbm_cid)lbm_dec_i(args[0]));
    res = ENC_SYM_TRUE;
  }
  return res;
}

static lbm_value fundamental_take(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 2 && IS_NUMBER(args[1]) && lbm_is_list(args[0])) {
    int len = lbm_dec_as_i32(args[1]);
    res = lbm_list_copy(&len, args[0]);
  }
  return res;
}

static lbm_value fundamental_drop(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 2 && IS_NUMBER(args[1]) && lbm_is_list(args[0])) {
    res = lbm_list_drop(lbm_dec_as_u32(args[1]), args[0]);
  }
  return res;
}
/* (mkarray size) */
static lbm_value fundamental_mkarray(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 1 && IS_NUMBER(args[0])) {
    lbm_heap_allocate_lisp_array(&res, lbm_dec_as_u32(args[0]));
  }
  return res;
}

static lbm_value fundamental_array_to_list(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 1 && lbm_is_lisp_array_r(args[0])) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(args[0]);
    lbm_value *arrdata = (lbm_value*)header->data;
    lbm_uint size = (header->size / sizeof(lbm_uint));
    res = lbm_heap_allocate_list(size);
    if (lbm_is_symbol(res)) return res;
    lbm_value curr = res;
    lbm_uint ix = 0;
    while (lbm_is_cons(curr)) {
      lbm_set_car(curr, arrdata[ix]);
      ix ++;
      curr = lbm_cdr(curr);
    }
  }
  return res;
}

static lbm_value fundamental_list_to_array(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 1 && lbm_is_list(args[0])) {
    lbm_int len = (lbm_int)lbm_list_length(args[0]);
    if ( len > 0 ) {
      lbm_heap_allocate_lisp_array(&res, (lbm_uint)len);
      if (lbm_is_symbol(res)) return res;
      lbm_value curr = args[0];
      int ix = 0;
      lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(res);
      lbm_value *arrdata = (lbm_value*)header->data;
      while (lbm_is_cons(curr)) {
        arrdata[ix] = lbm_car(curr);
        ix ++;
        curr = lbm_cdr(curr);
      }
    } else {
      res = ENC_SYM_NIL; // could be a unique array-empty symbol
    }
  }
  return res;
}

static lbm_value fundamental_dm_create(lbm_value *args, lbm_uint argn, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_uint n = lbm_dec_as_uint(args[0]);
    res = lbm_defrag_mem_create(n);
  }
  return res;
}

static lbm_value fundamental_dm_alloc(lbm_value *args, lbm_uint argn, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_number(args[1])) {
    if (lbm_type_of(args[0]) == LBM_TYPE_DEFRAG_MEM) {
      lbm_uint *dm = (lbm_uint*)lbm_car(args[0]);
      res = lbm_defrag_mem_alloc(dm, lbm_dec_as_uint(args[1]));
    }
  }
  return res;
}

static lbm_value fundamental_is_list(lbm_value *args, lbm_uint argn, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1) {
    res = lbm_is_list_rw(args[0]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_is_number(lbm_value *args, lbm_uint argn, eval_context_t *ctx) {
  (void) ctx;
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1) {
    res = lbm_is_number(args[0]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_int_div(lbm_value *args, lbm_uint argn, eval_context_t *ctx) {
  lbm_value res = fundamental_div(args, argn, ctx);
  switch (lbm_type_of(res)) {
    case LBM_TYPE_FLOAT: {
      res = lbm_enc_i((lbm_int)lbm_dec_float(res));
      break;
    }
    case LBM_TYPE_DOUBLE: {
      res = lbm_enc_i((lbm_int)lbm_dec_double(res));
      break;
    }
  }

  return res;
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
   fundamental_mkarray,
   fundamental_array_to_list,
   fundamental_list_to_array,
   fundamental_dm_create,
   fundamental_dm_alloc,
   fundamental_is_list,
   fundamental_is_number,
   fundamental_int_div,
  };
