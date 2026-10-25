/*
    Copyright 2019, 2021 - 2026      Joel Svensson   svenssonjoel@yahoo.se
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
#include "print.h" // printable string?

#include <stdio.h>
#include <math.h>

#ifdef LBM_OPT_FUNDAMENTALS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_FUNDAMENTALS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif
/* Type promotion ranks

   32bit LBM:
   byte < i < u < i32 < u32 < i64 < u64 < float < double

   64bit LBM:
   byte < i32 < u32 < i < u < i64 < u64 < float < double
 */

static inline lbm_type promote_type(lbm_type a, lbm_type b) {
#ifdef LBM64
  if (a == LBM_TYPE_FLOAT && b != LBM_TYPE_DOUBLE) return LBM_TYPE_FLOAT;
  if (b == LBM_TYPE_FLOAT && a != LBM_TYPE_DOUBLE) return LBM_TYPE_FLOAT;
#endif
  return (a < b) ? b : a;
}

#define PROMOTE(t, a, b) t = promote_type(lbm_type_of_functional(a), lbm_type_of_functional(b));


#define IS_NUMBER lbm_is_number

// Todo: It may be possible perform some of these operations
//       on encoded values followed by a "correction" of the result values
//       type bits.
//       But the checks required to figure out if it is possible to apply the
//       operation in this way has a cost too...

static lbm_uint div2(lbm_uint a, lbm_uint b) {
  lbm_uint retval = ENC_SYM_TERROR;
  if (IS_NUMBER(a) && IS_NUMBER(b)) {
    lbm_type t;
    PROMOTE(t, a, b);
    switch (t) {
    case LBM_TYPE_CHAR: if (lbm_dec_char(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_char((uint8_t)(lbm_dec_char(a) / lbm_dec_char(b))); break;
#ifdef LBM64
    case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i64(a) / lbm_dec_as_i64(b)); break;
    case LBM_TYPE_U: if (lbm_dec_as_u64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u64(a) / lbm_dec_as_u64(b)); break;
#else
    case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
    case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
#endif
    case LBM_TYPE_U32: if (lbm_dec_as_u32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u32(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
    case LBM_TYPE_I32: if (lbm_dec_as_i32(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i32(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
    case LBM_TYPE_FLOAT: if (lbm_dec_as_float(b) == 0.0f || lbm_dec_as_float(b) == -0.0f) {return ENC_SYM_DIVZERO;} retval = lbm_enc_float(lbm_dec_as_float(a) / lbm_dec_as_float(b)); break;
    case LBM_TYPE_U64: if (lbm_dec_as_u64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u64(lbm_dec_as_u64(a) / lbm_dec_as_u64(b)); break;
    case LBM_TYPE_I64: if (lbm_dec_as_i64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_i64(lbm_dec_as_i64(a) / lbm_dec_as_i64(b)); break;
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
    case LBM_TYPE_U: if (lbm_dec_as_u64(b) == 0) {return ENC_SYM_DIVZERO;} retval = lbm_enc_u(lbm_dec_as_u64(a) % lbm_dec_as_u64(b)); break;
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

// a and b must be bytearrays!
static bool bytearray_equality(lbm_value a, lbm_value b) {
  lbm_array_header_t *a_ = (lbm_array_header_t*)lbm_car(a);
  lbm_array_header_t *b_ = (lbm_array_header_t*)lbm_car(b);
  bool res = false;
  // A NULL array arriving here should be impossible.
  // if the a and b are not valid arrays at this point, the data
  // is most likely nonsense - corrupted by cosmic radiation.
  if ((a_ && b_) && a_->size == b_->size) {
    res = (memcmp((char*)a_->data, (char*)b_->data, a_->size) == 0);
  }
  return res;
}

// a and b must be arrays!
static bool array_struct_equality(lbm_value a, lbm_value b, int rlevel) {
  if (rlevel >= LBM_MAX_C_RECURSION) return false;
  lbm_array_header_t *a_ = (lbm_array_header_t*)lbm_car(a);
  lbm_array_header_t *b_ = (lbm_array_header_t*)lbm_car(b);
  bool res = false;
  if ((a_ && b_) &&  a_->size == b_->size) {
    res = true;
    lbm_value *adata = (lbm_value*)a_->data;
    lbm_value *bdata = (lbm_value*)b_->data;
    lbm_uint size = (lbm_uint)a_->size / (lbm_uint)sizeof(lbm_value);
    for (lbm_uint i = 0; i < size; i ++ ) {
      res = struct_eq(adata[i], bdata[i], rlevel+1);
      if (!res) break;
    }
  }
  return res;
}

// ////////////////////////////////////////////////////////////
// Added pointer comparison before recurring on arrays and lists.
// struct_eq can exhaust stack! but the pointer check makes it
// a highly unlikely event occurring normally.
//
// To run out of C stack now you would have to
// create in memory two identical structures that are
// large and pointer-wise unique but structuraly identical.
//
// Unknowns that impact this is argument is that
// the stack depth is unknown to me (it is a result of the integrator's choices).
bool struct_eq(lbm_value a, lbm_value b, int rlevel) {
  if (rlevel >= LBM_MAX_C_RECURSION) return false;
  bool res;
 struct_eq_quickpath:
  res = false;
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
      if (a == b) { // quick path if pointers equal (pointers equal => structural equal)
        res = true;
        break;
      }
      if (struct_eq(lbm_car(a),lbm_car(b),rlevel+1)) {
        a = lbm_cdr(a);
        b = lbm_cdr(b);
        // Do not use any stack in the proper lisp case.
        goto struct_eq_quickpath;
      }
      return false;
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
      if (a == b) { // quick path if pointers equal (pointers equal => structural equal)
        res = true;
        break;
      }
      res =  array_struct_equality(a, b, rlevel+1); break;
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

static lbm_value assoc_lookup(lbm_value key, lbm_value assoc) {
  lbm_value curr = assoc;
  lbm_value res = ENC_SYM_NO_MATCH;
  while (lbm_is_cons(curr)) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (lbm_is_cons(c)) {
      if (struct_eq(lbm_ref_cell(c)->car, key,0)) {
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
      if (struct_eq(lbm_ref_cell(c)->cdr, key,0)) {
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

#define ARITH_ADD 0
#define ARITH_SUB 1
#define ARITH_MUL 2

#define ARITH_DEC(TAG, ACC, DEC, DEC_AS, v)                             \
  ((ACC)((lbm_type_of_functional(v) == (TAG)) ? DEC(v) : DEC_AS(v)))

#define ARITH_CASE(TAG, ACC, CT, DEC, DEC_AS, ENC)                      \
  case TAG: {                                                           \
    ACC s = ARITH_DEC(TAG, ACC, DEC, DEC_AS, args[0]);                  \
    switch (op) {                                                       \
    case ARITH_ADD:                                                     \
      for (lbm_uint i = 1; i < nargs; i ++)                             \
        s = (ACC)(s + ARITH_DEC(TAG, ACC, DEC, DEC_AS, args[i]));       \
      break;                                                            \
    case ARITH_SUB:                                                     \
      for (lbm_uint i = 1; i < nargs; i ++)                             \
        s = (ACC)(s - ARITH_DEC(TAG, ACC, DEC, DEC_AS, args[i]));       \
      break;                                                            \
    default:                                                            \
      for (lbm_uint i = 1; i < nargs; i ++)                             \
        s = (ACC)(s * ARITH_DEC(TAG, ACC, DEC, DEC_AS, args[i]));       \
      break;                                                            \
    }                                                                   \
    return ENC((CT)s);                                                  \
  }

static lbm_value arith_fold(lbm_value *args, lbm_uint nargs, int op) {
  lbm_type t = LBM_TYPE_CHAR;
  for (lbm_uint i = 0; i < nargs; i ++) {
    lbm_value v = args[i];
    if (!IS_NUMBER(v)) {
      lbm_set_error_suspect(v);
      return ENC_SYM_TERROR;
    }
    t = promote_type(t, lbm_type_of_functional(v));
  }

  switch (t) {
    ARITH_CASE(LBM_TYPE_CHAR, uint8_t, uint8_t, lbm_dec_char, lbm_dec_as_char, lbm_enc_char)
    ARITH_CASE(LBM_TYPE_I, lbm_uint, lbm_int, lbm_dec_i, lbm_dec_as_int, lbm_enc_i)
    ARITH_CASE(LBM_TYPE_U, lbm_uint, lbm_uint, lbm_dec_u, lbm_dec_as_uint, lbm_enc_u)
    ARITH_CASE(LBM_TYPE_I32, uint32_t, int32_t, lbm_dec_i32, lbm_dec_as_i32, lbm_enc_i32)
    ARITH_CASE(LBM_TYPE_U32, uint32_t, uint32_t, lbm_dec_u32, lbm_dec_as_u32, lbm_enc_u32)
    ARITH_CASE(LBM_TYPE_I64, uint64_t, int64_t, lbm_dec_i64, lbm_dec_as_i64, lbm_enc_i64)
    ARITH_CASE(LBM_TYPE_U64, uint64_t, uint64_t, lbm_dec_u64, lbm_dec_as_u64, lbm_enc_u64)
    ARITH_CASE(LBM_TYPE_FLOAT, float, float, lbm_dec_float, lbm_dec_as_float, lbm_enc_float)
    ARITH_CASE(LBM_TYPE_DOUBLE, double, double, lbm_dec_double, lbm_dec_as_double, lbm_enc_double)
  }
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_add(lbm_value *args, lbm_uint nargs) {
  if (nargs == 0) return lbm_enc_char(0);
  return arith_fold(args, nargs, ARITH_ADD);
}

static lbm_value fundamental_sub(lbm_value *args, lbm_uint nargs) {
  if (nargs == 0) return lbm_enc_char(0);
  if (nargs == 1) {
    lbm_value neg[2] = {lbm_enc_char(0), args[0]};
    return arith_fold(neg, 2, ARITH_SUB);
  }
  return arith_fold(args, nargs, ARITH_SUB);
}

static lbm_value fundamental_mul(lbm_value *args, lbm_uint nargs) {
  if (nargs == 0) return lbm_enc_char(1);
  return arith_fold(args, nargs, ARITH_MUL);
}

static lbm_value fundamental_div(lbm_value *args, lbm_uint nargs) {

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

static lbm_value fundamental_mod(lbm_value *args, lbm_uint nargs) {
  if (nargs == 2) {
    return mod2(args[0], args[1]);
  }
  lbm_set_error_reason((char*)lbm_error_str_num_args);
  return  ENC_SYM_EERROR;
}

static lbm_value fundamental_eq(lbm_value *args, lbm_uint nargs) {
  lbm_uint a = args[0];
  for (lbm_uint i = 1; i < nargs; i ++) {
    lbm_uint b = args[i];
    if (!struct_eq(a, b,0)) return ENC_SYM_NIL;
  }
  return ENC_SYM_TRUE;
}

static lbm_value fundamental_not_eq(lbm_value *args, lbm_uint nargs) {
  lbm_value r = fundamental_eq(args, nargs);
  return r ? ENC_SYM_NIL : ENC_SYM_TRUE; // Works because ENC_SYM_NIL == 0 and ENC_SYM_TRUE is != 0
}


static lbm_value fundamental_numeq(lbm_value *args, lbm_uint nargs) {
  if (nargs < 2) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_TERROR;
  }

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
      if (!(compare_num(a, b) == 0)) {
        res = ENC_SYM_NIL;
        break;
      }
    }
  }
  return res;
}

static lbm_value fundamental_num_not_eq(lbm_value *args, lbm_uint nargs) {
  lbm_value r = fundamental_numeq(args, nargs);
  // Needs the more expensive check as r can be ENC_SYM_TERROR.
  if (r == ENC_SYM_NIL) {
    r = ENC_SYM_TRUE;
  } else if (r == ENC_SYM_TRUE) {
    r = ENC_SYM_NIL;
  }
  return r;
}

#define CMP_LT 0x1
#define CMP_EQ 0x2
#define CMP_GT 0x4

static lbm_value compare_all(lbm_value *args, lbm_uint nargs, int mask) {
  if (nargs < 2) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_TERROR;
  }

  lbm_value a = args[0];
  bool r = true;

  if (!IS_NUMBER(a)) {
    lbm_set_error_suspect(a);
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    lbm_value b = args[i];
    if (!IS_NUMBER(b)) {
      lbm_set_error_suspect(b);
      return ENC_SYM_TERROR;
    }
    r = r && ((mask >> (compare_num(a, b) + 1)) & 1);
  }
  return r ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value fundamental_leq(lbm_value *args, lbm_uint nargs) {
  return compare_all(args, nargs, CMP_LT | CMP_EQ);
}

static lbm_value fundamental_geq(lbm_value *args, lbm_uint nargs) {
  return compare_all(args, nargs, CMP_GT | CMP_EQ);
}

static lbm_value fundamental_lt(lbm_value *args, lbm_uint nargs) {
  return compare_all(args, nargs, CMP_LT);
}

static lbm_value fundamental_gt(lbm_value *args, lbm_uint nargs) {
  return compare_all(args, nargs, CMP_GT);
}

static lbm_value fundamental_not(lbm_value *args, lbm_uint nargs) {
  if (nargs == 1) {
    return args[0] ? ENC_SYM_NIL : ENC_SYM_TRUE;
  }
  return ENC_SYM_EERROR;
}

static lbm_value fundamental_gc(lbm_value *args, lbm_uint nargs) {
  (void) args;
  (void) nargs;
  lbm_perform_gc();
  return ENC_SYM_TRUE;
}

static lbm_value fundamental_self(lbm_value *args, lbm_uint nargs) {
  (void) args;
  (void) nargs;
  // Current context cannot be NULL here as
  // fundamentals must be called from an LBM thread.
  return lbm_enc_i(lbm_get_current_context()->id);
}

static lbm_value fundamental_set_mailbox_size(lbm_value *args, lbm_uint nargs) {
  lbm_value r = ENC_SYM_EERROR;
  if (nargs == 1) {
    if (IS_NUMBER(args[0])) {
      uint32_t s = lbm_dec_as_u32(args[0]);
      // Current context cannot be NULL here as
      // fundamentals must be called from an LBM thread.
      if (lbm_mailbox_change_size(lbm_get_current_context(), s)) {
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

static lbm_value fundamental_cons(lbm_value *args, lbm_uint nargs) {
  lbm_value r = ENC_SYM_EERROR;
  if (nargs == 2) {
    lbm_uint a = args[0];
    lbm_uint b = args[1];
    r = lbm_cons(a,b);
  }
  return r;
}

static lbm_value fundamental_car(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_cdr(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_list(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_NIL;
  for (lbm_uint i = 1; i <= nargs; i ++) {
    result = lbm_cons(args[nargs-i], result);
    // This check may be a mostly useless optimisation.
    // Only triggers in case of running out of heap here.
    if (lbm_type_of(result) == LBM_TYPE_SYMBOL)
      break;
  }
  return result;
}

static lbm_value fundamental_append(lbm_value *args, lbm_uint nargs) {
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
    int len = -1;
    res = lbm_list_copy(&len, curr, res);
    if (lbm_is_symbol_merror(res)) return res;
  }
  return(res);
}

static lbm_value fundamental_undefine(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_buf_create(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1 && IS_NUMBER(args[0])) {
    lbm_heap_allocate_array(&result, lbm_dec_as_u32(args[0]));
  } else if (nargs == 2 && IS_NUMBER(args[1]) && lbm_type_of(args[0]) == LBM_TYPE_DEFRAG_MEM) {
    lbm_uint *dm = (lbm_uint*)lbm_car(args[0]);
    return lbm_defrag_mem_alloc(dm, lbm_dec_as_uint(args[1]));
  }
  return result;
}

static lbm_value fundamental_symbol_to_string(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_string_to_symbol(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = ENC_SYM_TERROR;
    lbm_array_header_t *arr = lbm_dec_array_r(args[0]);
    if (arr) {
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

static lbm_value fundamental_symbol_to_uint(lbm_value *args, lbm_uint nargs) {
  if (nargs < 1) return ENC_SYM_EERROR;
  lbm_value s = args[0];
  if (lbm_is_symbol(s))
    return lbm_enc_u(lbm_dec_sym(s));

  lbm_set_error_suspect(s);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_uint_to_symbol(lbm_value *args, lbm_uint nargs) {
  if (nargs < 1) return ENC_SYM_EERROR;
  lbm_value s = args[0];
  if (lbm_type_of_functional(s) == LBM_TYPE_U)
    return lbm_enc_sym(lbm_dec_u(s));

  lbm_set_error_suspect(s);
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_set_car(lbm_value *args, lbm_uint nargs) {
  lbm_value res = ENC_SYM_EERROR;
  if (nargs == 2) {
    res = lbm_set_car(args[0], args[1]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_set_cdr(lbm_value *args, lbm_uint nargs) {
  lbm_value res = ENC_SYM_EERROR;
  if (nargs == 2) {
    res = lbm_set_cdr(args[0],args[1]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_set_ix(lbm_value *args, lbm_uint nargs) {
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
        lbm_cons_t *curr_cell = lbm_ref_cell(curr);
        lbm_value next = curr_cell->cdr;
        if (i == ix) {
          curr_cell->car = args[2];
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
      int32_t index = lbm_dec_as_i32(args[1]);
      lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(args[0]);
      lbm_value *arrdata = (lbm_value*)header->data;
      lbm_uint size = header->size / sizeof(lbm_value);
      if (index < 0) index = (int32_t)size + index;
      if ((uint32_t)index < size) {
        arrdata[index] = args[2]; // value
        result = args[0];
      }  // index out of range will be eval error.
    }
  }
  return result;
}

static lbm_value fundamental_assoc(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_acons(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_TERROR;
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

// Since an a-list can be partially
// constant an additional check for read/write cell
// is needed before the actual update.
static lbm_value set_assoc(lbm_value assoc_list, lbm_value keyval) {
  lbm_value curr = assoc_list;
  lbm_value key = lbm_car(keyval);
  while (lbm_is_cons(curr)) {
    lbm_cons_t *curr_cell = lbm_ref_cell(curr);
    if (struct_eq(key, lbm_car(curr_cell->car),0)) {
      if (!lbm_ptr_is_constant(curr)) {
        curr_cell->car = keyval;
      }
      return assoc_list;
    }
    curr = curr_cell->cdr;
  }
  // Assoc-list does not contain key.
  // Note: Memory errors are implicitly handled by the caller.
  return lbm_cons(keyval, assoc_list);
}

static lbm_value fundamental_set_assoc(lbm_value *args, lbm_uint nargs) {
  lbm_value keyval;
  if (nargs == 3) {
    keyval = lbm_cons(args[1], args[2]);
    // Check if ENC_SYM_MERROR.
    if (lbm_is_symbol(keyval)) return keyval;
  } else if (nargs == 2 && lbm_is_cons(args[1])) {
    keyval = args[1];
  } else return ENC_SYM_TERROR;
  return set_assoc(args[0], keyval);
}

static lbm_value fundamental_cossa(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_ix(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 2 && IS_NUMBER(args[1])) {
    result = ENC_SYM_NIL;
    if (lbm_is_list(args[0])) {
      result = lbm_index_list(args[0], lbm_dec_as_i32(args[1]));
    } else if (lbm_is_lisp_array_r(args[0])) {
      lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(args[0]);
      lbm_value *arrdata = (lbm_value*)header->data;
      lbm_uint size = header->size / sizeof(lbm_value);
      int32_t index = lbm_dec_as_i32(args[1]);
      if (index < 0) index = (int32_t)size + index;
      if ((uint32_t)index < size) {
        result = arrdata[index];
      }
    }
  }
  return result;
}

static lbm_value fundamental_to_i(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_i((lbm_int)lbm_dec_as_i64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_i32(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_i32(lbm_dec_as_i32(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_u(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_u((lbm_uint)lbm_dec_as_u64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_u32(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_u32(lbm_dec_as_u32(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_float(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_float(lbm_dec_as_float(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_i64(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_i64(lbm_dec_as_i64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_u64(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_u64(lbm_dec_as_u64(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_double(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_double(lbm_dec_as_double(args[0]));
  }
  return result;
}

static lbm_value fundamental_to_byte(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1) {
    result = lbm_enc_char(lbm_dec_as_char(args[0]));
  }
  return result;
}

static lbm_value fundamental_shl(lbm_value *args, lbm_uint nargs) {
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (IS_NUMBER(args[0]) && IS_NUMBER(args[1])) {
      switch (lbm_type_of_functional(args[0])) {
      case LBM_TYPE_CHAR: retval = lbm_enc_char((uint8_t)(lbm_dec_char(args[0]) << lbm_dec_as_u32(args[1]))); break;
      case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) << lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) << lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(args[0]) << lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(args[0]) << lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(args[0]) << lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(args[0]) << lbm_dec_as_u32(args[1])); break;
      }
    }
  }
  return retval;
}

static lbm_value fundamental_shr(lbm_value *args, lbm_uint nargs) {
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (IS_NUMBER(args[0]) && IS_NUMBER(args[1])) {
      switch (lbm_type_of_functional(args[0])) {
      case LBM_TYPE_CHAR: retval = lbm_enc_char(lbm_dec_char(args[0]) >> lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(args[0]) >> lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(args[0]) >> lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(args[0]) >> lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(args[0]) >> lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(args[0]) >> lbm_dec_as_u32(args[1])); break;
      case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(args[0]) >> lbm_dec_as_u32(args[1])); break;
      }
    }
  }
  return retval;
}

static lbm_value fundamental_bitwise_and(lbm_value *args, lbm_uint nargs) {
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (IS_NUMBER(args[0]) && IS_NUMBER(args[1])) {
      switch (lbm_type_of_functional(args[0])) {
      case LBM_TYPE_CHAR: retval = lbm_enc_char(lbm_dec_char(args[0]) & lbm_dec_as_char(args[1])); break;
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
  }
  return retval;
}

static lbm_value fundamental_bitwise_or(lbm_value *args, lbm_uint nargs) {
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (IS_NUMBER(args[0]) && IS_NUMBER(args[1])) {
      switch (lbm_type_of_functional(args[0])) {
      case LBM_TYPE_CHAR: retval = lbm_enc_char(lbm_dec_char(args[0]) | lbm_dec_as_char(args[1])); break;
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
  }
  return retval;
}

static lbm_value fundamental_bitwise_xor(lbm_value *args, lbm_uint nargs) {
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 2) {
    retval = ENC_SYM_TERROR;
    if (IS_NUMBER(args[0]) && IS_NUMBER(args[1])) {
      switch (lbm_type_of_functional(args[0])) {
      case LBM_TYPE_CHAR: retval = lbm_enc_char(lbm_dec_char(args[0]) ^ lbm_dec_as_char(args[1])); break;
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
  }
  return retval;
}

static lbm_value fundamental_bitwise_not(lbm_value *args, lbm_uint nargs) {
  lbm_value retval = ENC_SYM_EERROR;
  if (nargs == 1) {
    retval = ENC_SYM_TERROR;
    if (IS_NUMBER(args[0])) {
      switch (lbm_type_of_functional(args[0])) {
      case LBM_TYPE_CHAR: retval = lbm_enc_char((uint8_t)(~lbm_dec_char(args[0]))); break;
      case LBM_TYPE_I: retval = lbm_enc_i(~lbm_dec_i(args[0])); break;
      case LBM_TYPE_U: retval = lbm_enc_u(~lbm_dec_u(args[0])); break;
      case LBM_TYPE_U32: retval = lbm_enc_u32(~lbm_dec_u32(args[0])); break;
      case LBM_TYPE_I32: retval = lbm_enc_i32(~lbm_dec_i32(args[0])); break;
      case LBM_TYPE_I64: retval = lbm_enc_i64(~lbm_dec_i64(args[0])); break;
      case LBM_TYPE_U64: retval = lbm_enc_u64(~lbm_dec_u64(args[0])); break;
      }
    }
  }
  return retval;
}

static lbm_value fundamental_custom_destruct(lbm_value *args, lbm_uint nargs) {
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1 && (lbm_type_of(args[0]) == LBM_TYPE_CUSTOM)) {
    lbm_uint *mem_ptr = (lbm_uint*)lbm_dec_custom(args[0]);
    if(!mem_ptr) return ENC_SYM_FATAL_ERROR;
    lbm_custom_type_destroy(mem_ptr);
    lbm_value tmp = lbm_set_ptr_type(args[0], LBM_TYPE_CONS);
    lbm_set_car(tmp, ENC_SYM_NIL);
    lbm_set_cdr(tmp, ENC_SYM_NIL);
    // The original value will still be of type custom_ptr
    result = ENC_SYM_TRUE;
  }
  return result;
}

static lbm_value fundamental_type_of(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_list_length(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_range(lbm_value *args, lbm_uint nargs) {
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

static lbm_value fundamental_reg_event_handler(lbm_value *args, lbm_uint nargs) {
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 1 && IS_NUMBER(args[0])) {
    lbm_set_event_handler_pid((lbm_cid)lbm_dec_i(args[0]));
    res = ENC_SYM_TRUE;
  }
  return res;
}

static lbm_value fundamental_take(lbm_value *args, lbm_uint nargs) {
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 2 && IS_NUMBER(args[1]) && lbm_is_list(args[0])) {
    int len = lbm_dec_as_i32(args[1]);
    res = lbm_list_copy(&len, args[0], ENC_SYM_NIL);
  }
  return res;
}

static lbm_value fundamental_drop(lbm_value *args, lbm_uint nargs) {
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 2 && IS_NUMBER(args[1]) && lbm_is_list(args[0])) {
    res = lbm_list_drop(lbm_dec_as_u32(args[1]), args[0]);
  }
  return res;
}
/* (mkarray size) */
static lbm_value fundamental_mkarray(lbm_value *args, lbm_uint nargs) {
  lbm_value res = ENC_SYM_TERROR;
  if (nargs == 1 && IS_NUMBER(args[0])) {
    lbm_heap_allocate_lisp_array(&res, lbm_dec_as_u32(args[0]));
  }
  // No high-level arrays in defrag mem until we figure out how to do it without overhead.
  //else if (nargs == 2 && IS_NUMBER(args[1]) && lbm_type_of(args[0]) == LBM_TYPE_DEFRAG_MEM) {
  //  lbm_uint *dm = (lbm_uint*)lbm_car(args[0]);
  //  res = lbm_defrag_mem_alloc_lisparray(dm, lbm_dec_as_u32(args[1]));
  //}
  return res;
}

// Create an array in a similar way to how list creates a list.
static lbm_value fundamental_array(lbm_value *args, lbm_uint nargs) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_heap_allocate_lisp_array(&res, nargs);
  if (!lbm_is_symbol_merror(res)) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(res);
    lbm_value *arrdata = (lbm_value*)header->data;
    for (lbm_uint i = 0; i < nargs; i ++) {
      arrdata[i] = args[i];
    }
  }
  return res;
}

static lbm_value fundamental_dm_create(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_number(args[0])) {
    lbm_uint n = lbm_dec_as_uint(args[0]);
    res = lbm_defrag_mem_create(n);
  }
  return res;
}

static lbm_value fundamental_dm_alloc(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_number(args[1])) {
    if (lbm_type_of(args[0]) == LBM_TYPE_DEFRAG_MEM) {
      lbm_uint *dm = (lbm_uint*)lbm_car(args[0]);
      res = lbm_defrag_mem_alloc(dm, lbm_dec_as_uint(args[1]));
    }
  }
  // NO high level arrays in Defrag mem until we can do it without overhead in the DM representation!
  //else if (argn == 3 && lbm_is_number(args[1]) && args[2] == ENC_SYM_TYPE_LISPARRAY)  {
  //  if (lbm_type_of(args[0]) == LBM_TYPE_DEFRAG_MEM) {
  //    lbm_uint *dm = (lbm_uint*)lbm_car(args[0]);
  //    res = lbm_defrag_mem_alloc_lisparray(dm, lbm_dec_as_uint(args[1]));
  //  }
  //}
  return res;
}

static lbm_value fundamental_is_list(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1) {
    res = lbm_is_list(args[0]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_is_number(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1) {
    res = lbm_is_number(args[0]) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

static lbm_value fundamental_int_div(lbm_value *args, lbm_uint argn) {
  lbm_value res = fundamental_div(args, argn);
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

static lbm_value fundamental_identity(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1) {
    res = args[0];
  }
  return res;
}

static lbm_value fundamental_is_string(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1) {
    char *str;
    res = lbm_value_is_printable_string(args[0], &str) ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  return res;
}

// Check if a value is a constant (stored in flash)
// Only half true for some shared arrays.. maybe rethink that.
// constant? is true for constant pointers.
// atoms could be considered constant in general but are not by constant?
static lbm_value fundamental_is_constant(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1) {
    lbm_type t = lbm_type_of(args[0]);
    return (((args[0] & LBM_PTR_BIT) && (t & LBM_PTR_TO_CONSTANT_BIT)) ? ENC_SYM_TRUE : ENC_SYM_NIL);
  }
  return res;
}

static lbm_value fundamental_member(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 && lbm_is_list(args[1])) {
    res = ENC_SYM_NIL;
    lbm_value curr = args[1];

    while (lbm_is_cons(curr)) {
      lbm_cons_t *cell = lbm_ref_cell(curr);
      if (struct_eq(cell->car, args[0],0)) {
#ifdef LBM_USE_MEMBER_TAIL
        res = curr;
#else
        res = args[1];
#endif
        break;
      }
      curr = cell->cdr;
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
   fundamental_dm_create,
   fundamental_dm_alloc,
   fundamental_is_list,
   fundamental_is_number,
   fundamental_int_div,
   fundamental_identity,
   fundamental_array,
   fundamental_is_string,
   fundamental_is_constant,
   fundamental_member
  };
