/*
    Copyright 2019, 2021, 2022 Joel Svensson   svenssonjoel@yahoo.se
                          2022 Benjamin Vedder

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
#include "print.h"
#include "lbm_variables.h"
#include "env.h"
#include "lbm_utils.h"

#include <stdio.h>
#include <math.h>

static lbm_uint shl(lbm_uint a, lbm_uint b) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  switch (lbm_type_of(a)) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(a) << lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(a) << lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(a) << lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(a) << lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(a) << lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(a) << lbm_dec_as_u32(b)); break;
  }
  return retval;
}

static lbm_uint shr(lbm_uint a, lbm_uint b) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  switch (lbm_type_of(a)) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(a) >> lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(a) >> lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(a) >> lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(a) >> lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(a) >> lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(a) >> lbm_dec_as_u32(b)); break;
  }
  return retval;
}

static lbm_uint bitwise_and(lbm_uint a, lbm_uint b) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  switch (lbm_type_of(a)) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(a) & lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(a) & lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(a) & lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(a) & lbm_dec_as_i32(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(a) & lbm_dec_as_i64(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(a) & lbm_dec_as_u64(b)); break;
  }
  return retval;
}

static lbm_uint bitwise_or(lbm_uint a, lbm_uint b) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  switch (lbm_type_of(a)) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(a) | lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(a) | lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(a) | lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(a) | lbm_dec_as_i32(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(a) | lbm_dec_as_i64(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(a) | lbm_dec_as_u64(b)); break;
  }
  return retval;
}

static lbm_uint bitwise_xor(lbm_uint a, lbm_uint b) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  switch (lbm_type_of(a)) {
  case LBM_TYPE_I: retval = lbm_enc_i(lbm_dec_i(a) ^ lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(lbm_dec_u(a) ^ lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(lbm_dec_u32(a) ^ lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(lbm_dec_i32(a) ^ lbm_dec_as_i32(b)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(lbm_dec_i64(a) ^ lbm_dec_as_i64(b)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(lbm_dec_u64(a) ^ lbm_dec_as_u64(b)); break;
  }
  return retval;
}

static lbm_uint bitwise_not(lbm_uint a) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a))) {
    return retval;
  }

  switch (lbm_type_of(a)) {
  case LBM_TYPE_I: retval = lbm_enc_i(~lbm_dec_i(a)); break;
  case LBM_TYPE_U: retval = lbm_enc_u(~lbm_dec_u(a)); break;
  case LBM_TYPE_U32: retval = lbm_enc_u32(~lbm_dec_u32(a)); break;
  case LBM_TYPE_I32: retval = lbm_enc_i32(~lbm_dec_i32(a)); break;
  case LBM_TYPE_I64: retval = lbm_enc_i64(~lbm_dec_i64(a)); break;
  case LBM_TYPE_U64: retval = lbm_enc_u64(~lbm_dec_u64(a)); break;
  }
  return retval;
}


static lbm_uint add2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
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

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
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

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
  switch (t) {
  case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_i(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_u(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: if (lbm_dec_as_u32(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_u32(lbm_dec_as_u32(a) / lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: if (lbm_dec_as_i32(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_i32(lbm_dec_as_i32(a) / lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: if (lbm_dec_as_float(b) == 0.0f || lbm_dec_as_float(b) == -0.0f) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_float(lbm_dec_as_float(a) / lbm_dec_as_float(b)); break;
  case LBM_TYPE_U64: if (lbm_dec_as_u64(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_u64(lbm_dec_as_u32(a) / lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: if (lbm_dec_as_i64(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_i64(lbm_dec_as_i32(a) / lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: if (lbm_dec_as_double(b) == (double)0.0 || lbm_dec_as_double(b) == (double)-0.0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_double(lbm_dec_as_double(a) / lbm_dec_as_double(b)); break;
  }
  return retval;
}

static lbm_uint mod2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
  switch (t) {
  case LBM_TYPE_I: if (lbm_dec_i(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_i(lbm_dec_as_i32(a) % lbm_dec_as_i32(b)); break;
  case LBM_TYPE_U: if (lbm_dec_as_u32(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_u(lbm_dec_as_u32(a) % lbm_dec_as_u32(b)); break;
  case LBM_TYPE_U32: if (lbm_dec_as_u32(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_u32(lbm_dec_as_u32(a) % lbm_dec_as_u32(b)); break;
  case LBM_TYPE_I32: if (lbm_dec_as_i32(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_i32(lbm_dec_as_i32(a) % lbm_dec_as_i32(b)); break;
  case LBM_TYPE_FLOAT: if (lbm_dec_as_float(b) == 0.0f || lbm_dec_as_float(b) == -0.0f) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_float(fmodf(lbm_dec_as_float(a), lbm_dec_as_float(b))); break;
  case LBM_TYPE_U64: if (lbm_dec_as_u64(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_u64(lbm_dec_as_u64(a) % lbm_dec_as_u64(b)); break;
  case LBM_TYPE_I64: if (lbm_dec_as_i64(b) == 0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_i64(lbm_dec_as_i64(a) % lbm_dec_as_i64(b)); break;
  case LBM_TYPE_DOUBLE: if (lbm_dec_as_double(b) == (double)0.0 || lbm_dec_as_double(b) == (double)-0.0) {return lbm_enc_sym(SYM_DIVZERO);} retval = lbm_enc_double(fmod(lbm_dec_as_double(a), lbm_dec_as_double(b))); break;
  }
  return retval;
}

static lbm_uint negate(lbm_uint a) {

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (lbm_type_of(a) > LBM_TYPE_CHAR) {
    switch (lbm_type_of(a)) {
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

  lbm_uint retval = lbm_enc_sym(SYM_TERROR);

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
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
  if (lbm_type_of(a) == LBM_TYPE_ARRAY &&
      lbm_type_of(a) == lbm_type_of(b)) {
    lbm_array_header_t *a_ = (lbm_array_header_t*)lbm_car(a);
    lbm_array_header_t *b_ = (lbm_array_header_t*)lbm_car(b);

    if (a_->elt_type == b_->elt_type &&
        a_->size == b_->size) {
      switch(a_->elt_type) {
      case LBM_TYPE_U:
      case LBM_TYPE_U32:
      case LBM_TYPE_I:
      case LBM_TYPE_I32:
      case LBM_TYPE_FLOAT:
        if (memcmp((char*)a_->data, (char*)b_->data, a_->size * sizeof(lbm_int)) == 0) return true;
        break;
      case LBM_TYPE_CHAR:
        if (memcmp((char*)a_->data, (char*)b_->data, a_->size) == 0) return true;
        break;
      default:
        break;
      }
    }
  }
  return false;
}

static bool struct_eq(lbm_value a, lbm_value b) {

  bool res = false;

  if (lbm_type_of(a) == lbm_type_of(b)) {
    switch(lbm_type_of(a)){
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

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval; // result is nonsense if arguments are not numbers.
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
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

void array_read(lbm_value *args, lbm_uint nargs, lbm_uint *result) {
  (void) nargs;
  if (nargs < 2) return;
  // Args are: array, index
  lbm_value arr = args[0];
  lbm_value index = args[1];
  lbm_value index_end = index;
  lbm_value acc = lbm_enc_sym(SYM_NIL);
  lbm_value curr = lbm_enc_sym(SYM_EERROR);
  bool read_many = false;

  if (nargs > 2) {
    index_end = args[2];
    read_many = true;
  }

  // Get array index
  lbm_uint ix;
  lbm_uint ix_end;

  if (lbm_is_number(index) && lbm_is_number(index_end)) {
    ix = lbm_dec_as_u32(index);
    ix_end = lbm_dec_as_u32(index_end);
  } else {
    return;
  }

  if (ix > ix_end) {
    lbm_uint tmp = ix;
    ix = ix_end;
    ix_end = tmp;
  }

  if (lbm_type_of(arr) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(arr);
    lbm_uint* data = array->data;

    for (lbm_int i = (lbm_int)ix_end; i >= (lbm_int)ix; i--) {
      if ((lbm_uint)i >= array->size){
        *result = lbm_enc_sym(SYM_NIL);
        return;
      }

      switch(array->elt_type) {
      case LBM_TYPE_CHAR:
        curr = lbm_enc_char(((char*)data)[i]);
        break;
      case LBM_TYPE_U:
        curr = lbm_enc_u((uint32_t)data[i]);
        break;
      case LBM_TYPE_I:
        curr = lbm_enc_i((int32_t)data[i]);
        break;
      case LBM_TYPE_U32:
        curr = lbm_enc_u32((uint32_t)data[i]);
        break;
      case LBM_TYPE_I32:
        curr = lbm_enc_i32((int32_t)data[i]);
        break;
      case LBM_TYPE_FLOAT: {
        float v;
        memcpy(&v, &data[i], sizeof(float));
        curr = lbm_enc_float(v);
      } break;
#ifndef LBM64
      case LBM_TYPE_U64: {
        uint64_t v = 0;
        v |= (uint64_t)data[i*2];
        v |= ((uint64_t)data[i*2+1]) << 32;
        curr = lbm_enc_u64(v);
      } break;
      case LBM_TYPE_I64: {
        uint64_t v = 0;
        v |= (uint64_t)data[i*2];
        v |= ((uint64_t)data[i*2+1]) << 32;
        curr = lbm_enc_i64((int64_t)v);
      } break;
      case LBM_TYPE_DOUBLE: {
        double v;
        memcpy(&v, &data[i*2], sizeof(double));
        curr = lbm_enc_double(v);
      } break;
#else
      case LBM_TYPE_U64:
        curr = lbm_enc_u64(data[i]);
        break;
      case LBM_TYPE_I64:
        curr = lbm_enc_i64((int64_t)data[i]);
        break;
      case LBM_TYPE_DOUBLE: {
        double v;
        memcpy(&v, &data[i], sizeof(double));
        curr = lbm_enc_double(v);
      } break;
#endif
      default:
        curr = lbm_enc_sym(SYM_EERROR);
        break;
      }
      if (read_many) {
        acc = lbm_cons(curr, acc);
      }
    } /* for i */
  }
  if (read_many) {
    *result = acc;
  } else {
    *result = curr;
  }
}

void array_write(lbm_value *args, lbm_uint nargs, lbm_uint *result) {
  (void) nargs;
  lbm_value arr = args[0];
  lbm_value index = args[1];
  lbm_value val = args[2];
  lbm_uint ix;

  *result = lbm_enc_sym(SYM_EERROR);

  if (lbm_is_number(index)) {
    ix = lbm_dec_as_u32(index);
  } else {
    return;
  }

  if (lbm_type_of(arr) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(arr);

    if (ix >= array->size) {
      *result =  lbm_enc_sym(SYM_NIL);
      return;
    }

    switch(array->elt_type) {
    case LBM_TYPE_CHAR: {
      char * data = (char *)array->data;
      data[ix] = lbm_dec_as_char(val);
      break;
    }
    case LBM_TYPE_U:
      /* fall through */
    case LBM_TYPE_U32: {
      lbm_uint *data = (lbm_uint*)array->data;
      data[ix] = lbm_dec_as_u32(val);
      break;
    }
    case LBM_TYPE_I:
      /* fall through */
    case LBM_TYPE_I32: {
      lbm_int *data = (lbm_int*)array->data;
      data[ix] = lbm_dec_as_i32(val);
      break;
    }
    case LBM_TYPE_FLOAT: {
      lbm_uint *data = (lbm_uint*)array->data;
      float v = lbm_dec_as_float(val);
      uint32_t t;
      memcpy(&t,&v,sizeof(uint32_t));
      data[ix] = t;
      break;
    }
#ifndef LBM64
    case LBM_TYPE_U64: {
      uint64_t v = lbm_dec_as_u64(val);
      lbm_uint *data = (lbm_uint*)array->data;
      data[ix*2] = (uint32_t)v;
      data[ix*2+1] = (uint32_t)(v >> 32);
      break;
    }
    case LBM_TYPE_I64: {
      int64_t v = lbm_dec_as_i64(val);
      lbm_uint *data = (lbm_uint*)array->data;
      data[ix*2] = (uint32_t)v;
      data[ix*2+1] = (uint32_t)(v >> 32);
      break;
    }
    case LBM_TYPE_DOUBLE: {
      double v = lbm_dec_as_double(val);
      uint64_t v2;
      memcpy(&v2,&v,sizeof(uint64_t));
      lbm_uint *data = (lbm_uint*)array->data;
      data[ix*2] = (uint32_t)v2;
      data[ix*2+1] = (uint32_t)(v2 >> 32);
      break;
    }
#else
    case LBM_TYPE_U64: {
      lbm_uint *data = (lbm_uint*)array->data;
      data[ix] = lbm_dec_as_u64(val);
      break;
    }
    case LBM_TYPE_I64: {
      lbm_int *data = (lbm_int*)array->data;
      data[ix] = lbm_dec_as_i64(val);
      break;
    }
    case LBM_TYPE_DOUBLE: {
      lbm_float *data = (lbm_float*)array->data;
      data[ix] = lbm_dec_as_double(val);
      break;
    }
#endif
    default:
      // Maybe result should be something else than arr here.
      break;
    }
    *result = arr;
    return;
  }
}


/* (array-create type size) */
void array_create(lbm_value *args, lbm_uint nargs, lbm_value *result) {
  *result = lbm_enc_sym(SYM_EERROR);
  if (nargs == 1 && lbm_is_number(args[0])) {
    lbm_heap_allocate_array(result, lbm_dec_as_u32(args[0]), LBM_TYPE_BYTE);
  } else if (nargs == 2) {
    if (lbm_type_of(args[0]) == LBM_TYPE_SYMBOL &&
        lbm_is_number(args[1])) {
      switch(lbm_dec_sym(args[0])) {
      case SYM_TYPE_CHAR: /* fall through */
      case SYM_TYPE_BYTE:
        lbm_heap_allocate_array(result, lbm_dec_as_u32(args[1]), LBM_TYPE_BYTE);
        break;
      case SYM_TYPE_I32:
        lbm_heap_allocate_array(result, lbm_dec_as_u32(args[1]), LBM_TYPE_I32);
        break;
      case SYM_TYPE_U32:
        lbm_heap_allocate_array(result, lbm_dec_as_u32(args[1]), LBM_TYPE_U32);
        break;
      case SYM_TYPE_FLOAT:
        lbm_heap_allocate_array(result, lbm_dec_as_u32(args[1]), LBM_TYPE_FLOAT);
        break;
      case SYM_TYPE_I64:
        lbm_heap_allocate_array(result, lbm_dec_as_u32(args[1]), LBM_TYPE_I64);
        break;
      case SYM_TYPE_U64:
        lbm_heap_allocate_array(result, lbm_dec_as_u32(args[1]), LBM_TYPE_U64);
        break;
      case SYM_TYPE_DOUBLE:
        lbm_heap_allocate_array(result, lbm_dec_as_u32(args[1]), LBM_TYPE_DOUBLE);
        break;
      default:
        break;
      }
    }
  }
}

void array_size(lbm_value *args, lbm_uint nargs, lbm_value *result) {
  *result = lbm_enc_sym(SYM_EERROR);
  if (nargs != 1) return;

  if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(args[0]);

    *result =  lbm_enc_u(array->size);
  }
  return;
}


lbm_value index_list(lbm_value l, unsigned int n) {
  lbm_value curr = l;
  while ( lbm_type_of(curr) == LBM_TYPE_CONS &&
          n > 0) {
    curr = lbm_cdr(curr);
    n --;
  }
  if (lbm_type_of(curr) == LBM_TYPE_CONS) {
    return lbm_car(curr);
  } else {
    return lbm_enc_sym(SYM_NIL);
  }
}

lbm_value assoc_lookup(lbm_value key, lbm_value assoc) {
  lbm_value curr = assoc;

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (struct_eq(lbm_ref_cell(c)->car, key)) {
      return lbm_ref_cell(c)->cdr;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return lbm_enc_sym(SYM_NOT_FOUND);
}

lbm_value lbm_fundamental(lbm_value* args, lbm_uint nargs, lbm_value op) {

  lbm_uint result = lbm_enc_sym(SYM_EERROR);
  int cmp_res = -1;

  switch (lbm_dec_sym(op)) {
  case SYM_PERFORM_GC:
    lbm_perform_gc();
    result = lbm_enc_sym(SYM_TRUE);
    break;
  case SYM_IX:
    if (nargs == 2 && lbm_is_number(args[1])) {
      result = index_list(args[0], lbm_dec_as_u32(args[1]));
    } break;
  case SYM_DECODE:
    if (nargs == 1 && (lbm_is_number(args[0]) ||
                       lbm_is_char(args[0]))) {
      switch (lbm_type_of(args[0])) {
      case LBM_TYPE_CHAR:
        /*fall through*/
      case LBM_TYPE_I:
        /* fall through */
      case LBM_TYPE_U: {
        lbm_uint v = lbm_dec_as_u32(args[0]);
        result = lbm_cons(lbm_enc_u(v & 0xFF), lbm_enc_sym(SYM_NIL));
        result = lbm_cons(lbm_enc_u(v >> 8 & 0xFF), result);
        result = lbm_cons(lbm_enc_u(v >> 16 & 0xFF), result);
        result = lbm_cons(lbm_enc_u(v >> 24 & 0xF), result);
      } break;
      case LBM_TYPE_FLOAT: {
        float tmp = (float)lbm_dec_float(args[0]);
        uint32_t  v;
        memcpy(&v, &tmp, sizeof(uint32_t));
        result = lbm_cons(lbm_enc_u(v & 0xFF), lbm_enc_sym(SYM_NIL));
        result = lbm_cons(lbm_enc_u(v >> 8 & 0xFF), result);
        result = lbm_cons(lbm_enc_u(v >> 16 & 0xFF), result);
        result = lbm_cons(lbm_enc_u(v >> 24 & 0xFF), result);
      } break;
      case LBM_TYPE_I32:
        /* fall through */
      case LBM_TYPE_U32: {
        lbm_uint v = lbm_dec_as_u32(args[0]);
        result = lbm_cons(lbm_enc_u(v & 0xFF), lbm_enc_sym(SYM_NIL));
        result = lbm_cons(lbm_enc_u(v >> 8 & 0xFF), result);
        result = lbm_cons(lbm_enc_u(v >> 16 & 0xFF), result);
        result = lbm_cons(lbm_enc_u(v >> 24 & 0xFF), result);
      } break;
      } // close if
    }break;
  /// Encode a list of up to 4 bytes as an i32
  case SYM_ENCODE_I32:
    if (nargs == 1 && lbm_type_of(args[0]) == LBM_TYPE_CONS) {
      lbm_value curr = args[0];
      uint32_t r = 0;
      int n = 4;
      while (lbm_type_of(curr) == LBM_TYPE_CONS && n > 0) {
        if (n < 4) r = r << 8;
        if (lbm_is_number(lbm_car(curr))) {
          uint32_t v = lbm_dec_as_u32(lbm_car(curr));
          r |= (0xFF & v);
          n --;
          curr = lbm_cdr(curr);
        } else {
          break;
        }
      }
      result = lbm_enc_i32((int32_t)r);
    }
    break;
  /// Encode a list of up to 4 bytes as an U32
  case SYM_ENCODE_U32:
      if (nargs == 1 && lbm_type_of(args[0]) == LBM_TYPE_CONS) {
        lbm_value curr = args[0];
        uint32_t r = 0;
        int n = 4;
        while (lbm_type_of(curr) == LBM_TYPE_CONS && n > 0) {
          if (n < 4) r = r << 8;
          if (lbm_is_number(lbm_car(curr))) {
            uint32_t v = lbm_dec_as_u32(lbm_car(curr));
            r |= (0xFF & v);
            n --;
            curr = lbm_cdr(curr);
          } else {
            break;
          }
        }
        result = lbm_enc_u32(r);
      }
      break;
  /// Encode a list of up to 4 bytes as a float
  case SYM_ENCODE_FLOAT:
    if (nargs == 1 && lbm_type_of(args[0]) == LBM_TYPE_CONS) {
      lbm_value curr = args[0];
      uint32_t r = 0;
      float f;
      int n = 4;
      while (lbm_type_of(curr) == LBM_TYPE_CONS && n > 0) {
        if (n < 4) r = r << 8;
        if (lbm_is_number(lbm_car(curr))) {
          uint32_t v = (uint32_t)lbm_dec_as_u32(lbm_car(curr));
          r |= (0xFF & v);
          n --;
          curr = lbm_cdr(curr);
        } else {
          break;
        }
      }
      memcpy(&f,&r, sizeof(float)); // float result
      result = lbm_enc_float(f);
    }
    break;
  case SYM_IS_FUNDAMENTAL:
    if (nargs < 1 ||
        lbm_type_of(args[0]) != LBM_TYPE_SYMBOL)
      result = lbm_enc_sym(SYM_NIL);
    else if (lbm_is_fundamental(args[0]))
      result = lbm_enc_sym(SYM_TRUE);
    else
      result = lbm_enc_sym(SYM_NIL);
    break;

  case SYM_SYMBOL_TO_STRING: {
    if (nargs < 1 ||
        lbm_type_of(args[0]) != LBM_TYPE_SYMBOL)
      return lbm_enc_sym(SYM_NIL);
    lbm_value sym = args[0];
    const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(sym));
    if (sym_str == NULL) return lbm_enc_sym(SYM_NIL);
    size_t len = strlen(sym_str);

    lbm_value v;
    if (lbm_heap_allocate_array(&v, len+1, LBM_TYPE_CHAR)) {
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(v);
      if (!arr) return lbm_enc_sym(SYM_MERROR);
      memset(arr->data,0,len+1);
      memcpy(arr->data,sym_str,len);
    } else {
      return lbm_enc_sym(SYM_MERROR);
    }
    result = v;
    break;
  }
  case SYM_STRING_TO_SYMBOL: {
    result = lbm_enc_sym(SYM_EERROR);
    if (nargs < 1 ||
        lbm_type_of(args[0] != LBM_TYPE_ARRAY))
      break;
    lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
    if (arr->elt_type != LBM_TYPE_CHAR)
      break;
    char *str = (char *)arr->data;
    lbm_uint sym;
    if (lbm_get_symbol_by_name(str, &sym)) {
      result = lbm_enc_sym(sym);
    } else if (lbm_add_symbol(str, &sym)) {
      result = lbm_enc_sym(sym);
    }
    break;
  }
  case SYM_SYMBOL_TO_UINT: {
    lbm_value s = args[0];
    if (lbm_type_of(s) == LBM_TYPE_SYMBOL)
      result = lbm_enc_u(lbm_dec_sym(s));
    else
      result = lbm_enc_sym(SYM_EERROR);
    break;
  }
  case SYM_UINT_TO_SYMBOL: {
    lbm_value s = args[0];
    if (lbm_type_of(s) == LBM_TYPE_U)
      result = lbm_enc_sym(lbm_dec_u(s));
    else
      result = lbm_enc_sym(SYM_EERROR);
    break;
  }
  case SYM_SET_CAR:
    if (nargs == 2) {
      if (lbm_set_car(args[0],args[1])) {
        result = lbm_enc_sym(SYM_TRUE);
      } else {
        result = lbm_enc_sym(SYM_NIL);
      }
    }
    break;
  case SYM_SET_CDR:
    if (nargs == 2) {
      if (lbm_set_cdr(args[0],args[1])) {
        result = lbm_enc_sym(SYM_TRUE);
      } else {
        result = lbm_enc_sym(SYM_NIL);
      }
    }
    break;
  case SYM_SET_IX: { // Destructive setix
    if (nargs == 3) {
      if (lbm_is_list(args[0]) &&
          lbm_is_number(args[1])) {
        lbm_value curr = args[0];
        lbm_uint i = 0;
        lbm_uint ix = lbm_dec_as_u32(args[1]);
        result = lbm_enc_sym(SYM_NIL);
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
  }break;
  case SYM_ASSOC: {
    if (nargs == 2 && lbm_is_list(args[0])) {
      lbm_value r = assoc_lookup(args[1], args[0]);
      if (lbm_is_symbol(r) &&
          lbm_dec_sym(r) == SYM_NOT_FOUND) {
        result = lbm_enc_sym(SYM_NIL);
      }
      else {
        result = r;
      }
    }
  } break;
  case SYM_ACONS: {
    if (nargs == 3) {
      lbm_value keyval = lbm_cons(args[0], args[1]);
      lbm_value new_alist = lbm_cons(keyval, args[2]);

      if (lbm_is_symbol(keyval) ||
          lbm_is_symbol(new_alist) )
        result = lbm_enc_sym(SYM_MERROR);
      else
        result = new_alist;
    } else if (nargs == 2) {
      result = lbm_cons(args[0], args[1]);
    }
  } break;
  case SYM_SET_ASSOC: {
    if (nargs == 3) {
      result = lbm_env_set(args[0], args[1], args[2]);
    } else if (nargs == 2 && lbm_is_list(args[1])) {
      lbm_value x = lbm_car(args[1]);
      lbm_value xs = lbm_cdr(args[1]);
      result = lbm_env_set(args[0], x, xs);
    }
  } break;
  case SYM_CONS: {
    lbm_uint a = args[0];
    lbm_uint b = args[1];
    result = lbm_cons(a,b);
    break;
  }
  case SYM_CAR: {
    result = lbm_car(args[0]);
    break;
  }
  case SYM_CDR: {
    result = lbm_cdr(args[0]);
    break;
  }
  case SYM_LIST: {
    result = lbm_enc_sym(SYM_NIL);
    for (lbm_uint i = 1; i <= nargs; i ++) {
      result = lbm_cons(args[nargs-i], result);
      if (lbm_type_of(result) == LBM_TYPE_SYMBOL)
        break;
    }
    break;
  }
  case SYM_APPEND: {
    if (nargs != 2) break;

    lbm_value a = args[0];
    lbm_value b = args[1];

    result = b;
    lbm_value curr = a;
    int n = 0;
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      n++;
      curr = lbm_cdr(curr);
    }

    for (int i = n-1; i >= 0; i --) {
      result = lbm_cons(index_list(a,(unsigned int)i), result);
      if (lbm_type_of(result) == LBM_TYPE_SYMBOL)
        break;
    }
    break;
  }
  case SYM_ADD: {
    lbm_uint sum = lbm_enc_u(0);
    for (lbm_uint i = 0; i < nargs; i ++) {
      sum = add2(sum, args[i]);
      if (lbm_type_of(sum) == LBM_TYPE_SYMBOL) {
        break;
      }
    }
    result = sum;
    break;
  }
  case SYM_SUB: {
    lbm_uint res = nargs == 0 ? lbm_enc_u(0) : args[0];

    if (nargs == 1) {
      res = negate(res);
    } else {
      for (lbm_uint i = 1; i < nargs; i ++) {
        res = sub2(res, args[i]);
        if (lbm_type_of(res) == LBM_TYPE_SYMBOL)
          break;
      }
    }
    result = res;
    break;
  }
  case SYM_MUL: {
    lbm_uint prod = lbm_enc_u(1);
    for (lbm_uint i = 0; i < nargs; i ++) {
      prod = mul2(prod, args[i]);
      if (lbm_type_of(prod) == LBM_TYPE_SYMBOL) {
        break;
      }
    }
    result = prod;
    break;
  }
  case SYM_DIV:  {
    if (nargs >= 1) {
      lbm_uint res = args[0];
      for (lbm_uint i = 1; i < nargs; i ++) {
        res = div2(res, args[i]);
        if (lbm_type_of(res) == LBM_TYPE_SYMBOL) {
          break;
        }
      }
      result = res;
    } else {
      result = lbm_enc_sym(SYM_EERROR);
    }
    break;
  }
  case SYM_MOD: {
    lbm_uint res = args[0];
    for (lbm_uint i = 1; i < nargs; i ++) {
      res = mod2(res, args[i]);
      if (lbm_type_of(res) == LBM_TYPE_SYMBOL) {
        break;
      }
    }
    result = res;
    break;
  }
  case SYM_EQ: {
    lbm_uint a = args[0];
    lbm_uint b;
    bool r = true;

    for (lbm_uint i = 1; i < nargs; i ++) {
      b = args[i];
      r = r && struct_eq(a, b);
    }
    if (r) {
      result = lbm_enc_sym(SYM_TRUE);
    } else {
      result = lbm_enc_sym(SYM_NIL);
    }
    break;
  }
  case SYM_NUMEQ:
    cmp_res = 0;
    /* fall through */
  case SYM_GT:
    if (lbm_dec_sym(op) == SYM_GT) cmp_res = 1;
    /* fall through */
  case SYM_LT: {
    lbm_uint a = args[0];
    lbm_uint b;
    bool r = true;
    bool ok = true;

    if (!lbm_is_number(a)) {
      result = lbm_enc_sym(SYM_TERROR);
      break;
    }
    for (lbm_uint i = 1; i < nargs; i ++) {
      b = args[i];
      if (!lbm_is_number(b)) {
        ok = false;
        break;
      }
      r = r && (compare(a, b) == cmp_res);
    }
    if (ok) {
      if (r) {
        result = lbm_enc_sym(SYM_TRUE);
      } else {
        result = lbm_enc_sym(SYM_NIL);
      }
    } else {
      result = lbm_enc_sym(SYM_TERROR);
    }
    break;
  }
  case SYM_LEQ: {
    lbm_uint a = args[0];
    lbm_uint b;
    bool r = true;
    bool ok = true;

    if (!lbm_is_number(a)) {
      result = lbm_enc_sym(SYM_TERROR);
      break;
    }
    for (lbm_uint i = 1; i < nargs; i ++) {
      b = args[i];
      if (!lbm_is_number(b)) {
        ok = false;
        break;
      }
      r = r && (compare(a, b) <= 0);
    }
    if (ok) {
      if (r) {
        result = lbm_enc_sym(SYM_TRUE);
      } else {
        result = lbm_enc_sym(SYM_NIL);
      }
    } else {
      result = lbm_enc_sym(SYM_TERROR);
    }
    break;
  }
  case SYM_GEQ: {
    lbm_uint a = args[0];
    lbm_uint b;
    bool r = true;
    bool ok = true;

    if (!lbm_is_number(a)) {
      result = lbm_enc_sym(SYM_TERROR);
      break;
    }
    for (lbm_uint i = 1; i < nargs; i ++) {
      b = args[i];
      if (!lbm_is_number(b)) {
        ok = false;
        break;
      }
      r = r && (compare(a, b) >= 0);
    }
    if (ok) {
      if (r) {
        result = lbm_enc_sym(SYM_TRUE);
      } else {
        result = lbm_enc_sym(SYM_NIL);
      }
    } else {
      result = lbm_enc_sym(SYM_TERROR);
    }
    break;
  }
  case SYM_NOT: {
    if (nargs == 0) {
      return lbm_enc_sym(SYM_NIL);
      break;
    }
    lbm_uint a = args[0];
    if (lbm_type_of(a) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(a) == SYM_NIL) {
      result = lbm_enc_sym(SYM_TRUE);
      break;
    }
    result = lbm_enc_sym(SYM_NIL);
    break;
  }
  case SYM_ARRAY_READ:
    array_read(args, nargs, &result);
    break;
  case SYM_ARRAY_WRITE:
    array_write(args, nargs, &result);
    break;
  case SYM_ARRAY_CREATE:
    array_create(args, nargs, &result);
    break;
  case SYM_ARRAY_SIZE:
    array_size(args, nargs, &result);
    break;
  case SYM_TYPE_OF:
    if (nargs != 1) return lbm_enc_sym(SYM_NIL);
    lbm_value val = args[0];
    switch(lbm_type_of(val)) {
    case LBM_TYPE_CONS: return lbm_enc_sym(SYM_TYPE_LIST);
    case LBM_TYPE_ARRAY: return lbm_enc_sym(SYM_TYPE_ARRAY);
    case LBM_TYPE_I32: return lbm_enc_sym(SYM_TYPE_I32);
    case LBM_TYPE_U32: return lbm_enc_sym(SYM_TYPE_U32);
    case LBM_TYPE_FLOAT: return lbm_enc_sym(SYM_TYPE_FLOAT);
    case LBM_TYPE_I64: return lbm_enc_sym(SYM_TYPE_I64);
    case LBM_TYPE_U64: return lbm_enc_sym(SYM_TYPE_U64);
    case LBM_TYPE_DOUBLE: return lbm_enc_sym(SYM_TYPE_DOUBLE);
    case LBM_TYPE_I: return lbm_enc_sym(SYM_TYPE_I);
    case LBM_TYPE_U: return lbm_enc_sym(SYM_TYPE_U);
    case LBM_TYPE_CHAR: return lbm_enc_sym(SYM_TYPE_CHAR);
    case LBM_TYPE_SYMBOL: return lbm_enc_sym(SYM_TYPE_SYMBOL);
    default: return lbm_enc_sym(SYM_TERROR);
    }
    break;
  case SYM_TO_I:
    if (nargs == 1) {
      result = lbm_enc_i((lbm_int)lbm_dec_as_i64(args[0]));
    }
    break;
  case SYM_TO_I32:
    if (nargs == 1) {
      result = lbm_enc_i32(lbm_dec_as_i32(args[0]));
    }
    break;
  case SYM_TO_U:
    if (nargs == 1) {
      result = lbm_enc_u((lbm_uint)lbm_dec_as_u64(args[0]));
    }
    break;
  case SYM_TO_U32:
    if (nargs == 1) {
      result = lbm_enc_u32(lbm_dec_as_u32(args[0]));
    }
    break;
  case SYM_TO_FLOAT:
    if (nargs == 1) {
      result = lbm_enc_float(lbm_dec_as_float(args[0]));
    }
    break;
  case SYM_TO_I64:
    if (nargs == 1) {
      result = lbm_enc_i64(lbm_dec_as_i64(args[0]));
    }
    break;
  case SYM_TO_U64:
    if (nargs == 1) {
      result = lbm_enc_u64(lbm_dec_as_u64(args[0]));
    }
    break;
  case SYM_TO_DOUBLE:
    if (nargs == 1) {
      result = lbm_enc_double(lbm_dec_as_double(args[0]));
    }
    break;
  case SYM_TO_BYTE:
    if (nargs == 1) {
      result = lbm_enc_char(lbm_dec_as_char(args[0]));
    }
    break;
  case SYM_SHL:
    if (nargs == 2) {
      result = shl(args[0],args[1]);
    }
    break;
  case SYM_SHR:
    if (nargs == 2) {
      result = shr(args[0],args[1]);
    }
    break;
  case SYM_BITWISE_AND:
    if (nargs == 2) {
      result = bitwise_and(args[0], args[1]);
    }
    break;
  case SYM_BITWISE_OR:
    if (nargs == 2) {
        result = bitwise_or(args[0], args[1]);
    }
    break;
  case SYM_BITWISE_XOR:
    if (nargs == 2) {
      result = bitwise_xor(args[0], args[1]);
    }
    break;
  case SYM_BITWISE_NOT:
    if (nargs == 1) {
      result = bitwise_not(args[0]);
    }
    break;
  default:
    result = lbm_enc_sym(SYM_EERROR);
    break;
  }
  return result;
}
