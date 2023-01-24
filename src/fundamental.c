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
#include "print.h"
#include "lbm_variables.h"
#include "env.h"
#include "lbm_utils.h"
#include "lbm_custom_type.h"
#include "lbm_constants.h"

#include <stdio.h>
#include <math.h>

static lbm_uint add2(lbm_uint a, lbm_uint b) {

  lbm_uint retval = ENC_SYM_TERROR;

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

  lbm_uint retval = ENC_SYM_TERROR;

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

  lbm_uint retval = ENC_SYM_TERROR;

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
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

  if (!(lbm_is_number(a) && lbm_is_number(b))) {
    return retval;
  }

  lbm_uint t = (lbm_type_of(a) < lbm_type_of(b)) ? lbm_type_of(b) : lbm_type_of(a);
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

  lbm_uint retval = ENC_SYM_TERROR;

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

    if (a_ == NULL || b_ == NULL) return false; // Not possible to properly report error from here.

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

bool struct_eq(lbm_value a, lbm_value b) {

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

static void array_read(lbm_value *args, lbm_uint nargs, lbm_uint *result) {
  (void) nargs;
  if (nargs < 2) return;
  // Args are: array, index
  lbm_value arr = args[0];
  lbm_value index = args[1];
  lbm_value index_end = index;
  lbm_value acc = ENC_SYM_NIL;
  lbm_value curr = ENC_SYM_EERROR;
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
        *result = ENC_SYM_NIL;
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
        curr = ENC_SYM_EERROR;
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

static void array_write(lbm_value *args, lbm_uint nargs, lbm_uint *result) {
  (void) nargs;
  lbm_value arr = args[0];
  lbm_value index = args[1];
  lbm_value val = args[2];
  lbm_uint ix;

  *result = ENC_SYM_EERROR;

  if (lbm_is_number(index)) {
    ix = lbm_dec_as_u32(index);
  } else {
    return;
  }

  if (lbm_type_of(arr) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(arr);
    if (array == NULL) {
      *result = ENC_SYM_FATAL_ERROR;
      return;
    }

    if (ix >= array->size) {
      *result =  ENC_SYM_NIL;
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
static void array_create(lbm_value *args, lbm_uint nargs, lbm_value *result) {
  *result = ENC_SYM_EERROR;
  uint32_t n;
  lbm_uint t_sym;

  if (nargs == 1 && lbm_is_number(args[0])) {
    n = lbm_dec_as_u32(args[0]);
    t_sym = SYM_TYPE_CHAR;
  } else if (nargs == 2 &&
             lbm_type_of(args[0]) == LBM_TYPE_SYMBOL &&
             lbm_is_number(args[1])) {
    n = lbm_dec_as_u32(args[1]);
    t_sym = lbm_dec_sym(args[0]);
  } else {
    *result = ENC_SYM_TERROR;
    return;
  }

  if (n > 0) {
    switch(t_sym) {
    case SYM_TYPE_CHAR: /* fall through */
    case SYM_TYPE_BYTE:
      lbm_heap_allocate_array(result, n, LBM_TYPE_BYTE);
      break;
    case SYM_TYPE_I32:
      lbm_heap_allocate_array(result, n, LBM_TYPE_I32);
      break;
    case SYM_TYPE_U32:
      lbm_heap_allocate_array(result, n, LBM_TYPE_U32);
      break;
    case SYM_TYPE_FLOAT:
      lbm_heap_allocate_array(result, n, LBM_TYPE_FLOAT);
      break;
    case SYM_TYPE_I64:
      lbm_heap_allocate_array(result, n, LBM_TYPE_I64);
      break;
    case SYM_TYPE_U64:
      lbm_heap_allocate_array(result, n, LBM_TYPE_U64);
      break;
    case SYM_TYPE_DOUBLE:
      lbm_heap_allocate_array(result, n, LBM_TYPE_DOUBLE);
      break;
    default:
      break;
    }
  } else {
    lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
  }
}

static int elt_size(lbm_type t) {
  switch(t) {
  case LBM_TYPE_BYTE:
    return 1;
  case LBM_TYPE_U32:  /* fall through */
  case LBM_TYPE_I32:
  case LBM_TYPE_FLOAT:
    return 4;
  case LBM_TYPE_U64:  /* fall through */
  case LBM_TYPE_I64:
  case LBM_TYPE_DOUBLE:
    return 8;
  default:
    return -1;
  }
}

static lbm_value index_list(lbm_value l, int32_t n) {
  lbm_value curr = l;

  if (n < 0) {
    int32_t len = (int32_t)lbm_list_length(l);
    n = len + n;
    if (n < 0) return ENC_SYM_NIL;
  }

  while ( lbm_type_of(curr) == LBM_TYPE_CONS &&
          n > 0) {
    curr = lbm_cdr(curr);
    n --;
  }
  if (lbm_type_of(curr) == LBM_TYPE_CONS) {
    return lbm_car(curr);
  } else {
    return ENC_SYM_NIL;
  }
}

static lbm_value assoc_lookup(lbm_value key, lbm_value assoc) {
  lbm_value curr = assoc;
  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (struct_eq(lbm_ref_cell(c)->car, key)) {
      return lbm_ref_cell(c)->cdr;
    }
    curr = lbm_ref_cell(curr)->cdr;
  }
  return ENC_SYM_NO_MATCH;
}

static lbm_value cossa_lookup(lbm_value key, lbm_value assoc) {
  lbm_value curr = assoc;
  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value c = lbm_ref_cell(curr)->car;
    if (struct_eq(lbm_ref_cell(c)->cdr, key)) {
      return lbm_ref_cell(c)->car;
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

  if (!lbm_is_number(a)) {
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) {
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

  if (!lbm_is_number(a)) {
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) {
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
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_gt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!lbm_is_number(a)) {
    return ENC_SYM_TERROR;
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) {
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
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_leq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!lbm_is_number(a)) {
    return ENC_SYM_TERROR;
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
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_geq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  bool ok = true;

  if (!lbm_is_number(a)) {
    return ENC_SYM_TERROR;
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
      return ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  return ENC_SYM_TERROR;
}

static lbm_value fundamental_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;

  if (nargs == 0) {
    return ENC_SYM_NIL;
  }
  lbm_uint a = args[0];
  if (lbm_type_of(a) == LBM_TYPE_SYMBOL &&
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

  if (nargs == 1 && lbm_is_number(args[0])) {
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
    if (lbm_type_of(args[0]) == LBM_TYPE_CONS) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      return cell->car;
    }
  }
  return ENC_SYM_NIL;
}

static lbm_value fundamental_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs == 1) {
    if (lbm_type_of(args[0]) == LBM_TYPE_CONS) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      return cell->cdr;
    }
  }
  return ENC_SYM_NIL;
}

static lbm_value fundamental_list(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = lbm_heap_allocate_list(nargs);
  if (lbm_is_cons(result)) {
    lbm_value curr = result;
    for (lbm_uint i = 0; i < nargs; i ++) {
      lbm_set_car(curr, args[i]);
      curr = lbm_cdr(curr);
    }
  }
  return result;
}

static lbm_value fundamental_append(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs < 2) return ENC_SYM_TERROR;

  lbm_value res = args[nargs-1];

  for (int i = (int)nargs -2; i >= 0; i --) {

    lbm_value curr = args[i];
    int n = 0;
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      n++;
      curr = lbm_cdr(curr);
    }

    curr = args[i];

    bool err = false;
    for (int j = n-1; j >= 0; j --) {
      res = lbm_cons(index_list(curr,j), res);
      if (lbm_is_symbol(res)) {
        err = true;
        break;
      }
    }
    if(err) break;
  }

  return res;
}

static lbm_value fundamental_undefine(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value env = lbm_get_env();
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1 && lbm_is_symbol(args[0])) {
    result = lbm_env_drop_binding(env, args[0]);
    *lbm_get_env_ptr() = result;
  } else if (nargs == 1 && lbm_is_cons(args[0])) {
    lbm_value curr = args[0];
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      lbm_value key = lbm_car(curr);
      result = lbm_env_drop_binding(env, key);
      curr = lbm_cdr(curr);
    }
    *lbm_get_env_ptr() = result;
  }
  return result;
}

static lbm_value fundamental_array_read(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  array_read(args, nargs, &result);
  return result;
}

static lbm_value fundamental_array_write(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  array_write(args, nargs, &result);
  return result;
}

static lbm_value fundamental_array_create(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  array_create(args, nargs, &result);
  return result;
}

static lbm_value fundamental_array_size(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs != 1) return result;

  if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(args[0]);
    if (array == NULL) {
      result = ENC_SYM_FATAL_ERROR;
      return result;
    }
    result =  lbm_enc_u(array->size);
  }
  return result;
}

static lbm_value fundamental_array_clear(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  lbm_value result = ENC_SYM_EERROR;
  if (nargs != 1) return ENC_SYM_EERROR;

  if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(args[0]);
    if (array == NULL) {
      return ENC_SYM_FATAL_ERROR;
    }
    int es = elt_size(array->elt_type);

    if (es < 0) return result;

    memset(array->data, 0, array->size );
    result = args[0];
  }
  return result;
}

static lbm_value fundamental_symbol_to_string(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs < 1 ||
      lbm_type_of(args[0]) != LBM_TYPE_SYMBOL)
    return ENC_SYM_NIL;
  lbm_value sym = args[0];
  const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(sym));
  if (sym_str == NULL) return ENC_SYM_NIL;
  size_t len = strlen(sym_str);

  lbm_value v;
  if (lbm_heap_allocate_array(&v, len+1, LBM_TYPE_CHAR)) {
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
      lbm_type_of(args[0] != LBM_TYPE_ARRAY))
    return result;
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
  if (!arr) return ENC_SYM_FATAL_ERROR;
  if (arr->elt_type != LBM_TYPE_CHAR)
    return result;
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
  if (lbm_type_of(s) == LBM_TYPE_SYMBOL)
    return lbm_enc_u(lbm_dec_sym(s));
  else
    return ENC_SYM_TERROR;
}

static lbm_value fundamental_uint_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs < 1) return ENC_SYM_EERROR;
  lbm_value s = args[0];
  if (lbm_type_of(s) == LBM_TYPE_U)
    return lbm_enc_sym(lbm_dec_u(s));
  else
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
        lbm_is_number(args[1])) {
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
  if (nargs == 2 && lbm_is_number(args[1])) {
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
    if (!(lbm_is_number(args[0]) && lbm_is_number(args[1]))) {
      return retval;
    }
    switch (lbm_type_of(args[0])) {
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
    if (!(lbm_is_number(args[0]) && lbm_is_number(args[1]))) {
      return retval;
    }
    switch (lbm_type_of(args[0])) {
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
    if (!(lbm_is_number(args[0]) && lbm_is_number(args[1]))) {
      return retval;
    }
    switch (lbm_type_of(args[0])) {
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
    if (!(lbm_is_number(args[0]) && lbm_is_number(args[1]))) {
      return retval;
    }
    switch (lbm_type_of(args[0])) {
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
    if (!(lbm_is_number(args[0]) && lbm_is_number(args[1]))) {
      return retval;
    }
    switch (lbm_type_of(args[0])) {
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
    if (!(lbm_is_number(args[0]))) {
      return retval;
    }
    switch (lbm_type_of(args[0])) {
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
  switch(lbm_type_of(val)) {
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

  if (nargs == 1 && lbm_is_number(args[0])) {
    start = 0;
    end = lbm_dec_as_i32(args[0]);
  } else if (nargs == 2 &&
             lbm_is_number(args[0]) &&
             lbm_is_number(args[1])) {
    start = lbm_dec_as_i32(args[0]);
    end = lbm_dec_as_i32(args[1]);
  } else {
    return result;
  }

  int num;
  if (end == start) return ENC_SYM_NIL;
  else if (end < start) {
    rev = true;
    num = start - end;
  } else {
    num = end - start;
  }

  lbm_value r_list = lbm_heap_allocate_list((unsigned int)num);
  if (lbm_is_cons(r_list)) {
    lbm_value curr = r_list;
    if (rev) {
      for (int i = start-1; i >= end; i --) {
        lbm_set_car(curr, lbm_enc_i(i));
        curr = lbm_cdr(curr);
      }
    } else {
      for (int i = start; i < end; i ++) {
        lbm_set_car(curr, lbm_enc_i(i));
        curr = lbm_cdr(curr);
      }
    }
  }
  return r_list;
}

static lbm_value fundamental_reg_event_handler(lbm_value *args, lbm_uint argn, eval_context_t *ctx) {
  (void)ctx;
  if (argn != 1 || !lbm_is_number(args[0])) {
    return ENC_SYM_EERROR;
  }

  lbm_set_event_handler_pid((lbm_cid)lbm_dec_i(args[0]));
  return ENC_SYM_TRUE;
}

const fundamental_fun fundamental_table[] =
  { fundamental_add,
    fundamental_sub,
    fundamental_mul,
    fundamental_div,
    fundamental_mod,
    fundamental_eq,
    fundamental_numeq,
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
    fundamental_array_read,
    fundamental_array_write,
    fundamental_array_create,
    fundamental_array_size,
    fundamental_array_clear,
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
    fundamental_num_not_eq,
    fundamental_not_eq,
    fundamental_reg_event_handler
  };
