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

extern eval_context_t *ctx_running;
extern void error_ctx(lbm_value);
extern int lbm_perform_gc(void);

#define RETURN(X)                     \
  {ctx->r = (X);                      \
  ctx->app_cont = true;               \
  lbm_stack_drop(&ctx->K, nargs + 1); \
  return;}

#define ERROR(X)       \
  {error_ctx(X);       \
    return;}

#define WITH_GC(y, x)                           \
  {                                             \
  lbm_value gc_tmp = (x);                       \
  if (lbm_is_symbol_merror(gc_tmp)) {           \
    lbm_perform_gc();                           \
    gc_tmp = (x);                               \
    if (lbm_is_symbol_merror(gc_tmp)) {         \
      ctx_running->done = true;                 \
      error_ctx(ENC_SYM_MERROR);                \
      return;                                   \
    }                                           \
  }                                             \
  (y) = gc_tmp;                                 \
  /* continue executing statements below */     \
  }

#define WITH_GC_RMBR(y, x, n, ...)              \
  {                                             \
  lbm_value gc_tmp  = (x);                      \
  if (lbm_is_symbol_merror(gc_tmp)) {           \
    lbm_gc_mark_phase((n), __VA_ARGS__);        \
    lbm_perform_gc();                           \
    gc_tmp = (x);                               \
    if (lbm_is_symbol_merror(gc_tmp)) {         \
      ctx_running->done = true;                 \
      error_ctx(ENC_SYM_MERROR);                \
      return;                                   \
    }                                           \
  }                                             \
  (y) = gc_tmp;                                 \
  /* continue executing statements below */     \
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

typedef struct  {
  lbm_type type;
  bool is_error;
  union {
    int32_t  ival;
    uint32_t uval;
    float    fval;
    uint64_t u64val;
    int64_t  i64val;
    double   dval;
    lbm_uint err_val;
  } value;
} number_t;

typedef void (*operation)(number_t *, lbm_value);

static bool value_to_number(number_t *n, lbm_value v) {
  switch(lbm_type_of(v)) {
  case LBM_TYPE_I: n->type = LBM_TYPE_I; n->value.ival = lbm_dec_i(v); break;
  case LBM_TYPE_U: n->type = LBM_TYPE_U; n->value.uval = lbm_dec_u(v); break;
  case LBM_TYPE_U32: n->type = LBM_TYPE_U32; n->value.uval = lbm_dec_u32(v); break;
  case LBM_TYPE_I32: n->type = LBM_TYPE_I32; n->value.ival = lbm_dec_i32(v); break;
  case LBM_TYPE_FLOAT: n->type = LBM_TYPE_FLOAT; n->value.fval = lbm_dec_float(v); break;
  case LBM_TYPE_U64: n->type = LBM_TYPE_U64; n->value.u64val = lbm_dec_u64(v); break;
  case LBM_TYPE_I64: n->type = LBM_TYPE_I64; n->value.i64val = lbm_dec_i64(v); break;
  case LBM_TYPE_DOUBLE: n->type = LBM_TYPE_DOUBLE; n->value.dval = lbm_dec_double(v); break;
  default:
    n->is_error = true;
    n->value.err_val = ENC_SYM_TERROR;
    return false;
  }
  n->is_error = false;
  return true;
}

static void number_to_i(number_t *n) {
  switch(n->type) {
  case LBM_TYPE_I:  break; // the same
  case LBM_TYPE_U:  break; // same binary representation
  case LBM_TYPE_U32: break;
  case LBM_TYPE_I32: break;
  case LBM_TYPE_FLOAT: n->value.ival = (int32_t)n->value.fval; break;
  case LBM_TYPE_U64: n->value.ival = (int32_t)n->value.u64val; break;
  case LBM_TYPE_I64: n->value.ival = (int32_t)n->value.i64val; break;
  case LBM_TYPE_DOUBLE: n->value.ival = (int32_t)n->value.dval; break;
  }
  n->type = LBM_TYPE_I;
}

static void number_to_u(number_t *n) {
  switch(n->type) {
  case LBM_TYPE_I:  break; // the same
  case LBM_TYPE_U:  break; // same binary representation
  case LBM_TYPE_U32: break;
  case LBM_TYPE_I32: break;
  case LBM_TYPE_FLOAT: n->value.uval = (uint32_t)n->value.fval; break;
  case LBM_TYPE_U64: n->value.uval = (uint32_t)n->value.u64val; break;
  case LBM_TYPE_I64: n->value.uval = (uint32_t)n->value.i64val; break;
  case LBM_TYPE_DOUBLE: n->value.uval = (uint32_t)n->value.dval; break;
  }
  n->type = LBM_TYPE_U;
}

static void number_to_f(number_t *n) {
  switch(n->type) {
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_I: n->value.fval = (float)n->value.ival; break;
  case LBM_TYPE_U:   /* fall through */
  case LBM_TYPE_U32: n->value.fval = (float)n->value.uval; break;
  case LBM_TYPE_FLOAT: break;
  case LBM_TYPE_U64: n->value.fval = (float)n->value.u64val; break;
  case LBM_TYPE_I64: n->value.fval = (float)n->value.i64val; break;
  case LBM_TYPE_DOUBLE: n->value.fval = (float)n->value.dval; break;
  }
  n->type = LBM_TYPE_FLOAT;
}

static void number_to_i64(number_t *n) {
  switch(n->type) {
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_I: n->value.i64val = (int64_t)n->value.ival; break;
  case LBM_TYPE_U:   /* fall through */
  case LBM_TYPE_U32: n->value.i64val = (int64_t)n->value.uval; break;
  case LBM_TYPE_FLOAT: n->value.i64val = (int64_t)n->value.fval; break;
  case LBM_TYPE_U64: n->value.i64val = (int64_t)n->value.u64val; break;
  case LBM_TYPE_I64: n->value.i64val = (int64_t)n->value.i64val; break;
  case LBM_TYPE_DOUBLE: n->value.i64val = (int64_t)n->value.dval; break;
  }
  n->type = LBM_TYPE_I64;
}

static void number_to_u64(number_t *n) {
  switch(n->type) {
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_I: n->value.u64val = (uint64_t)n->value.ival; break;
  case LBM_TYPE_U:   /* fall through */
  case LBM_TYPE_U32: n->value.u64val = (uint64_t)n->value.uval; break;
  case LBM_TYPE_FLOAT: n->value.u64val = (uint64_t)n->value.fval; break;
  case LBM_TYPE_U64: n->value.u64val = (uint64_t)n->value.u64val; break;
  case LBM_TYPE_I64: n->value.u64val = (uint64_t)n->value.i64val; break;
  case LBM_TYPE_DOUBLE: n->value.u64val = (uint64_t)n->value.dval; break;
  }
  n->type = LBM_TYPE_I64;
}

static void number_to_d(number_t *n) {
  switch(n->type) {
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_I: n->value.dval = (double)n->value.ival; break;
  case LBM_TYPE_U:   /* fall through */
  case LBM_TYPE_U32: n->value.dval = (double)n->value.uval; break;
  case LBM_TYPE_FLOAT: n->value.dval = (double)n->value.fval; break;
  case LBM_TYPE_U64: n->value.dval = (double)n->value.u64val; break;
  case LBM_TYPE_I64: n->value.dval = (double)n->value.i64val; break;
  case LBM_TYPE_DOUBLE: n->value.dval = (double)n->value.dval; break;
  }
  n->type = LBM_TYPE_DOUBLE;
}


static void add_op(number_t *n, lbm_value v) {
  lbm_type tval = lbm_type_of(v);
  lbm_uint t = n->type < tval ? tval : n->type;
  switch (t) {
  case LBM_TYPE_I: number_to_i(n); n->value.ival += lbm_dec_as_i32(v); n->type = LBM_TYPE_I; break;
  case LBM_TYPE_U: number_to_u(n); n->value.uval += lbm_dec_as_u32(v); n->type = LBM_TYPE_U; break;
  case LBM_TYPE_U32: number_to_u(n); n->value.uval += lbm_dec_as_u32(v); n->type = LBM_TYPE_U32; break;
  case LBM_TYPE_I32: number_to_i(n); n->value.ival += lbm_dec_as_i32(v);  n->type = LBM_TYPE_I32; break;
  case LBM_TYPE_FLOAT: number_to_f(n); n->value.fval += lbm_dec_as_float(v); n->type = LBM_TYPE_FLOAT; break;
  case LBM_TYPE_U64: number_to_u64(n); n->value.u64val += lbm_dec_as_u64(v); n->type = LBM_TYPE_U64; break;
  case LBM_TYPE_I64: number_to_i64(n); n->value.i64val += lbm_dec_as_i64(v); n->type = LBM_TYPE_I64; break;
  case LBM_TYPE_DOUBLE: number_to_d(n); n->value.dval += lbm_dec_as_double(v); n->type = LBM_TYPE_DOUBLE; break;
  }
}

static void sub_op(number_t *n, lbm_value v) {
  lbm_type tval = lbm_type_of(v);
  lbm_uint t = n->type < tval ? tval : n->type;
  switch (t) {
  case LBM_TYPE_I: number_to_i(n); n->value.ival -= lbm_dec_as_i32(v); n->type = LBM_TYPE_I; break;
  case LBM_TYPE_U: number_to_u(n); n->value.uval -= lbm_dec_as_u32(v); n->type = LBM_TYPE_U; break;
  case LBM_TYPE_U32: number_to_u(n); n->value.uval -= lbm_dec_as_u32(v); n->type = LBM_TYPE_U32; break;
  case LBM_TYPE_I32: number_to_i(n); n->value.ival -= lbm_dec_as_i32(v);  n->type = LBM_TYPE_I32; break;
  case LBM_TYPE_FLOAT: number_to_f(n); n->value.fval -= lbm_dec_as_float(v); n->type = LBM_TYPE_FLOAT; break;
  case LBM_TYPE_U64: number_to_u64(n); n->value.u64val -= lbm_dec_as_u64(v); n->type = LBM_TYPE_U64; break;
  case LBM_TYPE_I64: number_to_i64(n); n->value.i64val -= lbm_dec_as_i64(v); n->type = LBM_TYPE_I64; break;
  case LBM_TYPE_DOUBLE: number_to_d(n); n->value.dval -= lbm_dec_as_double(v); n->type = LBM_TYPE_DOUBLE; break;
  }
}

static void mul_op(number_t *n, lbm_value v) {
  lbm_type tval = lbm_type_of(v);
  lbm_uint t = n->type < tval ? tval : n->type;
  switch (t) {
  case LBM_TYPE_I: number_to_i(n); n->value.ival *= lbm_dec_as_i32(v); n->type = LBM_TYPE_I; break;
  case LBM_TYPE_U: number_to_u(n); n->value.uval *= lbm_dec_as_u32(v); n->type = LBM_TYPE_U; break;
  case LBM_TYPE_U32: number_to_u(n); n->value.uval *= lbm_dec_as_u32(v); n->type = LBM_TYPE_U32; break;
  case LBM_TYPE_I32: number_to_i(n); n->value.ival *= lbm_dec_as_i32(v);  n->type = LBM_TYPE_I32; break;
  case LBM_TYPE_FLOAT: number_to_f(n); n->value.fval *= lbm_dec_as_float(v); n->type = LBM_TYPE_FLOAT; break;
  case LBM_TYPE_U64: number_to_u64(n); n->value.u64val *= lbm_dec_as_u64(v); n->type = LBM_TYPE_U64; break;
  case LBM_TYPE_I64: number_to_i64(n); n->value.i64val *= lbm_dec_as_i64(v); n->type = LBM_TYPE_I64; break;
  case LBM_TYPE_DOUBLE: number_to_d(n); n->value.dval *= lbm_dec_as_double(v); n->type = LBM_TYPE_DOUBLE; break;
  }
}

static void div_op(number_t *n, lbm_value v) {
  lbm_type tval = lbm_type_of(v);
  lbm_uint t = n->type < tval ? tval : n->type;
  if (n->is_error) return;
  switch (t) {
  case LBM_TYPE_I: {
    int32_t i = lbm_dec_as_i32(v);
    if (i == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_i(n);
    n->value.ival /= i;
    n->type = LBM_TYPE_I;
    break;
  }
  case LBM_TYPE_U: {
    uint32_t u = lbm_dec_as_u32(v);
    if (u == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_u(n);
    n->value.uval /= u;
    n->type = LBM_TYPE_U;
    break;
  }
  case LBM_TYPE_U32: {
    uint32_t u = lbm_dec_as_u32(v);
    if (u == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_u(n);
    n->value.uval /= u;
    n->type = LBM_TYPE_U32;
    break;
  }
  case LBM_TYPE_I32: {
    int32_t i = lbm_dec_as_i32(v);
    if (i == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_i(n);
    n->value.ival /= i;
    n->type = LBM_TYPE_I32;
    break;
  }
  case LBM_TYPE_FLOAT: {
    float f = lbm_dec_as_float(v);
    if (f == 0.0f || f == -0.0f) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_f(n);
    n->value.fval /= f;
    n->type = LBM_TYPE_FLOAT;
    break;
  }
  case LBM_TYPE_U64: {
    uint64_t u = lbm_dec_as_u64(v);
    if (u == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_u64(n);
    n->value.u64val /= u;
    n->type = LBM_TYPE_U64;
    break;
  }
  case LBM_TYPE_I64: {
    int64_t i = lbm_dec_as_i64(v);
    if (i == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_i64(n);
    n->value.i64val /= i;
    n->type = LBM_TYPE_I64;
    break;
  }
  case LBM_TYPE_DOUBLE: {
    double d = lbm_dec_as_double(v);
    if (d == (double)0.0 || d == (double)-0.0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_d(n);
    n->value.dval /= d;
    n->type = LBM_TYPE_DOUBLE;
    break;
  }
  }
}

static void mod_op(number_t *n, lbm_value v) {
  lbm_type tval = lbm_type_of(v);
  lbm_uint t = n->type < tval ? tval : n->type;
  if (n->is_error) return;
  switch (t) {
  case LBM_TYPE_I: {
    int32_t i = lbm_dec_as_i32(v);
    if (i == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_i(n);
    n->value.ival %= i;
    n->type = LBM_TYPE_I;
    break;
  }
  case LBM_TYPE_U: {
    uint32_t u = lbm_dec_as_u32(v);
    if (u == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_u(n);
    n->value.uval %= u;
    n->type = LBM_TYPE_U;
    break;
  }
  case LBM_TYPE_U32: {
    uint32_t u = lbm_dec_as_u32(v);
    if (u == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_u(n);
    n->value.uval %= u;
    n->type = LBM_TYPE_U32;
    break;
  }
  case LBM_TYPE_I32: {
    int32_t i = lbm_dec_as_i32(v);
    if (i == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_i(n);
    n->value.ival %= i;
    n->type = LBM_TYPE_I32;
    break;
  }
  case LBM_TYPE_FLOAT: {
    float f = lbm_dec_as_float(v);
    if (f == 0.0f || f == -0.0f) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_f(n);
    n->value.fval = fmodf(n->value.fval, f);
    n->type = LBM_TYPE_FLOAT;
    break;
  }
  case LBM_TYPE_U64: {
    uint64_t u = lbm_dec_as_u64(v);
    if (u == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_u64(n);
    n->value.u64val %= u;
    n->type = LBM_TYPE_U64;
    break;
  }
  case LBM_TYPE_I64: {
    int64_t i = lbm_dec_as_i64(v);
    if (i == 0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_i64(n);
    n->value.i64val %= i;
    n->type = LBM_TYPE_I64;
    break;
  }
  case LBM_TYPE_DOUBLE: {
    double d = lbm_dec_as_double(v);
    if (d == (double)0.0 || d == (double)-0.0) {
      n->is_error = true;
      n->value.err_val = ENC_SYM_DIVZERO;
      return;
    }
    number_to_d(n);
    n->value.dval = fmod(n->value.dval,d);
    n->type = LBM_TYPE_DOUBLE;
    break;
  }
  }
}


static lbm_value encode_number(number_t *n) {
  lbm_value res;
  switch(n->type) {
  case LBM_TYPE_CHAR: res = lbm_enc_char((char)n->value.ival); break;
  case LBM_TYPE_I: res = lbm_enc_i(n->value.ival); break;
  case LBM_TYPE_U: res = lbm_enc_u(n->value.uval); break;
  case LBM_TYPE_U32: res = lbm_enc_u32(n->value.uval); break;
  case LBM_TYPE_I32: res = lbm_enc_i32(n->value.ival); break;
  case LBM_TYPE_FLOAT: res = lbm_enc_float(n->value.fval); break;
  case LBM_TYPE_U64: res = lbm_enc_u64(n->value.u64val); break;
  case LBM_TYPE_I64: res = lbm_enc_i64(n->value.i64val); break;
  case LBM_TYPE_DOUBLE: res = lbm_enc_double(n->value.dval); break;
  default: res = ENC_SYM_TERROR;
  }
  return res;
}

static bool numerical_reduce(operation op, number_t *id_res, lbm_value *args, lbm_uint nargs) {
  for (lbm_uint i = 0; i < nargs; i ++) {
    lbm_value val = args[i];
    if(!lbm_is_number(val)) return false;
    op(id_res, val);
  }
  return true;
}

void fundamental_add(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {

  number_t n;
  n.type = LBM_TYPE_U;
  n.value.uval = 0;
  if (numerical_reduce(add_op, &n, args, nargs)) {
    lbm_value r = encode_number(&n);
    if (lbm_is_symbol_merror(r)) {
      lbm_perform_gc();
      r = encode_number(&n);
      if (lbm_is_symbol_merror(r)) {
        ERROR(ENC_SYM_MERROR);
      }
    }
    RETURN(r);
  } else {
    ERROR(ENC_SYM_TERROR);
  }
}

void fundamental_sub(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  number_t n;
  bool ok = false;
  if (nargs == 1) {
    n.value.uval = 0;
    n.type = LBM_TYPE_U;
    if (numerical_reduce(sub_op, &n, args, nargs)) {
      ok = true;
    }
  } else {
    if (value_to_number(&n, args[0])) {
      if (numerical_reduce(sub_op, &n, args+1, nargs -1)) {
        ok = true;
      }
    }
  }
  if (ok) {
    lbm_value r = encode_number(&n);
    if (lbm_is_symbol_merror(r)) {
      lbm_perform_gc();
      r = encode_number(&n);
      if (lbm_is_symbol_merror(r)) {
        ERROR(ENC_SYM_MERROR);
      }
    }
    RETURN(r);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_mul(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {

  number_t n;
  n.type = LBM_TYPE_U;
  n.value.uval = 1;
  if (numerical_reduce(mul_op, &n, args, nargs)) {
    lbm_value r = encode_number(&n);
    if (lbm_is_symbol_merror(r)) {
      lbm_perform_gc();
      r = encode_number(&n);
      if (lbm_is_symbol_merror(r)) {
        ERROR(ENC_SYM_MERROR);
      }
    }
    RETURN(r);
  } else {
    ERROR(ENC_SYM_TERROR);
  }
}

void fundamental_div(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if(nargs >= 1) {
    number_t n;
    value_to_number(&n, args[0]);
    if (numerical_reduce(div_op, &n, args+1, nargs -1)) {
      lbm_value r = encode_number(&n);
      if (lbm_is_symbol_merror(r)) {
        lbm_perform_gc();
        r = encode_number(&n);
        if (lbm_is_symbol_merror(r)) {
          ERROR(ENC_SYM_MERROR);
        }
      }
      RETURN(r);
    } else if (n.is_error) {
      ERROR(n.value.err_val);
    }
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_mod(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if(nargs >= 1) {
    number_t n;
    value_to_number(&n, args[0]);
    if (numerical_reduce(mod_op, &n, args+1, nargs -1)) {
      lbm_value r = encode_number(&n);
      if (lbm_is_symbol_merror(r)) {
        lbm_perform_gc();
        r = encode_number(&n);
        if (lbm_is_symbol_merror(r)) {
          ERROR(ENC_SYM_MERROR);
        }
      }
      RETURN(r);
    } else if (n.is_error) {
      ERROR(n.value.err_val);
    }
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;

  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    r = r && struct_eq(a, b);
    if (!r) break;
  }
  if (r) {
    RETURN(ENC_SYM_TRUE);
  }
  RETURN(ENC_SYM_NIL);
}

void fundamental_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  fundamental_eq(args, nargs, ctx);
  if (ctx->r == ENC_SYM_NIL) {
    ctx->r = ENC_SYM_TRUE;
  } else {
    ctx->r = ENC_SYM_NIL;
  }
}


void fundamental_numeq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) ctx;
  if (nargs <= 1) RETURN(ENC_SYM_TRUE);

  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;

  if (!lbm_is_number(a)) {
    ERROR(ENC_SYM_TERROR);
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) {
      ERROR(ENC_SYM_TERROR);
      break;
    }
    r = r && (compare(a, b) == 0);
    if (!r) break;
  }
  if (r) {
    RETURN(ENC_SYM_TRUE);
  }
  RETURN(ENC_SYM_NIL);
}

void fundamental_num_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  fundamental_numeq(args, nargs, ctx);
  if (ctx->r == ENC_SYM_NIL) {
    ctx->r = ENC_SYM_TRUE;
    return;
  } else if (ctx->r == ENC_SYM_TERROR) {
    return;
  }
  ctx->r = ENC_SYM_NIL;
  return;
}


void fundamental_lt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;

  if (!lbm_is_number(a) || nargs < 2) {
    ERROR(ENC_SYM_TERROR);
  }
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) ERROR(ENC_SYM_TERROR);
    r = r && (compare(a, b) == -1);
  }
  if (r) {
    RETURN(ENC_SYM_TRUE)
  }
  RETURN(ENC_SYM_NIL);
}

void fundamental_gt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  if (!lbm_is_number(a)) ERROR(ENC_SYM_TERROR);
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) ERROR(ENC_SYM_TERROR);
    r = r && (compare(a, b) == 1);
  }
  if (r) {
    RETURN(ENC_SYM_TRUE);
  } else {
    RETURN(ENC_SYM_NIL);
  }
}

void fundamental_leq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  if (!lbm_is_number(a)) ERROR(ENC_SYM_TERROR);
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) ERROR(ENC_SYM_TERROR);
    r = r && (compare(a, b) <= 0);
  }
  if (r) {
    RETURN(ENC_SYM_TRUE);
  } else {
    RETURN(ENC_SYM_NIL);
  }
}

void fundamental_geq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_uint a = args[0];
  lbm_uint b;
  bool r = true;
  if (!lbm_is_number(a)) ERROR(ENC_SYM_TERROR);
  for (lbm_uint i = 1; i < nargs; i ++) {
    b = args[i];
    if (!lbm_is_number(b)) ERROR(ENC_SYM_TERROR);
    r = r && (compare(a, b) >= 0);
  }
  if (r) {
    RETURN(ENC_SYM_TRUE);
  } else {
    RETURN(ENC_SYM_NIL);
  }
}

void fundamental_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 0) {
    RETURN(ENC_SYM_NIL);
  }
  lbm_uint a = args[0];
  if (lbm_type_of(a) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(a) == SYM_NIL) {
    RETURN(ENC_SYM_TRUE);
  }
  RETURN(ENC_SYM_NIL);
}

void fundamental_gc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) args;
  (void) nargs;
  lbm_perform_gc();
  RETURN(ENC_SYM_TRUE);
}

void fundamental_self(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  (void) args;
  (void) nargs;
  RETURN(lbm_enc_i(ctx->id));
}

void fundamental_set_mailbox_size(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {

  if (nargs == 1 && lbm_is_number(args[0])) {
    uint32_t s = lbm_dec_as_u32(args[0]);
    if (lbm_mailbox_change_size(ctx, s)) {
      RETURN(ENC_SYM_TRUE);
    }
  }
  RETURN(ENC_SYM_NIL);
}

void fundamental_cons(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 2) ERROR(ENC_SYM_EERROR);
  lbm_uint a = args[0];
  lbm_uint b = args[1];
  lbm_value c = lbm_cons(a,b);
  if (lbm_is_symbol_merror(c)) {
    lbm_perform_gc();
    c = lbm_cons(a,b);
  }
  if (lbm_is_symbol_merror(c)) ERROR(ENC_SYM_MERROR);
  RETURN(c);
}

void fundamental_car(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    if (lbm_type_of(args[0]) == LBM_TYPE_CONS) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      RETURN(cell->car);
    }
  }
  RETURN(ENC_SYM_NIL);
}

void fundamental_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    if (lbm_type_of(args[0]) == LBM_TYPE_CONS) {
      lbm_cons_t *cell = lbm_ref_cell(args[0]);
      RETURN(cell->cdr);
    }
  }
  RETURN(ENC_SYM_NIL);
}

void fundamental_list(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value result = lbm_heap_allocate_list(nargs);
  if (lbm_is_symbol_merror(result)) {
    lbm_perform_gc();
    result = lbm_heap_allocate_list(nargs);
  }
  if (lbm_is_symbol_merror(result)) ERROR(result);
  if (lbm_is_cons(result)) {
    lbm_value curr = result;
    for (lbm_uint i = 0; i < nargs; i ++) {
      lbm_set_car(curr, args[i]);
      curr = lbm_cdr(curr);
    }
  }
  RETURN(result);
}

void fundamental_append(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 2) ERROR(ENC_SYM_TERROR);
  lbm_value res = args[nargs-1];
  for (int i = (int)nargs -2; i >= 0; i --) {
    lbm_value curr = args[i];
    int n = 0;
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      n++;
      curr = lbm_cdr(curr);
    }
    curr = args[i];
    for (int j = n-1; j >= 0; j --) {
      WITH_GC_RMBR(res, lbm_cons(index_list(curr,j), res),1,res);
    }
  }
  RETURN(res);
}

void fundamental_undefine(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value env = lbm_get_env();
  lbm_value result = ENC_SYM_EERROR;
  if (nargs == 1 && lbm_is_symbol(args[0])) {
    result = lbm_env_drop_binding(env, args[0]);
    *lbm_get_env_ptr() = result;
    RETURN(result);
  } else if (nargs == 1 && lbm_is_cons(args[0])) {
    lbm_value curr = args[0];
    while (lbm_type_of(curr) == LBM_TYPE_CONS) {
      lbm_value key = lbm_car(curr);
      result = lbm_env_drop_binding(env, key);
      curr = lbm_cdr(curr);
    }
    *lbm_get_env_ptr() = result;
    RETURN(result);
  }
  ERROR(result);
}

void fundamental_array_read(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 2) ERROR(ENC_SYM_TERROR);
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
        RETURN(ENC_SYM_NIL);
      }
      number_t r_val;
      switch(array->elt_type) {
      case LBM_TYPE_CHAR:
        r_val.type = LBM_TYPE_CHAR;
        r_val.value.ival = ((char*)data)[i];
        break;
      case LBM_TYPE_U:
        r_val.type = LBM_TYPE_U;
        r_val.value.uval = (uint32_t)data[i];
        break;
      case LBM_TYPE_I:
        r_val.type = LBM_TYPE_I;
        r_val.value.ival = (int32_t)data[i];
        break;
      case LBM_TYPE_U32:
        r_val.type = LBM_TYPE_U32;
        r_val.value.uval = (uint32_t)data[i];
        break;
      case LBM_TYPE_I32:
        r_val.type = LBM_TYPE_U32;
        r_val.value.ival = (int32_t)data[i];
        break;
      case LBM_TYPE_FLOAT: {
        float v;
        memcpy(&v, &data[i], sizeof(float));
        r_val.type = LBM_TYPE_FLOAT;
        r_val.value.fval = v;
      } break;
#ifndef LBM64
      case LBM_TYPE_U64: {
        uint64_t v = 0;
        v |= (uint64_t)data[i*2];
        v |= ((uint64_t)data[i*2+1]) << 32;
        r_val.type = LBM_TYPE_U64;
        r_val.value.u64val = v;
      } break;
      case LBM_TYPE_I64: {
        uint64_t v = 0;
        v |= (uint64_t)data[i*2];
        v |= ((uint64_t)data[i*2+1]) << 32;
        r_val.type = LBM_TYPE_I64;
        r_val.value.i64val = (int64_t)v;
      } break;
      case LBM_TYPE_DOUBLE: {
        double v;
        memcpy(&v, &data[i*2], sizeof(double));
        r_val.type = LBM_TYPE_DOUBLE;
        r_val.value.dval = v;
      } break;
#else
      case LBM_TYPE_U64:
        r_val.type = LBM_TYPE_U64;
        r_val.value.u64val = data[i];
        break;
      case LBM_TYPE_I64:
        r_val.type = LBM_TYPE_I64;
        r_val.value.i64val = (int64_t)data[i];
        break;
      case LBM_TYPE_DOUBLE: {
        double v;
        memcpy(&v, &data[i], sizeof(double));
        r_val.type = LBM_TYPE_DOUBLE;
        r_val.value.dval = v;
      } break;
#endif
      default:
        ERROR(ENC_SYM_EERROR);
      }
      curr = encode_number(&r_val);
      if (lbm_is_symbol_merror(curr)) {
        lbm_gc_mark_phase(1, acc);
        lbm_perform_gc();
        curr = encode_number(&r_val);
        if (lbm_is_symbol_merror(curr)) {
          ERROR(ENC_SYM_MERROR);
        }
      }
      if (read_many) {
        WITH_GC_RMBR(acc, lbm_cons(curr, acc),1, acc);
      }
    } /* for i */
  }
  if (read_many) {
    RETURN(acc);
  }
  RETURN(curr);

}

void fundamental_array_write(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 3) ERROR(ENC_SYM_TERROR);
  lbm_value arr = args[0];
  lbm_value index = args[1];
  lbm_value val = args[2];
  lbm_uint ix;

  if (lbm_is_number(index)) {
    ix = lbm_dec_as_u32(index);
  } else {
    return;
  }

  if (lbm_type_of(arr) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(arr);
    if (array == NULL) ERROR(ENC_SYM_FATAL_ERROR);
    if (ix >= array->size) RETURN(ENC_SYM_NIL);

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
    RETURN(arr);
  }
}

void fundamental_array_create(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
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
    ERROR(ENC_SYM_TERROR);
  }

  lbm_value result;
  if (n > 0) {
    bool retry = false;;
    do {
      switch(t_sym) {
      case SYM_TYPE_CHAR: /* fall through */
      case SYM_TYPE_BYTE:
        lbm_heap_allocate_array(&result, n, LBM_TYPE_BYTE);
        break;
      case SYM_TYPE_I32:
        lbm_heap_allocate_array(&result, n, LBM_TYPE_I32);
        break;
      case SYM_TYPE_U32:
        lbm_heap_allocate_array(&result, n, LBM_TYPE_U32);
        break;
      case SYM_TYPE_FLOAT:
        lbm_heap_allocate_array(&result, n, LBM_TYPE_FLOAT);
        break;
      case SYM_TYPE_I64:
        lbm_heap_allocate_array(&result, n, LBM_TYPE_I64);
        break;
      case SYM_TYPE_U64:
        lbm_heap_allocate_array(&result, n, LBM_TYPE_U64);
        break;
      case SYM_TYPE_DOUBLE:
        lbm_heap_allocate_array(&result, n, LBM_TYPE_DOUBLE);
        break;
      default:
        break;
      }
      if (!retry && lbm_is_symbol_merror(result)) {
        lbm_perform_gc();
        retry = true;
      }
      else retry = false;
    } while (retry);
  } else {
    lbm_set_error_reason((char*)lbm_error_str_incorrect_arg);
    ERROR(ENC_SYM_EERROR);
  }
  RETURN(result);
}

void fundamental_array_size(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs != 1) ERROR(ENC_SYM_TERROR);

  if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(args[0]);
    if (array == NULL) {
      ERROR(ENC_SYM_FATAL_ERROR);
    }
    RETURN(lbm_enc_u(array->size));
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_array_clear(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs != 1) ERROR(ENC_SYM_TERROR);

  if (lbm_type_of(args[0]) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(args[0]);
    if (array == NULL) {
      ERROR(ENC_SYM_FATAL_ERROR);
    }
    int es = elt_size(array->elt_type);
    if (es < 0) ERROR(ENC_SYM_TERROR);
    memset(array->data, 0, array->size );
  }
  RETURN(args[0]);
}

void fundamental_symbol_to_string(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 1 ||
      lbm_type_of(args[0]) != LBM_TYPE_SYMBOL)
    ERROR(ENC_SYM_TERROR);
  lbm_value sym = args[0];
  const char *sym_str = lbm_get_name_by_symbol(lbm_dec_sym(sym));
  if (sym_str == NULL) ERROR(ENC_SYM_FATAL_ERROR);
  size_t len = strlen(sym_str);

  lbm_value v;
  if (!lbm_heap_allocate_array(&v, len+1, LBM_TYPE_CHAR)) {
    lbm_perform_gc();
    lbm_heap_allocate_array(&v, len+1, LBM_TYPE_CHAR);
  }
  if (lbm_is_symbol_merror(v)) ERROR(v);
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(v);
  if (!arr) ERROR(ENC_SYM_FATAL_ERROR);
  memset(arr->data,0,len+1);
  memcpy(arr->data,sym_str,len);
  RETURN(v);
}

void fundamental_string_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 1 ||
      lbm_type_of(args[0] != LBM_TYPE_ARRAY))
    ERROR(ENC_SYM_TERROR);
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
  if (!arr) ERROR(ENC_SYM_FATAL_ERROR);
  if (arr->elt_type != LBM_TYPE_CHAR) ERROR(ENC_SYM_TERROR);
  char *str = (char *)arr->data;
  lbm_uint sym;
  if (lbm_get_symbol_by_name(str, &sym)) {
    RETURN(lbm_enc_sym(sym));
  } else if (lbm_add_symbol(str, &sym)) {
    RETURN(lbm_enc_sym(sym));
  }
  ERROR(ENC_SYM_FATAL_ERROR);
}

void fundamental_symbol_to_uint(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 1) ERROR(ENC_SYM_TERROR);
  lbm_value s = args[0];
  if (lbm_type_of(s) == LBM_TYPE_SYMBOL)
    RETURN(lbm_enc_u(lbm_dec_sym(s)));
  ERROR(ENC_SYM_TERROR);
}

void fundamental_uint_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs < 1) ERROR(ENC_SYM_TERROR);
  lbm_value s = args[0];
  if (lbm_type_of(s) == LBM_TYPE_U)
    RETURN(lbm_enc_sym(lbm_dec_u(s)));
  ERROR(ENC_SYM_TERROR);
}

void fundamental_set_car(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2) {
    if (lbm_set_car(args[0],args[1])) {
      RETURN(args[0]);
    } else {
      RETURN(ENC_SYM_NIL);
    }
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_set_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2) {
    if (lbm_set_cdr(args[0],args[1])) {
      RETURN(args[0]);
    } else {
      RETURN(ENC_SYM_NIL);
    }
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_set_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs != 3 ||
      !lbm_is_cons(args[0]) ||
      !lbm_is_number(args[1]))
    ERROR(ENC_SYM_TERROR);

  lbm_value curr = args[0];
  lbm_uint i = 0;
  lbm_uint ix = lbm_dec_as_u32(args[1]);
  lbm_value result = ENC_SYM_NIL;
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
  RETURN(result);
}

void fundamental_assoc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2) {
    if (lbm_is_cons(args[0])) {
      lbm_value r = assoc_lookup(args[1], args[0]);
      if (lbm_is_symbol(r) &&
          lbm_dec_sym(r) == SYM_NO_MATCH) {
        RETURN(ENC_SYM_NIL);
      } else {
        RETURN(r);
      }
    } else if (lbm_is_symbol(args[0]) &&
               lbm_dec_sym(args[0]) == SYM_NIL) {
      RETURN(args[0]); /* nil */
    } /* else error */
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_acons(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  lbm_value keyval;
  lbm_value new_alist;
  bool retry = false;
  do {
    if (nargs == 3) {
      keyval = lbm_cons(args[0], args[1]);
      new_alist =  lbm_cons(keyval,args[2]);
    } else if (nargs == 2) {
      new_alist = lbm_cons(args[0], args[1]);
      RETURN(new_alist);
    } else {
      ERROR(ENC_SYM_TERROR);
    }
    if (!retry && lbm_is_symbol_merror(new_alist)) {
      lbm_perform_gc();
      retry = true;
    } else retry = false;
  } while(retry);
  RETURN(new_alist);
}

void fundamental_set_assoc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 3) {
    RETURN(lbm_env_set(args[0], args[1], args[2]));
  } else if (nargs == 2 && lbm_is_cons(args[1])) {
    lbm_value x = lbm_car(args[1]);
    lbm_value xs = lbm_cdr(args[1]);
    RETURN(lbm_env_set(args[0], x, xs));
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_cossa(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2) {
    if (lbm_is_cons(args[0])) {
      lbm_value r = cossa_lookup(args[1], args[0]);
      if (lbm_is_symbol(r) &&
          lbm_dec_sym(r) == SYM_NO_MATCH) {
        RETURN(ENC_SYM_NIL);
      } else {
        RETURN(r);
      }
    } else if (lbm_is_symbol(args[0]) &&
               lbm_dec_sym(args[0]) == SYM_NIL) {
      RETURN(ENC_SYM_NIL);
    } /* else error */
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 && lbm_is_number(args[1])) {
    RETURN(index_list(args[0], lbm_dec_as_i32(args[1])));
  }
  ERROR(ENC_SYM_TERROR)
}

void fundamental_to_i(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    RETURN(lbm_enc_i((lbm_int)lbm_dec_as_i64(args[0])));
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_i32(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value result;
    WITH_GC(result, lbm_enc_i32(lbm_dec_as_i32(args[0])));
    RETURN(result);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_u(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    RETURN(lbm_enc_u((lbm_uint)lbm_dec_as_u64(args[0])));
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_u32(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value result;
    WITH_GC(result, lbm_enc_u32(lbm_dec_as_u32(args[0])));
    RETURN(result);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_float(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value result;
    WITH_GC(result, lbm_enc_float(lbm_dec_as_float(args[0])));
    RETURN(result);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_i64(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value result;
    WITH_GC(result, lbm_enc_i64(lbm_dec_as_i64(args[0])));
    RETURN(result);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_u64(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value result;
    WITH_GC(result, lbm_enc_u64(lbm_dec_as_u64(args[0])));
    RETURN(result);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_double(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value result;
    WITH_GC(result, lbm_enc_double(lbm_dec_as_double(args[0])));
    RETURN(result);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_to_byte(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    RETURN(lbm_enc_char(lbm_dec_as_char(args[0])));
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_shl(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 &&
      lbm_is_number(args[0]) && lbm_is_number(args[1])) {

    number_t n;
    switch (lbm_type_of(args[0])) {
    case LBM_TYPE_I: n.type = LBM_TYPE_I; n.value.ival = (lbm_dec_i(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U: n.type = LBM_TYPE_U; n.value.uval = (lbm_dec_u(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: n.type = LBM_TYPE_U32; n.value.uval = (lbm_dec_u32(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: n.type = LBM_TYPE_I32; n.value.ival = (lbm_dec_i32(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I64: n.type = LBM_TYPE_I64; n.value.i64val = (lbm_dec_i64(args[0]) << lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U64: n.type = LBM_TYPE_U64; n.value.u64val = (lbm_dec_u64(args[0]) << lbm_dec_as_u32(args[1])); break;
    default: ERROR(ENC_SYM_TERROR);
    }

    lbm_value res = encode_number(&n);
    if (lbm_is_symbol_merror(res)) {
      lbm_perform_gc();
      res = encode_number(&n);
      if (lbm_is_symbol_merror(res)) ERROR(ENC_SYM_MERROR);
    }
    RETURN(res);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_shr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 &&
      lbm_is_number(args[0]) && lbm_is_number(args[1])) {
    number_t n;
    switch (lbm_type_of(args[0])) {
    case LBM_TYPE_I: n.type = LBM_TYPE_I; n.value.ival = (lbm_dec_i(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U: n.type = LBM_TYPE_U; n.value.uval = (lbm_dec_u(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: n.type = LBM_TYPE_U32; n.value.uval = (lbm_dec_u32(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: n.type = LBM_TYPE_I32; n.value.ival = (lbm_dec_i32(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I64: n.type = LBM_TYPE_I64; n.value.i64val = (lbm_dec_i64(args[0]) >> lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U64: n.type = LBM_TYPE_U64; n.value.u64val = (lbm_dec_u64(args[0]) >> lbm_dec_as_u32(args[1])); break;
    default: ERROR(ENC_SYM_TERROR);
    }

    lbm_value res = encode_number(&n);
    if (lbm_is_symbol_merror(res)) {
      lbm_perform_gc();
      res = encode_number(&n);
      if (lbm_is_symbol_merror(res)) ERROR(ENC_SYM_MERROR);
    }
    RETURN(res);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_bitwise_and(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 &&
      lbm_is_number(args[0]) && lbm_is_number(args[1])) {

    number_t n;
    switch (lbm_type_of(args[0])) {
    case LBM_TYPE_I: n.type = LBM_TYPE_I; n.value.ival = (lbm_dec_i(args[0]) & lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: n.type = LBM_TYPE_U; n.value.uval = (lbm_dec_u(args[0]) & lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: n.type = LBM_TYPE_U32; n.value.uval = (lbm_dec_u32(args[0]) & lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: n.type = LBM_TYPE_I32; n.value.ival = (lbm_dec_i32(args[0]) & lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_I64: n.type = LBM_TYPE_I64; n.value.i64val = (lbm_dec_i64(args[0]) & lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U64: n.type = LBM_TYPE_U64; n.value.u64val = (lbm_dec_u64(args[0]) & lbm_dec_as_u64(args[1])); break;
    default: ERROR(ENC_SYM_TERROR);
    }

    lbm_value res = encode_number(&n);
    if (lbm_is_symbol_merror(res)) {
      lbm_perform_gc();
      res = encode_number(&n);
      if (lbm_is_symbol_merror(res)) ERROR(ENC_SYM_MERROR);
    }
    RETURN(res);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_bitwise_or(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 &&
      lbm_is_number(args[0]) && lbm_is_number(args[1])) {

    number_t n;
    switch (lbm_type_of(args[0])) {
    case LBM_TYPE_I: n.type = LBM_TYPE_I; n.value.ival = (lbm_dec_i(args[0]) | lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: n.type = LBM_TYPE_U; n.value.uval = (lbm_dec_u(args[0]) | lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: n.type = LBM_TYPE_U32; n.value.uval = (lbm_dec_u32(args[0]) | lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: n.type = LBM_TYPE_I32; n.value.ival = (lbm_dec_i32(args[0]) | lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_I64: n.type = LBM_TYPE_I64; n.value.i64val = (lbm_dec_i64(args[0]) | lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U64: n.type = LBM_TYPE_U64; n.value.u64val = (lbm_dec_u64(args[0]) | lbm_dec_as_u64(args[1])); break;
    default: ERROR(ENC_SYM_TERROR);
    }

    lbm_value res = encode_number(&n);
    if (lbm_is_symbol_merror(res)) {
      lbm_perform_gc();
      res = encode_number(&n);
      if (lbm_is_symbol_merror(res)) ERROR(ENC_SYM_MERROR);
    }
    RETURN(res);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_bitwise_xor(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 2 &&
      lbm_is_number(args[0]) && lbm_is_number(args[1])) {

    number_t n;
    switch (lbm_type_of(args[0])) {
    case LBM_TYPE_I: n.type = LBM_TYPE_I; n.value.ival = (lbm_dec_i(args[0]) ^ lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_U: n.type = LBM_TYPE_U; n.value.uval = (lbm_dec_u(args[0]) ^ lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_U32: n.type = LBM_TYPE_U32; n.value.uval = (lbm_dec_u32(args[0]) ^ lbm_dec_as_u32(args[1])); break;
    case LBM_TYPE_I32: n.type = LBM_TYPE_I32; n.value.ival = (lbm_dec_i32(args[0]) ^ lbm_dec_as_i32(args[1])); break;
    case LBM_TYPE_I64: n.type = LBM_TYPE_I64; n.value.i64val = (lbm_dec_i64(args[0]) ^ lbm_dec_as_i64(args[1])); break;
    case LBM_TYPE_U64: n.type = LBM_TYPE_U64; n.value.u64val = (lbm_dec_u64(args[0]) ^ lbm_dec_as_u64(args[1])); break;
    default: ERROR(ENC_SYM_TERROR);
    }

    lbm_value res = encode_number(&n);
    if (lbm_is_symbol_merror(res)) {
      lbm_perform_gc();
      res = encode_number(&n);
      if (lbm_is_symbol_merror(res)) ERROR(ENC_SYM_MERROR);
    }
    RETURN(res);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_bitwise_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 &&
      lbm_is_number(args[0])) {

    number_t n;
    switch (lbm_type_of(args[0])) {
    case LBM_TYPE_I: n.type = LBM_TYPE_I; n.value.ival = (~lbm_dec_i(args[0])); break;
    case LBM_TYPE_U: n.type = LBM_TYPE_U; n.value.uval = (~lbm_dec_u(args[0])); break;
    case LBM_TYPE_U32: n.type = LBM_TYPE_U; n.value.uval = (~lbm_dec_u32(args[0])); break;
    case LBM_TYPE_I32: n.type = LBM_TYPE_I; n.value.ival = (~lbm_dec_i32(args[0])); break;
    case LBM_TYPE_I64: n.type = LBM_TYPE_I64; n.value.i64val = (~lbm_dec_i64(args[0])); break;
    case LBM_TYPE_U64: n.type = LBM_TYPE_U64; n.value.u64val = (~lbm_dec_u64(args[0])); break;
    default: ERROR(ENC_SYM_TERROR);
    }
    lbm_value res = encode_number(&n);
    if (lbm_is_symbol_merror(res)) {
      lbm_perform_gc();
      res = encode_number(&n);
      if (lbm_is_symbol_merror(res)) ERROR(ENC_SYM_MERROR);
    }
    RETURN(res);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_custom_destruct(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 && (lbm_type_of(args[0]) == LBM_TYPE_CUSTOM)) {
    lbm_uint *mem_ptr = (lbm_uint*)lbm_dec_custom(args[0]);
    if(!mem_ptr) ERROR(ENC_SYM_FATAL_ERROR);
    lbm_custom_type_destroy(mem_ptr);
    lbm_value tmp = lbm_set_ptr_type(args[0], LBM_TYPE_CONS);
    lbm_set_car(tmp, ENC_SYM_NIL);
    lbm_set_cdr(tmp, ENC_SYM_NIL);
      /* The original value will still be of type custom_ptr */
    RETURN(ENC_SYM_TRUE);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_type_of(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1) {
    lbm_value val = args[0];
    lbm_value res;
    switch(lbm_type_of(val)) {
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
    default: res = ENC_SYM_NIL; break;
    }
    RETURN(res);
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_list_length(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs == 1 && lbm_is_list(args[0])) {
    int32_t len = (int32_t)lbm_list_length(args[0]);
    RETURN(lbm_enc_i(len));
  }
  ERROR(ENC_SYM_TERROR);
}

void fundamental_range(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {

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
    ERROR(ENC_SYM_TERROR);
  }

  int num;
  if (end == start) {
    RETURN(ENC_SYM_NIL);
  } else if (end < start) {
    rev = true;
    num = start - end;
  } else {
    num = end - start;
  }

  lbm_value r_list = lbm_heap_allocate_list((unsigned int)num);
  if (lbm_is_symbol_merror(r_list)) {
    lbm_perform_gc();
    r_list = lbm_heap_allocate_list((unsigned int)num);
  }
  if (lbm_is_symbol_merror(r_list)) {
    ERROR(ENC_SYM_MERROR);
  }
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
  RETURN(r_list)
}

void fundamental_reg_event_handler(lbm_value *args, lbm_uint nargs, eval_context_t *ctx) {
  if (nargs != 1 || !lbm_is_number(args[0])) {
    ERROR(ENC_SYM_TERROR);
  }

  lbm_set_event_handler_pid((lbm_cid)lbm_dec_i(args[0]));
  RETURN(ENC_SYM_TRUE);
}
