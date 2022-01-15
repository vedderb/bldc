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

#include "symrepr.h"
#include "stack.h"
#include "heap.h"
#include "eval_cps.h"
#include "print.h"

#include <stdio.h>
#include <math.h>

static UINT add2(UINT a, UINT b) {

  UINT retval = enc_sym(SYM_TERROR);

  if (!(is_number(a) && is_number(b))) {
    return retval;
  }

  UINT t = (type_of(a) < type_of(b)) ? type_of(b) : type_of(a);
  switch (t) {
  case VAL_TYPE_I: retval = enc_i(dec_as_i(a) + dec_as_i(b)); break;
  case VAL_TYPE_U: retval = enc_u(dec_as_u(a) + dec_as_u(b)); break;
  case PTR_TYPE_BOXED_U: retval = enc_U(dec_as_u(a) + dec_as_u(b)); break;
  case PTR_TYPE_BOXED_I: retval = enc_I(dec_as_i(a) + dec_as_i(b)); break;
  case PTR_TYPE_BOXED_F: retval = enc_F(dec_as_f(a) + dec_as_f(b)); break;
  }
  return retval;
}

static UINT mul2(UINT a, UINT b) {

  UINT retval = enc_sym(SYM_TERROR);

  if (!(is_number(a) && is_number(b))) {
    return retval;
  }

  UINT t = (type_of(a) < type_of(b)) ? type_of(b) : type_of(a);
  switch (t) {
  case VAL_TYPE_I: retval = enc_i(dec_as_i(a) * dec_as_i(b)); break;
  case VAL_TYPE_U: retval = enc_u(dec_as_u(a) * dec_as_u(b)); break;
  case PTR_TYPE_BOXED_U: retval = enc_U(dec_as_u(a) * dec_as_u(b)); break;
  case PTR_TYPE_BOXED_I: retval = enc_I(dec_as_i(a) * dec_as_i(b)); break;
  case PTR_TYPE_BOXED_F: retval = enc_F(dec_as_f(a) * dec_as_f(b)); break;
  }
  return retval;
}

static UINT div2(UINT a, UINT b) {

  UINT retval = enc_sym(SYM_TERROR);

  if (!(is_number(a) && is_number(b))) {
    return retval;
  }

  UINT t = (type_of(a) < type_of(b)) ? type_of(b) : type_of(a);
  switch (t) {
  case VAL_TYPE_I: if (dec_i(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_i(dec_as_i(a) / dec_as_i(b)); break;
  case VAL_TYPE_U: if (dec_as_u(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_u(dec_as_u(a) / dec_as_u(b)); break;
  case PTR_TYPE_BOXED_U: if (dec_as_u(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_U(dec_as_u(a) / dec_as_u(b)); break;
  case PTR_TYPE_BOXED_I: if (dec_as_i(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_I(dec_as_i(a) / dec_as_i(b)); break;
  case PTR_TYPE_BOXED_F: if (dec_as_f(b) == 0.0 || dec_as_f(b) == -0.0) {return enc_sym(SYM_DIVZERO);} retval = enc_F(dec_as_f(a) / dec_as_f(b)); break;
  }
  return retval;
}

static UINT mod2(UINT a, UINT b) {

  UINT retval = enc_sym(SYM_TERROR);

  if (!(is_number(a) && is_number(b))) {
    return retval;
  }

  UINT t = (type_of(a) < type_of(b)) ? type_of(b) : type_of(a);
  switch (t) {
  case VAL_TYPE_I: if (dec_i(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_i(dec_as_i(a) % dec_as_i(b)); break;
  case VAL_TYPE_U: if (dec_as_u(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_u(dec_as_u(a) % dec_as_u(b)); break;
  case PTR_TYPE_BOXED_U: if (dec_as_u(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_U(dec_as_u(a) % dec_as_u(b)); break;
  case PTR_TYPE_BOXED_I: if (dec_as_i(b) == 0) {return enc_sym(SYM_DIVZERO);} retval = enc_I(dec_as_i(a) % dec_as_i(b)); break;
  case PTR_TYPE_BOXED_F: if (dec_as_f(b) == 0.0 || dec_as_f(b) == -0.0) {return enc_sym(SYM_DIVZERO);} retval = enc_F(fmodf(dec_as_f(a), dec_as_f(b))); break;
  }
  return retval;
}

static UINT negate(UINT a) {

  UINT retval = enc_sym(SYM_TERROR);
  INT i0;
  UINT u0;
  FLOAT f0;

  if (type_of(a) > VAL_TYPE_CHAR) {
    switch (type_of(a)) {
    case VAL_TYPE_I:
      i0 = dec_i(a);
      retval = enc_i(-i0);
      break;
    case VAL_TYPE_U:
      u0 = dec_u(a);
      retval = enc_u(-u0);
      break;
    case PTR_TYPE_BOXED_U:
      u0 = dec_U(a);
      retval = enc_U(-u0); //cons(-u0, enc_sym(SYM_BOXED_U_TYPE));
      break;
    case PTR_TYPE_BOXED_I:
      i0 = dec_I(a);
      retval = enc_I(-i0); //cons(-i0, enc_sym(SYM_BOXED_I_TYPE));
      break;
    case PTR_TYPE_BOXED_F:
      f0 = dec_F(a);
      f0 = -f0;
      //memcpy(&retval, &f0, sizeof(FLOAT));
      retval = enc_F(f0); //cons(retval, enc_sym(SYM_BOXED_F_TYPE));
      break;
    }
  }
  return retval;
}

static UINT sub2(UINT a, UINT b) {

  UINT retval = enc_sym(SYM_TERROR);

  if (!(is_number(a) && is_number(b))) {
    return retval;
  }

  UINT t = (type_of(a) < type_of(b)) ? type_of(b) : type_of(a);
  switch (t) {
  case VAL_TYPE_I: retval = enc_i(dec_as_i(a) - dec_as_i(b)); break;
  case VAL_TYPE_U: retval = enc_u(dec_as_u(a) - dec_as_u(b)); break;
  case PTR_TYPE_BOXED_U: retval = enc_U(dec_as_u(a) - dec_as_u(b)); break;
  case PTR_TYPE_BOXED_I: retval = enc_I(dec_as_i(a) - dec_as_i(b)); break;
  case PTR_TYPE_BOXED_F: retval = enc_F(dec_as_f(a) - dec_as_f(b)); break;
  }
  return retval;
}

static bool array_equality(VALUE a, VALUE b) {
  if (type_of(a) == PTR_TYPE_ARRAY &&
      type_of(a) == type_of(b)) {
    array_header_t *a_ = (array_header_t*)car(a);
    array_header_t *b_ = (array_header_t*)car(b);

    if (a_->elt_type == b_->elt_type &&
        a_->size == b_->size) {
      switch(a_->elt_type) {
      case VAL_TYPE_U:
      case PTR_TYPE_BOXED_U:
        if (memcmp((char*)a_+8, (char*)b_+8, a_->size * sizeof(UINT)) == 0) return true;
        break;
      case VAL_TYPE_I:
      case PTR_TYPE_BOXED_I:
        if (memcmp((char*)a_+8, (char*)b_+8, a_->size * sizeof(INT)) == 0) return true;
        break;
      case VAL_TYPE_CHAR:
        if (memcmp((char*)a_+8, (char*)b_+8, a_->size) == 0) return true;
        break;
      case PTR_TYPE_BOXED_F:
        if (memcmp((char*)a_+8, (char*)b_+8, a_->size * sizeof(FLOAT)) == 0) return true;
        break;
      default:
        break;
      }
    }
  }
  return false;
}

static bool struct_eq(VALUE a, VALUE b) {

  if (!is_ptr(a) && !is_ptr(b)) {
    if (val_type(a) == val_type(b)){
      switch (val_type(a)) {
      case VAL_TYPE_SYMBOL:
        return (dec_sym(a) == dec_sym(b));
      case VAL_TYPE_I:
        return (dec_i(a) == dec_i(b));
      case VAL_TYPE_U:
        return (dec_u(a) == dec_u(b));
      case VAL_TYPE_CHAR:
        return (dec_char(a) == dec_char(b));
      default:
        return false;
        break;
      }
    } else {
      return false;
    }
  }

  if (is_ptr(a) && is_ptr(b)) {
    if (ptr_type(a) == ptr_type(b)) {
      switch (ptr_type(a)) {
      case PTR_TYPE_SYMBOL_INDIRECTION:
        return dec_symbol_indirection(a) == dec_symbol_indirection(b);
      case PTR_TYPE_CONS:
        return ( struct_eq(car(a),car(b)) &&
                 struct_eq(cdr(a),cdr(b)) );
      case PTR_TYPE_BOXED_I:
        return ((INT)car(a) == (INT)car(b));
      case PTR_TYPE_BOXED_U:
        return (car(a) == car(b));
      case PTR_TYPE_BOXED_F:
        return ((FLOAT)car(a) == (FLOAT)car(b));
      case PTR_TYPE_ARRAY:
        return array_equality(a, b);
      default:
        return false;
      }
    }
  }
  return false;
}

static int cmpi(INT a, INT b) {
  int res = (a > b) - (a < b);
  return res;
}

static int cmpu(UINT a, UINT b) {
  int res = (a > b) - (a < b);
  return res;
}

static int cmpf(FLOAT a, FLOAT b) {
  int res = (a > b) - (a < b);
  return  res;
}


/* returns -1 if a < b; 0 if a = b; 1 if a > b */
static int compare(UINT a, UINT b) {

  int retval = 0;

  if (!(is_number(a) && is_number(b))) {
    return retval;
  }

  UINT t = (type_of(a) < type_of(b)) ? type_of(b) : type_of(a);
  switch (t) {
  case VAL_TYPE_I: retval = cmpi(dec_as_i(a), dec_as_i(b)); break;
  case VAL_TYPE_U: retval = cmpu(dec_as_u(a), dec_as_u(b)); break;
  case PTR_TYPE_BOXED_U: retval = cmpu(dec_as_u(a), dec_as_u(b)); break;
  case PTR_TYPE_BOXED_I: retval = cmpi(dec_as_i(a), dec_as_i(b)); break;
  case PTR_TYPE_BOXED_F: retval = cmpf(dec_as_f(a), dec_as_f(b)); break;
  }
  return retval;  
}


void array_read(VALUE *args, UINT nargs, UINT *result) {
  (void) nargs;
  // Args are: array, index
  VALUE arr = args[0];
  VALUE index = args[1];

  // Get array index
  UINT ix;
  INT  tmp;

  *result = enc_sym(SYM_EERROR);
  switch (type_of(index)) {
  case VAL_TYPE_U:
    ix = dec_u(index);
    break;
  case VAL_TYPE_I:
    tmp = (INT)dec_i(index);
    if (tmp < 0) {
      *result = enc_sym(SYM_EERROR);
      return;
    }
    ix = (UINT)tmp;
    break;
  case PTR_TYPE_BOXED_U:
    ix = dec_U(index);
    break;
  case PTR_TYPE_BOXED_I:
    tmp = dec_I(index);
    if (tmp < 0) {
      *result = enc_sym(SYM_EERROR);
      return;
    }
    ix = (UINT) tmp;
    break;
  default:
    *result = enc_sym(SYM_NIL);
    return;
  }

  if (type_of(arr) == PTR_TYPE_ARRAY) {
    array_header_t *array = (array_header_t*)car(arr);

    if (ix >= array->size){
      *result = enc_sym(SYM_NIL);
      return;
    }

    switch(array->elt_type) {
    case VAL_TYPE_CHAR:
      *result = enc_char((UINT) ((char*)array+8)[ix]);
      break;
    case VAL_TYPE_U:
      *result = enc_u(((UINT*)array + 2)[ix]);
      break;
    case VAL_TYPE_I:
      *result = enc_i(((INT*)array + 2)[ix]);
      break;
    case PTR_TYPE_BOXED_U:
      *result = cons(((UINT*)array + 2)[ix], enc_sym(SYM_BOXED_U_TYPE));
      if (type_of(*result) == VAL_TYPE_SYMBOL) return;
      *result = set_ptr_type(*result, PTR_TYPE_BOXED_U);
      break;
    case PTR_TYPE_BOXED_I:
      *result = cons(((UINT*)array + 2)[ix], enc_sym(SYM_BOXED_I_TYPE));
      if (type_of(*result) == VAL_TYPE_SYMBOL) return;
      *result = set_ptr_type(*result, PTR_TYPE_BOXED_I);
      break;
    case PTR_TYPE_BOXED_F:
      *result = cons(((UINT*)array+2)[ix], enc_sym(SYM_BOXED_F_TYPE));
      if (type_of(*result) == VAL_TYPE_SYMBOL) return;
      *result = set_ptr_type(*result, PTR_TYPE_BOXED_F);
      break;
    default:
      *result = enc_sym(SYM_EERROR);
      return;
    }
    return;
  }
  *result = enc_sym(SYM_EERROR);
}

void array_write(VALUE *args, UINT nargs, UINT *result) {
  (void) nargs;
  VALUE arr = args[0];
  VALUE index = args[1];
  VALUE val = args[2];
  UINT ix;
  INT tmp;
  switch (type_of(index)) {
  case VAL_TYPE_U:
    ix = dec_u(index);
    break;
  case VAL_TYPE_I:
    tmp = (INT)dec_i(index);
    if (tmp < 0) {
      *result =  enc_sym(SYM_EERROR);
      return;
    }
    ix = (UINT) tmp;
    break;
  case PTR_TYPE_BOXED_U:
    ix = car(index);
    break;
  case PTR_TYPE_BOXED_I:
    tmp = (INT)car(index);
    if (tmp < 0) {
      *result = enc_sym(SYM_EERROR);
      return;
    }
    ix = (UINT) tmp;
    break;
  default:
    *result = enc_sym(SYM_NIL);
    return;
  }

  if (type_of(arr) == PTR_TYPE_ARRAY) {
    array_header_t *array = (array_header_t*)car(arr);

    if (type_of(val) != array->elt_type ||
        ix >= array->size) {
      *result =  enc_sym(SYM_NIL);
      return;
    }

    switch(array->elt_type) {
    case VAL_TYPE_CHAR: {
      char * data = (char *)array + 8;
      data[ix] = dec_char(val);
      break;
    }
    case VAL_TYPE_U: {
      UINT* data = (UINT*)array + 2;
      data[ix] = dec_u(val);
      break;
    }
    case VAL_TYPE_I: {
      INT *data = (INT*)array + 2;
      data[ix] = dec_i(val);
      break;
    }
    case PTR_TYPE_BOXED_U: {
      UINT *data = (UINT*)array + 2;
      data[ix] = dec_U(val);
      break;
    }
    case PTR_TYPE_BOXED_I: {
      INT *data = (INT*)array + 2;
      data[ix] = dec_I(val);
      break;
    }
    case PTR_TYPE_BOXED_F: {
      //uv = car(val);
      //memcpy(&v, &uv, sizeof(FLOAT));
      UINT *data = (UINT*)array + 2;
      data[ix] = car(val);
      break;
    }
    default:
      *result = enc_sym(SYM_EERROR);
      return;
    }
    *result = enc_sym(SYM_TRUE);
    return;
  }
  *result = enc_sym(SYM_NIL);
}


void array_create(VALUE *args, UINT nargs, UINT *result) {
  (void) args;
  (void) nargs;
  (void) result;

}


VALUE index_list(VALUE l, int n) {
  /* TODO: error checking */
  VALUE curr = l;
  while ( type_of(curr) == PTR_TYPE_CONS &&
          n > 0) {
    curr = cdr(curr);
    n --;
  }
  return car(curr);
}

VALUE fundamental_exec(VALUE* args, UINT nargs, VALUE op) {

  UINT result = enc_sym(SYM_EERROR);
  int cmp_res = -1;

  switch (dec_sym(op)) {
  case SYM_IS_FUNDAMENTAL:
    if (nargs < 1 ||
        type_of(args[0]) != VAL_TYPE_SYMBOL)
      result = enc_sym(SYM_NIL);
    else if (is_fundamental(args[0]))
      result = enc_sym(SYM_TRUE);
    else
      result = enc_sym(SYM_NIL);
    break;

  case SYM_SYMBOL_TO_STRING: {
    if (nargs < 1 ||
        type_of(args[0]) != VAL_TYPE_SYMBOL)
      return enc_sym(SYM_NIL);
    VALUE sym = args[0];
    const char *sym_str = symrepr_lookup_name(dec_sym(sym));
    if (sym_str == NULL) return enc_sym(SYM_NIL);
    size_t len = strlen(sym_str);

    VALUE v;
    if (heap_allocate_array(&v, len+1, VAL_TYPE_CHAR)) {
      array_header_t *arr = (array_header_t*)car(v);
      if (!arr) return enc_sym(SYM_MERROR);
      char *data = (char *)arr+8;
      memset(data,0,len+1);
      memcpy(data,sym_str,len);
    } else {
      return enc_sym(SYM_MERROR);
    }
    result = v;
    break;
  }
  case SYM_STRING_TO_SYMBOL: {
    result = enc_sym(SYM_EERROR);
    if (nargs < 1 ||
        type_of(args[0] != PTR_TYPE_ARRAY))
      break;
    array_header_t *arr = (array_header_t *)car(args[0]);
    if (arr->elt_type != VAL_TYPE_CHAR)
      break;
    char *str = (char *)arr+8;
    UINT sym;
    if (symrepr_lookup(str, &sym)) {
      result = enc_sym(sym);
    } else if (symrepr_addsym(str, &sym)) {
      result = enc_sym(sym);
    }
    break;
  }
  case SYM_SYMBOL_TO_UINT: {
    VALUE s = args[0];
    if (type_of(s) == VAL_TYPE_SYMBOL)
      result = enc_u(dec_sym(s));
    else
      result = enc_sym(SYM_EERROR);
    break;
  }
  case SYM_UINT_TO_SYMBOL: {
    VALUE s = args[0];
    if (type_of(s) == VAL_TYPE_U)
      result = enc_sym(dec_u(s));
    else
      result = enc_sym(SYM_EERROR);
    break;
  }
  //  Create a symbol indirection from an unsigned
  case SYM_MK_SYMBOL_INDIRECT: {
    VALUE s = args[0];
    if (type_of(s) == VAL_TYPE_U &&
        dec_u(s) < 33554432)
      result = enc_symbol_indirection(dec_u(s));
    else
      result = enc_sym(SYM_EERROR);
    break;
  }
  case SYM_SET_CAR:
    if (nargs == 2) {
      if (set_car(args[0],args[1])) {
        result = enc_sym(SYM_TRUE);
      } else {
        result = enc_sym(SYM_NIL);
      }
    }
    break;
  case SYM_SET_CDR:
    if (nargs == 2) {
      if (set_cdr(args[0],args[1])) {
        result = enc_sym(SYM_TRUE);
      } else {
        result = enc_sym(SYM_NIL);
      }
    }
    break;
  case SYM_CONS: {
    UINT a = args[0];
    UINT b = args[1];
    result = cons(a,b);
    break;
  }
  case SYM_CAR: {
    result = car(args[0]);
    break;
  }
  case SYM_CDR: {
    result = cdr(args[0]);
    break;
  }
  case SYM_LIST: {
    result = enc_sym(SYM_NIL);
    for (UINT i = 1; i <= nargs; i ++) {
      result = cons(args[nargs-i], result);
      if (type_of(result) == VAL_TYPE_SYMBOL)
        break;
    }
    break;
  }
  case SYM_APPEND: {
    if (nargs != 2) break;

    VALUE a = args[0];
    VALUE b = args[1];

    result = b;
    VALUE curr = a;
    int n = 0;
    while (type_of(curr) == PTR_TYPE_CONS) {
      n++;
      curr = cdr(curr);
    }

    for (int i = n-1; i >= 0; i --) {
      result = cons(index_list(a,i), result);
      if (type_of(result) == VAL_TYPE_SYMBOL)
        break;
    }
    break;
  }
  case SYM_ADD: {
    UINT sum = args[0];
    for (UINT i = 1; i < nargs; i ++) {
      sum = add2(sum, args[i]);
      if (type_of(sum) == VAL_TYPE_SYMBOL) {
        break;
      }
    }
    result = sum;
    break;
  }
  case SYM_SUB: {
    UINT res = args[0];

    if (nargs == 1) {
      res = negate(res);
    } else {
      for (UINT i = 1; i < nargs; i ++) {
        res = sub2(res, args[i]);
        if (type_of(res) == VAL_TYPE_SYMBOL)
          break;
      }
    }
    result = res;
    break;
  }
  case SYM_MUL: {
    UINT prod = args[0];
    for (UINT i = 1; i < nargs; i ++) {
      prod = mul2(prod, args[i]);
      if (type_of(prod) == VAL_TYPE_SYMBOL) {
        break;
      }
    }
    result = prod;
    break;
  }
  case SYM_DIV:  {
    UINT res = args[0];
    for (UINT i = 1; i < nargs; i ++) {
      res = div2(res, args[i]);
      if (type_of(res) == VAL_TYPE_SYMBOL) {
        break;
      }
    }
    result = res;
    break;
  }
  case SYM_MOD: {
    UINT res = args[0];
    for (UINT i = 1; i < nargs; i ++) {
      res = mod2(res, args[i]);
      if (type_of(res) == VAL_TYPE_SYMBOL) {
        break;
      }
    }
    result = res;
    break;
  }
  case SYM_EQ: {
    UINT a = args[0];
    UINT b;
    bool r = true;

    for (UINT i = 1; i < nargs; i ++) {
      b = args[i];
      r = r && struct_eq(a, b);
    }
    if (r) {
      result = enc_sym(SYM_TRUE);
    } else {
      result = enc_sym(SYM_NIL);
    }
    break;
  }
  case SYM_NUMEQ:
    cmp_res = 0;
    /* fall through */
  case SYM_GT:
    if (dec_sym(op) == SYM_GT) cmp_res = 1;
    /* fall through */
  case SYM_LT: {
    UINT a = args[0];
    UINT b;
    bool r = true;
    bool ok = true;

    if (!is_number(a)) {
      result = enc_sym(SYM_TERROR);
      break;
    }
    for (UINT i = 1; i < nargs; i ++) {
      b = args[i];
      if (!is_number(b)) {
        ok = false;
        break;
      }
      r = r && (compare(a, b) == cmp_res);
    }
    if (ok) {
      if (r) {
        result = enc_sym(SYM_TRUE);
      } else {
        result = enc_sym(SYM_NIL);
      }
    } else {
      result = enc_sym(SYM_TERROR);
    }
    break;
  }
  case SYM_NOT: {
    if (nargs == 0) {
      return enc_sym(SYM_NIL);
      break;
    }
    UINT a = args[0];
    if (type_of(a) == VAL_TYPE_SYMBOL &&
        dec_sym(a) == SYM_NIL) {
      result = enc_sym(SYM_TRUE);
      break;
    }
    result = enc_sym(SYM_NIL);
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

  case SYM_TYPE_OF:
    if (nargs != 1) return enc_sym(SYM_NIL);
    VALUE val = args[0];
    switch(type_of(val)) {
    case PTR_TYPE_CONS:
      return enc_sym(SYM_TYPE_LIST);
    case PTR_TYPE_ARRAY:
      return enc_sym(SYM_TYPE_ARRAY);
    case PTR_TYPE_BOXED_I:
      return enc_sym(SYM_TYPE_I32);
    case PTR_TYPE_BOXED_U:
      return enc_sym(SYM_TYPE_U32);
    case PTR_TYPE_BOXED_F:
      return enc_sym(SYM_TYPE_FLOAT);
    case VAL_TYPE_I:
      return enc_sym(SYM_TYPE_I28);
    case VAL_TYPE_U:
      return enc_sym(SYM_TYPE_U28);
    case VAL_TYPE_CHAR:
      return enc_sym(SYM_TYPE_CHAR);
    case VAL_TYPE_SYMBOL:
      return enc_sym(SYM_TYPE_SYMBOL);
    default:
      return enc_sym(SYM_TERROR);
    }
    break;
  default:
    result = enc_sym(SYM_EERROR);
    break;
  }

  return result;
}
