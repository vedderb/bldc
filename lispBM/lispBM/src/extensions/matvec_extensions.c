/*
    Copyright 2023 Joel Svensson        svenssonjoel@yahoo.se

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

#include "extensions.h"
#include "lbm_utils.h"
#include "lbm_custom_type.h"

#include <math.h>

static const char *vector_float_desc = "Vector-Float";
static const char *matrix_float_desc = "Matrix-Float";

typedef struct {
  lbm_uint size;
  float data[1];
} vector_float_t;

static bool common_destructor(lbm_uint value) {
  lbm_free((void*)value);
  return true;
}

static lbm_value vector_float_allocate(lbm_uint size) {
  vector_float_t *mem = lbm_malloc( 1 * sizeof(lbm_uint) +
                                    size * sizeof(float));
  if (!mem) return ENC_SYM_MERROR;
  mem->size = size;
  lbm_value res;
  lbm_custom_type_create((lbm_uint)mem,
                         common_destructor,
                         vector_float_desc,
                         &res);
  return res;
}

static bool is_vector_float(lbm_value v) {
  return ((lbm_uint)lbm_get_custom_descriptor(v) == (lbm_uint)vector_float_desc);
}

/* **************************************************
 * Matrices stored in row-major form
 */

typedef struct {
  lbm_uint rows;
  lbm_uint cols;
  float data[1];
} matrix_float_t;

static lbm_value matrix_float_allocate(unsigned int rows, unsigned int cols) {
  matrix_float_t *mem = lbm_malloc(1 * sizeof(lbm_uint) +
                                   1 * sizeof(lbm_uint) +
                                   rows * cols * sizeof(float));
  if (!mem) return ENC_SYM_MERROR;
  mem->rows = rows;
  mem->cols = cols;
  lbm_value res;
  lbm_custom_type_create((lbm_uint)mem,
                         common_destructor,
                         matrix_float_desc,
                         &res);
  return res;
}

static bool is_matrix_float(lbm_value m) {
  return ((lbm_uint)lbm_get_custom_descriptor(m) == (lbm_uint)matrix_float_desc);
}

/* **************************************************
 * Extension implementations
 */

static lbm_value ext_vector(lbm_value *args, lbm_uint argn) {

  LBM_CHECK_NUMBER_ALL();

  if (argn < 1) return ENC_SYM_TERROR;

  lbm_value vec = vector_float_allocate(argn);
  if (lbm_is_error(vec)) return vec;

  vector_float_t *lvec = (vector_float_t*)lbm_get_custom_value(vec);

  for (lbm_uint i = 0; i < argn; i ++) {
    lvec->data[i] = lbm_dec_as_float(args[i]);
  }
  return vec;
}

static lbm_value ext_list_to_vector(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;

  if (argn == 1 &&
      lbm_is_list(args[0])) {

    bool nums = true;
    unsigned int len = lbm_list_length_pred(args[0], &nums, lbm_is_number);

    if (len > 0 && nums) {
      lbm_value vec = vector_float_allocate(len);
      if (lbm_is_error(vec)) return vec;
      vector_float_t *lvec = (vector_float_t*)lbm_get_custom_value(vec);

      lbm_value curr = args[0];
      unsigned int i = 0;
      while (lbm_is_cons(curr)) {
        lvec->data[i] = lbm_dec_as_float(lbm_car(curr));
        i ++;
        curr = lbm_cdr(curr);
      }
      res = vec;
    }
  }
  return res;
}

static lbm_value ext_vector_to_list(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && is_vector_float(args[0])) {
    vector_float_t *lvec = (vector_float_t*)lbm_get_custom_value(args[0]);

    lbm_value result = lbm_heap_allocate_list(lvec->size);
    if (lbm_is_cons(result)) {
      lbm_value curr = result;
      for (lbm_uint i = 0; i < lvec->size; i ++) {
        lbm_value f_val = lbm_enc_float(lvec->data[i]);
        if (lbm_is_error(f_val)) {
          result = f_val;
          break;
        }
        lbm_set_car(curr, f_val);
        curr = lbm_cdr(curr);
      }
      res = result;
    }
  }
  return res;
}

static lbm_value ext_vproj(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_vector_float(args[0]) &&
      lbm_is_number(args[1])) {
    vector_float_t *vec = (vector_float_t*)lbm_get_custom_value(args[0]);
    uint32_t i = lbm_dec_as_u32(args[1]);
    if (i < vec->size) {
      res = lbm_enc_float(vec->data[i]);
    }
  }
  return res;
}

static lbm_value ext_axpy(lbm_value *args, lbm_uint argn ) {

  lbm_value res = ENC_SYM_TERROR;

  if (argn != 3) return res;
  lbm_value a = args[0];
  lbm_value x = args[1];
  lbm_value y = args[2];

  if (is_vector_float(x) && is_vector_float(y) && lbm_is_number(a)) {

    float alpha = lbm_dec_as_float(a);
    vector_float_t *X = (vector_float_t*)lbm_get_custom_value(x);
    vector_float_t *Y = (vector_float_t*)lbm_get_custom_value(y);

    if (X->size == Y->size) {

      lbm_uint res_size = X->size;

      res = vector_float_allocate(res_size);
      if (!lbm_is_symbol_merror(res)) {

        vector_float_t *R = (vector_float_t*)lbm_get_custom_value(res);

        for (unsigned i = 0; i < res_size; i ++) {
          R->data[i] = alpha * X->data[i] + Y->data[i];
        }
      }
    }
  }
  return res;
}

static lbm_value ext_dot(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;

  if (argn != 2) return res;
  lbm_value x = args[0];
  lbm_value y = args[1];

  if (is_vector_float(x) && is_vector_float(y)) {

    vector_float_t *X = (vector_float_t*)lbm_get_custom_value(x);
    vector_float_t *Y = (vector_float_t*)lbm_get_custom_value(y);

    if (X->size == Y->size) {
      lbm_uint res_size = X->size;

      float f_res = 0;
      for (unsigned i = 0; i < res_size; i ++) {
        f_res +=  X->data[i] * Y->data[i];
      }
      res = lbm_enc_float(f_res);
    }
  }
  return res;
}

static float vector_float_mag(vector_float_t *v) {
    float mag = 0.0;
    for (unsigned int i = 0; i < v->size; i ++) {
      mag += (v->data[i] * v->data[i]);
    }
    mag = sqrtf(mag);
    return mag;
}

static lbm_value ext_mag(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;

  if (argn == 1 &&
      is_vector_float(args[0])) {
    vector_float_t *v = (vector_float_t *)lbm_get_custom_value(args[0]);
    float mag = vector_float_mag(v);
    res = lbm_enc_float(mag);
  }
  return res;
}

static lbm_value ext_vmult(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      is_vector_float(args[1])) {

    float alpha = lbm_dec_as_float(args[0]);
    vector_float_t *x = (vector_float_t *)lbm_get_custom_value(args[1]);
    lbm_value y = vector_float_allocate(x->size);
    res = y;
    if (!lbm_is_error(y)) {
      vector_float_t *y_vec = (vector_float_t *)lbm_get_custom_value(y);
      for (unsigned int i = 0; i < x->size; i ++) {
        y_vec->data[i] = alpha * x->data[i];
      }
    }
  }
  return res;
}

static lbm_value ext_list_to_matrix(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;

  if (argn == 2 &&
      lbm_is_number(args[0]) &&
      lbm_is_list(args[1])) {

    bool nums = true;
    unsigned int len = lbm_list_length_pred(args[1], &nums, lbm_is_number);

    if (len > 0 && nums) {
      uint32_t cols = lbm_dec_as_u32(args[0]);
      uint32_t rows = len / cols;

      if (len % cols == 0) {
        lbm_value mat = matrix_float_allocate(rows, cols);
        if (lbm_is_error(mat)) return mat;
        matrix_float_t *lmat = (matrix_float_t*)lbm_get_custom_value(mat);

        lbm_value curr = args[1];
        unsigned int i = 0;
        while (lbm_is_cons(curr)) {
          lmat->data[i] = lbm_dec_as_float(lbm_car(curr));
          i ++;
          curr = lbm_cdr(curr);
        }
        res = mat;
      }
    }
  }
  return res;
}

static lbm_value ext_matrix_to_list(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && is_matrix_float(args[0])) {
    matrix_float_t *lmat = (matrix_float_t*)lbm_get_custom_value(args[0]);
    lbm_uint size = lmat->rows * lmat->cols;

    res = lbm_heap_allocate_list(size);
    if (lbm_is_cons(res)) {
      lbm_value curr = res;
      for (unsigned int i = 0; i < size; i ++) {
        lbm_value f_val = lbm_enc_float(lmat->data[i]);
        if (lbm_is_error(f_val)) {
          res = f_val;
          break;
        }
        lbm_set_car(curr, f_val);
        curr = lbm_cdr(curr);
      }
    }
  }
  return res;
}



/* **************************************************
 * Initialization
 */

bool lbm_matvec_extensions_init(void) {
  bool res = true;

  // Vectors
  res = res && lbm_add_extension("vector", ext_vector);
  res = res && lbm_add_extension("list-to-vector", ext_list_to_vector);
  res = res && lbm_add_extension("vector-to-list", ext_vector_to_list);
  res = res && lbm_add_extension("vproj", ext_vproj);
  res = res && lbm_add_extension("axpy", ext_axpy);
  res = res && lbm_add_extension("dot", ext_dot);
  res = res && lbm_add_extension("mag", ext_mag);
  res = res && lbm_add_extension("vmult", ext_vmult);

  // Matrices
  res = res && lbm_add_extension("list-to-matrix", ext_list_to_matrix);
  res = res && lbm_add_extension("matrix-to-list", ext_matrix_to_list);

  return res;
}

