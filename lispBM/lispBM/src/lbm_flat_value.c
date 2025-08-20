/*
    Copyright 2023, 2024, 2025 Joel Svensson    svenssonjoel@yahoo.se
              2023       Benjamin Vedder

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

#include <lbm_flat_value.h>
#include <eval_cps.h>
#include <stack.h>

#include <setjmp.h>
 
// ------------------------------------------------------------
// Access to GC from eval_cps
int lbm_perform_gc(void);


// ------------------------------------------------------------
// Flatteners
bool lbm_start_flatten(lbm_flat_value_t *v, size_t buffer_size) {
  bool res = false;
  uint8_t *data = lbm_malloc_reserve(buffer_size);
  if (data) {
    v->buf = data;
    v->buf_size = buffer_size;
    v->buf_pos = 0;
    res = true;
  }
  return res;
}

bool lbm_finish_flatten(lbm_flat_value_t *v) {
  lbm_uint size_words;
  if (v->buf_pos % sizeof(lbm_uint) == 0) {
    size_words = v->buf_pos / sizeof(lbm_uint);
  } else {
    size_words = (v->buf_pos / sizeof(lbm_uint)) + 1;
  }
  if (v->buf_size  <= size_words * sizeof(lbm_uint)) return true;
  v->buf_size = size_words * sizeof(lbm_uint);
  return (lbm_memory_shrink((lbm_uint*)v->buf, size_words) >= 0);
}

static bool write_byte(lbm_flat_value_t *v, uint8_t b) {
  bool res = false;
  if (v->buf_size >= v->buf_pos + 1) {
    v->buf[v->buf_pos++] = b;
    res = true;
  }
  return res;
}

static bool write_bytes(lbm_flat_value_t *v, uint8_t *data,lbm_uint num_bytes) {
  bool res = false;
  if (v->buf_size >= v->buf_pos + num_bytes) {
    memcpy(v->buf + v->buf_pos, data, num_bytes);
    v->buf_pos += num_bytes;
    res = true;
  }
  return res;
}

static bool write_word(lbm_flat_value_t *v, uint32_t w) {
  bool res = false;
  if (v->buf_size >= v->buf_pos + 4) {
    v->buf[v->buf_pos++] = (uint8_t)(w >> 24);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 16);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 8);
    v->buf[v->buf_pos++] = (uint8_t)w;
    res = true;
  }
  return res;
}

static bool write_dword(lbm_flat_value_t *v, uint64_t w) {
  bool res = false;
  if (v->buf_size >= v->buf_pos + 8) {
    v->buf[v->buf_pos++] = (uint8_t)(w >> 56);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 48);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 40);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 32);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 24);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 16);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 8);
    v->buf[v->buf_pos++] = (uint8_t)w;
    res = true;
  }
  return res;
}

bool f_cons(lbm_flat_value_t *v) {
  bool res = false;
  if (v->buf_size >= v->buf_pos + 1) {
    v->buf[v->buf_pos++] = S_CONS;
    res = true;
  }
  return res;
}

bool f_lisp_array(lbm_flat_value_t *v, uint32_t size) {
  // arrays are smaller than 2^32 elements long
  bool res = true;
  res = res && write_byte(v, S_LBM_LISP_ARRAY);
  res = res && write_word(v, size); // number of elements.
  return res;
}

bool f_sym(lbm_flat_value_t *v, lbm_uint sym_id) {
  bool res = true;
  res = res && write_byte(v,S_SYM_VALUE);
  #ifndef LBM64
  res = res && write_word(v,sym_id);
  #else
  res = res && write_dword(v,sym_id);
  #endif
  return res;
}

bool f_sym_string(lbm_flat_value_t *v, char *str) {
  bool res = false;
  if (str) {
    lbm_uint sym_bytes = strlen(str) + 1;
    if (write_byte(v, S_SYM_STRING) &&
        write_bytes(v, (uint8_t*)str, sym_bytes)) {
      res = true;
    }
  }
  return res;
}

// Potentially a difference between 32/64 bit version.
// strlen returns size_t which is different on 32/64 bit platforms.
int f_sym_string_bytes(lbm_value sym) {
  int res = FLATTEN_VALUE_ERROR_FATAL;
  if (lbm_is_symbol(sym)) {
    lbm_uint s = lbm_dec_sym(sym);
    char *sym_str = (char*)lbm_get_name_by_symbol(s);
    if (sym_str) {
      lbm_uint sym_bytes = strlen(sym_str) + 1;
      res = (int)sym_bytes;
    }
  }
  return res;
}

bool f_i(lbm_flat_value_t *v, lbm_int i) {
  bool res = true;
#ifndef LBM64
  res = res && write_byte(v,S_I28_VALUE);
  res = res && write_word(v,(uint32_t)i);
#else
  res = res && write_byte(v,S_I56_VALUE);
  res = res && write_dword(v, (uint64_t)i);
#endif
  return res;
}

bool f_u(lbm_flat_value_t *v, lbm_uint u) {
  bool res = true;
#ifndef LBM64
  res = res && write_byte(v,S_U28_VALUE);
  res = res && write_word(v,(uint32_t)u);
#else
  res = res && write_byte(v,S_U56_VALUE);
  res = res && write_dword(v,(uint64_t)u);
#endif
  return res;
}

bool f_b(lbm_flat_value_t *v, uint8_t b) {
  bool res = true;
  res = res && write_byte(v,S_BYTE_VALUE);
  res = res && write_byte(v,b);
  return res;
}

bool f_i32(lbm_flat_value_t *v, int32_t w) {
  bool res = true;
  res = res && write_byte(v, S_I32_VALUE);
  res = res && write_word(v, (uint32_t)w);
  return res;
}

bool f_u32(lbm_flat_value_t *v, uint32_t w) {
  bool res = true;
  res = res && write_byte(v, S_U32_VALUE);
  res = res && write_word(v, w);
  return res;
}

bool f_float(lbm_flat_value_t *v, float f) {
  bool res = true;
  res = res && write_byte(v, S_FLOAT_VALUE);
  uint32_t u;
  memcpy(&u, &f, sizeof(uint32_t));
  res = res && write_word(v, (uint32_t)u);
  return res;
}

bool f_double(lbm_flat_value_t *v, double d) {
  bool res = true;
  res = res && write_byte(v, S_DOUBLE_VALUE);
  uint64_t u;
  memcpy(&u, &d, sizeof(uint64_t));
  res = res && write_dword(v, u);
  return res;
}

bool f_i64(lbm_flat_value_t *v, int64_t w) {
  bool res = true;
  res = res && write_byte(v, S_I64_VALUE);
  res = res && write_dword(v, (uint64_t)w);
  return res;
}

bool f_u64(lbm_flat_value_t *v, uint64_t w) {
  bool res = true;
  res = res && write_byte(v, S_U64_VALUE);
  res = res && write_dword(v, w);
  return res;
}

// num_bytes is specifically an uint32_t
bool f_lbm_array(lbm_flat_value_t *v, uint32_t num_bytes, uint8_t *data) {
  bool res = write_byte(v, S_LBM_ARRAY);
  res = res && write_word(v, num_bytes);
  res = res && write_bytes(v, data, num_bytes);
  return res;
}

static int flatten_maximum_depth = FLATTEN_VALUE_MAXIMUM_DEPTH;

void lbm_set_max_flatten_depth(int depth) {
  flatten_maximum_depth = depth;
}

int lbm_get_max_flatten_depth(void) {
  return flatten_maximum_depth;
}

void flatten_error(jmp_buf jb, int val) {
  longjmp(jb, val);
}

int flatten_value_size_internal(jmp_buf jb, lbm_value v, int depth, bool image) {
  if (depth > flatten_maximum_depth) {
    flatten_error(jb, FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH);
  }

  lbm_uint t = lbm_type_of(v);
  if (t >= LBM_POINTER_TYPE_FIRST && t < LBM_POINTER_TYPE_LAST) {
    //  Clear constant bit, it is irrelevant to flattening
    t = t & ~(LBM_PTR_TO_CONSTANT_BIT);
  }

  if (image && lbm_is_ptr(v) && (v & LBM_PTR_TO_CONSTANT_BIT)) {
    // If flattening to image, constants can be stored by reference.
    return (sizeof(lbm_uint) + 1); // one byte tag, one word ptr
  }

  switch (t) {
  case LBM_TYPE_CONS: {
    int res = 0;
    int s1 = flatten_value_size_internal(jb,lbm_car(v), depth + 1, image);
    if (s1 > 0) {
      int s2 = flatten_value_size_internal(jb,lbm_cdr(v), depth + 1, image);
      if (s2 > 0) {
        res = (1 + s1 + s2);
      }
    }
    return res;
  }
  case LBM_TYPE_LISPARRAY: {
    int sum = 4 + 1; // sizeof(uint32_t) + 1;
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(v);
    if (header) {
      lbm_value *arrdata = (lbm_value*)header->data;
      lbm_uint size = header->size / sizeof(lbm_value);
      for (lbm_uint i = 0; i < size; i ++ ) {
        sum += flatten_value_size_internal(jb, arrdata[i], depth + 1, image);
      }
    } else {
      flatten_error(jb, FLATTEN_VALUE_ERROR_ARRAY);
    }
    return sum;
  }
  case LBM_TYPE_BYTE:
    return 1 + 1;
  case LBM_TYPE_U: /* fall through */
  case LBM_TYPE_I:
#ifndef LBM64
    return 1 + 4;
#else
    return 1 + 8;
#endif
  case LBM_TYPE_U32: /* fall through */
  case LBM_TYPE_I32:
  case LBM_TYPE_FLOAT:
    return 1 + 4;
  case LBM_TYPE_U64: /* fall through */
  case LBM_TYPE_I64:
  case LBM_TYPE_DOUBLE:
    return 1 + 8;
  case LBM_TYPE_SYMBOL: {
    if (!image) {
      int s = f_sym_string_bytes(v);
      if (s > 0) return 1 + s;
      flatten_error(jb, (int)s);
    } else {
      return 1 + sizeof(lbm_uint);
    }
  } return 0; // already terminated with error
  case LBM_TYPE_ARRAY: {
    // Platform dependent size.
    // TODO: Something needs to be done to these inconsistencies.
    lbm_int s = lbm_heap_array_get_size(v);
    if (s > 0)
      return 1 + 4 + (int)s;
    flatten_error(jb, (int)s);
  } return 0; // already terminated with error
  default:
    return FLATTEN_VALUE_ERROR_CANNOT_BE_FLATTENED;
  }
}

int flatten_value_size(lbm_value v, bool image) {
  jmp_buf jb;
  int r = setjmp(jb);
  if (r != 0) {
    return r;
  }
  return flatten_value_size_internal(jb, v, 0, image);
}

int flatten_value_c(lbm_flat_value_t *fv, lbm_value v) {

  lbm_uint t = lbm_type_of(v);
  if (t >= LBM_POINTER_TYPE_FIRST && t < LBM_POINTER_TYPE_LAST) {
    //  Clear constant bit, it is irrelevant to flattening
    t = t & ~(LBM_PTR_TO_CONSTANT_BIT);
  }

  switch (t) {
  case LBM_TYPE_CONS: {
    bool res = true;
    res = res && f_cons(fv);
    if (res) {
      int fv_r = flatten_value_c(fv, lbm_car(v));
      if (fv_r == FLATTEN_VALUE_OK) {
        fv_r = flatten_value_c(fv, lbm_cdr(v));
      }
      return fv_r;
    }
  }break;
  case LBM_TYPE_LISPARRAY: {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(v);
    if (header) {
      lbm_value *arrdata = (lbm_value*)header->data;
      // always exact multiple of sizeof(lbm_value)
      uint32_t size = (uint32_t)(header->size / sizeof(lbm_value));
      if (!f_lisp_array(fv, size)) return FLATTEN_VALUE_ERROR_NOT_ENOUGH_MEMORY;
      int fv_r = FLATTEN_VALUE_OK;
      for (lbm_uint i = 0; i < size; i ++ ) {
        fv_r =  flatten_value_c(fv, arrdata[i]);
        if (fv_r != FLATTEN_VALUE_OK) {
          break;
        }
      }
      return fv_r;
    } else {
      return FLATTEN_VALUE_ERROR_ARRAY;
    }
  } break;
  case LBM_TYPE_BYTE:
    if (f_b(fv, (uint8_t)lbm_dec_as_char(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_U:
    if (f_u(fv, lbm_dec_u(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_I:
    if (f_i(fv, lbm_dec_i(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_U32:
    if (f_u32(fv, lbm_dec_as_u32(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_I32:
    if (f_i32(fv, lbm_dec_as_i32(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_U64:
    if (f_u64(fv, lbm_dec_as_u64(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_I64:
    if (f_i64(fv, lbm_dec_as_i64(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_FLOAT:
    if (f_float(fv, lbm_dec_as_float(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_DOUBLE:
    if (f_double(fv, lbm_dec_as_double(v))) {
      return FLATTEN_VALUE_OK;
    }
    break;
  case LBM_TYPE_SYMBOL: {
    char *sym_str = (char*)lbm_get_name_by_symbol(lbm_dec_sym(v));
    if (f_sym_string(fv, sym_str)) {
      return FLATTEN_VALUE_OK;
    }
  } break;
  case LBM_TYPE_ARRAY: {
    lbm_int s = lbm_heap_array_get_size(v);
    const uint8_t *d = lbm_heap_array_get_data_ro(v);
    if (s > 0 && d != NULL) {
      if (f_lbm_array(fv, (uint32_t)s, (uint8_t*)d)) {
        return FLATTEN_VALUE_OK;
      }
    } else {
      return FLATTEN_VALUE_ERROR_ARRAY;
    }
  }break;
  default:
    return FLATTEN_VALUE_ERROR_CANNOT_BE_FLATTENED;
  }
  return FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL;
}

lbm_value handle_flatten_error(int err_val) {
  switch (err_val) {
  case FLATTEN_VALUE_ERROR_CANNOT_BE_FLATTENED:
    return ENC_SYM_EERROR;
  case FLATTEN_VALUE_ERROR_BUFFER_TOO_SMALL: /* fall through */
  case FLATTEN_VALUE_ERROR_FATAL:
    return ENC_SYM_FATAL_ERROR;
  case FLATTEN_VALUE_ERROR_CIRCULAR: /* fall through */
  case FLATTEN_VALUE_ERROR_MAXIMUM_DEPTH:
    return ENC_SYM_EERROR;
  case FLATTEN_VALUE_ERROR_ARRAY: /* fall through */
  case FLATTEN_VALUE_ERROR_NOT_ENOUGH_MEMORY:
    return ENC_SYM_MERROR;
  }
  return ENC_SYM_NIL;
}

lbm_value flatten_value(lbm_value v) {

  lbm_value array_cell = lbm_heap_allocate_cell(LBM_TYPE_CONS, ENC_SYM_NIL, ENC_SYM_ARRAY_TYPE);

  if (array_cell == ENC_SYM_MERROR) {
    return array_cell;
  }

  lbm_flat_value_t fv;

  lbm_array_header_t *array = NULL;
  int required_mem = flatten_value_size(v, false);
  if (required_mem > 0) {
    array = (lbm_array_header_t *)lbm_malloc(sizeof(lbm_array_header_t));
    if (array == NULL) {
      lbm_set_car_and_cdr(array_cell, ENC_SYM_NIL, ENC_SYM_NIL);
      return ENC_SYM_MERROR;
    }

    bool r = lbm_start_flatten(&fv, (lbm_uint)required_mem);
    if (!r) {
      lbm_free(array);
      lbm_set_car_and_cdr(array_cell, ENC_SYM_NIL, ENC_SYM_NIL);
      return ENC_SYM_MERROR;
    }

    if (flatten_value_c(&fv, v) == FLATTEN_VALUE_OK) {
      // it would be wasteful to run finish_flatten here.
      r = true;
    } else {
      r = false;
    }

    if (r)  {
      // lift flat_value
      array->data = (lbm_uint*)fv.buf;
      array->size = fv.buf_size;
      lbm_set_car(array_cell, (lbm_uint)array);
      array_cell = lbm_set_ptr_type(array_cell, LBM_TYPE_ARRAY);
      return array_cell;
    }
  }
  lbm_set_car_and_cdr(array_cell, ENC_SYM_NIL, ENC_SYM_NIL);
  return handle_flatten_error(required_mem);
}

// ------------------------------------------------------------
// Unflattening
static bool extract_byte(lbm_flat_value_t *v, uint8_t *r) {
  if (v->buf_size >= v->buf_pos + 1) {
    *r = v->buf[v->buf_pos++];
    return true;
  }
  return false;
}

static bool extract_word(lbm_flat_value_t *v, uint32_t *r) {
  bool res = false;
  if (v->buf_size >= v->buf_pos + 4) {
    uint32_t tmp = 0;
    tmp |= (lbm_value)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint32_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint32_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint32_t)v->buf[v->buf_pos++];
    *r = tmp;
    res = true;
  }
  return res;
}

static bool extract_dword(lbm_flat_value_t *v, uint64_t *r) {
  bool res = false;
  if (v->buf_size >= v->buf_pos + 8) {
    uint64_t tmp = 0;
    tmp |= (lbm_value)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint64_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint64_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint64_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint64_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint64_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint64_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint64_t)v->buf[v->buf_pos++];
    *r = tmp;
    res = true;;
  }
  return res;
}

static int lbm_unflatten_value_atom(lbm_flat_value_t *v, lbm_value *res) {

  uint8_t curr = v->buf[v->buf_pos++];

  if (v->buf_size <= v->buf_pos) return UNFLATTEN_MALFORMED;

  switch(curr) {
  case S_CONS: {
    return UNFLATTEN_MALFORMED;
  }
  case S_CONSTANT_REF: {
    lbm_uint tmp;
    bool b;
#ifndef LBM64
    b = extract_word(v, &tmp);
#else
    b = extract_dword(v, &tmp);
#endif
    if (b) {
      *res = tmp;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_SYM_VALUE: {
    lbm_uint tmp;
    bool b;
#ifndef LBM64
    b = extract_word(v, &tmp);
#else
    b = extract_dword(v, &tmp);
#endif
    if (b) {
      *res = lbm_enc_sym(tmp);
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_BYTE_VALUE: {
    uint8_t tmp;
    bool b = extract_byte(v, &tmp);
    if (b) {
      *res = lbm_enc_char((uint8_t)tmp);
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_I28_VALUE: {
    uint32_t tmp;
    bool b;
    b = extract_word(v, &tmp);
    if (b) {
      *res = lbm_enc_i((int32_t)tmp);
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_U28_VALUE: {
    uint32_t tmp;
    bool b;
    b = extract_word(v, &tmp);
    if (b) {
      *res = lbm_enc_u((uint32_t)tmp);
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_I56_VALUE: {
    uint64_t tmp;
    bool b;
    b = extract_dword(v, &tmp);
    if (b) {
#ifndef LBM64
      *res = lbm_enc_i64((int64_t)tmp);
#else
      *res = lbm_enc_i((int64_t)tmp);
#endif
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_U56_VALUE: {
    uint64_t tmp;
    bool b;
    b = extract_dword(v, &tmp);
    if (b) {
#ifndef LBM64
      *res = lbm_enc_u64(tmp);
#else
      *res = lbm_enc_u(tmp);
#endif
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_FLOAT_VALUE: {
    uint32_t tmp;
    bool b;
    b = extract_word(v, &tmp);
    if (b) {
      lbm_float f;
      memcpy(&f, &tmp, sizeof(lbm_float));
      lbm_value im  = lbm_enc_float(f);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_DOUBLE_VALUE: {
    uint64_t tmp;
    bool b;
    b = extract_dword(v, &tmp);
    if (b) {
      double f;
      memcpy(&f, &tmp, sizeof(uint64_t));
      lbm_value im  = lbm_enc_double(f);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_I32_VALUE: {
   uint32_t tmp;
    if (extract_word(v, &tmp)) {
      lbm_value im = lbm_enc_i32((int32_t)tmp);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_U32_VALUE: {
    uint32_t tmp;
    if (extract_word(v, &tmp)) {
      lbm_value im = lbm_enc_u32(tmp);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_I64_VALUE: {
   uint64_t tmp = 0;
    if (extract_dword(v, &tmp)) {
      lbm_value im = lbm_enc_i64((int64_t)tmp);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_U64_VALUE: {
    uint64_t tmp = 0;
    if (extract_dword(v, &tmp)) {
      lbm_value im = lbm_enc_u64(tmp);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_LBM_ARRAY: {
    uint32_t num_elt;
    // TODO: Feels slightly wrong with <= here.
    if (extract_word(v, &num_elt) && v->buf_pos + num_elt <= v->buf_size) {  
      if (lbm_heap_allocate_array(res, num_elt)) {
        lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(*res);
        lbm_uint num_bytes = num_elt;
        memcpy(arr->data, v->buf + v->buf_pos, num_bytes);
        v->buf_pos += num_bytes;
      } else {
        return UNFLATTEN_GC_RETRY;
      }
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_SYM_STRING: {
    lbm_uint sym_id;
    lbm_uint max_bytes = v->buf_size - v->buf_pos;
    lbm_uint num_bytes = max_bytes;
    bool found_null = false;
    for (lbm_uint i = 0; i < max_bytes; i ++) {
      if (v->buf[v->buf_pos + i] == 0) {
        num_bytes = i + 1;
        found_null = true;
        break;
      }
    }
    if (!found_null) return UNFLATTEN_MALFORMED;
    if (lbm_add_symbol((char *)(v->buf + v->buf_pos), &sym_id)) {
      v->buf_pos += num_bytes;
      *res = lbm_enc_sym(sym_id);
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_GC_RETRY;
  }
  default:
    return UNFLATTEN_MALFORMED;
  }
}

// ////////////////////////////////////////////////////////////
// Pointer-reversal-esque "stackless" deserialization of
// flattened (serialized) trees.
//
// Initially:
//   curr = LBM_NULL;    v->buf = { ... }
//
// FORWARDS PHASE: 
// Cons case:
//   Reading conses from the buffer builds a backpointing list.
//   Placeholder element acts as a 1 bit "visited" field.
//
//   curr = p;   v->buf = {S_CONS, ... }
//   =>
//   curr = [p, placeholder]; v->buf = { ... }
//
// Lisp array case:
//   An Array tag in the buffer leads to the creation of an array
//   with a backptr in the last element position. Placeholder element
//   is not needed as LBM-Arrays have a built in index field (used by GC)
//   that can keep a count of how far along the array we have progressed.
//
//   curr = p;  v->buf = {S_LBM_LISP_ARRAY, ... }
//   =>
//   curr = [| nil ... p |]; v->buf = { ... }
//
// Atom case:
//   Reading an atom triggers a backwards traversal along the backpointer
//   structure.
//
//   curr = X;   v->buf = {any_atom, ... } example integer, string.
//   =>
//   val = unflatten_atom(v->buf);      v->buf = { ... }
//
//   BACKWARDS PHASE: Start the backwards traversal:
//
//   Case on X
//     LBM_NULL;
//     => Done! result = val
//
//     [p, placeholder];
//     =>
//     [p, val]   Base case. Finishes back traversal.
//                Go back to FORWARDS PHASE.
//
//
//     [p, val0];
//     =>
//     tmp = [val0, val];  val = tmp;  curr = p;   continue backwards with value pointing to recently constructed final subresult.
//
//
//     [| a b nil ... p |]
//     =>
//     [| a b val ... p |]   Base case. Finishes back traversal.
//                           Array internal index field keeps track of write position.
//                           Go back to FORWARDS PHASE.
//
//
//    [| a0 a1 ... an p |]
//    =>
//    tmp =  [| a0 a1 ... an val |];  val = tmp; curr = p; continue backwards
//

static int lbm_unflatten_value_nostack(sharing_table *st, lbm_uint *target_map, lbm_flat_value_t *v, lbm_value *res) {
  bool done = false;
  lbm_value val0;
  lbm_value curr = lbm_enc_cons_ptr(LBM_PTR_NULL);
  while (!done) {
    int32_t set_ix = -1;
    if (v->buf[v->buf_pos] == S_SHARED) {
      v->buf_pos++;
      if (st && target_map) {
        bool b = false;
        lbm_uint tmp;
#ifndef LBM64
        b = extract_word(v, &tmp);
#else
        b = extract_dword(v, &tmp);
#endif
        if (b) {
          int32_t ix = sharing_table_contains(st, tmp);
          if (ix >= 0) {
            set_ix = ix;
          } else {
            return UNFLATTEN_SHARING_TABLE_ERROR;
          }
        } else {
          return UNFLATTEN_MALFORMED;
        }
      } else {
        return UNFLATTEN_SHARING_TABLE_REQUIRED;
      }
    }

    bool is_leaf = true;
    lbm_value unflattened = ENC_SYM_NIL;

    if (v->buf[v->buf_pos] == S_CONS) {
      lbm_value tmp = curr;
      curr = lbm_cons(tmp, ENC_SYM_PLACEHOLDER);
      if (lbm_is_symbol_merror(curr)) return UNFLATTEN_GC_RETRY;
      if (set_ix >= 0) target_map[set_ix] = curr;
      v->buf_pos ++;
      is_leaf = false;
    } else if (v->buf[v->buf_pos] == S_LBM_LISP_ARRAY) {
      uint32_t size;
      v->buf_pos ++;
      bool b = extract_word(v, &size);
      if (b) {
        // Abort if buffer cannot possibly hold that size array.
        // a flattened byte occupies 2 bytes in fv. so smallest possible
        // array is array of bytes. 
        if (size > 0 && v->buf_pos + (size * 2) > v->buf_size) return UNFLATTEN_MALFORMED;
        lbm_value array;
        lbm_heap_allocate_lisp_array(&array, size);
        lbm_array_header_extended_t *header = (lbm_array_header_extended_t*)lbm_car(array);
        if (size == 0) {
          unflattened = array;
          if (set_ix >= 0) target_map[set_ix] = array;
        } else {
          is_leaf = false;
          lbm_value *arrdata = (lbm_value*)header->data;
          if (lbm_is_symbol_merror(array)) return UNFLATTEN_GC_RETRY;
          header->index = 0;
          arrdata[size-1] = curr; // backptr
          curr = array;
          if (set_ix >= 0) target_map[set_ix] = curr;
        }
      } else {
        return UNFLATTEN_MALFORMED;
      }
    } else if (v->buf[v->buf_pos] == 0) {
      return UNFLATTEN_MALFORMED;
    } else if (v->buf[v->buf_pos] == S_REF) {
      v->buf_pos++;
      if (st && target_map) {
        bool b = false;
        lbm_uint tmp;
#ifndef LBM64
        b = extract_word(v, &tmp);
#else
        b = extract_dword(v, &tmp);
#endif
        if (b) {
          // Shared should have been hit before S_REF. So just look up index and copy from
          // the target_map.
          int32_t ix = sharing_table_contains(st, tmp);
          if (ix >= 0) {
            //curr = target_map[ix];
            unflattened = target_map[ix];
          } else {
            return UNFLATTEN_SHARING_TABLE_ERROR;
          }
        } else {
          return UNFLATTEN_MALFORMED;
        }
      } else {
        return UNFLATTEN_SHARING_TABLE_REQUIRED;
      }
    } else {
      int e_val = lbm_unflatten_value_atom(v, &unflattened);
      if (set_ix >= 0) {
        target_map[set_ix] = unflattened;
      }
      if (e_val != UNFLATTEN_OK) {
        return e_val;
      }
    }

    if (is_leaf) {
      val0 = unflattened;
      while (lbm_dec_ptr(curr) != LBM_PTR_NULL &&
             lbm_cdr(curr) != ENC_SYM_PLACEHOLDER) { // has done left
        if ( lbm_type_of(curr) == LBM_TYPE_LISPARRAY) {
          lbm_array_header_extended_t *header = (lbm_array_header_extended_t*)lbm_car(curr);
          lbm_value *arrdata = (lbm_value*)header->data;
          uint32_t arrlen = header->size / sizeof(lbm_value);
          if (header->index == arrlen - 1) {
            lbm_value prev = arrdata[arrlen-1];
            header->index = 0;
            arrdata[arrlen-1] = val0;
            val0 = curr;
            curr = prev;
          } else {
            arrdata[header->index++] = val0;
            break;
          }
        } else {
          lbm_value prev = lbm_car(curr);
          lbm_value r0   = lbm_cdr(curr);
          lbm_set_cdr(curr, val0);
          lbm_set_car(curr, r0);
          val0 = curr;
          curr = prev;
        }
      }
      if (lbm_dec_ptr(curr) == LBM_PTR_NULL) {
        *res = val0; // done
        break;
      } else if (lbm_type_of(curr) == LBM_TYPE_LISPARRAY) {
        // Do nothing in this case. It has been arranged..
      } else if (lbm_cdr(curr) == ENC_SYM_PLACEHOLDER) {
        lbm_set_cdr(curr, val0);
      } else {
        return UNFLATTEN_MALFORMED;
      }
    }
  }
  return UNFLATTEN_OK;
}

/* lbm_unflatten_value_nostack, does not backtrack
   upon error to swap pointer to the correct direction
   and to remove the LBM_PTR_NULL tag.

   unflatten_value_nostack does not alter the GC_FLAG or the GC_MARK,
   So really only reverse pointers and the NIL tag could be
   potential problems.
*/
bool lbm_unflatten_value(lbm_flat_value_t *v, lbm_value *res) {
  bool b = false;
#ifdef LBM_ALWAYS_GC
  lbm_perform_gc();
#endif
  int r = lbm_unflatten_value_nostack(NULL,NULL, v,res);
  if (r == UNFLATTEN_GC_RETRY) {
    lbm_perform_gc();
    v->buf_pos = 0;
    r = lbm_unflatten_value_nostack(NULL,NULL,v,res);
  }
  switch(r) {
  case UNFLATTEN_OK:
    b = true;
    break;
  case UNFLATTEN_GC_RETRY:
    *res = ENC_SYM_MERROR;
    break;
  default:
    *res = ENC_SYM_EERROR;
    break;
  }
  // Do not free the flat value buffer here.
  // there are 2 cases:
  // 1: unflatten was called from lisp code -> GC removes the buffer.
  // 2: unflatten called from event processing -> event processor frees buffer.
  return b;
}

bool lbm_unflatten_value_sharing(sharing_table *st, lbm_uint *target_map, lbm_flat_value_t *v, lbm_value *res) {
  bool b = false;
#ifdef LBM_ALWAYS_GC
  lbm_perform_gc();
#endif
  int r = lbm_unflatten_value_nostack(st,target_map, v,res);
  if (r == UNFLATTEN_GC_RETRY) {
    lbm_perform_gc();
    v->buf_pos = 0;
    r = lbm_unflatten_value_nostack(st,target_map,v,res);
  }
  switch(r) {
  case UNFLATTEN_OK:
    b = true;
    break;
  case UNFLATTEN_GC_RETRY:
    *res = ENC_SYM_MERROR;
    break;
  default:
    *res = ENC_SYM_EERROR;
    break;
  }
  // Do not free the flat value buffer here.
  // there are 2 cases:
  // 1: unflatten was called from lisp code -> GC removes the buffer.
  // 2: unflatten called from event processing -> event processor frees buffer.
  return b;
}
