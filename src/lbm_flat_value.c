/*
    Copyright 2023 Joel Svensson    svenssonjoel@yahoo.se
              2023 Benjamin Vedder

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


// ------------------------------------------------------------
// Access to GC from eval_cps
int lbm_perform_gc(void);


// ------------------------------------------------------------
// Flatteners

bool lbm_start_flatten(lbm_flat_value_t *v, size_t buffer_size) {

  uint8_t *data = lbm_malloc_reserve(buffer_size);
  if (!data) return false;

  v->buf = data;
  v->buf_size = buffer_size;
  v->buf_pos = 0;
  return true;
}

bool lbm_finish_flatten(lbm_flat_value_t *v) {

  lbm_uint size_words;

  if (v->buf_pos % sizeof(lbm_uint) == 0) {
    size_words = v->buf_pos / sizeof(lbm_uint);
  } else {
    size_words = (v->buf_pos / sizeof(lbm_uint)) + 1;
  }

  return (lbm_memory_shrink((lbm_uint*)v->buf, size_words) >= 0);
}

static bool write_byte(lbm_flat_value_t *v, uint8_t b) {
  if (v->buf_size >= v->buf_pos + 1) {
    v->buf[v->buf_pos++] = b;
    return true;
  }
  return false;
}

static bool write_word(lbm_flat_value_t *v, uint32_t w) {

  if (v->buf_size >= v->buf_pos + 4) {
    v->buf[v->buf_pos++] = (uint8_t)(w >> 24);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 16);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 8);
    v->buf[v->buf_pos++] = (uint8_t)w;
    return true;
  }
  return false;
}

static bool write_dword(lbm_flat_value_t *v, uint64_t w) {
  if (v->buf_size >= v->buf_pos + 8) {
    v->buf[v->buf_pos++] = (uint8_t)(w >> 56);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 48);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 40);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 32);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 24);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 16);
    v->buf[v->buf_pos++] = (uint8_t)(w >> 8);
    v->buf[v->buf_pos++] = (uint8_t)w;
    return true;
  }
  return false;
}

bool f_cons(lbm_flat_value_t *v) {
  if (v->buf_size >= v->buf_pos + 1) {
    v->buf[v->buf_pos++] = S_CONS;
    return true;
  }
  return false;
}

bool f_sym(lbm_flat_value_t *v, lbm_uint sym) {
  bool res = true;
  res = res && write_byte(v,S_SYM_VALUE);
  #ifndef LBM64
  res = res && write_word(v,sym);
  #else
  res = res && write_dword(v,sym);
  #endif
  return res;
}

bool f_i(lbm_flat_value_t *v, lbm_int i) {
  bool res = true;
  res = res && write_byte(v,S_I_VALUE);
  res = res && write_word(v,(uint32_t)i);
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

bool f_lbm_array(lbm_flat_value_t *v, uint32_t num_elts, lbm_uint t, uint8_t *data) {
  bool res = true;
  res = res && write_byte(v, S_LBM_ARRAY);
  res = res && write_word(v, num_elts);
#ifndef LBM64
  res = res && write_word(v, t);
#else
  res = res && write_dword(v, t);
#endif
  uint32_t num_bytes = num_elts;
  num_bytes *= lbm_size_of(t);
  if (res && v->buf_size >= v->buf_pos + num_bytes) {
    memcpy(v->buf + v->buf_pos, data, num_bytes);
    v->buf_pos += num_bytes;
  } else {
    res = false;
  }
  return res;
}


// ------------------------------------------------------------
// Unflattening
static bool extract_word(lbm_flat_value_t *v, uint32_t *r) {
  if (v->buf_size >= v->buf_pos + 4) {
    uint32_t tmp = 0;
    tmp |= (lbm_value)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint32_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint32_t)v->buf[v->buf_pos++];
    tmp = tmp << 8 | (uint32_t)v->buf[v->buf_pos++];
    *r = tmp;
    return true;
  }
  return false;
}

static bool extract_dword(lbm_flat_value_t *v, uint64_t *r) {
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
    return true;
  }
  return false;
}

#define UNFLATTEN_MALFORMED     -2
#define UNFLATTEN_GC_RETRY      -1
#define UNFLATTEN_OK             0

/* Recursive and potentially stack hungry for large flat values */
static int lbm_unflatten_value_internal(lbm_flat_value_t *v, lbm_value *res) {
  if (v->buf_size == v->buf_pos) return UNFLATTEN_MALFORMED;
  uint8_t curr = v->buf[v->buf_pos++];

  switch(curr) {
  case S_CONS: {
    lbm_value a;
    lbm_value b;
    int r = lbm_unflatten_value_internal(v, &a);
    if (r == UNFLATTEN_OK) {
      r = lbm_unflatten_value_internal(v, &b);
      if (r == UNFLATTEN_OK) {
        lbm_value c;
        c = lbm_cons(a,b);
        if (lbm_is_symbol_merror(c)) return UNFLATTEN_GC_RETRY;
        *res = c;
        r = UNFLATTEN_OK;
      }
    }
    return r;
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
  case S_I_VALUE: {
    lbm_uint tmp;
    bool b;
#ifndef LBM64
    b = extract_word(v, &tmp);
#else
    b = extract_dword(v, &tmp);
#endif
    if (b) {
      *res = lbm_enc_i((int32_t)tmp);
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_U_VALUE: {
    lbm_uint tmp;
    bool b;
#ifndef LBM64
    b = extract_word(v, &tmp);
#else
    b = extract_dword(v, &tmp);
#endif
    if (b) {
      *res = lbm_enc_u((uint32_t)tmp);
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_FLOAT_VALUE: {
    lbm_uint tmp;
    if (extract_word(v, &tmp)) {
      float f;
      memcpy(&f, &tmp, sizeof(float));
      lbm_value im  = lbm_enc_float(f);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_I32_VALUE: {
   lbm_uint tmp;
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
    lbm_uint tmp;
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
   uint64_t tmp;
    if (extract_dword(v, &tmp)) {
      lbm_value im = lbm_enc_i64((int32_t)tmp);
      if (lbm_is_symbol_merror(im)) {
        return UNFLATTEN_GC_RETRY;
      }
      *res = im;
      return UNFLATTEN_OK;
    }
    return UNFLATTEN_MALFORMED;
  }
  case S_U64_VALUE: {
    uint64_t tmp;
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
    lbm_uint t;
    if (extract_word(v, &num_elt)) {
      bool b;
#ifndef LBM64
      b = extract_word(v,&t);
#else
      b = extract_dword(v,&t);
#endif
      if (b) {
        if (lbm_heap_allocate_array(res, num_elt, t)) {
          lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(*res);
          lbm_uint num_bytes = num_elt;
          num_bytes *= lbm_size_of(t);
          memcpy(arr->data, v->buf + v->buf_pos, num_bytes);
          v->buf_pos += num_bytes;
        } else {
          return UNFLATTEN_GC_RETRY;
        }
        return UNFLATTEN_OK;
      }
    }
    return UNFLATTEN_MALFORMED;
  }
  default:
    return UNFLATTEN_MALFORMED;
  }
}

bool lbm_unflatten_value(lbm_flat_value_t *v, lbm_value *res) {
  bool b = false;
  int r = lbm_unflatten_value_internal(v,res);
  if (r == UNFLATTEN_GC_RETRY) {
    lbm_perform_gc();
    v->buf_pos = 0;
    r = lbm_unflatten_value_internal(v,res);
  }
  if (r == UNFLATTEN_MALFORMED) {
    *res = ENC_SYM_EERROR;
  } else if (r == UNFLATTEN_GC_RETRY) {
    *res = ENC_SYM_MERROR;
  } else {
    b = true;
  }
  lbm_free(v->buf);
  return b;
}
