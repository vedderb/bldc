/*
    Copyright 2022 - 2025 Joel Svensson        svenssonjoel@yahoo.se
    Copyright 2022, 2023  Benjamin Vedder

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

#include "extensions/array_extensions.h"

#include "extensions.h"
#include "symrepr.h"
#include "lbm_memory.h"

#include <math.h>

#ifdef LBM_OPT_ARRAY_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_ARRAY_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

static lbm_uint little_endian = 0;
static lbm_uint big_endian = 0;
static lbm_uint dbc_sym = 0;

static lbm_value array_extension_unsafe_free_array(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_i8(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_i16(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_i32(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_u8(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_u16(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_u24(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_u32(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_append_f32(lbm_value *args, lbm_uint argn);

static lbm_value array_extension_buffer_get_i8(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_get_i16(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_get_i32(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_get_u8(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_get_u16(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_get_u24(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_get_u32(lbm_value *args, lbm_uint argn);
static lbm_value array_extension_buffer_get_f32(lbm_value *args, lbm_uint argn);

static lbm_value array_extension_buffer_length(lbm_value *args, lbm_uint argn);

static lbm_value array_extensions_bufclear(lbm_value *args, lbm_uint argn);
static lbm_value array_extensions_bufcpy(lbm_value *args, lbm_uint argn);
static lbm_value array_extensions_bufset_bit(lbm_value *args, lbm_uint argn);

static lbm_value array_extensions_bufset_bits_u(lbm_value *args, lbm_uint argn);
static lbm_value array_extensions_bufset_bits_i(lbm_value *args, lbm_uint argn);
static lbm_value array_extensions_bufget_bits_u(lbm_value *args, lbm_uint argn);
static lbm_value array_extensions_bufget_bits_i(lbm_value *args, lbm_uint argn);
static lbm_value array_extensions_bufset_bits_f32(lbm_value *args, lbm_uint argn);
static lbm_value array_extensions_bufget_bits_f32(lbm_value *args, lbm_uint argn);

void lbm_array_extensions_init(void) {

  lbm_add_symbol_const("little-endian", &little_endian);
  lbm_add_symbol_const("big-endian", &big_endian);
  lbm_add_symbol_const("dbc", &dbc_sym);

  lbm_add_extension("free", array_extension_unsafe_free_array);
  lbm_add_extension("bufset-i8", array_extension_buffer_append_i8);
  lbm_add_extension("bufset-i16", array_extension_buffer_append_i16);
  lbm_add_extension("bufset-i32", array_extension_buffer_append_i32);
  lbm_add_extension("bufset-u8", array_extension_buffer_append_u8);
  lbm_add_extension("bufset-u16", array_extension_buffer_append_u16);
  lbm_add_extension("bufset-u24", array_extension_buffer_append_u24);
  lbm_add_extension("bufset-u32", array_extension_buffer_append_u32);
  lbm_add_extension("bufset-f32", array_extension_buffer_append_f32);

  lbm_add_extension("bufget-i8", array_extension_buffer_get_i8);
  lbm_add_extension("bufget-i16", array_extension_buffer_get_i16);
  lbm_add_extension("bufget-i32", array_extension_buffer_get_i32);
  lbm_add_extension("bufget-u8", array_extension_buffer_get_u8);
  lbm_add_extension("bufget-u16", array_extension_buffer_get_u16);
  lbm_add_extension("bufget-u24", array_extension_buffer_get_u24);
  lbm_add_extension("bufget-u32", array_extension_buffer_get_u32);
  lbm_add_extension("bufget-f32", array_extension_buffer_get_f32);

  lbm_add_extension("buflen",  array_extension_buffer_length);
  lbm_add_extension("bufclear", array_extensions_bufclear);
  lbm_add_extension("bufcpy", array_extensions_bufcpy);
  lbm_add_extension("bufset-bit", array_extensions_bufset_bit);

  lbm_add_extension("bufset-bits-u", array_extensions_bufset_bits_u);
  lbm_add_extension("bufset-bits-i", array_extensions_bufset_bits_i);
  lbm_add_extension("bufget-bits-u", array_extensions_bufget_bits_u);
  lbm_add_extension("bufget-bits-i", array_extensions_bufget_bits_i);
  lbm_add_extension("bufset-bits-f32", array_extensions_bufset_bits_f32);
  lbm_add_extension("bufget-bits-f32", array_extensions_bufget_bits_f32);
}

lbm_value array_extension_unsafe_free_array(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn == 1) {
    if (lbm_is_array_rw(args[0])) {
      if (lbm_heap_explicit_free_array(args[0])) {
        res = ENC_SYM_TRUE;
      } else {
        res = ENC_SYM_NIL;
      }
    } else {
      res = ENC_SYM_TERROR;
    }
  }
  return res;
}

static bool decode_append_args(lbm_value *error, lbm_value *args, lbm_uint argn, lbm_uint *index, bool *be, lbm_uint *a_size, uint8_t **a_data) {
  *be = true;
  *error = ENC_SYM_EERROR;
  bool res = false;
  switch(argn) {
  case 4:
    if (lbm_type_of(args[3]) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(args[3]) == little_endian) {
      *be = false;
    }
    /* fall through */
  case 3: {
    lbm_array_header_t *array = lbm_dec_array_rw(args[0]);
    if(array &&
       lbm_is_number(args[1]) &&
       lbm_is_number(args[2])) {
      *a_size = array->size;
      *a_data = (uint8_t*)array->data;
      *index = lbm_dec_as_u32(args[1]);
      res = true;
    } else {
      *error = ENC_SYM_TERROR;
    }
  }
  }
  return res;
}

static bool buffer_append_bytes(uint8_t *data, lbm_uint d_size, bool be, lbm_uint index, lbm_uint nbytes, lbm_uint value) {

  lbm_uint last_index = index + (nbytes - 1);
  bool res = false;
  if (last_index < d_size) {
    res = true;
    switch(nbytes) {
    case 1:
      data[index]    = (uint8_t) value;
      break;
    case 2:
      if (be) {
        data[index+1]  = (uint8_t)value;
        data[index]    = (uint8_t)(value >> 8);
      } else {
        data[index]    = (uint8_t)value;
        data[index +1] = (uint8_t)(value >> 8);
      }
      break;
    case 3:
      if (be) {
        data[index+2]  = (uint8_t)value;
        data[index+1]  = (uint8_t)(value >> 8);
        data[index]    = (uint8_t)(value >> 16);
      } else {
        data[index]    = (uint8_t)value;
        data[index+1]  = (uint8_t)(value >> 8);
        data[index+2]  = (uint8_t)(value >> 16);
      }
      break;
    default:
      if (be) {
        data[index+3]  = (uint8_t) value;
        data[index+2]  = (uint8_t) (value >> 8);
        data[index+1]  = (uint8_t) (value >> 16);
        data[index]    = (uint8_t) (value >> 24);
      } else {
        data[index]    = (uint8_t) value;
        data[index+1]  = (uint8_t) (value >> 8);
        data[index+2]  = (uint8_t) (value >> 16);
        data[index+3]  = (uint8_t) (value >> 24);
      }
      break;
    }
  }
  return res;
}

lbm_value array_extension_buffer_append_i8(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 1, (lbm_uint)lbm_dec_as_i32(args[2]))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}

lbm_value array_extension_buffer_append_i16(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 2, (lbm_uint)lbm_dec_as_i32(args[2]))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}

lbm_value array_extension_buffer_append_i32(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 4, (lbm_uint)lbm_dec_as_i32(args[2]))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}


lbm_value array_extension_buffer_append_u8(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 1, (lbm_uint)lbm_dec_as_u32(args[2]))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}

lbm_value array_extension_buffer_append_u16(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 2, (lbm_uint)lbm_dec_as_u32(args[2]))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}

lbm_value array_extension_buffer_append_u24(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 3, (lbm_uint)lbm_dec_as_u32(args[2]))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}

lbm_value array_extension_buffer_append_u32(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 4, (lbm_uint)lbm_dec_as_u32(args[2]))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}

static lbm_uint float_to_u(float number) {
  // Set subnormal numbers to 0 as they are not handled properly
  // using this method.
  if (fabsf(number) < 1.5e-38) {
    number = 0.0;
  }

  int e = 0;
  float sig = frexpf(number, &e);
  float sig_abs = fabsf(sig);
  uint32_t sig_i = 0;

  if (sig_abs >= 0.5) {
    sig_i = (uint32_t)((sig_abs - 0.5f) * 2.0f * 8388608.0f);
    e += 126;
  }

  uint32_t res = (((uint32_t)e & 0xFFu) << 23) | (uint32_t)(sig_i & 0x7FFFFFu);
  if (sig < 0) {
    res |= 1U << 31;
  }

  return res;
}

static lbm_float u_to_float(uint32_t v) {

  int e = (v >> 23) & 0xFF;
  uint32_t sig_i = v & 0x7FFFFF;
  bool neg = v & (1U << 31);

  float sig = 0.0;
  if (e != 0 || sig_i != 0) {
    sig = (float)sig_i / (8388608.0f * 2.0f) + 0.5f;
    e -= 126;
  }

  if (neg) {
    sig = -sig;
  }

  return ldexpf(sig, e);
}

lbm_value array_extension_buffer_append_f32(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;

  if (decode_append_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_append_bytes(data, d_size, be, index, 4, (lbm_uint)float_to_u(lbm_dec_as_float(args[2])))) {
      res = ENC_SYM_TRUE;
    } 
  }
  return res;
}

/* (buffer-get-i8 buffer index) */
/* (buffer-get-i16 buffer index little-endian) */

static bool decode_get_args(lbm_value *error, lbm_value *args, lbm_uint argn, lbm_uint *index, bool *be, lbm_uint *a_size, uint8_t **a_data) {
  bool res = false;

  *be=true;

  switch(argn) {
  case 3:
    if (lbm_type_of(args[2]) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(args[2]) == little_endian) {
      *be = false;
    }
    /* fall through */
  case 2: {
    lbm_array_header_t *array = lbm_dec_array_r(args[0]);
    if (array &&
        lbm_is_number(args[1])) {
      *a_size = array->size;
      *a_data = (uint8_t*)array->data;
      *index = lbm_dec_as_u32(args[1]);
      res = true;
    } else {
      *error = ENC_SYM_TERROR;
    }
  }
  }
  return res;
}

static bool buffer_get_uint(lbm_uint *r_value, uint8_t *data, lbm_uint d_size, bool be, lbm_uint index, lbm_uint nbytes) {

  bool res = false;
  lbm_uint last_index = index + (nbytes - 1);

  if (last_index < d_size) {
    lbm_uint value = 0;
    res = true;
    switch(nbytes) {
    case 1:
      value = (lbm_uint)data[index];
      break;
    case 2:
      if (be) {
        value =
          (lbm_uint) data[index+1] |
          (lbm_uint) data[index] << 8;
      } else {
        value =
          (lbm_uint) data[index] |
          (lbm_uint) data[index+1] << 8;
      }
      break;
    case 3:
      if (be) {
        value =
          (lbm_uint) data[index+2] |
          (lbm_uint) data[index+1] << 8 |
          (lbm_uint) data[index] << 16;
      } else {
        value =
          (lbm_uint) data[index] |
          (lbm_uint) data[index+1] << 8 |
          (lbm_uint) data[index+2] << 16;
      }
      break;
    case 4:
      if (be) {
        value =
          (uint32_t) data[index+3] |
          (uint32_t) data[index+2] << 8 |
          (uint32_t) data[index+1] << 16 |
          (uint32_t) data[index] << 24;
      } else {
        value =
          (uint32_t) data[index] |
          (uint32_t) data[index+1] << 8 |
          (uint32_t) data[index+2] << 16 |
          (uint32_t) data[index+3] << 24;
      }
      break;
    default:
      res = false;
    }
    *r_value = value;
  }
  return res;
}



lbm_value array_extension_buffer_get_i8(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 1)) {
      res =lbm_enc_i((int8_t)value);
    }
  }
  return res;
}

lbm_value array_extension_buffer_get_i16(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 2)) {
      res =lbm_enc_i((int16_t)value);
    }
  }
  return res;
}

lbm_value array_extension_buffer_get_i32(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 4)) {
      res =lbm_enc_i((int32_t)value);
    }
  }
  return res;
}

lbm_value array_extension_buffer_get_u8(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 1)) {
      res = lbm_enc_i((uint8_t)value);
    }
  }
  return res;
}

lbm_value array_extension_buffer_get_u16(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 2)) {
      res = lbm_enc_i((uint16_t)value);
    }
  }
  return res;
}

lbm_value array_extension_buffer_get_u24(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 3)) {
      res = lbm_enc_i((int32_t)value);
    }
  }
  return res;
}

lbm_value array_extension_buffer_get_u32(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 4)) {
      res = lbm_enc_u32((uint32_t)value);
    }
  }
  return res;
}

lbm_value array_extension_buffer_get_f32(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  uint8_t *data = NULL;
  lbm_uint d_size = 0;
  bool be = false;
  lbm_uint index = 0;
  lbm_uint value = 0;

  if (decode_get_args(&res, args, argn, &index, &be, &d_size, &data)) {
    if (buffer_get_uint(&value, data, d_size, be, index, 4)) {
      res = lbm_enc_float(u_to_float((uint32_t)value));
    }
  }
  return res;
}

lbm_value array_extension_buffer_length(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  lbm_array_header_t *array;
  if (argn == 1 &&
      (array = lbm_dec_array_r(args[0])) &&
      lbm_heap_array_valid(args[0])) {
    res = lbm_enc_i((lbm_int)array->size);
  }
  return res;
}

//TODO: Have to think about 32 vs 64 bit here
static lbm_value array_extensions_bufclear(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  if (argn >= 1 && argn <= 4) {
    res = ENC_SYM_TERROR;
    if (lbm_is_array_rw(args[0])) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);

      uint8_t clear_byte = 0;
      if (argn >= 2) {
        if (!lbm_is_number(args[1])) {
          return res;
        }
        clear_byte = (uint8_t)lbm_dec_as_u32(args[1]);
      }

      uint32_t start = 0;
      if (argn >= 3) {
        if (!lbm_is_number(args[2])) {
          return res;
        }
        uint32_t start_new = lbm_dec_as_u32(args[2]);
        if (start_new < array->size) {
          start = start_new;
        } else {
          return res;
        }
      }
      // Truncates size on 64 bit build
      uint32_t len = (uint32_t)array->size - start;
      if (argn >= 4) {
        if (!lbm_is_number(args[3])) {
          return res;
        }
        uint32_t len_new = lbm_dec_as_u32(args[3]);
        if (len_new <= len) {
          len = len_new;
        }
      }

      memset((char*)array->data + start, clear_byte, len);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

static lbm_value array_extensions_bufcpy(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;

  if (argn == 5) {
    res = ENC_SYM_TERROR;
    if (lbm_is_array_rw(args[0]) && lbm_is_number(args[1]) &&
        lbm_is_array_r(args[2]) && lbm_is_number(args[3]) &&lbm_is_number(args[4])) {
      lbm_array_header_t *array1 = (lbm_array_header_t *)lbm_car(args[0]);

      uint32_t start1 = lbm_dec_as_u32(args[1]);

      lbm_array_header_t *array2 = (lbm_array_header_t *)lbm_car(args[2]);

      uint32_t start2 = lbm_dec_as_u32(args[3]);
      uint32_t len = lbm_dec_as_u32(args[4]);

      if (start1 < array1->size && start2 < array2->size) {
        if (len > (array1->size - start1)) {
          len = ((uint32_t)array1->size - start1);
        }
        if (len > (array2->size - start2)) {
          len = ((uint32_t)array2->size - start2);
        }

        memcpy((char*)array1->data + start1, (char*)array2->data + start2, len);
      }
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

static lbm_value array_extensions_bufset_bit(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;

  if (argn == 3) {
    res = ENC_SYM_TERROR;
    if (lbm_is_array_rw(args[0]) &&
        lbm_is_number(args[1]) && lbm_is_number(args[2])) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);

      unsigned int pos = lbm_dec_as_u32(args[1]);
      unsigned int bit = lbm_dec_as_u32(args[2]) ? 1 : 0;

      unsigned int bytepos = pos / 8;

      if (bytepos < array->size) {
        unsigned int bitpos = pos % 8;
        ((uint8_t*)array->data)[bytepos] &= (uint8_t)~(1 << bitpos);
        ((uint8_t*)array->data)[bytepos] |= (uint8_t)(bit << bitpos);
      }

      res = ENC_SYM_TRUE;
    }
  }
  return res;
}

/* extract/set sequence of bits at arbitrary bit positon
   within a byte-array. Alternatively allowing DBC bit order
   with using the 'dbc symbol argument.
*/

static lbm_uint dbc_bit_pos(lbm_uint pos) {
  return (7 - (pos % 8)) + (pos / 8) * 8;
}

static int64_t sign_extend64(uint64_t v, lbm_uint len) {
  if (len >= 64) return (int64_t)v;
  uint64_t sign_bit = (uint64_t)1 << (len - 1);
  return (int64_t)((v ^ sign_bit) - sign_bit);
}

static void decode_bits_flags(lbm_value *args, lbm_uint argn, lbm_uint fixed_argn, bool *be, bool *dbc) {
  *be = true;
  *dbc = false;
  for (lbm_uint i = fixed_argn; i < argn; i ++) {
    if (lbm_type_of(args[i]) == LBM_TYPE_SYMBOL) {
      lbm_uint s = lbm_dec_sym(args[i]);
      if (s == little_endian) *be = false;
      else if (s == big_endian) *be = true;
      else if (s == dbc_sym) *dbc = true;
    }
  }
}

static bool decode_bits_get_args(lbm_value *error, lbm_value *args, lbm_uint argn,
                                  lbm_uint *pos, lbm_uint *len, bool *be, bool *dbc,
                                  lbm_uint *a_size, uint8_t **a_data) {
  *error = ENC_SYM_EERROR;
  if (argn < 3 || argn > 5) return false;
  lbm_array_header_t *array = lbm_dec_array_r(args[0]);
  if (!(array && lbm_is_number(args[1]) && lbm_is_number(args[2]))) {
    *error = ENC_SYM_TERROR;
    return false;
  }
  *a_size = array->size;
  *a_data = (uint8_t*)array->data;
  *pos = lbm_dec_as_u32(args[1]);
  *len = lbm_dec_as_u32(args[2]);
  decode_bits_flags(args, argn, 3, be, dbc);
  return true;
}

static bool decode_bits_set_args(lbm_value *error, lbm_value *args, lbm_uint argn,
                                  lbm_uint *pos, lbm_uint *len, bool *be, bool *dbc,
                                  lbm_uint *a_size, uint8_t **a_data) {
  *error = ENC_SYM_EERROR;
  if (argn < 4 || argn > 6) return false;
  lbm_array_header_t *array = lbm_dec_array_rw(args[0]);
  if (!(array && lbm_is_number(args[1]) && lbm_is_number(args[2]) && lbm_is_number(args[3]))) {
    *error = ENC_SYM_TERROR;
    return false;
  }
  *a_size = array->size;
  *a_data = (uint8_t*)array->data;
  *pos = lbm_dec_as_u32(args[1]);
  *len = lbm_dec_as_u32(args[2]);
  decode_bits_flags(args, argn, 4, be, dbc);
  return true;
}

static bool bits_insert(uint8_t *data, lbm_uint d_size, lbm_uint pos, lbm_uint len,
                         bool be, bool dbc, uint64_t number) {
  if (len == 0 || len > 64) return false;
  if (dbc && be) pos = dbc_bit_pos(pos);
  if (be) number <<= (64 - len);

  lbm_uint bitcnt = 0, remaining = len;
  while (remaining > 0) {
    lbm_uint bytepos = (pos + bitcnt) / 8;
    if (bytepos >= d_size) return false;
    lbm_uint shift = (pos + bitcnt) % 8;
    lbm_uint bits = 8 - shift;
    if (bits > remaining) bits = remaining;

    uint8_t bval, mask;
    if (be) {
      bval = (uint8_t)((number >> (64 - bits)) << (8 - bits - shift));
      mask = (uint8_t)(~(0xFFu << (8 - bits - shift)) | (0xFFu << (8 - shift)));
      number <<= bits;
    } else {
      bval = (uint8_t)(number << shift);
      mask = (uint8_t)(~(0xFFu >> (8 - bits - shift)) | (0xFFu >> (8 - shift)));
      number >>= bits;
    }
    data[bytepos] = (uint8_t)((data[bytepos] & mask) | bval);
    bitcnt += bits;
    remaining -= bits;
  }
  return true;
}

static bool bits_extract(const uint8_t *data, lbm_uint d_size, lbm_uint pos, lbm_uint len,
                          bool be, bool dbc, uint64_t *out) {
  if (len == 0 || len > 64) return false;
  if (dbc && be) pos = dbc_bit_pos(pos);

  uint64_t res = 0;
  lbm_uint bitcnt = 0, remaining = len;
  while (remaining > 0) {
    lbm_uint bytepos = (pos + bitcnt) / 8;
    if (bytepos >= d_size) return false;
    lbm_uint shift = (pos + bitcnt) % 8;
    lbm_uint bits = 8 - shift;
    if (bits > remaining) bits = remaining;

    if (be) {
      uint8_t bval = (uint8_t)(data[bytepos] & (0xFFu >> shift));
      res = (res + bval) << (bits + shift);
    } else {
      uint8_t mask = (uint8_t)~(0xFFu << bits);
      uint8_t bval = (uint8_t)((data[bytepos] >> shift) & mask);
      res |= ((uint64_t)bval) << bitcnt;
    }
    bitcnt += bits;
    remaining -= bits;
  }
  if (be) res >>= 8;
  *out = res;
  return true;
}

// Number of bits that fit in a u/i type.
#define BITS_IMMEDIATE_SAFE (((lbm_uint)sizeof(lbm_uint) * 8) - LBM_VAL_SHIFT)

static lbm_value array_extensions_bufset_bits_u(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  lbm_uint pos, len, d_size; uint8_t *data; bool be, dbc;
  if (decode_bits_set_args(&res, args, argn, &pos, &len, &be, &dbc, &d_size, &data)) {
    res = bits_insert(data, d_size, pos, len, be, dbc,
                       lbm_dec_as_u64(args[3])) ? ENC_SYM_TRUE : ENC_SYM_EERROR;
  }
  return res;
}

static lbm_value array_extensions_bufset_bits_i(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  lbm_uint pos, len, d_size; uint8_t *data; bool be, dbc;
  if (decode_bits_set_args(&res, args, argn, &pos, &len, &be, &dbc, &d_size, &data)) {
    res = bits_insert(data, d_size, pos, len, be, dbc,
                       (uint64_t)lbm_dec_as_i64(args[3])) ? ENC_SYM_TRUE : ENC_SYM_EERROR;
  }
  return res;
}

static lbm_value array_extensions_bufget_bits_u(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  lbm_uint pos, len, d_size; uint8_t *data; bool be, dbc; uint64_t raw;
  if (decode_bits_get_args(&res, args, argn, &pos, &len, &be, &dbc, &d_size, &data) &&
      bits_extract(data, d_size, pos, len, be, dbc, &raw)) {
    if (len <= BITS_IMMEDIATE_SAFE) res = lbm_enc_u((lbm_uint)raw);
    else if (len <= 32)             res = lbm_enc_u32((uint32_t)raw);
    else                            res = lbm_enc_u64(raw);
  }
  return res;
}

static lbm_value array_extensions_bufget_bits_i(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  lbm_uint pos, len, d_size; uint8_t *data; bool be, dbc; uint64_t raw;
  if (decode_bits_get_args(&res, args, argn, &pos, &len, &be, &dbc, &d_size, &data) &&
      bits_extract(data, d_size, pos, len, be, dbc, &raw)) {
    int64_t sv = sign_extend64(raw, len);
    if (len <= BITS_IMMEDIATE_SAFE) res = lbm_enc_i((lbm_int)sv);
    else if (len <= 32)             res = lbm_enc_i32((int32_t)sv);
    else                            res = lbm_enc_i64(sv);
  }
  return res;
}

static bool decode_bits_f32_get_args(lbm_value *error, lbm_value *args, lbm_uint argn,
                                   lbm_uint *pos, bool *be, bool *dbc,
                                   lbm_uint *a_size, uint8_t **a_data) {
  *error = ENC_SYM_EERROR;
  if (argn < 2 || argn > 4) return false;
  lbm_array_header_t *array = lbm_dec_array_r(args[0]);
  if (!(array && lbm_is_number(args[1]))) {
    *error = ENC_SYM_TERROR;
    return false;
  }
  *a_size = array->size;
  *a_data = (uint8_t*)array->data;
  *pos = lbm_dec_as_u32(args[1]);
  decode_bits_flags(args, argn, 2, be, dbc);
  return true;
}

static bool decode_bits_f32_set_args(lbm_value *error, lbm_value *args, lbm_uint argn,
                                   lbm_uint *pos, bool *be, bool *dbc,
                                   lbm_uint *a_size, uint8_t **a_data) {
  *error = ENC_SYM_EERROR;
  if (argn < 3 || argn > 5) return false;
  lbm_array_header_t *array = lbm_dec_array_rw(args[0]);
  if (!(array && lbm_is_number(args[1]) && lbm_is_number(args[2]))) {
    *error = ENC_SYM_TERROR;
    return false;
  }
  *a_size = array->size;
  *a_data = (uint8_t*)array->data;
  *pos = lbm_dec_as_u32(args[1]);
  decode_bits_flags(args, argn, 3, be, dbc);
  return true;
}

static lbm_value array_extensions_bufset_bits_f32(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  lbm_uint pos, d_size; uint8_t *data; bool be, dbc;
  if (decode_bits_f32_set_args(&res, args, argn, &pos, &be, &dbc, &d_size, &data)) {
    res = bits_insert(data, d_size, pos, 32, be, dbc,
                       (uint64_t)float_to_u(lbm_dec_as_float(args[2]))) ? ENC_SYM_TRUE : ENC_SYM_EERROR;
  }
  return res;
}

static lbm_value array_extensions_bufget_bits_f32(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_EERROR;
  lbm_uint pos, d_size; uint8_t *data; bool be, dbc; uint64_t raw;
  if (decode_bits_f32_get_args(&res, args, argn, &pos, &be, &dbc, &d_size, &data) &&
      bits_extract(data, d_size, pos, 32, be, dbc, &raw)) {
    res = lbm_enc_float(u_to_float((uint32_t)raw));
  }
  return res;
}
