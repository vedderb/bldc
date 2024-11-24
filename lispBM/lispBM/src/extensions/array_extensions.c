/*
    Copyright 2022, 2023, 2024 Joel Svensson        svenssonjoel@yahoo.se
    Copyright 2022, 2023 Benjamin Vedder

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

static lbm_uint little_endian = 0;
static lbm_uint big_endian = 0;

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

void lbm_array_extensions_init(void) {

  lbm_add_symbol_const("little-endian", &little_endian);
  lbm_add_symbol_const("big-endian", &big_endian);

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
  case 3:
    if(lbm_is_array_rw(args[0]) &&
       lbm_is_number(args[1]) &&
       lbm_is_number(args[2])) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);

      *a_size = array->size;
      *a_data = (uint8_t*)array->data;
      *index = lbm_dec_as_u32(args[1]);

      res = true;
    } else {
      *error = ENC_SYM_TERROR;
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
  case 2:
    if (lbm_is_array_r(args[0]) &&
        lbm_is_number(args[1])) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
      *a_size = array->size;
      *a_data = (uint8_t*)array->data;
      *index = lbm_dec_as_u32(args[1]);
      res = true;
    } else {
      *error = ENC_SYM_TERROR;
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
  if (argn == 1 &&
      lbm_is_array_r(args[0]) &&
      lbm_heap_array_valid(args[0])) {
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);
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
