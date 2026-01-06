/*
    Copyright 2025 Joel Svensson        svenssonjoel@yahoo.se

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

#include <math.h>
#include <string.h>
#include <stdio.h>

#ifdef LBM_OPT_DSP_EXTENSIONS_SIZE
#pragma GCC optimize ("-Os")
#endif
#ifdef LBM_OPT_DSP_EXTENSIONS_SIZE_AGGRESSIVE
#pragma GCC optimize ("-Oz")
#endif

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define LBM_SYSTEM_LITTLE_ENDIAN true
#else
#define LBM_SYSTEM_LITTLE_ENDIAN false
#endif


static lbm_uint sym_inverse = 0;
static lbm_uint sym_big_endian = 0;
static lbm_uint sym_little_endian = 0;

static inline uint32_t byte_order_swap(uint32_t w) {
  uint32_t r = w;
  uint8_t *bytes = (uint8_t*)&r;
  uint8_t tmp = bytes[0];
  bytes[0] = bytes[3];
  bytes[3] = tmp;
  tmp = bytes[1];
  bytes[1] = bytes[2];
  bytes[2] = tmp;

  return r;
}

static inline float read_float(float *ptr, bool swap) {
  if (swap) {
    union { uint32_t u; float f; } pun;
    pun.f = *ptr;
    uint32_t swapped = byte_order_swap(pun.u);
    pun.u = swapped;
    return pun.f;
  }
  return *ptr;
}


static inline void write_float(float *ptr, float value, bool swap) {
  if (swap) {
    union { uint32_t u; float f; } pun;
    pun.f = value;
    uint32_t swapped = byte_order_swap(pun.u);
    pun.u = swapped;
    *ptr = pun.f;
  } else {
    *ptr = value;
  }
}


static void lbm_corr(float *s1,
                     unsigned int s1_len,
                     float *s2,
                     unsigned int s2_len,
                     float *output,
                     unsigned int output_len,
                     bool swap_byte_order) {
  for (int n = 0; n < (int)output_len; n++) {
    float sum = 0.0f;

    for (int k = 0; k < (int)s2_len; k++) {
      int j = n + k;
      if (j >= 0 && j < (int)s1_len) {
        sum +=
          read_float(&s1[j], swap_byte_order) *
          read_float(&s2[k], swap_byte_order);
      }
    }
    write_float(&output[n], sum, swap_byte_order);
  }
}


static void lbm_complex_corr(float *s1_re,
                             float *s1_im,
                             unsigned int s1_len,
                             float *s2_re,
                             float *s2_im,
                             unsigned int s2_len,
                             float *output_re,
                             float *output_im,
                             unsigned int output_len,
                             bool swap_byte_order) {
  for (int n = 0; n < (int)output_len; n++) {
    float sum_re = 0.0f;
    float sum_im = 0.0f;

    for (int k = 0; k < (int)s2_len; k++) {
      int j = n + k;
      if (j >= 0 && j < (int)s1_len) {
        float x_r = read_float(&s1_re[j], swap_byte_order);
        float x_i = read_float(&s1_im[j], swap_byte_order);
        float h_r = read_float(&s2_re[k], swap_byte_order);
        float h_i = read_float(&s2_im[k], swap_byte_order);
        // Note conjugate
        sum_re += x_r * h_r + x_i * h_i;  // ac + bd
        sum_im += x_i * h_r - x_r * h_i;  // bc - ad
      }
    }
    write_float(&output_re[n], sum_re, swap_byte_order);
    write_float(&output_im[n], sum_im, swap_byte_order);
  }
}

static lbm_value ext_correlate(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if ((argn == 2 || argn == 3) &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    bool be = true;
    if (argn == 3 &&
        lbm_is_symbol(args[2])) {
      if (lbm_dec_sym(args[2]) == sym_little_endian) {
        be = false;
      }
    }
    bool swap_byte_order =
      (be && LBM_SYSTEM_LITTLE_ENDIAN) ||
      (!be && !LBM_SYSTEM_LITTLE_ENDIAN);

    lbm_array_header_t *s1_arr = lbm_dec_array_r(args[0]);
    lbm_array_header_t *s2_arr = lbm_dec_array_r(args[1]);
    // The input arrays have been checked with lbm_is_array_r.
    // If there is no way to create a byte array from a null pointer,
    // then there is no way that these arrays can be null.
#ifdef __INFER__
    __infer_assume(s1_arr != NULL);
    __infer_assume(s2_arr != NULL);
#endif

    unsigned int s1_len = s1_arr->size / 4;
    unsigned int s2_len = s2_arr->size / 4;

    lbm_value output;

    unsigned int out_len = s1_len + s2_len - 1;
    if (lbm_heap_allocate_array(&output, out_len * sizeof(float))) {
      lbm_array_header_t *out_arr = lbm_dec_array_r(output);

      lbm_corr((float*)s1_arr->data,
               s1_len,
               (float*)s2_arr->data,
               s2_len,
               (float*)out_arr->data,
               out_len,
               swap_byte_order);
      r = output;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

static lbm_value ext_complex_correlate(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if ((argn == 4 || argn == 5) && // Allow only 4 or 5 args
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1]) &&
      lbm_is_array_r(args[2]) &&
      lbm_is_array_r(args[3])) {

    bool be = true;
    if (argn == 5 &&
        lbm_is_symbol(args[4])) {
      if (lbm_dec_sym(args[4]) == sym_little_endian) {
        be = false;
      }
    }
    bool swap_byte_order =
      (be && LBM_SYSTEM_LITTLE_ENDIAN) ||
      (!be && !LBM_SYSTEM_LITTLE_ENDIAN);

    lbm_array_header_t *s1_re_arr = lbm_dec_array_r(args[0]);
    lbm_array_header_t *s1_im_arr = lbm_dec_array_r(args[1]);
    lbm_array_header_t *s2_re_arr = lbm_dec_array_r(args[2]);
    lbm_array_header_t *s2_im_arr = lbm_dec_array_r(args[3]);

    if (!s1_re_arr || !s1_im_arr || !s2_re_arr || !s2_im_arr) {
      return ENC_SYM_TERROR;
    }

    if (s1_re_arr->size != s1_im_arr->size ||
        s2_re_arr->size != s2_im_arr->size) {
      return ENC_SYM_TERROR;
    }

    unsigned int s1_len = s1_re_arr->size / sizeof(float);
    unsigned int s2_len = s2_re_arr->size / sizeof(float);

    lbm_value output_re;
    lbm_value output_im;

    unsigned int out_len = s1_len + s2_len - 1;
    lbm_value r_cons = lbm_cons(ENC_SYM_NIL, ENC_SYM_NIL);
    if (r_cons != ENC_SYM_MERROR &&
        lbm_heap_allocate_array(&output_re, out_len * sizeof(float)) &&
        lbm_heap_allocate_array(&output_im, out_len * sizeof(float))) {
      lbm_array_header_t *out_re_arr = lbm_dec_array_r(output_re);
      lbm_array_header_t *out_im_arr = lbm_dec_array_r(output_im);

      lbm_complex_corr((float*)s1_re_arr->data,
                       (float*)s1_im_arr->data,
                       s1_len,
                       (float*)s2_re_arr->data,
                       (float*)s2_im_arr->data,
                       s2_len,
                       (float*)out_re_arr->data,
                       (float*)out_im_arr->data,
                       out_len,
                       swap_byte_order);
      lbm_set_car_and_cdr(r_cons, output_re, output_im);
      r = r_cons;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}


static void lbm_convolve(float *signal,
                         unsigned int signal_len,
                         float *filter,
                         unsigned int filter_len,
                         float *output,
                         unsigned int output_len,
                         bool swap_byte_order) {
  for (int n = 0; n < (int)output_len; n++) {
    float sum = 0.0f;

    for (int k = 0; k < (int)filter_len; k++) {
      int j = n - k;
      if (j >= 0 && j < (int)signal_len) {
        sum +=
          read_float(&signal[j], swap_byte_order) *
          read_float(&filter[k], swap_byte_order);
      }
    }
    write_float(&output[n], sum, swap_byte_order);
  }
}


void lbm_complex_convolve(float *signal_re,
                          float *signal_im,
                          unsigned int signal_len,
                          float *filter_re,
                          float *filter_im,
                          unsigned int filter_len,
                          float *output_re,
                          float *output_im,
                          unsigned int output_len,
                          bool swap_byte_order) {
  for (int n = 0; n < (int)output_len; n++) {
    float real_sum = 0.0f;
    float imag_sum = 0.0f;

    for (int k = 0; k < (int)filter_len; k++) {
      int j = n - k;
      if (j >= 0 && j < (int)signal_len) {
        float x_r = read_float(&signal_re[j], swap_byte_order);
        float x_i = read_float(&signal_im[j], swap_byte_order);
        float h_r = read_float(&filter_re[k], swap_byte_order);
        float h_i = read_float(&filter_im[k], swap_byte_order);

        real_sum += x_r * h_r - x_i * h_i;
        imag_sum += x_r * h_i + x_i * h_r;
      }
    }
    write_float(&output_re[n], real_sum, swap_byte_order);
    write_float(&output_im[n], imag_sum, swap_byte_order);
  }
}

static lbm_value ext_convolve(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if ((argn == 2 || argn == 3) &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    bool be = true;
    if (argn == 3 &&
        lbm_is_symbol(args[2])) {
      if (lbm_dec_sym(args[2]) == sym_little_endian) {
        be = false;
      }
    }
    bool swap_byte_order =
      (be && LBM_SYSTEM_LITTLE_ENDIAN) ||
      (!be && !LBM_SYSTEM_LITTLE_ENDIAN);

    lbm_array_header_t *sig_arr = lbm_dec_array_r(args[0]);
    lbm_array_header_t *fil_arr = lbm_dec_array_r(args[1]);
    // The input arrays have been checked with lbm_is_array_r.
    // If there is no way to create a byte array from a null pointer,
    // then there is no way that these arrays can be null.
#ifdef __INFER__
    __infer_assume(sig_arr != NULL);
    __infer_assume(fil_arr != NULL);
#endif

    unsigned int sig_len = sig_arr->size / 4;
    unsigned int fil_len = fil_arr->size / 4;

    lbm_value output;

    unsigned int out_len = sig_len + fil_len - 1;
    if (lbm_heap_allocate_array(&output, out_len * sizeof(float))) {
      lbm_array_header_t *out_arr = lbm_dec_array_r(output);

      lbm_convolve((float*)sig_arr->data,
                   sig_len,
                   (float*)fil_arr->data,
                   fil_len,
                   (float*)out_arr->data,
                   out_len,
                   swap_byte_order);
      r = output;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}

static lbm_value ext_complex_convolve(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if ((argn == 4 || argn == 5) &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1]) &&
      lbm_is_array_r(args[2]) &&
      lbm_is_array_r(args[3])) {

    bool be = true;
    if (argn == 5 &&
        lbm_is_symbol(args[4])) {
      if (lbm_dec_sym(args[4]) == sym_little_endian) {
        be = false;
      }
    }
    bool swap_byte_order =
      (be && LBM_SYSTEM_LITTLE_ENDIAN) ||
      (!be && !LBM_SYSTEM_LITTLE_ENDIAN);

    lbm_array_header_t *sig_re_arr = lbm_dec_array_r(args[0]);
    lbm_array_header_t *sig_im_arr = lbm_dec_array_r(args[1]);
    lbm_array_header_t *fil_re_arr = lbm_dec_array_r(args[2]);
    lbm_array_header_t *fil_im_arr = lbm_dec_array_r(args[3]);

    if (!sig_re_arr || !sig_im_arr || !fil_re_arr || !fil_im_arr) {
      return ENC_SYM_TERROR;
    }

    // Check that real and imaginary parts have matching sizes
    if (sig_re_arr->size != sig_im_arr->size ||
        fil_re_arr->size != fil_im_arr->size) {
      return ENC_SYM_TERROR;
    }

    unsigned int sig_len = sig_re_arr->size / sizeof(float);
    unsigned int fil_len = fil_re_arr->size / sizeof(float);

    lbm_value output_re;
    lbm_value output_im;

    unsigned int out_len = sig_len + fil_len - 1;
    lbm_value r_cons = lbm_cons(ENC_SYM_NIL, ENC_SYM_NIL);
    if (lbm_heap_allocate_array(&output_re, out_len * sizeof(float)) &&
        lbm_heap_allocate_array(&output_im, out_len * sizeof(float))) {
      lbm_array_header_t *out_re_arr = lbm_dec_array_r(output_re);
      lbm_array_header_t *out_im_arr = lbm_dec_array_r(output_im);

      lbm_complex_convolve((float*)sig_re_arr->data,
                           (float*)sig_im_arr->data,
                           sig_len,
                           (float*)fil_re_arr->data,
                           (float*)fil_im_arr->data,
                           fil_len,
                           (float*)out_re_arr->data,
                           (float*)out_im_arr->data,
                           out_len,
                           swap_byte_order);
      lbm_set_car_and_cdr(r_cons, output_re, output_im);
      r = r_cons;
    } else {
      r = ENC_SYM_MERROR;
    }
  }
  return r;
}


static void lbm_fft(float *real, float *imag, int n, int inverse) {
  int k = 0;
  for (int i = 1; i < n; i++) {
    int bit = n >> 1;
    while (k & bit) {
      k ^= bit;
      bit >>= 1;
    }
    k ^= bit;

    if (i < k) {
      float temp = real[i];
      real[i] = real[k];
      real[k] = temp;

      temp = imag[i];
      imag[i] = imag[k];
      imag[k] = temp;
    }
  }

  for (int len = 2; len <= n; len <<= 1) {
    float angle = (inverse ? 2.0f : -2.0f) * (float)M_PI / (float)len;
    float wlen_r = cosf(angle);
    float wlen_i = sinf(angle);

    for (int i = 0; i < n; i += len) {
      float w_r = 1.0f;
      float w_i = 0.0f;

      for (int j = 0; j < len / 2; j++) {
        float u_r = real[i + j];
        float u_i = imag[i + j];
        float v_r = real[i + j + len / 2];
        float v_i = imag[i + j + len / 2];

        float t_r = w_r * v_r - w_i * v_i;
        float t_i = w_r * v_i + w_i * v_r;

        real[i + j] = u_r + t_r;
        imag[i + j] = u_i + t_i;
        real[i + j + len / 2] = u_r - t_r;
        imag[i + j + len / 2] = u_i - t_i;

        float next_w_r = w_r * wlen_r - w_i * wlen_i;
        float next_w_i = w_r * wlen_i + w_i * wlen_r;
        w_r = next_w_r;
        w_i = next_w_i;
      }
    }
  }

  if (inverse) {
    for (int i = 0; i < n; i++) {
      real[i] /= (float)n;
      imag[i] /= (float)n;
    }
  }
}

static lbm_value ext_fft_f32(lbm_value *args, lbm_uint argn) {
  lbm_value r = ENC_SYM_TERROR;
  if (argn >= 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    int inverse = 0;
    bool be = true;

    if (argn > 2 &&
        lbm_is_symbol(args[2])) {
      lbm_uint sym = lbm_dec_sym(args[2]);
      if (sym == sym_inverse) {
        inverse = 1;
      } else if (sym == sym_little_endian) {
        be = false;
      }
    }

    if (argn > 3 &&
        lbm_is_symbol(args[3])) {
      if (lbm_dec_sym(args[3]) == sym_little_endian) {
        be = false;
      }
    }

    bool swap_byte_order =
      (be && LBM_SYSTEM_LITTLE_ENDIAN) ||
      (!be && !LBM_SYSTEM_LITTLE_ENDIAN);

    lbm_array_header_t *real_arr = lbm_dec_array_r(args[0]);
    lbm_array_header_t *imag_arr = lbm_dec_array_r(args[1]);

    if (!real_arr || !imag_arr) {
      return ENC_SYM_TERROR;
    }

    if (real_arr->size == imag_arr->size &&
        real_arr->size > sizeof(float) && // need more than 4 bytes for a float
        (real_arr->size % sizeof(float) == 0)) {
      lbm_uint n = real_arr->size / sizeof(float);
      lbm_uint padded_size = n;
      if ((n & (n - 1)) != 0) { // not a power of two. needs padding.
        padded_size = 1;
        while (padded_size < n) padded_size <<= 1;
      }

      lbm_value real_copy;
      lbm_value imag_copy;
      if (lbm_heap_allocate_array(&real_copy, padded_size * sizeof(float)) &&
          lbm_heap_allocate_array(&imag_copy, padded_size * sizeof(float))) {
        // cppcheck-suppress invalidPointerCast
        float *real_data = (float*)real_arr->data;
        // cppcheck-suppress invalidPointerCast
        float *imag_data = (float*)imag_arr->data;

        lbm_array_header_t *real_copy_arr = lbm_dec_array_r(real_copy);
        lbm_array_header_t *imag_copy_arr = lbm_dec_array_r(imag_copy);
        // cppcheck-suppress invalidPointerCast
        float *real_copy_data = (float*)real_copy_arr->data;
        // cppcheck-suppress invalidPointerCast
        float *imag_copy_data = (float*)imag_copy_arr->data;
        memset(real_copy_data, 0, padded_size * sizeof(float));
        memset(imag_copy_data, 0, padded_size * sizeof(float));

        if (swap_byte_order) {
          for (lbm_uint w = 0; w < n; w ++) {
            // cppcheck-suppress invalidPointerCast
            uint32_t swapped = byte_order_swap(((uint32_t*)real_data)[w]);
            // cppcheck-suppress invalidPointerCast
            ((uint32_t*)real_copy_data)[w] = swapped;
            // cppcheck-suppress invalidPointerCast
            swapped = byte_order_swap(((uint32_t*)imag_data)[w]);
            // cppcheck-suppress invalidPointerCast
            ((uint32_t*)imag_copy_data)[w] = swapped;
          }
        } else {
          memcpy(real_copy_data, real_data, real_arr->size);
          memcpy(imag_copy_data, imag_data, real_arr->size);
        }

        lbm_fft(real_copy_data, imag_copy_data, (int)padded_size, inverse);

        if (swap_byte_order) {
          for (lbm_uint w = 0; w < padded_size; w ++) {
            // cppcheck-suppress invalidPointerCast
            uint32_t swapped = byte_order_swap(((uint32_t*)real_copy_data)[w]);
            // cppcheck-suppress invalidPointerCast
            ((uint32_t*)real_copy_data)[w] = swapped;
            // cppcheck-suppress invalidPointerCast
            swapped = byte_order_swap(((uint32_t*)imag_copy_data)[w]);
            // cppcheck-suppress invalidPointerCast
            ((uint32_t*)imag_copy_data)[w] = swapped;
          }
        }
        r = lbm_cons(real_copy, imag_copy);
      } else {
        r = ENC_SYM_MERROR;
      }
    } else {
      r = ENC_SYM_NIL;
    }
  }
  return r;
}

void lbm_dsp_extensions_init(void) {
  lbm_add_symbol("inverse", &sym_inverse);
  lbm_add_symbol("little-endian", &sym_little_endian);
  lbm_add_symbol("big-endian", &sym_big_endian);

  lbm_add_extension("correlate", ext_correlate);
  lbm_add_extension("complex-correlate", ext_complex_correlate);
  lbm_add_extension("convolve", ext_convolve);
  lbm_add_extension("complex-convolve", ext_complex_convolve);
  lbm_add_extension("fft", ext_fft_f32);
}
