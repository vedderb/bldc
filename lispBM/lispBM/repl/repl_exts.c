/*
  Copyright 2024 2025 Joel Svensson  svenssonjoel@yahoo.se
            2022 Benjamin Vedder benjamin@vedder.se

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

#include "repl_exts.h"

#include <unistd.h>
#include <stdio.h>
#include <dirent.h>

#ifndef LBM_WIN
#include <sys/time.h>
#include <sys/wait.h>
#endif

#include "extensions/array_extensions.h"
#include "extensions/string_extensions.h"
#include "extensions/math_extensions.h"
#include "extensions/runtime_extensions.h"
#include "extensions/set_extensions.h"
#include "extensions/display_extensions.h"
#include "extensions/mutex_extensions.h"
#include "extensions/lbm_dyn_lib.h"
#include "extensions/ttf_extensions.h"
#include "extensions/random_extensions.h"

#include "eval_cps.h"
#include "lbm_image.h"
#include "lbm_flat_value.h"

#include <png.h>

#ifdef LBM_WIN
#include <windows.h>
#endif

// ////////////////////////////////////////////////////////////
// Utility
#ifdef LBM_WIN

int gettimeofday(struct timeval * tp, struct timezone * tzp)
{
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
}
#endif

// ////////////////////////////////////////////////////////////
// Math

static lbm_value ext_rand(lbm_value *args, lbm_uint argn) {
  if (argn != 0 && argn != 1) {
    lbm_set_error_reason((char*)lbm_error_str_num_args);
    return ENC_SYM_TERROR;
  }

  unsigned int seed = 0;
  bool seed_set = false;

  if (argn == 1) {
    if (!lbm_is_number(args[0])) {
      lbm_set_error_reason((char*)lbm_error_str_no_number);
      return ENC_SYM_TERROR;
    }

    seed = lbm_dec_as_u32(args[0]);
    seed_set = true;
  }

  if (seed_set) {
    srand(seed);
  }

  return lbm_enc_i32(rand());
}

static lbm_value ext_rand_max(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  return lbm_enc_i32(RAND_MAX);
}


// BITS

/*
 * args[0]: Initial value
 * args[1]: Offset in initial value to modify
 * args[2]: Value to modify with
 * args[3]: Size in bits of value to modify with
 */
static lbm_value ext_bits_enc_int(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(4)
    uint32_t initial = lbm_dec_as_u32(args[0]);
  uint32_t offset = lbm_dec_as_u32(args[1]);
  uint32_t number = lbm_dec_as_u32(args[2]);
  uint32_t bits = lbm_dec_as_u32(args[3]);
  initial &= ~((0xFFFFFFFF >> (32 - bits)) << offset);
  initial |= (number << (32 - bits)) >> (32 - bits - offset);

  if (initial > ((1 << 27) - 1)) {
    return lbm_enc_i32((int32_t)initial);
  } else {
    return lbm_enc_i((int32_t)initial);
  }
}

/*
 * args[0]: Value
 * args[1]: Offset in initial value to get
 * args[2]: Size in bits of value to get
 */
static lbm_value ext_bits_dec_int(lbm_value *args, lbm_uint argn) {
  LBM_CHECK_ARGN_NUMBER(3)
    uint32_t val = lbm_dec_as_u32(args[0]);
  uint32_t offset = lbm_dec_as_u32(args[1]);
  uint32_t bits = lbm_dec_as_u32(args[2]);
  val >>= offset;
  val &= 0xFFFFFFFF >> (32 - bits);

  if (val > ((1 << 27) - 1)) {
    return lbm_enc_i32((int32_t)val);
  } else {
    return lbm_enc_i((int32_t)val);
  }
}



// TIME

uint32_t timestamp(void) {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

static lbm_value ext_systime(lbm_value *args, lbm_uint argn) {

  uint32_t time = timestamp();

  return lbm_enc_u32(time);
}

static lbm_value ext_secs_since(lbm_value *args, lbm_uint argn) {
  uint32_t t_now = timestamp();

  if (argn != 1 || !lbm_is_number(args[0])) return ENC_SYM_EERROR;

  uint32_t t_then = lbm_dec_as_u32(args[0]);
  uint32_t diff = t_now - t_then;
  return lbm_enc_float((float)diff / 1000000.0f);
}


static bool allow_print = true;
void set_allow_print(bool on) {
  allow_print = on;
}

lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  if (argn < 1) return lbm_enc_sym(SYM_NIL);

  if (!allow_print) return lbm_enc_sym(SYM_TRUE);

  char output[1024];

  for (unsigned int i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      char *data = (char*)array->data;
      lbm_printf_callback("%s", data);
    } else {
      lbm_print_value(output, 1024, t);
      lbm_printf_callback("%s", output);
    }
  }
  lbm_printf_callback("\n");
  return lbm_enc_sym(SYM_TRUE);
}

// ------------------------------------------------------------
// File IO

static const char *lbm_file_handle_desc = "File-Handle";

typedef struct {
  FILE *fp;
} lbm_file_handle_t;

static bool file_handle_destructor(lbm_uint value) {

  lbm_file_handle_t *h = (lbm_file_handle_t *)value;
  if (h->fp) {
    fclose(h->fp);
  }
  return true;
}

// A filehandle is only a filehandle unless it has been explicitly closed.
static bool is_file_handle(lbm_value arg) {
  if ((lbm_uint)lbm_get_custom_descriptor(arg) == (lbm_uint)lbm_file_handle_desc) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(arg);
    if (h->fp) return true;
  }
  return false;
}

static lbm_value ext_fclose(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    fclose(h->fp);
    h->fp = NULL;
    res = ENC_SYM_TRUE;
  }
  return res;
}

static lbm_value ext_fopen(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) {

    FILE *fp = NULL;

    char *filename = lbm_dec_str(args[0]);
    char *mode = lbm_dec_str(args[1]);

    fp = fopen(filename, mode);
    if (fp) {
      lbm_file_handle_t *mem = lbm_malloc(sizeof(lbm_file_handle_t));
      if (!mem) {
        fclose(fp);
        return ENC_SYM_MERROR;
      }
      mem->fp = fp;
      lbm_custom_type_create((lbm_uint)mem,
                             file_handle_destructor,
                             lbm_file_handle_desc,
                             &res);
    } else {
      return ENC_SYM_NIL;
    }
  }
  return res;
}

static lbm_value ext_load_file(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_file_handle(args[0])) {

    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    res = ENC_SYM_EERROR;
    if (fseek(h->fp, 0, SEEK_END) >= 0) {
      res = ENC_SYM_MERROR;

      long int  size = ftell(h->fp);
      rewind(h->fp);

      if (size > 0) {
        uint8_t *data = lbm_malloc((size_t)size);
        if (data) {

          lbm_value val;
          lbm_lift_array(&val, (char*)data, (lbm_uint)size);
          if (!lbm_is_symbol(val)) {
            size_t n = fread(data, 1, (size_t)size, h->fp);
            if ( n > 0) {
              res = val;
            } else {
              res = ENC_SYM_NIL; // or some empty indicator?
            }
          }
        } else {
          res = ENC_SYM_MERROR;
        }
      } else {
        res = ENC_SYM_NIL;
      }
    }
  }
  return res;
}


lbm_value sym_seek_set;
lbm_value sym_seek_cur;
lbm_value sym_seek_end;

static lbm_value ext_fseek(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 3 &&
      is_file_handle(args[0]) &&
      lbm_is_number(args[1]) &&
      lbm_is_symbol(args[2])) {

    int whence;
    if (args[2] == sym_seek_set) {
      whence = SEEK_SET;
    } else if (args[2] == sym_seek_cur) {
      whence = SEEK_CUR;
    } else if (args[2] == sym_seek_end) {
      whence = SEEK_END;
    } else {
      return res;
    }

    long offset = (long)lbm_dec_as_i64(args[1]);

    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);

    if (fseek(h->fp, offset, whence) == 0) {
      res =  ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

static lbm_value ext_ftell(lbm_value *args, lbm_uint argn) {
 lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_file_handle(args[0])) {

    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);

    long pos = ftell(h->fp);
    res = lbm_enc_i64((int64_t)pos);
  }
  return res;
}

static lbm_value ext_fread_byte(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    char c;
    size_t num = fread(&c, 1, 1, h->fp);
    if (num == 1) {
      res =  lbm_enc_char((uint8_t) c);
    } else {
      res = ENC_SYM_NIL;
    }
  }

  return res;
}

static lbm_value ext_fwrite(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_file_handle(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
    if (array) {
      fwrite(array->data, 1, array->size, h->fp);
      fflush(h->fp);
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;

}

static lbm_value ext_fwrite_str(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_file_handle(args[0]) &&
      lbm_is_array_r(args[1])) {

    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[1]);
    if (array) {
      fwrite(array->data, 1, strlen((char*)array->data), h->fp);
      fflush(h->fp);
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

static lbm_value ext_fwrite_value(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 2 &&
      is_file_handle(args[0])) {
    res = ENC_SYM_NIL;
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);

    lbm_set_max_flatten_depth(10000);
    int32_t fv_size = flatten_value_size(args[1], 0);
    if (fv_size > 0) {
      lbm_flat_value_t fv;
      fv.buf = malloc((uint32_t)fv_size);
      if (fv.buf) {
        fv.buf_size = (uint32_t)fv_size;
        fv.buf_pos = 0;
        if (flatten_value_c(&fv, args[1]) == FLATTEN_VALUE_OK) {
          fwrite(fv.buf, 1, (size_t)fv_size, h->fp);
          fflush(h->fp);
          res = ENC_SYM_TRUE;
        } else {
          lbm_printf_callback("ALERT: Unable to flatten result value\n");
        }
      } else {
        lbm_printf_callback("ALERT: Out of memory to allocate result buffer\n");
      }
    } else {
      lbm_printf_callback("ALERT: Incorrect FV size: %d \n", fv_size);
    }
  }
  return res;
}

static lbm_value ext_fwrite_image(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      is_file_handle(args[0])) {
    lbm_file_handle_t *h = (lbm_file_handle_t*)lbm_get_custom_value(args[0]);
    uint32_t *image_data = lbm_image_get_image();
    if (image_data) {
      size_t size = (size_t)lbm_image_get_size();
      fwrite((uint8_t*)image_data, 1, size * sizeof(uint32_t), h->fp);
      fflush(h->fp);
      res = ENC_SYM_TRUE;
    } else {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}

static bool all_arrays(lbm_value *args, lbm_uint argn) {
  bool r = true;
  for (uint32_t i = 0; i < argn; i ++) {
    r = r && lbm_is_array_r(args[i]);
  }
  return r;
}

#ifndef LBM_WIN
static lbm_value ext_exec(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;

  if (all_arrays(args, argn) && argn >= 1) {
    char **strs = malloc(argn * sizeof(char*) + 1);
    for (uint32_t i = 0; i < argn; i ++) {
      strs[i] = lbm_dec_str(args[i]);
    }
    strs[argn] = NULL;
    fflush(stdout);
    int status = 0;
    int pid = fork();
    if (pid == 0) {
      execvp(strs[0], &strs[1]);
      exit(0);
    } else {
      waitpid(pid, &status, 0);
      res = ENC_SYM_TRUE;
    }
  }
  return res;
}
#endif

static lbm_value ext_unsafe_call_system(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])) {
    int r = system(lbm_dec_str(args[0]));
    if (r == 0)  {
      res = ENC_SYM_TRUE;
    } else {
      res = lbm_enc_i(r);
    }
  }
  return res;
}


// ------------------------------------------------------------
// image to png

// a display driver for displaying images onto an image RGB888

// blit into a buffer that is guaranteed large enough.
static void buffer_blast_indexed2(uint8_t *dest, uint8_t *img, color_t *colors) {
  uint8_t *data = image_buffer_data(img);
  uint16_t w    = image_buffer_width(img);
  uint16_t h    = image_buffer_height(img);
  int num_pix = w * h;

  uint32_t t_pos = 0;
  for (int i = 0; i < num_pix; i ++) {
    int byte = i >> 3;
    int bit  = 7 - (i & 0x7);
    int color_ind = (data[byte] & (1 << bit)) >> bit;

    uint32_t color = (uint32_t)COLOR_TO_RGB888(colors[color_ind],
                                     i % w, i / w);
    dest[t_pos++] = (uint8_t)(color >> 16);
    dest[t_pos++] = (uint8_t)(color >> 8);
    dest[t_pos++] = (uint8_t)(color);
  }
}

static void buffer_blast_indexed4(uint8_t *dest, uint8_t *img, color_t *colors) {
  uint8_t *data = image_buffer_data(img);
  uint16_t w    = image_buffer_width(img);
  uint16_t h    = image_buffer_height(img);
  int num_pix = w * h;

  uint32_t t_pos = 0;
  for (int i = 0; i < num_pix; i ++) {
    int byte = i >> 2;
    int bit = (3 - (i & 0x03)) * 2;
    int color_ind = (data[byte] & (0x03 << bit)) >> bit;

    uint32_t color = COLOR_TO_RGB888(colors[color_ind],
                                     i % w, i / w);
    dest[t_pos++] = (uint8_t)(color >> 16);
    dest[t_pos++] = (uint8_t)(color >> 8);
    dest[t_pos++] = (uint8_t)(color);
  }
}

static void buffer_blast_indexed16(uint8_t *dest, uint8_t *img, color_t *colors) {
  uint8_t *data = image_buffer_data(img);
  uint16_t w    = image_buffer_width(img);
  uint16_t h    = image_buffer_height(img);
  int num_pix = w * h;

  uint32_t t_pos = 0;
  for (int i = 0; i < num_pix; i ++) {
    int byte = i >> 1;    // byte to access is pix / 2
    int bit = (1 - (i & 0x01)) * 4; // bit position to access within byte
    int color_ind = (data[byte] & (0x0F << bit)) >> bit; // extract 4 bit value.

    uint32_t color = COLOR_TO_RGB888(colors[color_ind],
                                     i % w, i / w);
    dest[t_pos++] = (uint8_t)(color >> 16);
    dest[t_pos++] = (uint8_t)(color >> 8);
    dest[t_pos++] = (uint8_t)(color);
  }
}

static void buffer_blast_rgb332(uint8_t *dest, uint8_t *img) {
  uint8_t *data = image_buffer_data(img);
  uint16_t w    = image_buffer_width(img);
  uint16_t h    = image_buffer_height(img);
  int num_pix = w * h;

  uint32_t t_pos = 0;
  for (int i = 0; i < num_pix; i ++) {
    uint8_t pix = data[i];
    uint32_t r = (uint32_t)((pix >> 5) & 0x7);
    uint32_t g = (uint32_t)((pix >> 2) & 0x7);
    uint32_t b = 2 * (uint32_t)(pix & 0x3) +1;
    r = (r == 7) ? 255 : 36 * r;
    g = (g == 7) ? 255 : 36 * g;
    b = (b == 7) ? 255 : 36 * b;
    dest[t_pos++] = (uint8_t)r;
    dest[t_pos++] = (uint8_t)g;
    dest[t_pos++] = (uint8_t)b;
  }
}

static void buffer_blast_rgb565(uint8_t *dest, uint8_t *img) {
  uint8_t *data = image_buffer_data(img);
  uint16_t w    = image_buffer_width(img);
  uint16_t h    = image_buffer_height(img);
  int num_pix = w * h;

  uint32_t t_pos = 0;
  for (int i = 0; i < num_pix; i ++) {
    uint16_t pix = (uint16_t)((uint16_t)(data[2 * i] << 8) | ((uint16_t)data[2 * i + 1]));

    uint32_t r = (uint32_t)(pix >> 11);
    uint32_t g = (uint32_t)((pix >> 5) & 0x3F);
    uint32_t b = (uint32_t)(pix & 0x1F);
    dest[t_pos++] = (uint8_t)r;
    dest[t_pos++] = (uint8_t)g;
    dest[t_pos++] = (uint8_t)b;
  }
}

static void buffer_blast_rgb888(uint8_t *dest, uint8_t *img) {
  uint8_t *data = image_buffer_data(img);
  uint16_t w    = image_buffer_width(img);
  uint16_t h    = image_buffer_height(img);
  int num_pix = w * h;
  memcpy(dest, data, (size_t)num_pix * 3);
}

void copy_image_area(uint8_t*target, uint16_t tw, uint16_t th, uint16_t x, uint16_t y, uint8_t * buffer, uint16_t w, uint16_t h) {

  if (x < tw && y < th) {  // if at all on screen
    int end_y = y + h > th ? th : y + h;
    int start_y = y;

    int len = (x + w > tw) ? w - (tw - (x + w)) : w;
    int read_y = 0;
    for (int i = start_y; i < end_y; i ++){
      memcpy(target + (i * tw * 3) + (x * 3), buffer + (read_y * w * 3), (size_t)(len * 3));
      read_y++;
    }
  }
}

lbm_value active_image = ENC_SYM_NIL;

static lbm_value ext_set_active_image(lbm_value *args, lbm_uint argn) {

  lbm_value r = ENC_SYM_TERROR;
  if (argn == 1 && lbm_is_array_r(args[0])){
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(args[0]);
    if (image_buffer_is_valid((uint8_t*)arr->data, arr->size) &&
        image_buffer_format((uint8_t*)arr->data) == rgb888) {
      active_image = args[0];
      r = ENC_SYM_TRUE;
    }
  }
  return r;
}

static lbm_value ext_save_active_image(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(active_image)) {

    FILE *fp = NULL;

    char *filename = lbm_dec_str(args[0]);

    fp = fopen(filename, "w");
    if (fp) {
      png_structp png_ptr = NULL;
      png_infop info_ptr = NULL;
      png_bytep* row_pointers = NULL;

      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(active_image);

      uint8_t *img_buf = (uint8_t*)arr->data;
      uint8_t *data = image_buffer_data(img_buf);
      uint32_t w = image_buffer_width(img_buf);
      uint32_t h = image_buffer_height(img_buf);


      png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
      info_ptr = png_create_info_struct(png_ptr);

      png_set_IHDR(png_ptr,
                   info_ptr,
                   w,
                   h,
                   8, // per channel
                   PNG_COLOR_TYPE_RGB,
                   PNG_INTERLACE_NONE,
                   PNG_COMPRESSION_TYPE_BASE,
                   PNG_FILTER_TYPE_BASE);

      row_pointers = (png_bytep*)malloc(sizeof(png_bytep) * h);

      if (row_pointers) {
        for (uint32_t i = 0; i < h; ++i) {
          row_pointers[i] = data + (i * w * 3);
        }

        png_init_io(png_ptr, fp);
        png_set_rows(png_ptr, info_ptr, row_pointers);
        png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);
      }

      fclose(fp);

      png_destroy_write_struct(&png_ptr, &info_ptr);
      png_ptr = NULL;
      info_ptr = NULL;
      free(row_pointers);
      res = ENC_SYM_TRUE;
    } else {
      return ENC_SYM_NIL;
    }
  }
  return res;
}

static bool image_renderer_render(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) {

  bool r = false;

  if (lbm_is_array_r(active_image)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(active_image);
    uint8_t *target_image = (uint8_t*)arr->data;

    uint16_t w = img->width;
    uint16_t h = img->height;
    uint8_t* data = img->mem_base;

    uint8_t *buffer = malloc((size_t)(w * h * 3)); // RGB 888
    if (buffer) {
      uint8_t  bpp = img->fmt;
      switch(bpp) {
      case indexed2:
        buffer_blast_indexed2(buffer, data, colors);
        break;
      case indexed4:
        buffer_blast_indexed4(buffer, data, colors);
        break;
      case indexed16:
        buffer_blast_indexed16(buffer, data, colors);
        break;
      case rgb332:
        buffer_blast_rgb332(buffer, data);
        break;
      case rgb565:
        buffer_blast_rgb565(buffer, data);
        break;
      case rgb888:
        buffer_blast_rgb888(buffer, data);
        break;
      default:
        break;
      }
      uint16_t t_w = image_buffer_width(target_image);
      uint16_t t_h = image_buffer_height(target_image);
      if (t_w == w && t_h == h) {
        memcpy(image_buffer_data(target_image), buffer, (size_t)w * h * 3);
      } else {
        copy_image_area(image_buffer_data(target_image), t_w, t_h, x, y, buffer, w, h);
      }
      free(buffer);
      r = true;
    }
  }
  return r;
}

static void image_renderer_clear(uint32_t color) {
  if (lbm_is_array_r(active_image)) {
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(active_image);
    image_buffer_t img;
    img.fmt = image_buffer_format((uint8_t*)arr->data);
    img.width = image_buffer_width((uint8_t*)arr->data);
    img.height = image_buffer_height((uint8_t*)arr->data);
    img.data = image_buffer_data((uint8_t*)arr->data);
    image_buffer_clear(&img, color);
  }
}

static void image_renderer_reset(void) {
  return;
}

static lbm_value ext_display_to_image(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  lbm_display_extensions_set_callbacks(
                                       image_renderer_render,
                                       image_renderer_clear,
                                       image_renderer_reset);


  return ENC_SYM_TRUE;
}
// boot images, snapshots, workspaces....

lbm_value ext_image_save(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;

  bool r = lbm_image_save_global_env();

  lbm_uint main_sym = ENC_SYM_NIL;
  if (lbm_get_symbol_by_name("main", &main_sym)) {
    lbm_value binding;
    if ( lbm_global_env_lookup(&binding, lbm_enc_sym(main_sym))) {
      if (lbm_is_cons(binding) && lbm_car(binding) == ENC_SYM_CLOSURE) {
        goto image_has_main;
      }
    }
  }
  lbm_set_error_reason("No main function in image\n");
  return ENC_SYM_EERROR;
 image_has_main:
  r = r && lbm_image_save_extensions();
  r = r && lbm_image_save_constant_heap_ix();
  return r ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

lbm_value ext_image_save_const_heap_ix(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  return lbm_image_save_constant_heap_ix() ? ENC_SYM_TRUE : ENC_SYM_NIL;
}


int dummy_f(lbm_value v, bool shared, void *arg) {
  if (shared) {
    lbm_printf_callback("shared node detected\n");
  }
  if (lbm_is_cons(v)) {
    lbm_printf_callback("cons\n");
  } else {
    char buf[256];
    lbm_print_value(buf,256, v);
    lbm_printf_callback("atom: %s\n", buf);
  }
  return TRAV_FUN_SUBTREE_CONTINUE;
}

lbm_value ext_rt(lbm_value *args, lbm_uint argn) {
  if (argn == 1) {
    lbm_ptr_rev_trav(dummy_f, args[0], NULL);
    return ENC_SYM_TRUE;
  }
  return ENC_SYM_TERROR;
}

// ------------------------------------------------------------
// Init

int init_exts(void) {

  lbm_array_extensions_init();
  lbm_string_extensions_init();
  lbm_math_extensions_init();
  lbm_runtime_extensions_init();
  lbm_set_extensions_init();
  lbm_display_extensions_init();
  lbm_mutex_extensions_init();
  lbm_dyn_lib_init();
  lbm_ttf_extensions_init();
  lbm_random_extensions_init();

  //lbm_value sym_seek_set;
  //lbm_value sym_seek_cur;
  //lbm_value sym_seem_end;

  lbm_uint seek_set = 0;
  lbm_uint seek_cur = 0;
  lbm_uint seek_end = 0;

  lbm_add_symbol("seek-set", &seek_set);
  lbm_add_symbol("seek-cur", &seek_cur);
  lbm_add_symbol("seek-end", &seek_end);

  sym_seek_set = lbm_enc_sym(seek_set);
  sym_seek_cur = lbm_enc_sym(seek_cur);
  sym_seek_end = lbm_enc_sym(seek_end);

  lbm_add_extension("rt", ext_rt);

  lbm_add_extension("unsafe-call-system", ext_unsafe_call_system);
#ifndef LBM_WIN
  lbm_add_extension("exec", ext_exec);
#endif
  lbm_add_extension("fclose", ext_fclose);
  lbm_add_extension("fopen", ext_fopen);
  lbm_add_extension("load-file", ext_load_file);
  lbm_add_extension("fwrite", ext_fwrite);
  lbm_add_extension("fwrite-str", ext_fwrite_str);
  lbm_add_extension("fwrite-value", ext_fwrite_value);
  lbm_add_extension("fwrite-image", ext_fwrite_image);
  lbm_add_extension("fread-byte", ext_fread_byte);
  lbm_add_extension("fseek", ext_fseek);
  lbm_add_extension("ftell", ext_ftell);
  lbm_add_extension("print", ext_print);
  lbm_add_extension("systime", ext_systime);
  lbm_add_extension("secs-since", ext_secs_since);

  // boot images, snapshots, workspaces....
  lbm_add_extension("image-save-const-heap-ix", ext_image_save_const_heap_ix);
  lbm_add_extension("image-save", ext_image_save);
  // Math
  lbm_add_extension("rand", ext_rand);
  lbm_add_extension("rand-max", ext_rand_max);

  // Bit operations
  lbm_add_extension("bits-enc-int", ext_bits_enc_int);
  lbm_add_extension("bits-dec-int", ext_bits_dec_int);

  //displaying to active image
  lbm_add_extension("set-active-img", ext_set_active_image);
  lbm_add_extension("save-active-img", ext_save_active_image);
  lbm_add_extension("display-to-img", ext_display_to_image);

  if (lbm_get_num_extensions() < lbm_get_max_extensions()) {
    return 1;
  }
  return 0;
}


// Dynamic loader

bool dynamic_loader(const char *str, const char **code) {
  return lbm_dyn_lib_find(str, code);
}
