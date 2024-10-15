/*
  Copyright 2023, 2024 Benjamin Vedder		benjamin@vedder.se
  Copyright 2023, 2024 Joel Svensson		svenssonjoel@yahoo.se
  Copyright 2023       Rasmus Söderhielm	rasmus.soderhielm@gmail.com

  This file is part of LispBM. (Originally a part of the vesc_express FW)

  LispBM is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LispBM is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "tjpgd.h"

#include <math.h>

#include <extensions/display_extensions.h>
#include <lbm_utils.h>
#include <lbm_custom_type.h>
#include <lbm_defrag_mem.h>

#define MAX_WIDTH 32000
#define MAX_HEIGHT 32000

static const uint8_t cos_tab_256[] = {
  255, 255, 255, 255, 254, 254, 254, 253, 253, 252, 251,
  250, 250, 249, 248, 246, 245, 244, 243, 241, 240, 238, 237, 235, 234,
  232, 230, 228, 226, 224, 222, 220, 218, 215, 213, 211, 208, 206, 203,
  201, 198, 196, 193, 190, 188, 185, 182, 179, 176, 173, 170, 167, 165,
  162, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131, 127, 124, 121,
  118, 115, 112, 109, 106, 103, 100, 97, 93, 90, 88, 85, 82, 79, 76, 73,
  70, 67, 65, 62, 59, 57, 54, 52, 49, 47, 44, 42, 40, 37, 35, 33, 31, 29,
  27, 25, 23, 21, 20, 18, 17, 15, 14, 12, 11, 10, 9, 7, 6, 5, 5, 4, 3, 2,
  2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 7, 9, 10,
  11, 12, 14, 15, 17, 18, 20, 21, 23, 25, 27, 29, 31, 33, 35, 37, 40, 42,
  44, 47, 49, 52, 54, 57, 59, 62, 65, 67, 70, 73, 76, 79, 82, 85, 88, 90,
  93, 97, 100, 103, 106, 109, 112, 115, 118, 121, 124, 128, 131, 134, 137,
  140, 143, 146, 149, 152, 155, 158, 162, 165, 167, 170, 173, 176, 179,
  182, 185, 188, 190, 193, 196, 198, 201, 203, 206, 208, 211, 213, 215,
  218, 220, 222, 224, 226, 228, 230, 232, 234, 235, 237, 238, 240, 241,
  243, 244, 245, 246, 248, 249, 250, 250, 251, 252, 253, 253, 254, 254,
  254, 255, 255, 255
};

uint32_t lbm_display_rgb888_from_color(color_t color, int x, int y) {
  switch (color.type) {
  case COLOR_REGULAR:
    return (uint32_t)color.color1;

  case COLOR_GRADIENT_X:
  case COLOR_GRADIENT_Y: {
    uint32_t res;
    uint32_t r1 = (uint32_t)color.color1 >> 16;
    uint32_t g1 = (uint32_t)color.color1 >> 8 & 0xFF;
    uint32_t b1 = (uint32_t)color.color1 & 0xff;

    uint32_t r2 = (uint32_t)color.color2 >> 16;
    uint32_t g2 = (uint32_t)color.color2 >> 8 & 0xFF;
    uint32_t b2 = (uint32_t)color.color2 & 0xff;

    int used_len = color.mirrored ? 256 : 128;

    int pos = color.type == COLOR_GRADIENT_X ? x : y;
    // int tab_pos = ((pos * 256) / color.param1 + color.param2) % 256;
    int tab_pos = (((pos - color.param2) * 256) / color.param1 / 2) % used_len;
    if (tab_pos < 0) {
      tab_pos += used_len;
    }

    uint32_t tab_val = (uint32_t)cos_tab_256[tab_pos];

    uint32_t r = (r1 * tab_val + r2 * (255 - tab_val)) / 255;
    uint32_t g = (g1 * tab_val + g2 * (255 - tab_val)) / 255;
    uint32_t b = (b1 * tab_val + b2 * (255 - tab_val)) / 255;

    res = r << 16 | g << 8 | b;
    return res;
  }

  default:
    return 0;
  }
}

static const char *color_desc = "Color";

static lbm_uint symbol_indexed2 = 0;
static lbm_uint symbol_indexed4 = 0;
static lbm_uint symbol_indexed16 = 0;
static lbm_uint symbol_rgb332 = 0;
static lbm_uint symbol_rgb565 = 0;
static lbm_uint symbol_rgb888 = 0;

static lbm_uint symbol_thickness = 0;
static lbm_uint symbol_filled = 0;
static lbm_uint symbol_rounded = 0;
static lbm_uint symbol_dotted = 0;
static lbm_uint symbol_scale = 0;
static lbm_uint symbol_rotate = 0;
static lbm_uint symbol_resolution = 0;

static lbm_uint symbol_regular = 0;
static lbm_uint symbol_gradient_x = 0;
static lbm_uint symbol_gradient_y = 0;
static lbm_uint symbol_gradient_x_pre = 0;
static lbm_uint symbol_gradient_y_pre = 0;
static lbm_uint symbol_repeat = 0;
static lbm_uint symbol_mirrored = 0;

static lbm_uint symbol_color_0 = 0;
static lbm_uint symbol_color_1 = 0;
static lbm_uint symbol_width = 0;
static lbm_uint symbol_offset = 0;
static lbm_uint symbol_repeat_type = 0;

static lbm_uint symbol_down = 0;
static lbm_uint symbol_up = 0;

static color_format_t sym_to_color_format(lbm_value v) {
  lbm_uint s = lbm_dec_sym(v);
  if (s == symbol_indexed2) return indexed2;
  if (s == symbol_indexed4) return indexed4;
  if (s == symbol_indexed16) return indexed16;
  if (s == symbol_rgb332) return rgb332;
  if (s == symbol_rgb565) return rgb565;
  if (s == symbol_rgb888) return rgb888;
  return format_not_supported;
}

static uint32_t image_dims_to_size_bytes(color_format_t fmt, uint16_t width, uint16_t height) {
  uint32_t num_pix = (uint32_t)width * (uint32_t)height;
  switch(fmt) {
  case indexed2:
    if (num_pix % 8 != 0) return (num_pix / 8) + 1;
    else return (num_pix / 8);
    break;
  case indexed4:
    if (num_pix % 4 != 0) return (num_pix / 4) + 1;
    else return (num_pix / 4);
    break;
  case indexed16: // Two pixels per byte
    if (num_pix % 2 != 0) return (num_pix / 2) + 1;
    else return (num_pix / 2);
  case rgb332:
    return num_pix;
    break;
  case rgb565:
    return num_pix * 2;
    break;
  case rgb888:
    return num_pix * 3;
  default:
    return 0;
  }
}

static lbm_value image_buffer_lift(uint8_t *buf, color_format_t fmt, uint16_t width, uint16_t height) {
  lbm_value res = ENC_SYM_MERROR;
  lbm_uint size = image_dims_to_size_bytes(fmt, width, height);
  if ( lbm_lift_array(&res, (char*)buf, IMAGE_BUFFER_HEADER_SIZE + size)) {
    buf[0] = (uint8_t)(width >> 8);
    buf[1] = (uint8_t)width;
    buf[2] = (uint8_t)(height >> 8);
    buf[3] = (uint8_t)height;
    buf[4] = color_format_to_byte(fmt);
  }
  return res;
}

static bool color_destructor(lbm_uint value) {
  color_t *color = (color_t*)value;
  if (color->precalc) {
    lbm_free((void*)color->precalc);
  }
  lbm_free((void*)color);
  return true;
}

static lbm_value color_allocate(COLOR_TYPE type, int32_t color1, int32_t color2, uint16_t param1, uint16_t param2, bool mirrored) {
  color_t *color = lbm_malloc(sizeof(color_t));
  if (!color) {
    return ENC_SYM_MERROR;
  }

  uint32_t *pre = 0;
  if (type == COLOR_PRE_X || type == COLOR_PRE_Y) {
    pre = lbm_malloc(COLOR_PRECALC_LEN * sizeof(uint32_t));
    if (!pre) {
      lbm_free(color);
      return ENC_SYM_MERROR;
    }
  }

  lbm_value res;
  if (!lbm_custom_type_create((lbm_uint)color,
                              color_destructor, color_desc, &res)) {
    lbm_free(color);
    if (pre) {
      lbm_free(pre);
    }
    return ENC_SYM_MERROR;
  }

  color->type = type;
  color->color1 = color1;
  color->color2 = color2;
  color->param1 = param1;
  color->param2 = param2;
  color->mirrored = mirrored;
  color->precalc = pre;

  if (pre) {
    COLOR_TYPE type_old = color->type;
    if (type == COLOR_PRE_X) {
      color->type = COLOR_GRADIENT_X;
    } else if (type == COLOR_PRE_Y) {
      color->type = COLOR_GRADIENT_Y;
    }

    if (color->param1 > COLOR_PRECALC_LEN) {
      color->param1 = COLOR_PRECALC_LEN;
    }

    for (int i = 0;i < color->param1;i++) {
      pre[i] = lbm_display_rgb888_from_color(*color, i + color->param2, i + color->param2);
    }

    color->type = type_old;
  }

  return res;
}

static lbm_value image_buffer_allocate(color_format_t fmt, uint16_t width, uint16_t height) {
  uint32_t size_bytes = image_dims_to_size_bytes(fmt, width, height);

  uint8_t *buf = lbm_malloc(IMAGE_BUFFER_HEADER_SIZE + size_bytes);
  if (!buf) {
    return ENC_SYM_MERROR;
  }
  memset(buf, 0, size_bytes + IMAGE_BUFFER_HEADER_SIZE);
  lbm_value res = image_buffer_lift(buf, fmt, width, height);
  if (lbm_is_symbol(res)) { /* something is wrong, free */
    lbm_free(buf);
  }
  return res;
}

static lbm_value image_buffer_allocate_dm(lbm_uint *dm, color_format_t fmt, uint16_t width, uint16_t height) {
  uint32_t size_bytes = image_dims_to_size_bytes(fmt, width, height);

  lbm_value res = lbm_defrag_mem_alloc(dm, IMAGE_BUFFER_HEADER_SIZE + size_bytes);
  if (lbm_is_symbol(res)) {
    return res;
  }
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
  uint8_t *buf = (uint8_t*)arr->data;
  buf[0] = (uint8_t)(width >> 8);
  buf[1] = (uint8_t)width;
  buf[2] = (uint8_t)(height >> 8);
  buf[3] = (uint8_t)height;
  buf[4] = color_format_to_byte(fmt);  
  return res;
}

// Exported interface
bool display_is_color(lbm_value v) {
  return (lbm_is_custom(v) && ((lbm_uint)lbm_get_custom_descriptor(v) == (lbm_uint)color_desc));
}

// Register symbols

static bool register_symbols(void) {
  bool res = true;
  res = res && lbm_add_symbol_const("indexed2", &symbol_indexed2);
  res = res && lbm_add_symbol_const("indexed4", &symbol_indexed4);
  res = res && lbm_add_symbol_const("indexed16", &symbol_indexed16);
  res = res && lbm_add_symbol_const("rgb332", &symbol_rgb332);
  res = res && lbm_add_symbol_const("rgb565", &symbol_rgb565);
  res = res && lbm_add_symbol_const("rgb888", &symbol_rgb888);

  res = res && lbm_add_symbol_const("thickness", &symbol_thickness);
  res = res && lbm_add_symbol_const("filled", &symbol_filled);
  res = res && lbm_add_symbol_const("rounded", &symbol_rounded);
  res = res && lbm_add_symbol_const("dotted", &symbol_dotted);
  res = res && lbm_add_symbol_const("scale", &symbol_scale);
  res = res && lbm_add_symbol_const("rotate", &symbol_rotate);
  res = res && lbm_add_symbol_const("resolution", &symbol_resolution);

  res = res && lbm_add_symbol_const("regular", &symbol_regular);
  res = res && lbm_add_symbol_const("gradient_x", &symbol_gradient_x);
  res = res && lbm_add_symbol_const("gradient_y", &symbol_gradient_y);
  res = res && lbm_add_symbol_const("gradient_x_pre", &symbol_gradient_x_pre);
  res = res && lbm_add_symbol_const("gradient_y_pre", &symbol_gradient_y_pre);
  res = res && lbm_add_symbol_const("mirrored", &symbol_mirrored);
  res = res && lbm_add_symbol_const("repeat", &symbol_repeat);

  res = res && lbm_add_symbol_const("color-0", &symbol_color_0);
  res = res && lbm_add_symbol_const("color-1", &symbol_color_1);
  res = res && lbm_add_symbol_const("width", &symbol_width);
  res = res && lbm_add_symbol_const("offset", &symbol_offset);
  res = res && lbm_add_symbol_const("repeat-type", &symbol_repeat_type);

  res = res && lbm_add_symbol_const("down", &symbol_down);
  res = res && lbm_add_symbol_const("up", &symbol_up);

  return res;
}

// Internal functions

static int sign(int v) {
  if (v > 0) {
    return 1;
  } else if (v < 0) {
    return -1;
  } else {
    return 0;
  }
}

// Geometry utility functions

// Checks if a point is past a line formed by the given end and start points.
// The returned value is 1 if it is past, -1 if it's on the other side of the
// line, or 0 if it's exactly on the line.
// Don't ask me what is considered the "positive" side of the line ;)
//
// It would probably be more logical if the sign of the result was flipped...
static int point_past_line(int x, int y, int line_start_x, int line_start_y, int line_end_x, int line_end_y) {
  // source: https://stackoverflow.com/a/11908158/15507414

  // this is not really a cross product, but whatever...
  int cross_prod = (x - line_start_x) * (line_end_y - line_start_y)
    - (y - line_start_y) * (line_end_x - line_start_x);

  if (cross_prod > 0) {
    return 1;
  } else if (cross_prod < 0) {
    return -1;
  } else {
    return 0;
  }
}

static bool points_same_quadrant(int x0, int y0, int x1, int y1) {
  return (sign(x0) == sign(x1) || sign(x0) == 0 || sign(x1) == 0)
    && (sign(y0) == sign(y1) || sign(y0) == 0 || sign(y1) == 0);
}

static inline void norm_angle(float *angle) {
  while (*angle < -M_PI) { *angle += 2.0f * (float)M_PI; }
  while (*angle >=  M_PI) { *angle -= 2.0f * (float)M_PI; }
}

static inline void norm_angle_0_2pi(float *angle) {
  while (*angle < 0) { *angle += 2.0f * (float)M_PI; }
  while (*angle >= 2.0 * M_PI) { *angle -= 2.0f * (float)M_PI; }
}

static uint8_t rgb888to332(uint32_t rgb) {
  uint8_t r = (uint8_t)(rgb >> (16 + 5));
  uint8_t g = (uint8_t)(rgb >> (8 + 5));
  uint8_t b = (uint8_t)(rgb >> 6);
  r = (uint8_t)(r << 5);
  g = (g & 0x7) << 2;  ;
  b = (b & 0x3);
  uint8_t res_rgb332 = r | g | b;
  return res_rgb332;
}

static uint16_t rgb888to565(uint32_t rgb) {
  uint16_t r = (uint16_t)(rgb >> (16 + 3));
  uint16_t g = (uint16_t)(rgb >> (8 + 2));
  uint16_t b = (uint16_t)(rgb >> 3);
  r = (uint8_t)(r << 11);
  g = (g & 0x3F) << 5;
  b = (b & 0x1F);
  uint16_t res_rgb565 = r | g | b;
  return res_rgb565;
}

static uint32_t rgb332to888(uint8_t rgb) {
  uint32_t r = (uint32_t)((rgb>>5) & 0x7);
  uint32_t g = (uint32_t)((rgb>>2) & 0x7);
  uint32_t b = (uint32_t)(rgb & 0x3);
  uint32_t res_rgb888 = r << (16 + 5) | g << (8 + 5) | b << 6;
  return res_rgb888;
}

static uint32_t  rgb565to888(uint16_t rgb) {
  uint32_t r = (uint32_t)(rgb >> 11);
  uint32_t g = (uint32_t)((rgb >> 5) & 0x3F);
  uint32_t b = (uint32_t)(rgb & 0x1F);
  uint32_t res_rgb888 = r << (16 + 3) | g << (8 + 2) | b << 3;
  return res_rgb888;
}

void image_buffer_clear(image_buffer_t *img, uint32_t cc) {
  color_format_t fmt = img->fmt;
  uint32_t w = img->width;
  uint32_t h = img->height;
  uint32_t img_size = w * h;
  uint8_t *data = img->data;
  switch (fmt) {
  case indexed2: {
    uint32_t bytes = (img_size / 8) + (img_size % 8 ? 1 : 0);
    uint8_t c8 = (uint8_t)((cc & 1) ? 0xFF : 0x0);
    memset(data, c8, bytes);
  }
    break;
  case indexed4: {
    static const uint8_t index4_table[4] = {0x00, 0x55, 0xAA, 0xFF};
    uint32_t bytes = (img_size / 4) + (img_size % 4 ? 1 : 0);
    uint8_t ix = (uint8_t)(cc & 0x3);
    memset(data, index4_table[ix], bytes);
  }
    break;
  case indexed16: {
    uint32_t bytes = (img_size / 2) + (img_size % 2 ? 1 : 0);
    uint8_t ix = (uint8_t)(cc & 0xF);
    uint8_t color = (uint8_t)(ix | ix << 4);  // create a color based on duplication of index
    memset(data, color, bytes);
  }
    break;
  case rgb332: {
    memset(data, rgb888to332(cc), img_size);
  }
    break;
  case rgb565: {
    uint16_t c = rgb888to565(cc);
    uint8_t *dp = (uint8_t*)data;
    for (unsigned int i = 0; i < img_size/2; i +=2) {
      dp[i] = (uint8_t)(c >> 8);
      dp[i+1] = (uint8_t)c;
    }
  }
    break;
  case rgb888: {
    uint8_t *dp = (uint8_t*)data;
    for (unsigned int i = 0; i < img_size * 3; i+= 3) {
      dp[i]   = (uint8_t)(cc >> 16);
      dp[i+1] = (uint8_t)(cc >> 8);
      dp[i+2] = (uint8_t)cc;
    }
  }
    break;
  default:
    break;
  }
}

static const uint8_t indexed4_mask[4] = {0x03, 0x0C, 0x30, 0xC0};
static const uint8_t indexed4_shift[4] = {0, 2, 4, 6};
static const uint8_t indexed16_mask[4] = {0x0F, 0xF0};
static const uint8_t indexed16_shift[4] = {0, 4};


static void putpixel(image_buffer_t* img, int x_i, int y_i, uint32_t c) {
  color_format_t fmt = img->fmt;
  uint16_t w = img->width;
  uint16_t h = img->height;
  uint16_t x = (uint16_t)x_i; // negative numbers become really large.
  uint16_t y = (uint16_t)y_i;

  if (x < w && y < h) {
    uint8_t *data = img->data;
    switch(fmt) {
    case indexed2: {
      uint32_t pos = (uint32_t)y * (uint32_t)w + (uint32_t)x;
      uint32_t byte = pos >> 3;
      uint32_t bit  = 7 - (pos & 0x7);
      if (c) {
        data[byte] |= (uint8_t)(1 << bit);
      } else {
        data[byte] &= (uint8_t)~(1 << bit);
      }
      break;
    }
    case indexed4: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 2;
      uint32_t ix  = 3 - (pos & 0x3);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed4_mask[ix]) | (uint8_t)(c << indexed4_shift[ix]));
      break;
    }
    case indexed16: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 1;
      uint32_t ix  = 1 - (pos & 0x1);
      data[byte] = (uint8_t)((uint8_t)(data[byte] & ~indexed16_mask[ix]) | (uint8_t)(c << indexed16_shift[ix]));
      break;
    }
    case rgb332: {
      int pos = y*w + x;
      data[pos] = rgb888to332(c);
      break;
    }
    case rgb565: {
      int pos = y*(w<<1) + (x<<1) ;
      uint16_t color = rgb888to565(c);
      data[pos] = (uint8_t)(color >> 8);
      data[pos+1] = (uint8_t)color;
      break;
    }
    case rgb888: {
      int pos = y*(w*3) + (x*3);
      data[pos] = (uint8_t)(c>>16);
      data[pos+1] = (uint8_t)(c>>8);
      data[pos+2] = (uint8_t)c;
      break;
    }
    default:
      break;
    }
  }
}

static uint32_t getpixel(image_buffer_t* img, int x_i, int y_i) {
  color_format_t fmt = img->fmt;
  uint16_t w = img->width;
  uint16_t h = img->height;
  uint16_t x = (uint16_t)x_i;
  uint16_t y = (uint16_t)y_i;

  if (x < w && y < h) {
    uint8_t *data = img->data;
    switch(fmt) {
    case indexed2: {
      uint32_t pos = (uint32_t)y * w + x;
      uint32_t byte = pos >> 3;
      uint32_t bit  = 7 - (pos & 0x7);
      return (uint32_t)(data[byte] >> bit) & 0x1;
    }
    case indexed4: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 2;
      uint32_t ix  = 3 - (pos & 0x3);
      return (uint32_t)((data[byte] & indexed4_mask[ix]) >> indexed4_shift[ix]);
    }
    case indexed16: {
      uint32_t pos = (uint32_t)y*w + x;
      uint32_t byte = pos >> 1;
      uint32_t ix  = 1 - (pos & 0x1);
      return (uint32_t)((data[byte] & indexed16_mask[ix]) >> indexed16_shift[ix]);
    }
    case rgb332: {
      int pos = y*w + x;
      return rgb332to888(data[pos]);
    }
    case rgb565: {
      int pos = y*(w<<1) + (x<<1);
      uint16_t c = (uint16_t)(((uint16_t)data[pos] << 8) | (uint16_t)data[pos+1]);
      return rgb565to888(c);
    }
    case rgb888: {
      int pos = y*(w*3) + (x*3);
      uint32_t r = data[pos];
      uint32_t g = data[pos+1];
      uint32_t b = data[pos+2];
      return (r << 16 | g << 8 | b);
    }
    default:
      break;
    }
  }
  return 0;
}

static void h_line(image_buffer_t* img, int x, int y, int len, uint32_t c) {
  for (int i = 0; i < len; i ++) {
    putpixel(img, x+i, y, c);
  }
}

static void v_line(image_buffer_t* img, int x, int y, int len, uint32_t c) {
  for (int i = 0; i < len; i ++) {
    putpixel(img, x, y+i, c);
  }
}

static void fill_circle(image_buffer_t *img, int x, int y, int radius, uint32_t color) {
  switch (radius) {
  case 0:
    break;

  case 1:
    putpixel(img, x - 1, y - 1, color);
    putpixel(img, x, y - 1, color);
    putpixel(img, x - 1, y, color);
    putpixel(img, x, y, color);
    break;

  case 2:
    h_line(img, x - 1, y - 2, 2, color);
    h_line(img, x - 2, y - 1, 4, color);
    h_line(img, x - 2, y, 4, color);
    h_line(img, x - 1, y + 1, 2, color);
    break;

  case 3:
    h_line(img, x - 2, y - 3, 4, color);
    h_line(img, x - 3, y - 2, 6, color);
    h_line(img, x - 3, y - 1, 6, color);
    h_line(img, x - 3, y, 6, color);
    h_line(img, x - 3, y + 1, 6, color);
    h_line(img, x - 2, y + 2, 4, color);
    break;

  case 4:
    h_line(img, x - 2, y - 4, 4, color);
    h_line(img, x - 3, y - 3, 6, color);
    h_line(img, x - 4, y - 2, 8, color);
    h_line(img, x - 4, y - 1, 8, color);
    h_line(img, x - 4, y, 8, color);
    h_line(img, x - 4, y + 1, 8, color);
    h_line(img, x - 3, y + 2, 6, color);
    h_line(img, x - 2, y + 3, 4, color);
    break;

  default: {
    int r_sq = radius * radius;
    for (int y1 = -radius; y1 <= radius; y1++) {
      for (int x1 = -radius; x1 <= radius; x1++) {
        if (x1 * x1 + y1 * y1 <= r_sq) {
          // Compute the start and end position for x axis
          int x_left = x1;
          while ((x1 + 1) <= radius && ((x1 + 1) * (x1 + 1) + y1 * y1) <= r_sq) {
            x1++;
          }
          int x_right = x1;

          // Draw line at this level y
          int length = x_right - x_left + 1;
          h_line(img, x + x_left, y + y1, length, color);

          // Break out of innter loop for this level y
          break;
        }
      }
    }
  } break;
  }
}

// Circle helper function, to draw a circle with an inner and outer radius.
// Draws the slice at the given outer radius point.
static void handle_circle_slice(int outer_x, int outer_y, image_buffer_t *img, int c_x, int c_y, int radius_inner, uint32_t color, int radius_inner_dbl_sq) {
  int width;

  bool slice_filled;
  if (outer_y < 0) {
    slice_filled = -outer_y > radius_inner;
  } else {
    slice_filled = outer_y >= radius_inner;
  }

  if (slice_filled) {
    if (outer_x < 0) {
      width = -outer_x;
    } else {
      width = outer_x + 1;
      outer_x = 0;
    }
  } else {
    int cur_x = outer_x;
    int delta = outer_x > 0 ? -1 : 1;

    // TODO: this could probably be binary searched
    int y_dbl_off = outer_y * 2 + 1;
    int y_dbl_off_sq = y_dbl_off * y_dbl_off;
    while (true) {
      cur_x += delta;
      int x_dbl_off = cur_x * 2 + 1;
      if (x_dbl_off * x_dbl_off + y_dbl_off_sq <= radius_inner_dbl_sq
          || abs(cur_x) > 2000) { // failsafe
        break;
      }
    }
    width = abs(cur_x - outer_x);
    if (outer_x > 0) {
      outer_x = cur_x + 1;
    }
  }

  h_line(img, outer_x + c_x, outer_y + c_y, width, color);
}

// thickness extends inwards from the given radius circle
static void circle(image_buffer_t *img, int x, int y, int radius, int thickness, uint32_t color) {
  if (thickness <= 0) {
    int x0 = 0;
    int y0 = radius;
    int d = 5 - 4*radius;
    int da = 12;
    int db = 20 - 8*radius;

    while (x0 < y0) {
      putpixel(img, x + x0, y + y0, color);
      putpixel(img, x + x0, y - y0, color);
      putpixel(img, x - x0, y + y0, color);
      putpixel(img, x - x0, y - y0, color);
      putpixel(img, x + y0, y + x0, color);
      putpixel(img, x + y0, y - x0, color);
      putpixel(img, x - y0, y + x0, color);
      putpixel(img, x - y0, y - x0, color);
      if (d < 0) { d = d + da; db = db+8; }
      else  { y0 = y0 - 1; d = d+db; db = db + 16; }
      x0 = x0+1;
      da = da + 8;
    }
  } else {
    int radius_inner = radius - thickness;

    int radius_outer_dbl_sq = radius * radius * 4;
    int radius_inner_dbl_sq = radius_inner * radius_inner * 4;

    for (int y0 = 0; y0 < radius; y0++) {
      int y_dbl_offs = 2 * y0 + 1;
      int y_dbl_offs_sq = y_dbl_offs * y_dbl_offs;

      for (int x0 = -radius; x0 <= 0; x0++) {
        int x_dbl_offs = 2 * x0 + 1;
        if (x_dbl_offs * x_dbl_offs + y_dbl_offs_sq <= radius_outer_dbl_sq) {
          // This is horrible...
          handle_circle_slice(x0, y0,
                              img, x, y, radius_inner, color, radius_inner_dbl_sq);
          handle_circle_slice(-x0 - 1, y0,
                              img, x, y, radius_inner, color, radius_inner_dbl_sq);
          handle_circle_slice(x0, -y0 - 1,
                              img, x, y, radius_inner, color, radius_inner_dbl_sq);
          handle_circle_slice(-x0 - 1, -y0 - 1,
                              img, x, y, radius_inner, color, radius_inner_dbl_sq);
          break;
        }
      }
    }
  }
}

// Thickness extends outwards and inwards from the given line equally, resulting
// in double the total thickness.
// TODO: This should be more efficient
// http://homepages.enterprise.net/murphy/thickline/index.html
// https://github.com/ArminJo/STMF3-Discovery-Demos/blob/master/lib/BlueDisplay/LocalGUI/ThickLine.hpp
static void line(image_buffer_t *img, int x0, int y0, int x1, int y1, int thickness, int dot1, int dot2, uint32_t c) {
  int dx = abs(x1 - x0);
  int sx = x0 < x1 ? 1 : -1;
  int dy = -abs(y1 - y0);
  int sy = y0 < y1 ? 1 : -1;
  int error = dx + dy;

  if (dot1 > 0) {
    // These are used to deal with consecutive calls with
    // possibly overlapping pixels.
    static int dotcnt = 0;
    static int x_last = 0;
    static int y_last = 0;

    while (true) {
      if (dotcnt <= dot1) {
        if (thickness > 1) {
          fill_circle(img, x0, y0, thickness, c);
        } else {
          putpixel(img, x0, y0, c);
        }
      }

      if (x0 != x_last || y0 != y_last) {
        dotcnt++;
      }

      x_last = x0;
      y_last = y0;

      if (dotcnt >= (dot1 + dot2)) {
        dotcnt = 0;
      }

      if (x0 == x1 && y0 == y1) {
        break;
      }
      if ((error * 2) >= dy) {
        if (x0 == x1) {
          break;
        }
        error += dy;
        x0 += sx;
      }
      if ((error * 2) <= dx) {
        if (y0 == y1) {
          break;
        }
        error += dx;
        y0 += sy;
      }
    }
  } else {
    while (true) {
      if (thickness > 1) {
        fill_circle(img, x0, y0, thickness, c);
      } else {
        putpixel(img, x0, y0, c);
      }

      if (x0 == x1 && y0 == y1) {
        break;
      }
      if ((error * 2) >= dy) {
        if (x0 == x1) {
          break;
        }
        error += dy;
        x0 += sx;
      }
      if ((error * 2) <= dx) {
        if (y0 == y1) {
          break;
        }
        error += dx;
        y0 += sy;
      }
    }
  }
}

// thickness extends inwards from the given rectangle edge.
static void rectangle(image_buffer_t *img, int x, int y, int width, int height,
                      bool fill, int thickness, int dot1, int dot2, uint32_t color) {
  thickness /= 2;

  if (fill) {
    for (int i = y; i < (y + height);i++) {
      h_line(img, x, i, width, color);
    }
  } else {
    if (thickness <= 0 && dot1 == 0) {
      h_line(img, x, y, width, color);
      h_line(img, x, y + height, width, color);
      v_line(img, x, y, height, color);
      v_line(img, x + width, y, height, color);
    } else {
      x += thickness;
      y += thickness;
      width -= thickness * 2;
      height -= thickness * 2;
      // top
      line(img, x, y, x + width, y, thickness, dot1, dot2, color);
      // bottom
      line(img, x, y + height, x + width, y + height, thickness, dot1, dot2, color);
      // left
      line(img, x, y, x, y + height, thickness, dot1, dot2, color);
      // right
      line(img, x + width, y, x + width, y + height, thickness, dot1, dot2, color);
    }
  }
}

#define NMIN(a, b) ((a) < (b) ? (a) : (b))
#define NMAX(a, b) ((a) > (b) ? (a) : (b))

static void fill_triangle(image_buffer_t *img, int x0, int y0,
                          int x1, int y1, int x2, int y2, uint32_t color) {
  int x_min = NMIN(x0, NMIN(x1, x2));
  int x_max = NMAX(x0, NMAX(x1, x2));
  int y_min = NMIN(y0, NMIN(y1, y2));
  int y_max = NMAX(y0, NMAX(y1, y2));

  for (int y = y_min;y <= y_max;y++) {
    for (int x = x_min;x <= x_max;x++) {
      int w0 = point_past_line(x, y, x1, y1, x2, y2);
      int w1 = point_past_line(x, y, x2, y2, x0, y0);
      int w2 = point_past_line(x, y, x0, y0, x1, y1);

      if ((w0 >= 0 && w1 >= 0 && w2 >= 0)
          || (w0 <= 0 && w1 <= 0 && w2 <= 0)) {
        putpixel(img, x, y, color);
      }
    }
  }
}

static void generic_arc(image_buffer_t *img, int x, int y, int rad, float ang_start, float ang_end,
                        int thickness, bool filled, int dot1, int dot2, int res, bool sector, bool segment, uint32_t color) {
  ang_start *= (float)M_PI / 180.0f;
  ang_end *= (float)M_PI / 180.0f;

  norm_angle(&ang_start);
  norm_angle(&ang_end);

  float ang_range = ang_end - ang_start;

  if (ang_range < 0.0) {
    ang_range += 2.0f * (float)M_PI;
  }

  if (res <= 0) {
    res = 80;
  }

  float steps = ceilf((float)res * ang_range * (0.5f / (float)M_PI));

  float ang_step = ang_range / steps;
  float sa = sinf(ang_step);
  float ca = cosf(ang_step);

  float px_start = cosf(ang_start) * (float)rad;
  float py_start = sinf(ang_start) * (float)rad;


  float px = px_start;
  float py = py_start;

  for (int i = 0;i < steps;i++) {
    float px_before = px;
    float py_before = py;

    px = px * ca - py * sa;
    py = py * ca + px_before * sa;

    if (filled) {
      if (sector) {
        fill_triangle(img,
                      x + (int)px_before, y + (int)py_before,
                      x + (int)px, y + (int)py,
                      x, y,
                      color);
      } else {
        fill_triangle(img,
                      x + (int)px_before, y + (int)py_before,
                      x + (int)px, y + (int)py,
                      x + (int)px_start, y + (int)py_start,
                      color);
      }
    } else {
      line(img, x + (int)px_before, y + (int)py_before,
           x + (int)px, y + (int)py, thickness, dot1, dot2, color);
    }
  }

  if (!filled && sector) {
    line(img, x + (int)px, y + (int)py,
         x, y,
         thickness, dot1, dot2, color);
    line(img, x, y,
         x + (int)px_start, y + (int)py_start,
         thickness, dot1, dot2, color);
  }

  if (!filled && segment) {
    line(img, x + (int)px, y + (int)py,
         x + (int)px_start, y + (int)py_start,
         thickness, dot1, dot2, color);
  }
}

// thin arc helper function
// handles a single pixel in the complete circle, checking if the pixel is part
// of the arc.
static void handle_thin_arc_pixel(image_buffer_t *img, int x, int y,
                                  int c_x, int c_y, int cap0_x, int cap0_y, int cap1_x, int cap1_y, int min_y, int max_y, bool angle_is_closed, uint32_t color) {
  if (y > max_y || y < min_y) {
    return;
  }

  int line_is_past_0 = point_past_line(x, y, 0, 0, cap0_x, cap0_y);
  int line_is_past_1 = -point_past_line(x, y, 0, 0, cap1_x, cap1_y);

  bool in_cap0_quadrant = points_same_quadrant(
                                               x, y, cap0_x, cap0_y);
  bool in_cap1_quadrant = points_same_quadrant(
                                               x, y, cap1_x, cap1_y);

  if (angle_is_closed) {
    if (line_is_past_0 == 1 && line_is_past_1 == 1) {
      return;
    }
  } else {
    if (line_is_past_0 == 1 || line_is_past_1 == 1
        || (line_is_past_0 == 0 && !in_cap0_quadrant)
        || (line_is_past_1 == 0 && !in_cap1_quadrant)) {
      return;
    }
  }

  putpixel(img, c_x + x, c_y + y, color);
}

// single pixel wide arc
static void thin_arc(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1, bool sector, bool segment, uint32_t color) {
  if (radius == 0) {
    return;
  }

  angle0 *= (float)M_PI / 180.0f;
  angle1 *= (float)M_PI / 180.0f;
  norm_angle_0_2pi(&angle0);
  norm_angle_0_2pi(&angle1);

  if (angle0 == angle1) {
    return;
  }

  bool angle_is_closed;
  // if the angle of the filled in part of the arc is greater than 180°
  // honestly unsure if it'd be better if this was called angle_is_open
  if (angle1 - angle0 > 0.0) {
    angle_is_closed = fabsf(angle1 - angle0) > M_PI;
  } else {
    angle_is_closed = fabsf(angle1 - angle0) < M_PI;
  }

  int cap0_x = (int)(cosf(angle0) * (float)(radius));
  int cap0_y = (int)(sinf(angle0) * (float)(radius));

  int cap1_x = (int)(cosf(angle1) * (float)(radius));
  int cap1_y = (int)(sinf(angle1) * (float)(radius));

  // Highest and lowest (y coord wise) drawn line of the base arc (excluding
  // the circular end caps). This range is *inclusive*!
  // Note that these might be slightly off due to inconsistent rounding between
  // my circle drawing algorithm and point rotation.
  int min_y = MIN(cap0_y, cap1_y);
  int max_y = MAX(cap0_y, cap1_y);
  if (angle0 < angle1) {
    if (angle0 < M_PI_2 && angle1 >= M_3PI_2) {
      min_y = -radius;
      max_y = radius;
    } else if (angle0 < M_3PI_2 && angle1 > M_3PI_2) {
      min_y = -radius;
    } else if (angle0 < M_PI_2 && angle1 > M_PI_2) {
      max_y = radius;
    }
  } else {
    if ((angle0 < M_3PI_2 && angle1 >= M_PI_2)
        || (angle0 < M_PI_2)
        || (angle1 > M_3PI_2)) {
      min_y = -radius;
      max_y = radius;
    } else if (angle0 < M_3PI_2 && angle1 < M_PI_2) {
      min_y = -radius;
    } else if (angle0 > M_PI_2 && angle1 > M_PI_2) {
      max_y = radius;
    }
  }

  int radius_dbl_sq = radius * radius * 4;

  int last_x = 0;
  for (int y = radius - 1; y >= 0; y--) {
    int y_dbl_offs = 2 * y + 1;
    int y_dbl_offs_sq = y_dbl_offs * y_dbl_offs;

    for (int x = -radius; x <= 0; x++) {
      int x_dbl_offs = 2 * x + 1;
      if (x_dbl_offs * x_dbl_offs + y_dbl_offs_sq <= radius_dbl_sq) {
        if (last_x - x < 2) {
          // This is horrible...
          handle_thin_arc_pixel(img, x, y,
                                c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);
          handle_thin_arc_pixel(img, -x - 1, y,
                                c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);

          handle_thin_arc_pixel(img, x, -y - 1,
                                c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);
          handle_thin_arc_pixel(img, -x - 1, -y - 1,
                                c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);
        } else {
          for (int x0 = x; x0 < last_x; x0++) {
            handle_thin_arc_pixel(img, x0, y,
                                  c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);
            handle_thin_arc_pixel(img, -x0 - 1, y,
                                  c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);

            handle_thin_arc_pixel(img, x0, -y - 1,
                                  c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);
            handle_thin_arc_pixel(img, -x0 - 1, -y - 1,
                                  c_x, c_y, cap0_x, cap0_y, cap1_x, cap1_y, min_y, max_y, angle_is_closed, color);
          }
        }

        last_x = x;
        break;
      }
    }
  }

  if (sector) {
    line(img, c_x, c_y, c_x + cap0_x, c_y + cap0_y, 1, 0, 0, color);
    line(img, c_x, c_y, c_x + cap1_x, c_y + cap1_y, 1, 0, 0, color);
  }

  if (segment) {
    line(img, c_x + cap0_x, c_y + cap0_y, c_x + cap1_x, c_y + cap1_y, 1, 0, 0, color);
  }
}

// arc helper function
// handles a horizontal slice at the given outer arc point
static void handle_arc_slice(image_buffer_t *img, int outer_x, int outer_y, int c_x, int c_y, uint32_t color,
                             int outer_x0, int outer_y0, int outer_x1, int outer_y1,
                             int cap0_min_y, int cap0_max_y, int cap1_min_y, int cap1_max_y,
                             int radius_outer, int radius_inner,
                             int min_y, int max_y,
                             float angle0, float angle1, bool angle_is_closed,
                             bool filled, bool segment,
                             int radius_inner_dbl_sq) {
  (void) radius_outer;
  if (outer_y > max_y || outer_y < min_y) {
    return;
  }

  int line_is_past_0, line_is_past_1;
  line_is_past_0 = point_past_line(outer_x, outer_y, 0, 0, outer_x0, outer_y0);
  line_is_past_1 = -point_past_line(outer_x, outer_y, 0, 0, outer_x1, outer_y1);

  int outer_x_sign = sign(outer_x);
  int outer_y_sign = sign(outer_y);

  int outer_x0_sign = sign(outer_x0);
  int outer_y0_sign = sign(outer_y0);

  int outer_x1_sign = sign(outer_x1);
  int outer_y1_sign = sign(outer_y1);

  bool in_cap0, in_cap1, in_both_caps;
  if (segment && filled) {
    in_cap0 = outer_y <= MAX(outer_y0, outer_y1)
      && outer_y >= MIN(outer_y0, outer_y1);
    in_cap1 = false;
  }
  else if (filled) {
    in_cap0 = outer_y <= cap0_max_y
      && outer_x0_sign == outer_x_sign
      && outer_y0_sign == outer_y_sign;
    in_cap1 = outer_y <= cap1_max_y
      && outer_x1_sign == outer_x_sign
      && outer_y1_sign == outer_y_sign;
  } else {
    in_cap0 = outer_y >= cap0_min_y
      && outer_y <= cap0_max_y
      && outer_x_sign == outer_x0_sign;
    in_cap1 = outer_y >= cap1_min_y
      && outer_y <= cap1_max_y
      && outer_x_sign == outer_x1_sign;
  }
  in_both_caps = in_cap0 && in_cap1;

  bool in_cap0_quadrant = points_same_quadrant(outer_x, outer_y, outer_x0, outer_y0);
  bool in_cap1_quadrant = points_same_quadrant(outer_x, outer_y, outer_x1, outer_y1);

  bool caps_in_same_quadrant = points_same_quadrant(outer_x0, outer_y0, outer_x1, outer_y1);

  // Check if slice is outside caps and drawn sections of the arc.
  if (!in_cap0 && !in_cap1) {
    if (angle_is_closed) {
      if (line_is_past_0 == 1 && line_is_past_1 == 1
          // Failsafe for closed angles with a very small difference.
          // Otherwise a tiny section at the opposite side of the arc
          // might get skipped.
          && (!caps_in_same_quadrant || (in_cap0_quadrant && in_cap1_quadrant))) {
        return;
      }
    } else {
      if (line_is_past_0 == 1 || line_is_past_1 == 1
          || (line_is_past_0 == 0 && !in_cap0_quadrant)
          || (line_is_past_1 == 0 && !in_cap1_quadrant)) {
        return;
      }
    }
  }

  // Find slice width if arc spanned the complete circle.
  int x, x1;
  int width = 0;
  int width1 = 0;
  bool slice_is_split = false;

  bool slice_filled;
  if (filled) {
    slice_filled = true;
  } else {
    if (outer_y < 0) {
      slice_filled = -outer_y > radius_inner;
    } else {
      slice_filled = outer_y >= radius_inner;
    }
  }

  if (slice_filled) {
    if (outer_x < 0) {
      x = outer_x;
      width = -x;
    } else {
      x = 0;
      width = outer_x + 1;
    }
  } else {
    x = outer_x;
    int cur_x = outer_x;
    int delta = outer_x > 0 ? -1 : 1;

    // TODO: this could probably be binary searched
    int y_dbl_off = outer_y * 2 + 1;
    int y_dbl_off_sq = y_dbl_off * y_dbl_off;
    while (true) {
      cur_x += delta;
      int x_dbl_off = cur_x * 2 + 1;
      if (x_dbl_off * x_dbl_off + y_dbl_off_sq <= radius_inner_dbl_sq
          || abs(x) > 2000) { // failsafe
        break;
      }
    }
    width = abs(cur_x - x);
    if (outer_x > 0) {
      x = cur_x + 1;
    }
  }

  // Check which cap lines intersects this slice
  if ((in_cap0 || in_cap1) && !(segment && filled)) {
    // the range from x_start to x_end is *inclusive*
    int x_start = x;
    int x_end = x_start + width - 1;

    int x_start1;
    int x_end1;

    // when a point is "past" a line, it is on the wrong cleared side of it
    int start_is_past0 = point_past_line(x_start, outer_y,
                                         0, 0,
                                         outer_x0, outer_y0);
    int end_is_past0 = point_past_line(x_end, outer_y,
                                       0, 0,
                                       outer_x0, outer_y0);

    int start_is_past1 = -point_past_line(x_start, outer_y,
                                          0, 0,
                                          outer_x1, outer_y1);
    int end_is_past1 = -point_past_line(x_end, outer_y,
                                        0, 0,
                                        outer_x1, outer_y1);

    bool slice_overlaps0 = start_is_past0 != end_is_past0
      && (start_is_past0 != 0 || end_is_past0 != 0);
    bool slice_overlaps1 = start_is_past1 != end_is_past1
      && (start_is_past1 != 0 || end_is_past1 != 0);

    if ((in_cap0 && !in_cap1 && start_is_past0 == 1 && end_is_past0 == 1)
        || (!in_cap0 && in_cap1 && start_is_past1 == 1 && end_is_past1 == 1)
        || (in_both_caps && !angle_is_closed && (
                                                 (start_is_past0 == 1 && end_is_past0 == 1)
                                                 || (start_is_past1 == 1 && end_is_past1 == 1)
                                                 ))
        || (in_both_caps && angle_is_closed && (
                                                (start_is_past0 == 1 && end_is_past0 == 1)
                                                && (start_is_past1 == 1 && end_is_past1 == 1)				
                                                ))) {
      return;
    }

    // The repetition in all these cases could probably be reduced...
    if ((in_both_caps && slice_overlaps0 && !slice_overlaps1)
        || (in_cap0 && !in_cap1 && slice_overlaps0)) {
      // intersect with cap line 0
      if (start_is_past0 != -1 && end_is_past0 != 1) {
        while (start_is_past0 == 1) {
          x_start += 1;
          start_is_past0 = point_past_line(x_start, outer_y,
                                           0, 0,
                                           outer_x0, outer_y0);
        }
      } else {
        while (end_is_past0 == 1) {
          x_end -= 1;
          end_is_past0 = point_past_line(x_end, outer_y,
                                         0, 0,
                                         outer_x0, outer_y0);
        }
      }
    } else if ((in_both_caps && !slice_overlaps0 && slice_overlaps1)
               || (!in_cap0 && in_cap1 && slice_overlaps1)) {
      // intersect with cap line 1
      if (start_is_past1 != -1 && end_is_past1 != 1) {
        while (start_is_past1 == 1) {
          x_start += 1;
          start_is_past1 = -point_past_line(x_start, outer_y,
                                            0, 0,
                                            outer_x1, outer_y1);
        }
      } else {
        while (end_is_past1 == 1) {
          x_end -= 1;
          end_is_past1 = -point_past_line(x_end, outer_y,
                                          0, 0,
                                          outer_x1, outer_y1);
        }
      }
    } else if (in_both_caps && slice_overlaps0 && slice_overlaps1) {
      // intersect with both cap lines
      if (angle0 < angle1) {
        if (angle0 < M_PI) {
          while (start_is_past1 == 1) {
            x_start += 1;
            start_is_past1 = -point_past_line(x_start, outer_y,
                                              0, 0,
                                              outer_x1, outer_y1);
          }
          while (end_is_past0 == 1) {
            x_end -= 1;
            end_is_past0 = point_past_line(x_end, outer_y,
                                           0, 0,
                                           outer_x0, outer_y0);
          }
        } else {
          while (start_is_past0 == 1) {
            x_start += 1;
            start_is_past0 = point_past_line(x_start, outer_y,
                                             0, 0,
                                             outer_x0, outer_y0);
          }
          while (end_is_past1 == 1) {
            x_end -= 1;
            end_is_past1 = -point_past_line(x_end, outer_y,
                                            0, 0,
                                            outer_x1, outer_y1);
          }
        }
      } else {
        // split the slice into two

        slice_is_split = true;

        x_start1 = x_start;
        x_end1 = x_end;

        if (angle0 < M_PI) {
          while (end_is_past0 == 1) {
            x_end -= 1;
            end_is_past0 = point_past_line(x_end, outer_y,
                                           0, 0,
                                           outer_x0, outer_y0);
          }
          while (start_is_past1 == 1) {
            x_start1 += 1;
            start_is_past1 = -point_past_line(x_start1, outer_y,
                                              0, 0,
                                              outer_x1, outer_y1);
          }
        } else {
          while (end_is_past1 == 1) {
            x_end1 -= 1;
            end_is_past1 = -point_past_line(x_end1, outer_y,
                                            0, 0,
                                            outer_x1, outer_y1);
          }
          while (start_is_past0 == 1) {
            x_start += 1;
            start_is_past0 = point_past_line(x_start, outer_y,
                                             0, 0,
                                             outer_x0, outer_y0);
          }
        }

        x1 = x_start1;
        width1 = x_end1 + 1 - x_start1 ;
      }
    }
    x = x_start;
    width = x_end + 1 - x_start;
  } else if (in_cap0 && segment && filled) {
    // the range from x_start to x_end is *inclusive*
    int x_start = x;
    int x_end = x_start + width - 1;

    // when a point is "past" a line, it is on the wrong cleared side of it
    int start_is_past = -point_past_line(x_start, outer_y,
                                         outer_x0, outer_y0, outer_x1, outer_y1);
    int end_is_past = -point_past_line(x_end, outer_y,
                                       outer_x0, outer_y0, outer_x1, outer_y1);

    bool slice_overlaps = start_is_past != end_is_past
      && (start_is_past != 0 || end_is_past != 0);

    if (start_is_past == 1 && end_is_past == 1) {
      return;
    }

    if (slice_overlaps) {
      if (start_is_past != -1 && end_is_past != 1) {
        while (start_is_past == 1) {
          x_start += 1;
          start_is_past = -point_past_line(x_start, outer_y,
                                           outer_x0, outer_y0, outer_x1, outer_y1);
        }
      } else {
        while (end_is_past == 1) {
          x_end -= 1;
          end_is_past = -point_past_line(x_end, outer_y,
                                         outer_x0, outer_y0, outer_x1, outer_y1);
        }
      }
    }

    x = x_start;
    width = x_end + 1 - x_start;
  }

  h_line(img, c_x + x, c_y + outer_y, width, color);
  if (slice_is_split) {
    h_line(img, c_x + x1, c_y + outer_y, width1, color);
  }
}

// TODO: Fix unwanted slice with angles 130 to 115 (I think, angles might be
// slightly off).
// TODO: Look into buggy rendering with angles around 180°-270°. This seems to
// affect arcs, sectors, and segments likewise.
static void arc(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1,
                int thickness, bool rounded, bool filled, bool sector, bool segment, int dot1, int dot2, int resolution, uint32_t color) {
  if (dot1 > 0 && !filled) {
    thickness /= 2;

    radius -= thickness;

    if (thickness == 0) {
      thickness = 1;
    }

    generic_arc(img, c_x, c_y, radius, angle0, angle1, thickness, false, dot1, dot2, resolution, sector, segment, color);

    return;
  }

  if (thickness <= 1 && !filled) {
    thin_arc(img, c_x, c_y, radius, angle0, angle1, sector, segment, color);

    return;
  }

  if (radius == 0) {
    return;
  }

  angle0 *= (float)M_PI / 180.0f;
  angle1 *= (float)M_PI / 180.0f;
  norm_angle_0_2pi(&angle0); // theses are probably unecessary?
  norm_angle_0_2pi(&angle1); // but who knows with floating point imprecision...

  if (angle0 == angle1) {
    return;
  }

  bool angle_is_closed;
  // if the angle of the filled in part of the arc is greater than 180°
  if (angle1 - angle0 > 0.0) {
    angle_is_closed = fabsf(angle1 - angle0) > M_PI;
  } else {
    angle_is_closed = fabsf(angle1 - angle0) < M_PI;
  }

  // angles smaller than 1 degree seem to cause issues (with a radius of 62)
  // this is kinda ugly though, and it will probably still break at larger
  // radii or something...
  if (!angle_is_closed && fabsf(angle1 - angle0) < 0.0174532925) { // one degree in radians
    if (rounded) {
      float rad_f = (float)radius - ((float)thickness / 2.0f);

      float angle = (angle0 + angle1) / 2.0f;

      int cap_center_x = (int)floorf(cosf(angle) * rad_f);
      int cap_center_y = (int)floorf(sinf(angle) * rad_f);

      fill_circle(img, c_x + cap_center_x, c_y + cap_center_y, thickness / 2, color);
    }
    return;
  }

  if (thickness >= radius) {
    filled = true;
  }

  int radius_outer, radius_inner;
  if (filled) {
    radius_outer = radius;
    radius_inner = 0;
  } else {
    radius_outer = radius;
    radius_inner = radius - thickness;
  }
  int radius_outer_dbl_sq = radius_outer * radius_outer * 4;
  int radius_inner_dbl_sq = radius_inner * radius_inner * 4;

  float angle0_cos = cosf(angle0);
  float angle0_sin = sinf(angle0);
  float angle1_cos = cosf(angle1);
  float angle1_sin = sinf(angle1);

  int outer_x0 = (int)(angle0_cos * (float)radius_outer);
  int outer_y0 = (int)(angle0_sin * (float)radius_outer);

  int outer_x1 = (int)(angle1_cos * (float)radius_outer);
  int outer_y1 = (int)(angle1_sin * (float)radius_outer);

  int inner_y0;
  int inner_y1;

  if (filled) {
    inner_y0 = 0;

    inner_y1 = 0;
  } else {
    inner_y0 = (int)(angle0_sin * (float)radius_inner);

    inner_y1 = (int)(angle1_sin * (float)radius_inner);
  }

  int cap0_min_y = MIN(inner_y0, outer_y0);
  int cap0_max_y = MAX(inner_y0, outer_y0);

  int cap1_min_y = MIN(inner_y1, outer_y1);
  int cap1_max_y = MAX(inner_y1, outer_y1);

  // Highest and lowest (y coord wise) drawn line of the base arc (excluding
  // the circular end caps). This range is *inclusive*!
  // Note that these might be slightly off due to inconsistent rounding between
  // Bresenhamn's algorithm and point rotation. (I don't think the point about
  // Bresenhamn is relevant as we don't use it anymore. Still wouldn't trust
  // them completely though...)
  int min_y = MIN(outer_y0, MIN(outer_y1, MIN(inner_y0, inner_y1)));
  int max_y = MAX(outer_y0, MAX(outer_y1, MAX(inner_y0, inner_y1)));
  if (angle0 < angle1) {
    if (angle0 < M_PI_2 && angle1 >= M_3PI_2) {
      min_y = -radius_outer;
      max_y = radius_outer;
    } else if (angle0 < M_3PI_2 && angle1 > M_3PI_2) {
      min_y = -radius_outer;
    } else if (angle0 < M_PI_2 && angle1 > M_PI_2) {
      max_y = radius_outer;
    }
  } else {
    if ((angle0 < M_3PI_2 && angle1 >= M_PI_2)
        || (angle0 < M_PI_2)
        || (angle1 > M_3PI_2)) {
      min_y = -radius_outer;
      max_y = radius_outer;
    } else if (angle0 < M_3PI_2 && angle1 < M_PI_2) {
      min_y = -radius_outer;
    } else if (angle0 > M_PI_2 && angle1 > M_PI_2) {
      max_y = radius_outer;
    }
  }

  for (int y = 0; y < radius_outer; y++) {
    int y_dbl_offs = 2 * (y + 1);
    int y_dbl_offs_sq = y_dbl_offs * y_dbl_offs;

    for (int x = -radius_outer; x <= 0; x++) {
      int x_dbl_offs = 2 * (x + 1);
      if (x_dbl_offs * x_dbl_offs + y_dbl_offs_sq <= radius_outer_dbl_sq) {
        // This is horrible...
        handle_arc_slice(img, x, y,
                         c_x, c_y, color, outer_x0, outer_y0, outer_x1, outer_y1,
                         cap0_min_y, cap0_max_y, cap1_min_y, cap1_max_y, radius_outer, radius_inner, min_y, max_y,
                         angle0, angle1, angle_is_closed, filled, segment, radius_inner_dbl_sq);
        handle_arc_slice(img, -x - 1, y,
                         c_x, c_y, color, outer_x0, outer_y0, outer_x1, outer_y1,
                         cap0_min_y, cap0_max_y, cap1_min_y, cap1_max_y, radius_outer, radius_inner, min_y, max_y,
                         angle0, angle1, angle_is_closed, filled, segment, radius_inner_dbl_sq);

        handle_arc_slice(img, x, -y - 1,
                         c_x, c_y, color, outer_x0, outer_y0, outer_x1, outer_y1,
                         cap0_min_y, cap0_max_y, cap1_min_y, cap1_max_y, radius_outer, radius_inner, min_y, max_y,
                         angle0, angle1, angle_is_closed, filled, segment, radius_inner_dbl_sq);
        handle_arc_slice(img, -x - 1, -y - 1,
                         c_x, c_y, color, outer_x0, outer_y0, outer_x1, outer_y1,
                         cap0_min_y, cap0_max_y, cap1_min_y, cap1_max_y, radius_outer, radius_inner, min_y, max_y,
                         angle0, angle1, angle_is_closed, filled, segment, radius_inner_dbl_sq);

        break;
      }
    }
  }

  // draw rounded line corners
  if (rounded && !filled && !sector && !segment) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);

    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);

    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);

    thickness /= 2;

    fill_circle(img, c_x + cap0_center_x, c_y + cap0_center_y, thickness, color);
    fill_circle(img, c_x + cap1_center_x, c_y + cap1_center_y, thickness, color);
  }

  // draw sector arc cap to center lines
  // (sectors are always rounded)
  if (sector && !filled) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);

    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);

    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);

    thickness /= 2;

    line(img, c_x + cap0_center_x, c_y + cap0_center_y,
         c_x, c_y, thickness, 0, 0, color);
    line(img, c_x + cap1_center_x, c_y + cap1_center_y,
         c_x, c_y, thickness, 0, 0, color);
  }

  if (segment && !filled) {
    float rad_f = (float)radius - ((float)thickness / 2.0f);

    int cap0_center_x = (int)floorf(angle0_cos * rad_f);
    int cap0_center_y = (int)floorf(angle0_sin * rad_f);

    int cap1_center_x = (int)floorf(angle1_cos * rad_f);
    int cap1_center_y = (int)floorf(angle1_sin * rad_f);

    thickness /= 2;

    line(img, c_x + cap0_center_x, c_y + cap0_center_y,
         c_x + cap1_center_x, c_y + cap1_center_y, thickness, 0, 0, color);
  }
}

static void img_putc(image_buffer_t *img, int x, int y, uint32_t *colors, int num_colors,
                     uint8_t *font_data, uint8_t ch, bool up, bool down) {
  uint8_t w = font_data[0];
  uint8_t h = font_data[1];
  uint8_t char_num = font_data[2];
  uint8_t bits_per_pixel = font_data[3];

  int pixels_per_byte = (int)(8 / bits_per_pixel);
  int bytes_per_char = (int)((w * h) / pixels_per_byte);
  if ((w * h) % pixels_per_byte != 0) {
    bytes_per_char += 1;
  }

  // There are some expectations on ch that are not documented here.
  if (char_num == 10) {  
    ch = (uint8_t)(ch - '0');
  } else {
    ch = (uint8_t)(ch - ' ');
  }

  if (ch >= char_num) {
    return;
  }

  if (bits_per_pixel == 2) {
    if (num_colors < 4) {
      return;
    }

    for (int i = 0; i < w * h; i++) {
      uint8_t byte = font_data[4 + bytes_per_char * ch + (i / 4)];
      uint8_t bit_pos = (uint8_t)(i % pixels_per_byte);
      uint8_t pixel_value = (byte >> (bit_pos * 2)) & 0x03;
      int x0 = i % w;
      int y0 = i / w;
      if (up) {
        putpixel(img, x + y0, y - x0, colors[pixel_value]);
      } else if (down) {
        putpixel(img, x - y0, y + x0, colors[pixel_value]);
      } else {
        putpixel(img, x + x0, y + y0, colors[pixel_value]);
      }
    }
  } else {
    if (num_colors < 1) {
      return;
    }

    int32_t fg = (int32_t)colors[0];
    int32_t bg = -1;

    if (num_colors > 1) {
      bg = (int32_t)colors[1];
    }

    for (int i = 0; i < w * h; i++) {
      uint8_t byte = font_data[4 + bytes_per_char * ch + (i / 8)];
      uint8_t bit_pos = (uint8_t)(i % 8);
      uint8_t bit = (uint8_t)(byte & (1 << bit_pos));
      if (bit || bg >= 0) {
        int x0 = i % w;
        int y0 = i / w;

        if (up) {
          putpixel(img, x + y0, y - x0, bit ? (uint32_t)fg : (uint32_t)bg);
        } else if (down) {
          putpixel(img, x - y0, y + x0, bit ? (uint32_t)fg : (uint32_t)bg);
        } else {
          putpixel(img, x + x0, y + y0, bit ? (uint32_t)fg : (uint32_t)bg);
        }
      }
    }
  }
}

static void blit_rot_scale(
                           image_buffer_t *img_dest,
                           image_buffer_t *img_src,
                           int x, int y, // Where on display
                           float xr, float yr, // Pixel to rotate around
                           float rot, // Rotation angle in degrees
                           float scale, // Scale factor
                           int32_t transparent_color) {

  int src_w = img_src->width;
  int src_h = img_src->height;
  int des_w = img_dest->width;
  int des_h = img_dest->height;

  int des_x_start = 0;
  int des_y_start = 0;
  int des_x_end = (des_x_start + des_w);
  int des_y_end = (des_y_start + des_h);

  if (des_x_start < 0) des_x_start = 0;
  if (des_x_end > des_w) des_x_end = des_w;
  if (des_y_start < 0) des_y_start = 0;
  if (des_y_end > des_h) des_y_end = des_h;

  if (rot == 0.0 && scale == 1.0) {
    if (x > 0) des_x_start += x;
    if (y > 0) des_y_start += y;
    if ((des_x_end - x) > src_w) des_x_end = src_w + x;
    if ((des_y_end - y) > src_h) des_y_end = src_h + y;

    for (int j = des_y_start; j < des_y_end; j++) {
      for (int i = des_x_start; i < des_x_end; i++) {
        int px = i - x;
        int py = j - y;

        if (px >= 0 && px < src_w && py >= 0 && py < src_h) {
          uint32_t p = getpixel(img_src, px, py);

          if (p != (uint32_t) transparent_color) {
            putpixel(img_dest, i, j, p);
          }
        }
      }
    }
  } else if (rot == 0.0) {
    xr *= scale;
    yr *= scale;

    const int fp_scale = 1000;

    int xr_i = (int)xr;
    int yr_i = (int)yr;
    int scale_i = (int)(scale * (float) fp_scale);

    for (int j = des_y_start; j < des_y_end; j++) {
      for (int i = des_x_start; i < des_x_end; i++) {
        int px = (i - x - xr_i) * fp_scale;
        int py = (j - y - yr_i) * fp_scale;

        px += xr_i * fp_scale;
        py += yr_i * fp_scale;

        px /= scale_i;
        py /= scale_i;

        if (px >= 0 && px < src_w && py >= 0 && py < src_h) {
          uint32_t p = getpixel(img_src, px, py);

          if (p != (uint32_t) transparent_color) {
            putpixel(img_dest, i, j, p);
          }
        }
      }
    }
  } else {
    float sr = sinf(-rot * (float)M_PI / 180.0f);
    float cr = cosf(-rot * (float)M_PI / 180.0f);

    xr *= scale;
    yr *= scale;

    const int fp_scale = 1000;

    int sr_i = (int)(sr * (float)fp_scale);
    int cr_i = (int)(cr * (float)fp_scale);
    int xr_i = (int)xr;
    int yr_i = (int)yr;
    int scale_i = (int)(scale * (float) fp_scale);

    for (int j = des_y_start; j < des_y_end; j++) {
      for (int i = des_x_start; i < des_x_end; i++) {
        int px = (i - x - xr_i) * cr_i + (j - y - yr_i) * sr_i;
        int py = -(i - x - xr_i) * sr_i + (j - y - yr_i) * cr_i;

        px += xr_i * fp_scale;
        py += yr_i * fp_scale;

        px /= scale_i;
        py /= scale_i;

        if (px >= 0 && px < src_w && py >= 0 && py < src_h) {
          uint32_t p = getpixel(img_src, px, py);

          if (p != (uint32_t) transparent_color) {
            putpixel(img_dest, i, j, p);
          }
        }
      }
    }
  }
}

// Extensions

#define ATTR_MAX_ARGS	3
#define ARG_MAX_NUM		8

typedef struct {
  bool is_valid;
  uint16_t arg_num;
  lbm_value args[ATTR_MAX_ARGS];
} attr_t;

typedef struct {
  bool is_valid;
  image_buffer_t img;
  lbm_value args[ARG_MAX_NUM];
  attr_t attr_thickness;
  attr_t attr_filled;
  attr_t attr_rounded;
  attr_t attr_dotted;
  attr_t attr_scale;
  attr_t attr_rotate;
  attr_t attr_resolution;
} img_args_t;

static img_args_t decode_args(lbm_value *args, lbm_uint argn, int num_expected) {
  img_args_t res;
  memset(&res, 0, sizeof(res));

  if (!lbm_is_array_r(args[0])) {
    return res;
  }
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(args[0]);

  if (!image_buffer_is_valid((uint8_t*)arr->data, arr->size)) {
    res.is_valid = false;
    return res;
  }

  res.img.width = image_buffer_width((uint8_t*)arr->data);
  res.img.height = image_buffer_height((uint8_t*)arr->data);
  res.img.fmt = image_buffer_format((uint8_t*)arr->data);
  res.img.mem_base = (uint8_t*)arr->data;
  res.img.data = image_buffer_data((uint8_t*)arr->data);


  int num_dec = 0;
  for (unsigned int i = 1;i < argn;i++) {
    if (!lbm_is_number(args[i]) && !lbm_is_cons(args[i])) {
      return res;
    }

    if (lbm_is_number(args[i])) {
      res.args[num_dec] = args[i];
      num_dec++;

      if (num_dec > ARG_MAX_NUM) {
        return res;
      }
    } else {
      lbm_value curr = args[i];
      int attr_ind = 0;
      attr_t *attr_now = 0;
      while (lbm_is_cons(curr)) {
        lbm_value  arg = lbm_car(curr);

        if (attr_ind == 0) {
          if (!lbm_is_symbol(arg)) {
            return res;
          }

          if (lbm_dec_sym(arg) == symbol_thickness) {
            attr_now = &res.attr_thickness;
            attr_now->arg_num = 1;
          } else if (lbm_dec_sym(arg) == symbol_filled) {
            attr_now = &res.attr_filled;
            attr_now->arg_num = 0;
          } else if (lbm_dec_sym(arg) == symbol_rounded) {
            attr_now = &res.attr_rounded;
            attr_now->arg_num = 1;
          } else if (lbm_dec_sym(arg) == symbol_dotted) {
            attr_now = &res.attr_dotted;
            attr_now->arg_num = 2;
          } else if (lbm_dec_sym(arg) == symbol_scale) {
            attr_now = &res.attr_scale;
            attr_now->arg_num = 1;
          } else if (lbm_dec_sym(arg) == symbol_rotate) {
            attr_now = &res.attr_rotate;
            attr_now->arg_num = 3;
          } else if (lbm_dec_sym(arg) == symbol_resolution) {
            attr_now = &res.attr_resolution;
            attr_now->arg_num = 1;
          } else {
            return res;
          }
        } else {
          if (!lbm_is_number(arg)) {
            return res;
          }

          attr_now->args[attr_ind - 1] = arg;
        }

        attr_ind++;
        if (attr_ind > (ATTR_MAX_ARGS + 1)) {
          return res;
        }

        curr = lbm_cdr(curr);
      }

      // does this really compare the pointer addresses?
      if (attr_now == &res.attr_rounded && attr_ind == 1) {
        attr_now->arg_num = 0; // the `rounded` attribute may be empty
      }


      if ((attr_ind - 1) == attr_now->arg_num) {
        attr_now->is_valid = true;
      } else {
        return res;
      }
    }
  }

  if (num_dec != num_expected) {
    return res;
  }

  res.is_valid = true;
  return res;
}

static lbm_value ext_image_dims(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 0);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  lbm_value dims = lbm_heap_allocate_list(2);
  if (lbm_is_symbol(dims)) {
    return dims;
  }
  lbm_value curr = dims;
  lbm_set_car(curr, lbm_enc_i(arg_dec.img.width));
  curr = lbm_cdr(curr);
  lbm_set_car(curr, lbm_enc_i(arg_dec.img.height));
  return dims;
}

static lbm_value ext_image_buffer(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  bool args_ok = false;
  color_format_t fmt = indexed2;
  lbm_uint w = 0;
  lbm_uint h = 0;
  
  if (argn == 4 &&
      lbm_is_defrag_mem(args[0]) &&
      lbm_is_symbol(args[1]) &&
      lbm_is_number(args[2]) &&
      lbm_is_number(args[3])) {
    fmt = sym_to_color_format(args[1]);
    w = lbm_dec_as_u32(args[2]);
    h = lbm_dec_as_u32(args[3]);
    args_ok = true;
  } else if (argn == 3 &&
	     lbm_is_symbol(args[0]) &&
	     lbm_is_number(args[1]) &&
	     lbm_is_number(args[2])) {
    fmt = sym_to_color_format(args[0]);
    w = lbm_dec_as_u32(args[1]);
    h = lbm_dec_as_u32(args[2]);
    args_ok = true;
  }
    
  if (args_ok && fmt != format_not_supported && w > 0 && h > 0 && w < MAX_WIDTH && h < MAX_HEIGHT) {
    if (argn == 3) {
      res = image_buffer_allocate(fmt, (uint16_t)w, (uint16_t)h);
    } else {
      res = image_buffer_allocate_dm((lbm_uint*)lbm_car(args[0]), fmt, (uint16_t)w, (uint16_t)h);
    }
  }
  return res;
}


static lbm_value ext_is_image_buffer(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;

  if (argn == 1) {
    res = ENC_SYM_NIL;
    if (lbm_is_array_r(args[0])) {
      lbm_value arr = args[0];
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(arr);
      uint8_t *data = (uint8_t*)array->data;
      if (image_buffer_is_valid(data, array->size)) {
	res = ENC_SYM_TRUE;;
      }
    }
  }
  return res;
}

static lbm_value ext_color(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;

  if (argn >= 2 && argn <= 6 &&
      lbm_is_symbol(args[0]) &&
      lbm_is_number(args[1])) {

    // Color1 and color2 are int in the struct and decoded as i32, why
    // where they stored in uint32_t?
    int32_t color1 = lbm_dec_as_i32(args[1]);

    int32_t color2 = 0;
    if (argn >= 3) {
      if (lbm_is_number(args[2])) {
        color2 = lbm_dec_as_i32(args[2]);
      } else {
        return ENC_SYM_TERROR;
      }
    }

    int32_t param1 = 0;
    if (argn >= 4) {
      if (lbm_is_number(args[3])) {
        param1 = lbm_dec_as_i32(args[3]);
      } else {
        return ENC_SYM_TERROR;
      }
    }

    int32_t param2 = 0;
    if (argn >= 5) {
      if (lbm_is_number(args[4])) {
        param2 = lbm_dec_as_i32(args[4]);
      } else {
        return ENC_SYM_TERROR;
      }
    }

    bool mirrored = false;
    if (argn >= 6) {
      if (lbm_is_symbol(args[5])) {
        lbm_uint sym = lbm_dec_sym(args[5]);
        if (sym == symbol_repeat) {
          mirrored = false;
        } else if (sym == symbol_mirrored) {
          mirrored = true;
        } else {
          return ENC_SYM_TERROR;
        }
      } else {
        return ENC_SYM_TERROR;
      }
    }

    COLOR_TYPE t;
    if (lbm_dec_sym(args[0]) == symbol_regular) {
      t = COLOR_REGULAR;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_x) {
      t = COLOR_GRADIENT_X;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_y) {
      t = COLOR_GRADIENT_Y;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_x_pre) {
      t = COLOR_PRE_X;
    } else if (lbm_dec_sym(args[0]) == symbol_gradient_y_pre) {
      t = COLOR_PRE_Y;
    } else {
      return ENC_SYM_TERROR;
    }

    // Maybe check if param is in ranges first ?
    res = color_allocate(t, color1, color2, (uint16_t)param1, (uint16_t)param2, mirrored);
  }

  return res;
}

static lbm_value ext_color_set(lbm_value *args, lbm_uint argn) {
  if (argn != 3 || !display_is_color(args[0]) ||
      !lbm_is_symbol(args[1])) {
    return ENC_SYM_TERROR;
  }

  color_t *color = (color_t*)lbm_get_custom_value(args[0]);

  bool is_regular = color->type == COLOR_REGULAR;
  bool is_gradient = color->type == COLOR_GRADIENT_X || color->type == COLOR_GRADIENT_Y;
  bool is_pre = color->type == COLOR_PRE_X || color->type == COLOR_PRE_Y;

  lbm_uint prop = lbm_dec_sym(args[1]);
  if (prop == symbol_color_0) {
    if (!lbm_is_number(args[2]) || !(is_regular || is_gradient)) {
      return ENC_SYM_TERROR;
    }
    color->color1 = lbm_dec_as_i32(args[2]);
  } else if (prop == symbol_color_1) {
    if (!lbm_is_number(args[2]) || !is_gradient) {
      return ENC_SYM_TERROR;
    }
    color->color2 = lbm_dec_as_i32(args[2]);
  } else if (prop == symbol_width) {
    if (!lbm_is_number(args[2]) || !is_gradient) {
      return ENC_SYM_TERROR;
    }
    color->param1 = (uint16_t)lbm_dec_as_u32(args[2]);
  } else if (prop == symbol_offset) {
    if (!lbm_is_number(args[2]) || !(is_gradient || is_pre)) {
      return ENC_SYM_TERROR;
    }
    color->param2 = (uint16_t)lbm_dec_as_u32(args[2]);
  } else if (prop == symbol_repeat_type) {
    if (!lbm_is_symbol(args[2]) || !(is_gradient || is_pre)) {
      return ENC_SYM_TERROR;
    }
    lbm_uint sym = lbm_dec_sym(args[2]);
    if (sym == symbol_repeat) {
      color->mirrored = false;
    } else if (sym == symbol_mirrored) {
      color->mirrored = true;
    } else {
      return ENC_SYM_TERROR;
    }
  } else {
    return ENC_SYM_TERROR;
  }

  return ENC_SYM_TRUE;
}

static lbm_value ext_color_get(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !display_is_color(args[0]) ||
      !lbm_is_symbol(args[1])) {
    return ENC_SYM_TERROR;
  }

  color_t *color = (color_t*)lbm_get_custom_value(args[0]);

  bool is_gradient = color->type == COLOR_GRADIENT_X || color->type == COLOR_GRADIENT_Y;
  bool is_pre = color->type == COLOR_PRE_X || color->type == COLOR_PRE_Y;

  lbm_uint prop = lbm_dec_sym(args[1]);
  if (prop == symbol_color_0) {
    // always allowed
    return lbm_enc_u32((uint32_t)color->color1);
  } else if (prop == symbol_color_1) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_u32((uint32_t)color->color2);
  } else if (prop == symbol_width) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_i32((int32_t)color->param1);
  } else if (prop == symbol_offset) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_i32((int32_t)color->param2);
  } else if (prop == symbol_repeat_type) {
    if (!is_gradient && !is_pre) {
      return ENC_SYM_TERROR;
    }
    return lbm_enc_sym(color->mirrored ? symbol_mirrored : symbol_repeat);
  } else {
    return ENC_SYM_TERROR;
  }

  return ENC_SYM_TRUE;
}

static lbm_value ext_color_setpre(lbm_value *args, lbm_uint argn) {
  if (argn != 3 || !display_is_color(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2])) {
    return ENC_SYM_TERROR;
  }

  color_t *color = (color_t*)lbm_get_custom_value(args[0]);

  uint32_t pos = lbm_dec_as_u32(args[1]);
  int new_color = lbm_dec_as_i32(args[2]);

  if (color->precalc == 0 || pos >= COLOR_PRECALC_LEN) {
    return ENC_SYM_EERROR;
  }

  color->precalc[pos] = (uint32_t)new_color;

  return ENC_SYM_TRUE;
}

static lbm_value ext_color_getpre(lbm_value *args, lbm_uint argn) {
  if (argn != 2 || !display_is_color(args[0]) ||
      !lbm_is_number(args[1])) {
    return ENC_SYM_TERROR;
  }

  color_t *color = (color_t*)lbm_get_custom_value(args[0]);

  uint32_t pos = lbm_dec_as_u32(args[1]);

  if (color->precalc == 0 || pos >= COLOR_PRECALC_LEN) {
    return ENC_SYM_EERROR;
  }

  return lbm_enc_u32(color->precalc[pos]);
}

static lbm_value ext_clear(lbm_value *args, lbm_uint argn) {
  if ((argn != 1 && argn != 2) ||
      !array_is_image_buffer(args[0]) ||
      (argn == 2 && !lbm_is_number(args[1]))) {
    return ENC_SYM_TERROR;
  }

  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
  image_buffer_t img_buf;
  img_buf.width = image_buffer_width((uint8_t*)arr->data);
  img_buf.height = image_buffer_height((uint8_t*)arr->data);
  img_buf.fmt = image_buffer_format((uint8_t*)arr->data);
  img_buf.mem_base = (uint8_t*)arr->data;
  img_buf.data = image_buffer_data((uint8_t*)arr->data);

  uint32_t color = 0;
  if (argn == 2) {
    color = lbm_dec_as_u32(args[1]);
  }

  image_buffer_clear(&img_buf, color);

  return ENC_SYM_TRUE;
}

static lbm_value ext_putpixel(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 3);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  putpixel(&arg_dec.img,
           lbm_dec_as_i32(arg_dec.args[0]),
           lbm_dec_as_i32(arg_dec.args[1]),
           lbm_dec_as_u32(arg_dec.args[2]));
  return ENC_SYM_TRUE;
}

// lisp args: img x1 y1 x2 y2 color opt-attr1 ... opt-attrN
static lbm_value ext_line(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 5);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  line(&arg_dec.img,
       lbm_dec_as_i32(arg_dec.args[0]),
       lbm_dec_as_i32(arg_dec.args[1]),
       lbm_dec_as_i32(arg_dec.args[2]),
       lbm_dec_as_i32(arg_dec.args[3]),
       lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
       lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
       lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
       lbm_dec_as_u32(arg_dec.args[4]));

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r color opt-attr1 ... opt-attrN
static lbm_value ext_circle(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 4);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  if (arg_dec.attr_filled.is_valid) {
    fill_circle(&arg_dec.img,
                lbm_dec_as_i32(arg_dec.args[0]),
                lbm_dec_as_i32(arg_dec.args[1]),
                lbm_dec_as_i32(arg_dec.args[2]),
                lbm_dec_as_u32(arg_dec.args[3]));
  } if (arg_dec.attr_dotted.is_valid) {
    arc(&arg_dec.img,
        lbm_dec_as_i32(arg_dec.args[0]),
        lbm_dec_as_i32(arg_dec.args[1]),
        lbm_dec_as_i32(arg_dec.args[2]),
        0, 359.9f,
        lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
        arg_dec.attr_rounded.is_valid, // currently does nothing as the line function doesn't support square ends.
        false,
        false, false,
        lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
        lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
        lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
        lbm_dec_as_u32(arg_dec.args[3]));
  } else {
    circle(&arg_dec.img,
           lbm_dec_as_i32(arg_dec.args[0]),
           lbm_dec_as_i32(arg_dec.args[1]),
           lbm_dec_as_i32(arg_dec.args[2]),
           lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
           lbm_dec_as_u32(arg_dec.args[3]));
  }

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_arc(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
      arg_dec.attr_rounded.is_valid,
      arg_dec.attr_filled.is_valid,
      false, false,
      lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
      lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
      lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
      lbm_dec_as_u32(arg_dec.args[5]));

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_circle_sector(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
      true,
      arg_dec.attr_filled.is_valid,
      true, false,
      lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
      lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
      lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
      lbm_dec_as_u32(arg_dec.args[5]));

  return ENC_SYM_TRUE;
}

// lisp args: img cx cy r ang-s ang-e color opt-attr1 ... opt-attrN
static lbm_value ext_circle_segment(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 6);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  arc(&arg_dec.img,
      lbm_dec_as_i32(arg_dec.args[0]),
      lbm_dec_as_i32(arg_dec.args[1]),
      lbm_dec_as_i32(arg_dec.args[2]),
      lbm_dec_as_float(arg_dec.args[3]),
      lbm_dec_as_float(arg_dec.args[4]),
      lbm_dec_as_i32(arg_dec.attr_thickness.args[0]),
      true,
      arg_dec.attr_filled.is_valid,
      false, true,
      lbm_dec_as_i32(arg_dec.attr_dotted.args[0]),
      lbm_dec_as_i32(arg_dec.attr_dotted.args[1]),
      lbm_dec_as_i32(arg_dec.attr_resolution.args[0]),
      lbm_dec_as_u32(arg_dec.args[5]));


  return ENC_SYM_TRUE;
}

// lisp args: img x y width height color opt-attr1 ... opt-attrN
static lbm_value ext_rectangle(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 5);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  image_buffer_t *img = &arg_dec.img;
  int x = lbm_dec_as_i32(arg_dec.args[0]);
  int y = lbm_dec_as_i32(arg_dec.args[1]);
  int width = lbm_dec_as_i32(arg_dec.args[2]);
  int height = lbm_dec_as_i32(arg_dec.args[3]);
  int rad = lbm_dec_as_i32(arg_dec.attr_rounded.args[0]);
  int thickness = lbm_dec_as_i32(arg_dec.attr_thickness.args[0]);
  uint32_t color = lbm_dec_as_u32(arg_dec.args[4]);
  int dot1 = lbm_dec_as_i32(arg_dec.attr_dotted.args[0]);
  int dot2 = lbm_dec_as_i32(arg_dec.attr_dotted.args[1]);
  int resolution = lbm_dec_as_i32(arg_dec.attr_resolution.args[0]);

  if (arg_dec.attr_rounded.is_valid) {
    if (arg_dec.attr_filled.is_valid) {
      rectangle(img, x + rad, y, width - 2 * rad, rad, 1, 1, 0, 0, color);
      rectangle(img, x + rad, y + height - rad, width - 2 * rad, rad, 1, 1, 0, 0, color);
      rectangle(img, x, y + rad, width, height - 2 * rad, 1, 1, 0, 0, color);
      fill_circle(img, x + rad, y + rad, rad, color);
      fill_circle(img, x + rad, y + height - rad, rad, color);
      fill_circle(img, x + width - rad, y + rad, rad, color);
      fill_circle(img, x + width - rad, y + height - rad, rad, color);
    } else {
      // Remember to change these to use the rounded attribute,
      // when/if line supports it!

      int line_thickness = thickness / 2;
      thickness = line_thickness * 2; // round it to even for consistency.

      // top
      line(img, x + rad, y + line_thickness, x + width - rad, y + line_thickness, line_thickness, dot1, dot2, color);
      // bottom
      line(img, x + rad, y + height - line_thickness, x + width - rad, y + height - line_thickness, line_thickness, dot1, dot2, color);
      // left
      line(img, x + line_thickness, y + rad, x + line_thickness, y + height - rad, line_thickness, dot1, dot2, color);
      // right
      line(img, x + width - line_thickness, y + rad, x + width - line_thickness, y + height - rad, line_thickness, dot1, dot2, color);

      // upper left
      arc(img, x + rad, y + rad, rad, 180, 270, thickness, false, false, false, false, dot1, dot2, resolution, color);
      // upper right
      arc(img, x + width - rad, y + rad, rad, 270, 0, thickness, false, false, false, false, dot1, dot2, resolution, color);
      // bottom left
      arc(img, x + rad, y + height - rad, rad, 90, 180, thickness, false, false, false, false, dot1, dot2, resolution, color);
      // bottom right
      arc(img, x + width - rad, y + height - rad, rad, 0, 90, thickness, false, false, false, false, dot1, dot2, resolution, color);
    }
  } else {
    rectangle(img,
              x, y,
              width, height,
              arg_dec.attr_filled.is_valid,
              thickness,
              dot1, dot2,
              color);
  }

  return ENC_SYM_TRUE;
}

// lisp args: img x1 y1 x2 y2 x3 y3 color opt-attr1 ... opt-attrN
static lbm_value ext_triangle(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args, argn, 7);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  image_buffer_t *img = &arg_dec.img;
  int x0 = lbm_dec_as_i32(arg_dec.args[0]);
  int y0 = lbm_dec_as_i32(arg_dec.args[1]);
  int x1 = lbm_dec_as_i32(arg_dec.args[2]);
  int y1 = lbm_dec_as_i32(arg_dec.args[3]);
  int x2 = lbm_dec_as_i32(arg_dec.args[4]);
  int y2 = lbm_dec_as_i32(arg_dec.args[5]);
  int thickness = lbm_dec_as_i32(arg_dec.attr_thickness.args[0]);
  int dot1 = lbm_dec_as_i32(arg_dec.attr_dotted.args[0]);
  int dot2 = lbm_dec_as_i32(arg_dec.attr_dotted.args[1]);
  uint32_t color = lbm_dec_as_u32(arg_dec.args[6]);

  if (arg_dec.attr_filled.is_valid) {
    fill_triangle(img, x0, y0, x1, y1, x2, y2, color);
  } else {
    line(img, x0, y0, x1, y1, thickness, dot1, dot2, color);
    line(img, x1, y1, x2, y2, thickness, dot1, dot2, color);
    line(img, x2, y2, x0, y0, thickness, dot1, dot2, color);
  }

  return ENC_SYM_TRUE;
}

// lisp args: img x y fg bg font str
static lbm_value ext_text(lbm_value *args, lbm_uint argn) {
  bool up = false;
  bool down = false;

  if (argn >= 7 && lbm_is_symbol(args[argn - 1])) {
    if (lbm_dec_sym(args[argn - 1]) == symbol_up) {
      up = true;
      argn--;
    }

    if (lbm_dec_sym(args[argn - 1]) == symbol_down) {
      down = true;
      argn--;
    }
  }

  if (argn != 6 && argn != 7) {
    return ENC_SYM_TERROR;
  }

  int x = lbm_dec_as_i32(args[1]);
  int y = lbm_dec_as_i32(args[2]);

  int32_t colors[4] = {-1, -1, -1, -1}; // how big? int vs int32
  if (argn == 7) {
    if (!lbm_is_number(args[3]) || !lbm_is_number(args[4])) {
      return ENC_SYM_TERROR;
    }
    colors[0] = lbm_dec_as_i32(args[3]);
    colors[1] = lbm_dec_as_i32(args[4]);
  } else {
    lbm_value curr = args[3];
    int ind = 0;
    while (lbm_is_cons(curr)) {
      lbm_value  arg = lbm_car(curr);
      if (lbm_is_number(arg)) {
        colors[ind++] = lbm_dec_as_i32(arg);
      } else {
        return ENC_SYM_TERROR;
      }

      if (ind == 4) {
        break;
      }

      curr = lbm_cdr(curr);
    }
  }

  if (!array_is_image_buffer(args[0])) {
  return ENC_SYM_TERROR;
  }
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
  image_buffer_t img_buf;
  img_buf.width = image_buffer_width((uint8_t*)arr->data);
  img_buf.height = image_buffer_height((uint8_t*)arr->data);
  img_buf.fmt = image_buffer_format((uint8_t*)arr->data);
  img_buf.mem_base = (uint8_t*)arr->data;
  img_buf.data = image_buffer_data((uint8_t*)arr->data);

  lbm_array_header_t *font = 0;
  if (lbm_type_of(args[5]) == LBM_TYPE_ARRAY) {
    font = (lbm_array_header_t *)lbm_car(args[argn - 2]);
  }

  char *txt = lbm_dec_str(args[argn - 1]);

  if (!font || !txt || font->size < (4 + 5 * 5 * 10)) {
    return ENC_SYM_TERROR;
  }

  uint8_t *font_data = (uint8_t*)font->data;
  uint8_t w = font_data[0];
  uint8_t h = font_data[1];

  int incx = 1;
  int incy = 0;
  if (up) {
    incx = 0;
    incy = -1;
  } else if (down) {
    incx = 0;
    incy = 1;
  }

  int ind = 0;
  while (txt[ind] != 0) {
    img_putc(&img_buf, x + ind * w * incx, y + ind * h * incy,
      (uint32_t *)colors, 4, font_data, (uint8_t)txt[ind], up, down);
    ind++;
  }

  return ENC_SYM_TRUE;
}

static lbm_value ext_blit(lbm_value *args, lbm_uint argn) {
  img_args_t arg_dec = decode_args(args + 1, argn - 1, 3);

  if (!arg_dec.is_valid) {
    return ENC_SYM_TERROR;
  }

  if (!array_is_image_buffer(args[0])) {
    return ENC_SYM_TERROR;
  }
  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);
  image_buffer_t dest_buf;
  dest_buf.width = image_buffer_width((uint8_t*)arr->data);
  dest_buf.height = image_buffer_height((uint8_t*)arr->data);
  dest_buf.fmt = image_buffer_format((uint8_t*)arr->data);
  dest_buf.mem_base = (uint8_t*)arr->data;
  dest_buf.data = image_buffer_data((uint8_t*)arr->data);

  float scale = 1.0;
  if (arg_dec.attr_scale.is_valid) {
    scale = lbm_dec_as_float(arg_dec.attr_scale.args[0]);
  }

  blit_rot_scale(
                 &dest_buf,
                 &arg_dec.img,
                 lbm_dec_as_i32(arg_dec.args[0]),
                 lbm_dec_as_i32(arg_dec.args[1]),
                 lbm_dec_as_float(arg_dec.attr_rotate.args[0]),
                 lbm_dec_as_float(arg_dec.attr_rotate.args[1]),
                 lbm_dec_as_float(arg_dec.attr_rotate.args[2]),
                 scale,
                 lbm_dec_as_i32(arg_dec.args[2]));

  return ENC_SYM_TRUE;
}

void display_dummy_reset(void) {
  return;
}

void display_dummy_clear(uint32_t color) {
  (void) color;
  return;
}

bool display_dummy_render_image(image_buffer_t *img, uint16_t x, uint16_t y,  color_t *colors) {
  (void) img;
  (void) x;
  (void) y;
  (void) colors;
  return false;
}

static bool(* volatile disp_render_image)(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) = display_dummy_render_image;
static void(* volatile disp_clear)(uint32_t color) = display_dummy_clear;
static void(* volatile disp_reset)(void) = display_dummy_reset;

static char *msg_not_supported = "Command not supported or display driver not initialized";

static lbm_value ext_disp_reset(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;

  if (disp_reset == NULL) {
    lbm_set_error_reason(msg_not_supported);
    return ENC_SYM_EERROR;
  }

  disp_reset();

  return ENC_SYM_TRUE;
}

static lbm_value ext_disp_clear(lbm_value *args, lbm_uint argn) {
  if (disp_clear == NULL) {
    lbm_set_error_reason(msg_not_supported);
    return ENC_SYM_EERROR;
  }

  if (argn > 1) {
    return ENC_SYM_TERROR;
  }

  uint32_t clear_color = 0;

  if (argn == 1) {
    if (!lbm_is_number(args[0])) {
      return ENC_SYM_TERROR;
    }

    clear_color = lbm_dec_as_u32(args[0]);
  }

  disp_clear(clear_color);

  return ENC_SYM_TRUE;
}

static lbm_value ext_disp_render(lbm_value *args, lbm_uint argn) {
  if (disp_render_image == NULL) {
    lbm_set_error_reason(msg_not_supported);
    return ENC_SYM_EERROR;
  }

  if ((argn != 3 && argn != 4) ||
      !array_is_image_buffer(args[0]) ||
      !lbm_is_number(args[1]) ||
      !lbm_is_number(args[2])) {
    return ENC_SYM_TERROR;
  }

  lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(args[0]);

  image_buffer_t img_buf;
  img_buf.fmt = image_buffer_format((uint8_t*)arr->data);
  img_buf.width = image_buffer_width((uint8_t*)arr->data);
  img_buf.height = image_buffer_height((uint8_t*)arr->data);
  img_buf.mem_base = (uint8_t*)arr->data;
  img_buf.data = image_buffer_data((uint8_t*)arr->data);

  color_t colors[16];
  memset(colors, 0, sizeof(color_t) * 16);

  if (argn == 4 && lbm_is_list(args[3])) {
    int i = 0;
    lbm_value curr = args[3];
    while (lbm_is_cons(curr) && i < 16) {
      lbm_value arg = lbm_car(curr);

      if (lbm_is_number(arg)) {
        colors[i].color1 = (int)lbm_dec_as_u32(arg);
      } else if (display_is_color(arg)) {
        colors[i] = *((color_t*)lbm_get_custom_value(arg));
      } else {
        return ENC_SYM_TERROR;
      }

      curr = lbm_cdr(curr);
      i++;
    }
  }

  // img_buf is a stack allocated image_buffer_t.
  bool render_res = disp_render_image(&img_buf, (uint16_t)lbm_dec_as_u32(args[1]), (uint16_t)lbm_dec_as_u32(args[2]), colors);

  if (!render_res) {
    lbm_set_error_reason("Could not render image. Check if the format and location is compatible with the display.");
    return ENC_SYM_EERROR;
  }

  return ENC_SYM_TRUE;
}

// Jpg decoder

typedef struct {
  uint8_t *data;
  int pos;
  int size;
  int ofs_x;
  int ofs_y;
} jpg_bufdef;

size_t jpg_input_func (JDEC* jd, uint8_t* buff, size_t ndata) {
  jpg_bufdef *dev = (jpg_bufdef*)jd->device;

  if ((int)ndata > (dev->size - dev->pos)) {
    ndata = (size_t)(dev->size - dev->pos);
  }

  if (buff) {
    memcpy(buff, dev->data + dev->pos, ndata);
  }
  dev->pos += (int)ndata;
  return ndata;
}

int jpg_output_func (	/* 1:Ok, 0:Aborted */
                     JDEC* jd,		/* Decompression object */
                     void* bitmap,	/* Bitmap data to be output */
                     JRECT* rect		/* Rectangular region to output */
                        ) {
  jpg_bufdef *dev = (jpg_bufdef*)jd->device;

  image_buffer_t img;
  img.mem_base = (uint8_t*)bitmap;
  img.data = (uint8_t*)bitmap;
  img.width = (uint16_t)(rect->right - rect->left + 1);
  img.height = (uint16_t)(rect->bottom - rect->top + 1);
  img.fmt = rgb888;

  disp_render_image(&img, (uint16_t)(rect->left + dev->ofs_x), (uint16_t)(rect->top + dev->ofs_y), 0);

  return 1;
}

static lbm_value ext_disp_render_jpg(lbm_value *args, lbm_uint argn) {

  if (argn != 3 ||
      !lbm_is_array_r(args[0]) ||
      !lbm_is_number(args[1]) ||
      !lbm_is_number(args[2])) {
    return ENC_SYM_TERROR;
  }

  JDEC jd;
  void *jdwork;
  // make a bit of room before the buffer.
  const size_t sz_work = 4096 + IMAGE_BUFFER_HEADER_SIZE;

  jdwork = lbm_malloc(sz_work);
  if (!jdwork) {
    return ENC_SYM_MERROR;
  }

  lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(args[0]);

  jpg_bufdef iodev;
  iodev.data = (uint8_t*)(array->data);
  iodev.size = (int)array->size;
  iodev.pos = 0;
  iodev.ofs_x = lbm_dec_as_i32(args[1]);
  iodev.ofs_y = lbm_dec_as_i32(args[2]);
  jd_prepare(&jd, jpg_input_func, jdwork, sz_work + IMAGE_BUFFER_HEADER_SIZE, &iodev);
  jd_decomp(&jd, jpg_output_func, 0);
  lbm_free(jdwork);
  return ENC_SYM_TRUE;
}

void lbm_display_extensions_init(void) {
  register_symbols();

  disp_render_image = NULL;
  disp_clear = NULL;
  disp_reset = NULL;

  lbm_add_extension("img-buffer", ext_image_buffer);
  lbm_add_extension("img-buffer?", ext_is_image_buffer);
  lbm_add_extension("img-color", ext_color);
  lbm_add_extension("img-color-set", ext_color_set);
  lbm_add_extension("img-color-get", ext_color_get);
  lbm_add_extension("img-color-setpre", ext_color_setpre);
  lbm_add_extension("img-color-getpre", ext_color_getpre);
  lbm_add_extension("img-dims", ext_image_dims);
  lbm_add_extension("img-setpix", ext_putpixel);
  lbm_add_extension("img-line", ext_line);
  lbm_add_extension("img-text", ext_text);
  lbm_add_extension("img-clear", ext_clear);
  lbm_add_extension("img-circle", ext_circle);
  lbm_add_extension("img-arc", ext_arc);
  lbm_add_extension("img-circle-sector", ext_circle_sector);
  lbm_add_extension("img-circle-segment", ext_circle_segment);
  lbm_add_extension("img-rectangle", ext_rectangle);
  lbm_add_extension("img-triangle", ext_triangle);
  lbm_add_extension("img-blit", ext_blit);

  lbm_add_extension("disp-reset", ext_disp_reset);
  lbm_add_extension("disp-clear", ext_disp_clear);
  lbm_add_extension("disp-render", ext_disp_render);
  lbm_add_extension("disp-render-jpg", ext_disp_render_jpg);
}

void lbm_display_extensions_set_callbacks(
                                          bool(* volatile render_image)(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors),
                                          void(* volatile clear)(uint32_t color),
                                          void(* volatile reset)(void)
                                          ) {
  disp_render_image = render_image ? render_image : display_dummy_render_image;  
  disp_clear = clear ? clear : display_dummy_clear;
  disp_reset = reset ? reset : display_dummy_reset;
}
