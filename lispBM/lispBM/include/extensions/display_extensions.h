/*
  Copyright 2023       Benjamin Vedder	benjamin@vedder.se
  Copyright 2023, 2024 Joel Svensson    svenssonjoel@yahoo.se

  This file is part of the VESC firmware.

  The VESC firmware is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  The VESC firmware is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef DISPLAY_EXTENSIONS_H_
#define DISPLAY_EXTENSIONS_H_

#include <stdint.h>
#include <stdbool.h>

#include "lispbm.h"

typedef enum { // correspond to bits
  indexed2 = 1,
  indexed4 = 2,
  indexed16 = 4,
  rgb332 = 8,
  rgb565 = 16,
  rgb888 = 24,
  format_not_supported
} color_format_t;

typedef enum {
  COLOR_REGULAR = 0,
  COLOR_GRADIENT_X,
  COLOR_GRADIENT_Y,
  COLOR_PRE_X,
  COLOR_PRE_Y,
} COLOR_TYPE;

typedef struct {
  color_format_t fmt;
  uint16_t width;
  uint16_t height;
  uint8_t  *data;
  uint8_t  *mem_base;
} image_buffer_t;


typedef struct {
  int color1;    // I dont know why these are int when most uses of them are as if uint32_t.
  int color2;
  uint16_t param1;
  uint16_t param2;
  bool mirrored;
  COLOR_TYPE type;
  uint32_t *precalc;
} color_t;

#define COLOR_PRECALC_LEN	512

#define IMAGE_BUFFER_HEADER_SIZE (lbm_uint)5

static inline uint8_t color_format_to_byte(color_format_t fmt) {
  return (uint8_t)fmt;
}

static inline uint16_t image_buffer_width(uint8_t *data) {
  return (uint16_t)(data[0] << 8 | data[1]);
}

static inline uint16_t image_buffer_height(uint8_t *data) {
  return (uint16_t)(data[2] << 8 | data[3]);
}

static inline uint8_t image_buffer_format(uint8_t *data) {
  return (uint8_t)data[4];
}

static inline uint8_t *image_buffer_data(uint8_t *data) {
  return &(data[5]);
}

static inline void image_buffer_set_width(uint8_t *data, uint16_t w) {
  data[0] = (uint8_t)(w >> 8);
  data[1] = (uint8_t)w;
}

static inline void image_buffer_set_height(uint8_t *data, uint16_t h) {
  data[2] = (uint8_t)(h >> 8);
  data[3] = (uint8_t)h;
}

static inline void image_buffer_set_format(uint8_t *data, color_format_t fmt) {
  data[4] = color_format_to_byte(fmt);
}

static inline uint32_t image_buffer_size_bytes(uint8_t *data) {
  return (((uint32_t)image_buffer_width(data) *
           (uint32_t)image_buffer_height(data) *
           (uint32_t)image_buffer_format(data)) / 8);
}

static inline bool image_buffer_is_valid(uint8_t *data, lbm_uint size) {
  return
    (size > IMAGE_BUFFER_HEADER_SIZE) &&
    (size >= (IMAGE_BUFFER_HEADER_SIZE + image_buffer_size_bytes(data)));
}

static inline bool array_is_image_buffer(lbm_value v) {
  bool res = lbm_is_array_r(v);
  if (res) {
    lbm_array_header_t *arr = (lbm_array_header_t *)lbm_car(v);
    res = image_buffer_is_valid((uint8_t*)arr->data, arr->size);
  }
  return res;
}

static inline uint32_t color_apply_precalc(color_t color, int x, int y) {
  int pos;
  switch (color.type) {
  case COLOR_PRE_X: {
    pos = x;
    break;
  }
  case COLOR_PRE_Y: {
    pos = y;
    break;
  }
  default: {
    return 0;
  }
  }

  int i;
  if (color.mirrored) {
    i = (pos - color.param2) % (color.param1 * 2);
    if (i < 0) {
      i += color.param1 * 2;
    }
    if (i >= color.param1) {
      i = color.param1 * 2 - i - 1;
    }
  } else {
    i = (pos - color.param2) % (color.param1);
    if (i < 0) {
      i += color.param1;
    }
  }
  return color.precalc[i];
}

#define COLOR_CHECK_PRE(color, x, y) (color.precalc ? color_apply_precalc(color, x, y) : lbm_display_rgb888_from_color(color, x, y))
#define COLOR_TO_RGB888(color, x, y) (color.type == COLOR_REGULAR ? (uint32_t)color.color1 : COLOR_CHECK_PRE(color, x, y))

// Interface
bool lbm_display_is_color(lbm_value v);
uint32_t lbm_display_rgb888_from_color(color_t color, int x, int y);
void image_buffer_clear(image_buffer_t *img, uint32_t cc);


void lbm_display_extensions_init(void);
void lbm_display_extensions_set_callbacks(
                                          bool(* volatile render_image)(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors),
                                          void(* volatile clear)(uint32_t color),
                                          void(* volatile reset)(void)
                                          );

#endif
