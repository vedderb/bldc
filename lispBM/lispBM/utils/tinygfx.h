/*
  Copyright 2023 - 2025 Benjamin Vedder            benjamin@vedder.se
  Copyright 2023 - 2026 Joel Svensson              svenssonjoel@yahoo.se
  Copyright 2023        Rasmus Söderhielm          rasmus.soderhielm@gmail.com
  Copyright 2025        Joakim Lundborg            joakim.lundborg@gmail.com

  This file is part of LispBM.

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

#ifndef TINYGFX_H_
#define TINYGFX_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "tjpgd.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum { // correspond to bits
  indexed2 = 1,
  indexed4 = 2,
  indexed16 = 4,
  rgb332 = 8,
  rgb565 = 16,
  rgb888 = 24,
  format_not_supported
} color_format_t;

typedef struct {
  color_format_t fmt;
  uint16_t width;
  uint16_t height;
  uint8_t  *data;
  uint8_t  *mem_base;
} image_buffer_t;

typedef enum {
  COLOR_REGULAR = 0,
  COLOR_GRADIENT_X,
  COLOR_GRADIENT_Y,
  COLOR_PRE_X,
  COLOR_PRE_Y,
} COLOR_TYPE;

typedef struct {
  uint32_t magic;
  int color1;    // I dont know why these are int when most uses of them are as if uint32_t.
  int color2;
  uint16_t param1;
  uint16_t param2;
  bool mirrored;
  COLOR_TYPE type;
  uint32_t *precalc;
  uint8_t alpha;
} color_t;

////////////////////////////////////////////////////////////
//  COLOR / PIXEL FORMAT

uint32_t lbm_display_rgb888_from_color(color_t color, int x, int y);

static inline uint32_t color_apply_precalc(color_t color, int x, int y) {
  int pos;
  switch (color.type) {
  case COLOR_PRE_X: pos = x; break;
  case COLOR_PRE_Y: pos = y; break;
  default: return 0;
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

uint32_t image_dims_to_size_bytes(color_format_t fmt, uint16_t width, uint16_t height);

////////////////////////////////////////////////////////////
//  PIXEL BUFFER PRIMITIVES

void     image_buffer_clear(image_buffer_t *img, uint32_t cc);
void     putpixel(image_buffer_t *img, int x, int y, uint32_t c);
uint32_t getpixel(image_buffer_t *img, int x, int y);

////////////////////////////////////////////////////////////
//  LINES

// Thickness extends outwards and inwards from the given line equally, resulting
// in double the total thickness.
// TODO: This should be more efficient
// http://homepages.enterprise.net/murphy/thickline/index.html
// https://github.com/ArminJo/STMF3-Discovery-Demos/blob/master/lib/BlueDisplay/LocalGUI/ThickLine.hpp
void tinygfx_line(image_buffer_t *img, int x0, int y0, int x1, int y1,
                   int thickness, int dot1, int dot2, uint32_t c);

////////////////////////////////////////////////////////////
//  CIRCLES AND ARCS

// Attribute bundle shared by the arc family (generic_arc/arc_thin/arc_ring/arc).
typedef struct {
  int thickness;
  bool rounded;
  bool filled;
  bool sector;
  bool segment;
  int dot1;
  int dot2;
  int resolution;
  uint32_t color;
} arc_params_t;

void tinygfx_fill_circle(image_buffer_t *img, int x, int y, int radius, uint32_t color);
// thickness extends inwards from the given radius circle
void tinygfx_circle(image_buffer_t *img, int x, int y, int radius, int thickness, uint32_t color);

void tinygfx_arc(image_buffer_t *img, int c_x, int c_y, int radius, float angle0, float angle1,
                  const arc_params_t *p);

////////////////////////////////////////////////////////////
//  RECTANGLES

// Axis-aligned strip of exactly `thickness` pixels, extending down/right from (x, y).
void tinygfx_thick_hline(image_buffer_t *img, int x, int y, int len, int thickness, uint32_t color);
void tinygfx_thick_vline(image_buffer_t *img, int x, int y, int len, int thickness, uint32_t color);

// thickness extends inwards from the given rectangle edge.
void tinygfx_rectangle(image_buffer_t *img, int x, int y, int width, int height,
                        bool fill, int thickness, int dot1, int dot2, uint32_t color);

void tinygfx_fill_rounded_rectangle(image_buffer_t *img, int x, int y, int width, int height,
                                     int radius, uint32_t color);

// thickness extends inwards; dot1>0 for a dotted border.
void tinygfx_rounded_rectangle(image_buffer_t *img, int x, int y, int width, int height,
                                int radius, int thickness, int dot1, int dot2, int resolution,
                                uint32_t color);

////////////////////////////////////////////////////////////
//  TRIANGLES

void tinygfx_fill_triangle(image_buffer_t *img, int x0, int y0, int x1, int y1, int x2, int y2,
                            uint32_t color);

////////////////////////////////////////////////////////////
//  TEXT

// orient: 0=normal, 1=up(90°CCW), 2=180°, 3=down(90°CW)
void tinygfx_img_putc(image_buffer_t *img, int x, int y, uint32_t *colors, int num_colors,
                       const uint8_t *font_data, uint8_t ch, int orient, float mag);

////////////////////////////////////////////////////////////
//  BLIT

// How blit maps and samples src pixels into dest.
typedef struct {
  float rot_x, rot_y; // point in src coords to rotate around
  float rot_angle;    // rotation angle in degrees
  float scale;        // scale factor
  bool tile;           // tile src to fill dest
  int clip_x, clip_y;  // clip start in dest
  int clip_w, clip_h;  // clip width and height
} blit_transform_t;

// Optional per-blit compositing controls. Pass NULL for a direct passthrough
// copy (the only supported mode when img_src is not an indexed format, or
// when img_src and img_dest share the same indexed format).
//
// palette, when non-NULL, remaps a raw source index (0..palette_len-1) to the
// value written to dest -- a dest index when dest is indexed, or an rgb888
// color when dest is rgb. transparent_color (a separate, older parameter on
// tinygfx_blit/tinygfx_blit_transform below) is still matched against the
// raw, pre-remap source value.
//
// alpha (0-255, 255 = opaque) and alpha_buf (a per-source-pixel alpha map,
// same width/height as img_src, format indexed2/indexed4/indexed16 -- read as
// a raw index scaled to 0-255 -- or rgb332 -- read as a raw byte 0-255, not
// decoded as a color) both only take effect when img_dest is an rgb format;
// they are silently ignored when img_dest is indexed, since there is no
// actual color at draw time to blend against. When both are given they
// combine multiplicatively (alpha_buf's sample scaled by alpha/255).
typedef struct {
  const uint32_t *palette;
  int palette_len;
  uint8_t alpha;
  image_buffer_t *alpha_buf;
} blit_compose_t;

// Plain positioned copy: no rotation, scale, tiling, or clipping (dest
// canvas bounds are the implicit clip).
void tinygfx_blit(image_buffer_t *img_dest, image_buffer_t *img_src,
                   int dest_offset_x, int dest_offset_y,
                   int32_t transparent_color,
                   const blit_compose_t *compose);

void tinygfx_blit_transform(image_buffer_t *img_dest, image_buffer_t *img_src,
                             int dest_offset_x, int dest_offset_y,
                             blit_transform_t transform,
                             int32_t transparent_color,
                             const blit_compose_t *compose);

////////////////////////////////////////////////////////////
//  JPEG

typedef struct {
  const uint8_t *data;
  size_t pos;
  size_t size;
  image_buffer_t *dest;
  int ofs_x;
  int ofs_y;
} tinygfx_jpg_io_t;

size_t tinygfx_jpg_input(JDEC *jd, uint8_t *buff, size_t ndata);

bool tinygfx_decode_jpg(image_buffer_t *dest, const uint8_t *jpg_data, size_t jpg_size,
                         int ofs_x, int ofs_y, void *work_buf, size_t work_buf_size);

#ifdef __cplusplus
}
#endif
#endif
