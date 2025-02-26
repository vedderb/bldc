/*
  Copyright 2025 Joel Svensson              svenssonjoel@yahoo.se

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

#include <extensions/ttf_extensions.h>
#include <extensions.h>
#include <buffer.h>

#include "schrift.h"


static bool mk_font_raw(SFT_Font *ft, lbm_value font_val) {
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(font_val);
  ft->memory = (uint8_t*)arr->data;
  ft->size = (uint_fast32_t)arr->size;
  ft->unitsPerEm = 0;
  ft->locaFormat = 0;
  ft->numLongHmtx = 0;
  if (init_font(ft) < 0) {
    return false;
  }
  return true;
}

static SFT mk_sft(SFT_Font *ft, float x_scale, float y_scale) {
  SFT sft;
  sft.font = ft;
  sft.xScale = x_scale;
  sft.yScale = y_scale;
  sft.xOffset = 0;
  sft.yOffset = 0;
  sft.flags = SFT_DOWNWARD_Y;

  return sft;
}

// If we are not bin searching then sorting the UTF32 codes is not needed.

#define FONT_MAX_ID_STRING_LENGTH   10
#define FONT_VERSION                0
#define FONT_MAGIC_STRING           "font"
#define FONT_LINE_METRICS_STRING    "lmtx"
#define FONT_KERNING_STRING         "kern"
#define FONT_GLYPHS_STRING          "glyphs"

// sizeof when used on string literals include the the terminating 0
#define FONT_PREAMBLE_SIZE          (sizeof(uint16_t) * 2 + sizeof(FONT_MAGIC_STRING))
#define FONT_LINE_METRICS_SIZE      (sizeof(uint32_t) + (sizeof(float) * 3) + sizeof(FONT_LINE_METRICS_STRING))

// "header sizes" excluding data payload
#define FONT_KERN_PAIR_SIZE         (uint32_t)(4 + 4 + 4)
#define FONT_KERN_ROW_SIZE          (uint32_t)(4 + 4)
#define FONT_KERN_TABLE_SIZE        (uint32_t)(sizeof(FONT_KERNING_STRING) + 4 + 4)
#define FONT_GLYPH_TABLE_SIZE       (uint32_t)(sizeof(FONT_GLYPHS_STRING) + 4 + 4 + 4)
#define FONT_GLYPH_SIZE             (uint32_t)(6*4)

static int num_kern_pairs_row(SFT *sft, uint32_t utf32, uint32_t *codes, uint32_t num_codes) {

  int num = 0;

  SFT_Glyph lgid;
  if (sft_lookup(sft, utf32, &lgid) < 0) {
    return -1;
  }

  for (uint32_t i = 0; i < num_codes; i ++) {
    uint32_t right_utf32 = codes[i];
    SFT_Kerning kern;
    kern.xShift = 0.0;
    kern.yShift = 0.0;

    SFT_Glyph rgid;
    if (sft_lookup(sft, right_utf32, &rgid) < 0) {
      return -1;
    }

    if (sft->font->pairAdjustOffset) {
      sft_gpos_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
    }
    if (kern.xShift == 0.0 && kern.yShift == 0.0) {
      sft_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
    }
    if (kern.xShift != 0.0 || kern.yShift != 0.0) {
      num++;
    }
  }
  return num;
}

static bool kern_table_dims(SFT *sft, uint32_t *codes, uint32_t num_codes, int *rows, int *tot_pairs) {

  int num_rows = 0;
  int tot_kern_pairs = 0;
  for (uint32_t i = 0; i < num_codes; i ++) {
    int r = num_kern_pairs_row(sft, codes[i], codes, num_codes);
    if (r > 0) {
      num_rows ++;
      tot_kern_pairs += r;
    } else if (r < 0) {
      return false;
    }
  }
  *rows = num_rows;
  *tot_pairs = tot_kern_pairs;
  return true;
}

static int kern_table_size_bytes(SFT *sft, uint32_t *codes, uint32_t num_codes) {
  int rows = 0;
  int tot_pairs = 0;

  int size_bytes;
  if (kern_table_dims(sft, codes, num_codes, &rows, &tot_pairs)) {
    size_bytes =
      (int)(FONT_KERN_PAIR_SIZE * (uint32_t)tot_pairs +
            FONT_KERN_ROW_SIZE * (uint32_t)rows +
            FONT_KERN_TABLE_SIZE);
  } else {
    return -1;
  }
  return size_bytes;
}


static void buffer_append_string(uint8_t *buffer, char *str, int32_t *index) {
  size_t n = strlen(str);
  memcpy(&buffer[*index], str, n + 1); // include the 0
  *index = *index + (int32_t)n + 1;
}

static void buffer_append_font_preamble(uint8_t *buffer, int32_t *index) {
  buffer_append_uint16(buffer, 0, index); // 2 leading zero bytes
  buffer_append_uint16(buffer, 0, index); // version 0
  buffer_append_string(buffer, FONT_MAGIC_STRING, index);
}

static void buffer_append_line_metrics(uint8_t *buffer, float ascender, float descender, float line_gap, int32_t *index) {
  buffer_append_string(buffer, FONT_LINE_METRICS_STRING, index);
  buffer_append_uint32(buffer, sizeof(float) * 3, index);
  buffer_append_float32_auto(buffer, ascender, index);
  buffer_append_float32_auto(buffer, descender, index);
  buffer_append_float32_auto(buffer, line_gap, index);
}


static bool buffer_append_kerning_table(uint8_t *buffer, SFT *sft, uint32_t *codes, uint32_t num_codes, int32_t *index) {

  int num_rows = 0;
  int tot_pairs = 0;

  if (kern_table_dims(sft, codes, num_codes, &num_rows, &tot_pairs)) {

    // TODO: compute size of "payload" only
    uint32_t size_bytes =
      FONT_KERN_PAIR_SIZE * (uint32_t)tot_pairs +
      FONT_KERN_ROW_SIZE * (uint32_t)num_rows +
      + 4; // number of rows field

    buffer_append_string(buffer, FONT_KERNING_STRING, index);
    buffer_append_uint32(buffer, size_bytes, index); // distance to jump ahead from index if not interested in kerning.
    buffer_append_uint32(buffer, (uint32_t)num_rows, index);

    for (uint32_t left_ix = 0; left_ix < num_codes; left_ix ++) { // loop over all codes
      int32_t row_len = num_kern_pairs_row(sft, codes[left_ix], codes, num_codes);
      if ( row_len > 0) {
        SFT_Glyph lgid;
        if (sft_lookup(sft, codes[left_ix], &lgid) < 0) {
          return false;
        }

        // format kerning table row
        // - UTF32 : leftGlyph
        // - uint32 : numKernPairs
        // - KernPair[]

        buffer_append_uint32(buffer, codes[left_ix],index);
        buffer_append_uint32(buffer, (uint32_t)row_len, index);

        for (uint32_t right_ix = 0; right_ix < num_codes; right_ix ++) { // and all codes
          uint32_t right_utf32 = codes[right_ix];
          SFT_Kerning kern;
          kern.xShift = 0.0;
          kern.yShift = 0.0;

          // format KernPair
          // - UTF32 : rightGlyph
          // - float : xShift
          // - float : yShift

          SFT_Glyph rgid;
          if (sft_lookup(sft, right_utf32, &rgid) < 0) {
            return false;
          }

          if (sft->font->pairAdjustOffset) {
            sft_gpos_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
          }
          if (kern.xShift == 0.0 && kern.yShift == 0.0) {
            sft_kerning(sft, lgid, rgid, &kern); //TODO: can it fail?
          }
          if (kern.xShift != 0.0 || kern.yShift != 0.0) {
            buffer_append_uint32(buffer, right_utf32, index);
            buffer_append_float32_auto(buffer, kern.xShift, index);
            buffer_append_float32_auto(buffer, kern.yShift, index);
          }
        }
      }
    }
  }
  return true;
}

int glyphs_img_data_size(SFT *sft, color_format_t fmt, uint32_t *codes, uint32_t num_codes) {
  int total_size = 0;
  for (uint32_t i = 0; i < num_codes; i ++) {
    SFT_Glyph gid;
    if (sft_lookup(sft, codes[i], &gid) < 0)  return -1;
    SFT_GMetrics gmtx;
    if (sft_gmetrics(sft, gid, &gmtx) < 0) return -1;
    total_size += (int)image_dims_to_size_bytes(fmt, (uint16_t)gmtx.minWidth, (uint16_t)gmtx.minHeight);
  }
  return total_size;
}

static int buffer_append_glyph(uint8_t *buffer, SFT *sft, color_format_t fmt, uint32_t utf32, int32_t *index){
  SFT_Glyph gid;
  if (sft_lookup(sft, utf32, &gid) < 0)  return -1;
  SFT_GMetrics gmtx;
  if (sft_gmetrics(sft, gid, &gmtx) < 0) return -1;

  buffer_append_uint32(buffer, utf32, index);
  buffer_append_float32_auto(buffer, gmtx.advanceWidth, index);
  buffer_append_float32_auto(buffer, gmtx.leftSideBearing, index);
  buffer_append_int32(buffer,gmtx.yOffset,index);
  buffer_append_int32(buffer,gmtx.minWidth, index);
  buffer_append_int32(buffer,gmtx.minHeight, index);

  image_buffer_t img;
  img.width = (uint16_t)gmtx.minWidth;
  img.height = (uint16_t)gmtx.minHeight;
  img.fmt = fmt;
  img.mem_base = &buffer[*index];
  img.data = &buffer[*index];

  int r = sft_render(sft, gid, &img);
  *index += (int32_t)image_dims_to_size_bytes(fmt, (uint16_t)gmtx.minWidth, (uint16_t)gmtx.minHeight);
  return r;
}

static int buffer_append_glyph_table(uint8_t *buffer, SFT *sft, color_format_t fmt, uint32_t *codes, uint32_t num_codes, int32_t *index) {

  uint32_t size_bytes =
    4 + // number of glyphs
    4 + // image format
    num_codes * 24 + // glyph metrics
    (uint32_t)glyphs_img_data_size(sft,fmt,codes,num_codes);

  buffer_append_string(buffer, FONT_GLYPHS_STRING, index);
  buffer_append_uint32(buffer, size_bytes, index); // distance to jump ahead from index if not interested in kerning.
  buffer_append_uint32(buffer, num_codes, index);
  buffer_append_uint32(buffer, (uint32_t)fmt, index);

  int r = 0;
  for (uint32_t i = 0; i < num_codes; i ++) {
    r = buffer_append_glyph(buffer,sft,fmt,codes[i], index);
    if (r < 0) return r;
  }
  return r;
}

//returns the increment for n
static int insert_nub(uint32_t *arr, uint32_t n, uint32_t new_elt) {
  uint32_t i;
  for (i = 0; i < n; i ++) {
    if (arr[i] == new_elt) return 0;
    if (arr[i] > new_elt) {
      memmove(&arr[i+1], &arr[i], (n - i) * 4);
      arr[i] = new_elt;
      return 1;
    }
  }
  arr[i] = new_elt;
  return 1;
}

// (ttf-prepare-bin font font-scale img-fmt chars-string)
lbm_value ext_ttf_prepare_bin(lbm_value *args, lbm_uint argn) {
  if (argn == 4 &&
      lbm_is_array_r(args[0]) && // font file data
      lbm_is_number(args[1])  &&
      lbm_is_symbol(args[2])  &&
      lbm_is_array_r(args[3])) {

    float x_scale = lbm_dec_as_float(args[1]);
    float y_scale = x_scale;

    color_format_t fmt = sym_to_color_format(args[2]);

    lbm_value result_array_cell = lbm_heap_allocate_cell(LBM_TYPE_CONS, ENC_SYM_NIL, ENC_SYM_ARRAY_TYPE);

    if (result_array_cell == ENC_SYM_MERROR) return result_array_cell;
    lbm_array_header_t *result_array_header = (lbm_array_header_t *)lbm_malloc(sizeof(lbm_array_header_t));
    if (!result_array_header) return ENC_SYM_MERROR;

    lbm_array_header_t *utf8_array_header = (lbm_array_header_t*)(lbm_car(args[3]));

    // Try to keep the utf8 array as nubbed as possible or there will be waste of mem.
    // Unfortunate dynamic tmp storage...
    uint32_t* unique_utf32 = lbm_malloc(utf8_array_header->size * sizeof(uint32_t));

    if (unique_utf32) {

      SFT_Font ft;
      if (!mk_font_raw(&ft,args[0])) {
        lbm_free(unique_utf32);
        return ENC_SYM_EERROR;
      }
      SFT sft = mk_sft(&ft, x_scale, y_scale);

      // We know which glyphs to prerender...
      // So time to start collecting information to put into the binary prerender format
      // and to figure out how much prerender space to allocate!

      uint32_t i = 0;
      uint32_t next_i = 0;
      uint32_t utf32;
      uint32_t n = 0;

      while (get_utf32((uint8_t*)utf8_array_header->data, &utf32, i, &next_i)) {
        n += (uint32_t)insert_nub(unique_utf32, n, utf32);
        i = next_i;
      }

      // There could be zero kerning pairs and then we dont
      // need the kerning table at all.
      // TODO: Fix this.
      int kern_tab_bytes = kern_table_size_bytes(&sft, unique_utf32, n);
      if (kern_tab_bytes <=  0) {
        lbm_free(unique_utf32);
        return ENC_SYM_EERROR;
      } 

      int glyph_gfx_size = glyphs_img_data_size(&sft, fmt, unique_utf32, n);
      if (glyph_gfx_size <= 0) {
        lbm_free(unique_utf32);
        return ENC_SYM_EERROR;
      }

      uint32_t bytes_required =
        FONT_PREAMBLE_SIZE +
        FONT_LINE_METRICS_SIZE +
        (uint32_t)kern_tab_bytes +
        FONT_GLYPH_TABLE_SIZE +
        n * FONT_GLYPH_SIZE + // per glyph metrics
        (uint32_t)glyph_gfx_size;

      uint8_t *buffer = (uint8_t*)lbm_malloc(bytes_required);
      if (!buffer) {
        lbm_free(unique_utf32);
        return ENC_SYM_MERROR;
      }
      memset(buffer,0, bytes_required);

      SFT_LMetrics lmtx;
      if (sft_lmetrics(&sft, &lmtx) < 0) {
        lbm_free(unique_utf32);
        return ENC_SYM_EERROR;
      }
      int32_t index = 0;

      buffer_append_font_preamble(buffer, &index);
      buffer_append_line_metrics(buffer,
                                 lmtx.ascender,
                                 lmtx.descender,
                                 lmtx.lineGap,
                                 &index);
      buffer_append_kerning_table(buffer, &sft, unique_utf32, n, &index);

      int r = buffer_append_glyph_table(buffer, &sft, fmt, unique_utf32, n, &index);
      if ( r == SFT_MEM_ERROR) {
        lbm_free(unique_utf32);
        lbm_free(buffer);
        lbm_set_car_and_cdr(result_array_cell, ENC_SYM_NIL, ENC_SYM_NIL);
        return ENC_SYM_MERROR;
      } else if (r < 0) {
        lbm_free(unique_utf32);
        lbm_free(buffer);
        lbm_set_car_and_cdr(result_array_cell, ENC_SYM_NIL, ENC_SYM_NIL);
        return ENC_SYM_EERROR;
      }

      lbm_free(unique_utf32); // tmp data nolonger needed
      result_array_header->size = (lbm_uint)index;
      result_array_header->data = (lbm_uint*)buffer;
      lbm_set_car(result_array_cell, (lbm_uint)result_array_header);
      result_array_cell = lbm_set_ptr_type(result_array_cell, LBM_TYPE_ARRAY);
      return result_array_cell;
    } else {
      return ENC_SYM_MERROR;
    }
  }
  return ENC_SYM_TERROR;
}

bool buffer_get_font_preamble(uint8_t* buffer, uint16_t *version, int32_t *index) {

  uint16_t zero = buffer_get_uint16(buffer, index);
  if (zero == 0) {
    *version = buffer_get_uint16(buffer, index);
    if (strncmp((const char *)&buffer[*index], "font", 4) == 0) {
      *index += (int32_t)sizeof("font"); // includes 0 for constant string
      return true;
    }
  }
  return false;
}

static bool font_get_line_metrics(uint8_t *buffer, int32_t buffer_size, float *ascender, float *descender, float *line_gap ,int32_t index) {

  while(index < buffer_size) {
    char *str = (char*)&buffer[index];
    if (strncmp(str, "lmtx", 4) == 0) {
      int32_t i = index + 5 + 4; // skip over string and size field;
      *ascender = buffer_get_float32_auto(buffer, &i);
      *descender = buffer_get_float32_auto(buffer, &i);
      *line_gap = buffer_get_float32_auto(buffer, &i);
      return true;
    }
    index += (int32_t)(strlen(str) + 1);
    index += (int32_t)buffer_get_uint32(buffer,&index); // just to next position
  }
  return false;
}

static bool font_get_kerning_table_index(uint8_t *buffer, int32_t buffer_size, int32_t *res_index, int32_t index) {

  while (index < buffer_size) {
    char *str = (char*)&buffer[index];
    if (strncmp(str, "kern", 4) == 0) {
      *res_index = index + 5 + 4;
      return true;
    }
    index += (int32_t)(strlen(str) + 1);
    index += (int32_t)buffer_get_uint32(buffer,&index); // jump to next position
  }
  return false;
}

static bool font_get_glyphs_table_index(uint8_t *buffer, int32_t buffer_size, int32_t *res_index, uint32_t *num_codes, uint32_t *fmt, int32_t index) {
  while (index < buffer_size) {
    char *str = (char*)&buffer[index];
    if (strncmp(str, "glyphs", 6) == 0) {
      int32_t i = index + 7 + 4;
      *num_codes = buffer_get_uint32(buffer,&i);
      *fmt = buffer_get_uint32(buffer,&i);
      *res_index = i;
      return true;
    }
    index += (int32_t)(strlen(str) + 1);
    index += (int32_t)buffer_get_uint32(buffer,&index);
  }
  return false;
}

static bool font_get_glyph(uint8_t *buffer,
                           float *advance_width,
                           float *left_side_bearing,
                           int32_t *y_offset,
                           int32_t *width,
                           int32_t *height,
                           uint8_t **gfx,
                           uint32_t utf32,
                           uint32_t num_codes,
                           color_format_t fmt,
                           int32_t index) {

  uint32_t i = 0;
  while (i < num_codes) {
    uint32_t c = buffer_get_uint32(buffer, &index);
    if (c == utf32) {
      *advance_width = buffer_get_float32_auto(buffer, &index);
      *left_side_bearing = buffer_get_float32_auto(buffer, &index);
      *y_offset = buffer_get_int32(buffer, &index);
      *width = buffer_get_int32(buffer, &index);
      *height = buffer_get_int32(buffer,&index);
      *gfx = &buffer[index];
      return true;
    } else {
      index += 12;
      int32_t w = buffer_get_int32(buffer, &index);
      int32_t h = buffer_get_int32(buffer, &index);
      index += (int32_t)image_dims_to_size_bytes(fmt, (uint16_t)w, (uint16_t)h);
    }
    i++;
  }
  return false;
}

bool font_get_kerning(uint8_t *buffer, uint32_t left, uint32_t right, float *x_shift, float *y_shift, int32_t index) {

  uint32_t num_rows = buffer_get_uint32(buffer, &index);

  for (uint32_t row = 0; row < num_rows; row ++) {

    uint32_t row_code = buffer_get_uint32(buffer, &index);
    uint32_t row_len  = buffer_get_uint32(buffer, &index);

    if (row_code == left) {
      for (uint32_t col = 0; col < row_len; col ++) {
        uint32_t col_code = buffer_get_uint32(buffer, &index);
        if (col_code == right) {
          *x_shift = buffer_get_float32_auto(buffer, &index);
          *y_shift = buffer_get_float32_auto(buffer, &index);
          return true;
        } else {
          index += 8;
        }
      }
    } else {
      index += (int32_t)(row_len * FONT_KERN_PAIR_SIZE);
    }
  }
  return false;
}

lbm_value ttf_text_bin(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_array_header_t *img_arr;
  lbm_value font;
  char *utf8_str;
  uint32_t colors[16];
  uint32_t next_arg = 0;
  if (argn >= 6 &&
      (img_arr = get_image_buffer(args[0])) &&
      lbm_is_number(args[1]) &&  // x position
      lbm_is_number(args[2]) &&  // y position
      lbm_is_cons(args[3]) &&    // list of colors
      lbm_is_array_r(args[4]) && // Binary font
      lbm_is_array_r(args[5])) { // sequence of utf8 characters
    lbm_value curr = args[3];
    int i = 0;
    while(lbm_is_cons(curr) && i < 16) {
      colors[i] = lbm_dec_as_u32(lbm_car(curr));
      curr = lbm_cdr(curr);
      i ++;
    }
    font = args[4];
    utf8_str = lbm_dec_str(args[5]);
    next_arg = 6;
  } else {
    return res;
  }

  int x_pos = lbm_dec_as_i32(args[1]);
  int y_pos = lbm_dec_as_i32(args[2]);

  float line_spacing = 1.0f;
  bool up = false;
  bool down = false;
  for (uint32_t i = next_arg; i < argn; i ++) {
    if (lbm_is_symbol(args[i])) {
      up = display_is_symbol_up(args[i]);
      down = display_is_symbol_down(args[i]);
    } else if (lbm_is_number(args[i])) {
      line_spacing = lbm_dec_as_float(args[i]);
    }
  }

  lbm_array_header_t *font_arr = lbm_dec_array_r(font);
  if (font_arr->size < 10) return ENC_SYM_EERROR;

  int32_t index = 0;
  uint16_t version;

  if (!buffer_get_font_preamble((uint8_t*)font_arr->data, &version, &index)) {
    return ENC_SYM_EERROR;
  }

  float ascender;
  float descender;
  float line_gap;

  if(!font_get_line_metrics((uint8_t*)font_arr->data, (int32_t)font_arr->size, &ascender, &descender, &line_gap , index)) {
    return ENC_SYM_EERROR;
  }

  int32_t kern_index = 0;

  if (!font_get_kerning_table_index((uint8_t*)font_arr->data, (int32_t)font_arr->size, &kern_index, index)) {
    return ENC_SYM_EERROR;
  }

  int32_t glyphs_index = 0;
  uint32_t num_codes;
  uint32_t color_fmt;

  if (!font_get_glyphs_table_index((uint8_t*)font_arr->data, (int32_t)font_arr->size, &glyphs_index, &num_codes, &color_fmt, index)) {
    return ENC_SYM_EERROR;
  }

  color_format_t fmt = (color_format_t)color_fmt;
  float x = 0.0;
  float y = 0.0;

  image_buffer_t tgt;
  tgt.width = image_buffer_width((uint8_t*)img_arr->data);
  tgt.height = image_buffer_height((uint8_t*)img_arr->data);
  tgt.fmt = image_buffer_format((uint8_t*)img_arr->data);
  tgt.mem_base = (uint8_t*)img_arr->data;
  tgt.data = image_buffer_data((uint8_t*)img_arr->data);

  uint32_t utf32;
  uint32_t prev;
  bool has_prev = false;
  uint32_t i = 0;
  uint32_t next_i = 0;
  while (get_utf32((uint8_t*)utf8_str, &utf32, i, &next_i)) {
    if (utf32 == '\n') {
      x = 0.0;
      y += line_spacing * (ascender - descender + line_gap);
      i++;
      continue; // next iteration
    }

    float x_n = x;
    float y_n = y;

    float advance_width;
    float left_side_bearing;
    int32_t y_offset;
    int32_t width;
    int32_t height;
    uint8_t *gfx;

    if (font_get_glyph((uint8_t*)font_arr->data,
                       &advance_width,
                       &left_side_bearing,
                       &y_offset,
                       &width,
                       &height,
                       &gfx,
                       utf32,
                       num_codes,
                       fmt,
                       glyphs_index)) {

      float x_shift = 0;
      float y_shift = 0;
      if (has_prev) {
        font_get_kerning((uint8_t*)font_arr->data,
                         prev,
                         utf32,
                         &x_shift,
                         &y_shift,
                         kern_index);
      }
      x_n += x_shift;
      y_n += y_shift;
      y_n += y_offset;

      image_buffer_t src;
      src.width = (uint16_t)width;
      src.height = (uint16_t)height;
      src.fmt = fmt;
      //src.mem_base = gfx;
      src.data = gfx;

      uint32_t num_colors = 1 << src.fmt;
      for (int j = 0; j < src.height; j++) {
        for (int i = 0; i < src.width; i ++) {
          // the bearing should not be accumulated into the advances

          uint32_t p = getpixel(&src, i, j);
          if (p) { // only draw colored
            uint32_t c = colors[p & (num_colors-1)]; // ceiled
            if (up) {
              putpixel(&tgt, x_pos + (j + (int)y_n), y_pos - (i + (int)(x_n + left_side_bearing)), c);
            } else if (down) {
              putpixel(&tgt, x_pos - (j + (int)y_n), y_pos + (i + (int)(x_n + left_side_bearing)), c);
            } else {
              putpixel(&tgt, x_pos + (i + (int)(x_n + left_side_bearing)), y_pos + (j + (int)y_n), c);
            }
          }
        }
      }
    } else {
      lbm_set_error_reason("Character is not one of those listed in ttf-prepare\n");
      return ENC_SYM_EERROR;
    }
    x = x_n + advance_width;
    i = next_i;
    prev = utf32;
    has_prev = true;
  }
  return ENC_SYM_TRUE;
}

lbm_value ext_ttf_wh(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_value font;
  char *utf8_str;
  uint32_t next_arg = 0;
  if (argn >= 2 &&
      lbm_is_array_r(args[0]) && // Binary font
      lbm_is_array_r(args[1])) { // sequence of utf8 characters
    font = args[0];
    utf8_str = lbm_dec_str(args[1]);
    next_arg = 2;
  } else {
    return res;
  }

  float line_spacing = 1.0f;
  bool up = false;
  bool down = false;
  for (uint32_t i = next_arg; i < argn; i ++) {
    if (lbm_is_symbol(args[i])) {
      up = display_is_symbol_up(args[i]);
      down = display_is_symbol_down(args[i]);
    } else if (lbm_is_number(args[i])) {
      line_spacing = lbm_dec_as_float(args[i]);
    }
  }

  lbm_value r_list = lbm_heap_allocate_list(2);
  if (lbm_is_symbol(r_list)) return r_list;

  lbm_array_header_t *font_arr = lbm_dec_array_r(font);
  if (font_arr->size < 10) return ENC_SYM_EERROR;

  int32_t index = 0;
  uint16_t version;

  if (!buffer_get_font_preamble((uint8_t*)font_arr->data, &version, &index)) {
    return ENC_SYM_EERROR;
  }

  float ascender;
  float descender;
  float line_gap;

  if(!font_get_line_metrics((uint8_t*)font_arr->data, (int32_t)font_arr->size, &ascender, &descender, &line_gap , index)) {
    return ENC_SYM_EERROR;
  }

  int32_t kern_index = 0;

  if (!font_get_kerning_table_index((uint8_t*)font_arr->data, (int32_t)font_arr->size, &kern_index, index)) {
    return ENC_SYM_EERROR;
  }

  int32_t glyphs_index = 0;
  uint32_t num_codes;
  uint32_t color_fmt;

  if (!font_get_glyphs_table_index((uint8_t*)font_arr->data, (int32_t)font_arr->size, &glyphs_index, &num_codes, &color_fmt, index)) {
    return ENC_SYM_EERROR;
  }

  float x = 0.0;
  float y = 0.0;
  float max_x = 0.0;

  uint32_t utf32;
  uint32_t prev;
  bool has_prev = false;
  uint32_t i = 0;
  uint32_t next_i = 0;
  while (get_utf32((uint8_t*)utf8_str, &utf32, i, &next_i)) {
    if (utf32 == '\n') {
      if (x > max_x) max_x = x;
      x = 0.0;
      y += line_spacing * (ascender - descender + line_gap);
      i++;
      continue; // next iteration
    }

    float x_n = x;
    float y_n = y;

    float advance_width;
    float left_side_bearing;
    int32_t y_offset;
    int32_t width;
    int32_t height;
    uint8_t *gfx;

    if (font_get_glyph((uint8_t*)font_arr->data,
                       &advance_width,
                       &left_side_bearing,
                       &y_offset,
                       &width,
                       &height,
                       &gfx,
                       utf32,
                       num_codes,
                       (color_format_t)color_fmt,
                       glyphs_index)) {

      float x_shift = 0;
      float y_shift = 0;
      if (has_prev) {
        font_get_kerning((uint8_t*)font_arr->data,
                         prev,
                         utf32,
                         &x_shift,
                         &y_shift,
                         kern_index);
      }
      x_n += x_shift;
      y_n += y_shift;
      y_n += y_offset;
    } else {
      return ENC_SYM_EERROR;
    }
    x = x_n + advance_width;
    i = next_i;
    prev = utf32;
    has_prev = true;
  }
  if (max_x < x) max_x = x;
  lbm_value rest = lbm_cdr(r_list);
  if (up || down) {
    lbm_set_car(r_list, lbm_enc_u((uint32_t)(y + line_spacing * (ascender - descender + line_gap))));
    lbm_set_car(rest, lbm_enc_u((uint32_t)max_x));
  } else {
    lbm_set_car(r_list, lbm_enc_u((uint32_t)max_x));
    lbm_set_car(rest, lbm_enc_u((uint32_t)(y + line_spacing * (ascender - descender + line_gap))));
  }
  return r_list;
}

lbm_value ext_ttf_glyph_dims(lbm_value *args, lbm_uint argn) {
  if (argn == 2 &&
      lbm_is_array_r(args[0]) &&
      lbm_is_array_r(args[1])) { // string utf8,

    lbm_array_header_t *font_arr = lbm_dec_array_r(args[0]);
    if (font_arr->size < 10) return ENC_SYM_EERROR;

    int32_t index = 0;
    uint16_t version;

    if (!buffer_get_font_preamble((uint8_t*)font_arr->data, &version, &index)) {
      return ENC_SYM_EERROR;
    }

    int32_t glyphs_index = 0;
    uint32_t num_codes;
    uint32_t color_fmt;

    if (!font_get_glyphs_table_index((uint8_t*)font_arr->data, (int32_t)font_arr->size, &glyphs_index, &num_codes, &color_fmt, index)) {
      return ENC_SYM_EERROR;
    }

    lbm_array_header_t *utf8_array_header = (lbm_array_header_t*)(lbm_car(args[1]));

    uint32_t next_i = 0;
    uint32_t utf32 = 0;
    get_utf32((uint8_t*)utf8_array_header->data, &utf32, 0, &next_i);

    float advance_width;
    float left_side_bearing;
    int32_t y_offset;
    int32_t width;
    int32_t height;
    uint8_t *gfx;

    if (font_get_glyph((uint8_t*)font_arr->data,
                       &advance_width,
                       &left_side_bearing,
                       &y_offset,
                       &width,
                       &height,
                       &gfx,
                       utf32,
                       num_codes,
                       (color_format_t)color_fmt,
                       glyphs_index)) {

      return lbm_heap_allocate_list_init(2,
                                        lbm_enc_u((uint32_t)(width)),
                                        lbm_enc_u((uint32_t)height));
    }
  }
  return ENC_SYM_TERROR;
}

lbm_value ext_ttf_line_height(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_array_r(args[0])) {

    lbm_array_header_t *font_arr = lbm_dec_array_r(args[0]);
    if (font_arr->size < 10) return ENC_SYM_EERROR;

    int32_t index = 0;
    uint16_t version;

    if (!buffer_get_font_preamble((uint8_t*)font_arr->data, &version, &index)) {
      return ENC_SYM_EERROR;
    }

    float ascender;
    float descender;
    float line_gap;

    if(!font_get_line_metrics((uint8_t*)font_arr->data, (int32_t)font_arr->size, &ascender, &descender, &line_gap , index)) {
      return ENC_SYM_EERROR;
    }

    res = lbm_enc_float(ascender - descender + line_gap);
  }
  return res;
}

lbm_value ext_ttf_ascender(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_array_r(args[0])) {

    lbm_array_header_t *font_arr = lbm_dec_array_r(args[0]);
    if (font_arr->size < 10) return ENC_SYM_EERROR;

    int32_t index = 0;
    uint16_t version;

    if (!buffer_get_font_preamble((uint8_t*)font_arr->data, &version, &index)) {
      return ENC_SYM_EERROR;
    }

    float ascender;
    float descender;
    float line_gap;

    if(!font_get_line_metrics((uint8_t*)font_arr->data, (int32_t)font_arr->size, &ascender, &descender, &line_gap , index)) {
      return ENC_SYM_EERROR;
    }

    res = lbm_enc_float(ascender);
  }
  return res;
}

lbm_value ext_ttf_descender(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_array_r(args[0])) {

    lbm_array_header_t *font_arr = lbm_dec_array_r(args[0]);
    if (font_arr->size < 10) return ENC_SYM_EERROR;

    int32_t index = 0;
    uint16_t version;

    if (!buffer_get_font_preamble((uint8_t*)font_arr->data, &version, &index)) {
      return ENC_SYM_EERROR;
    }

    float ascender;
    float descender;
    float line_gap;

    if(!font_get_line_metrics((uint8_t*)font_arr->data, (int32_t)font_arr->size, &ascender, &descender, &line_gap , index)) {
      return ENC_SYM_EERROR;
    }

    res = lbm_enc_float(descender);
  }
  return res;
}

lbm_value ext_ttf_line_gap(lbm_value *args, lbm_uint argn) {
  lbm_value res = ENC_SYM_TERROR;
  if (argn == 1 &&
      lbm_is_array_r(args[0])) {

    lbm_array_header_t *font_arr = lbm_dec_array_r(args[0]);
    if (font_arr->size < 10) return ENC_SYM_EERROR;

    int32_t index = 0;
    uint16_t version;

    if (!buffer_get_font_preamble((uint8_t*)font_arr->data, &version, &index)) {
      return ENC_SYM_EERROR;
    }

    float ascender;
    float descender;
    float line_gap;

    if(!font_get_line_metrics((uint8_t*)font_arr->data, (int32_t)font_arr->size, &ascender, &descender, &line_gap , index)) {
      return ENC_SYM_EERROR;
    }

    res = lbm_enc_float(line_gap);
  }
  return res;
}

void lbm_ttf_extensions_init(void) {

  // metrics
  lbm_add_extension("ttf-line-height", ext_ttf_line_height);
  lbm_add_extension("ttf-ascender", ext_ttf_ascender);
  lbm_add_extension("ttf-descender", ext_ttf_descender);
  lbm_add_extension("ttf-line-gap", ext_ttf_line_gap);
  lbm_add_extension("ttf-text-dims",ext_ttf_wh);
  lbm_add_extension("ttf-glyph-dims",ext_ttf_glyph_dims);

  // Prepare
  lbm_add_extension("ttf-prepare", ext_ttf_prepare_bin);

  // Draw text.
  lbm_add_extension("ttf-text", ttf_text_bin);
}
