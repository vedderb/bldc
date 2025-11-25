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

#include "ttf_backend.h"
#include "ttf_backend_freetype.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include <string.h>

int init_font(SFT_Font *font) {
  if (FT_Init_FreeType(&font->library)) return -1;

  if (FT_New_Memory_Face(font->library,
                         font->memory,
                         (FT_Long)font->size,
                         0,
                         &font->face)) {
    FT_Done_FreeType(font->library);
    return -1;
  }
  return 0;
}

void free_font(SFT_Font *font) {
  if (font->face) {
    FT_Done_Face(font->face);
    font->face = NULL;
  }
  if (font->library) {
    FT_Done_FreeType(font->library);
    font->library = NULL;
  }
}

int sft_lmetrics(const SFT *sft, SFT_LMetrics *metrics) {
  FT_Face face = sft->font->face;
  int r = -1;
  
  if (!FT_Set_Pixel_Sizes(face, 0, (FT_UInt)sft->yScale)) {
    metrics->ascender = face->size->metrics.ascender / 64.0f;
    metrics->descender = face->size->metrics.descender / 64.0f;
    metrics->lineGap =
      (face->size->metrics.height / 64.0f) -
      (metrics->ascender - metrics->descender);
    r = 0;
  } 
  return r;
}

int sft_lookup(const SFT *sft, SFT_UChar codepoint, SFT_Glyph *glyph) {
  *glyph = FT_Get_Char_Index(sft->font->face, codepoint);
  if (*glyph == 0) return -1;
  return 0;
}

int sft_gmetrics(const SFT *sft, SFT_Glyph glyph, SFT_GMetrics *metrics) {
  FT_Face face = sft->font->face;

  if (FT_Set_Pixel_Sizes(face, 0, (FT_UInt)sft->yScale)) return -1;
  if (FT_Load_Glyph(face, glyph, FT_LOAD_DEFAULT)) return -1;

  FT_GlyphSlot slot = face->glyph;

  metrics->advanceWidth = slot->advance.x / 64.0f;

  if (FT_Render_Glyph(slot, FT_RENDER_MODE_NORMAL)) return -1;

  metrics->leftSideBearing = (float)slot->bitmap_left;  
  metrics->yOffset = SFT_DOWNWARD_Y ? -slot->bitmap_top : slot->bitmap_top;
  metrics->minWidth = (int)slot->bitmap.width;
  metrics->minHeight = (int)slot->bitmap.rows;

  return 0;
}

int sft_kerning(const SFT *sft, SFT_Glyph leftGlyph, SFT_Glyph rightGlyph, SFT_Kerning *kerning) {
  FT_Face face = sft->font->face;
  FT_Vector delta;

  if (FT_Set_Pixel_Sizes(face, 0, (FT_UInt)sft->yScale)) return -1;
  if (FT_Get_Kerning(face, leftGlyph, rightGlyph, FT_KERNING_DEFAULT, &delta)) return -1;
  
  kerning->xShift = delta.x / 64.0f;
  kerning->yShift = delta.y / 64.0f;

  return 0;
}

// There are different color modes in ttf fonts.
// But, FT_RENDER_MODE_NORMAL will result in grayscale bitmaps of 256 levels.
// FT_LOAD_DEFAULT does not read color data.
// FT_RENDER_MODE_NORMAL cannot be used to render colored font data.
int sft_render(const SFT *sft, SFT_Glyph glyph, image_buffer_t *image) {
  if (image->fmt == indexed2 ||
      image->fmt == indexed4 ||
      image->fmt == indexed16) {
    FT_Face face = sft->font->face;

    if (FT_Set_Pixel_Sizes(face, 0, (FT_UInt)sft->yScale)) return -1;
    if (FT_Load_Glyph(face, glyph, FT_LOAD_DEFAULT)) return -1;
    if (FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL)) return -1;

    FT_Bitmap *bitmap = &face->glyph->bitmap;

    if (bitmap->width != image->width || bitmap->rows != image->height) {
      return SFT_MEM_ERROR;
    }

    uint32_t num_colors = 1 << image->fmt;

    for (int y = 0; y < (int)bitmap->rows; y++) {
      for (int x = 0; x < (int)bitmap->width; x++) {
        uint8_t gray = bitmap->buffer[y * bitmap->pitch + x];
        uint32_t indexed = (gray * (num_colors - 1) + 127) / 255;
        putpixel(image, x, y, indexed);
      }
    }
    return 0;
  }
  return -1;
}

bool sft_gpos_kerning(const SFT *sft, SFT_Glyph leftGlyph, SFT_Glyph rightGlyph, SFT_Kerning *kerning) {
  (void)sft;
  (void)leftGlyph;
  (void)rightGlyph;
  (void)kerning;
  return false;
}
