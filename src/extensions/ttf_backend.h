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

/* Common TTF backend interface - abstracts Schrift vs FreeType */

#ifndef TTF_BACKEND_H
#define TTF_BACKEND_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <extensions/display_extensions.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SFT_DOWNWARD_Y       0x01
#define SFT_MEM_ERROR        -2

typedef uint_least32_t      SFT_UChar;
typedef uint_fast32_t       SFT_Glyph;

// Backend specific
typedef struct SFT_Font SFT_Font;
  
// common
typedef struct SFT      SFT;
typedef struct SFT_LMetrics SFT_LMetrics;
typedef struct SFT_GMetrics SFT_GMetrics;
typedef struct SFT_Kerning  SFT_Kerning;

struct SFT
{
  SFT_Font *font;
  float     xScale;
  float     yScale;
  float     xOffset;
  float     yOffset;
  int       flags;
};

struct SFT_LMetrics
{
  float ascender;
  float descender;
  float lineGap;
};

struct SFT_GMetrics
{
  float advanceWidth;
  float leftSideBearing;
  int   yOffset;
  int   minWidth;
  int   minHeight;
};

struct SFT_Kerning
{
  float xShift;
  float yShift;
};

// ttf_backend.c  
bool get_utf32(uint8_t *utf8, uint32_t *utf32, uint32_t ix, uint32_t *next_ix);

// schrift.c and ttf_backend_freetype.c
int init_font(SFT_Font *font);
void free_font(SFT_Font *font);

int sft_lmetrics(const SFT *sft, SFT_LMetrics *metrics);
int sft_lookup(const SFT *sft, SFT_UChar codepoint, SFT_Glyph *glyph);
int sft_gmetrics(const SFT *sft, SFT_Glyph glyph, SFT_GMetrics *metrics);
int sft_kerning(const SFT *sft, SFT_Glyph leftGlyph, SFT_Glyph rightGlyph, SFT_Kerning *kerning);
int sft_render(const SFT *sft, SFT_Glyph glyph, image_buffer_t *image);
bool sft_gpos_kerning(const SFT *sft, SFT_Glyph leftGlyph, SFT_Glyph rightGlyph, SFT_Kerning *kerning);



#ifdef __cplusplus
}
#endif

#endif /* TTF_BACKEND_H */
