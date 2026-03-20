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

#ifndef TTF_BACKEND_FREETYPE_H_
#define TTF_BACKEND_FREETYPE_H_

#include <ft2build.h>
#include FT_FREETYPE_H

#include "ttf_backend.h"

struct SFT_Font
{
  FT_Library library;
  FT_Face    face;
  const uint8_t *memory;
  uint_fast32_t  size;
};

#endif
