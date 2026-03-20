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

// extract an utf32 value from an utf8 string starting at index ix.
bool get_utf32(uint8_t *utf8, uint32_t *utf32, uint32_t ix, uint32_t *next_ix) {
  uint8_t *u = &utf8[ix];
  uint32_t c = 0;

  if (u[0] == 0) return false;

  if (!(u[0] & 0x80U)) {
    *utf32 = u[0];
    *next_ix = ix + 1;
  } else if ((u[0] & 0xe0U) == 0xc0U) {
    c = (u[0] & 0x1fU) << 6;
    if ((u[1] & 0xc0U) != 0x80U) return false;
    *utf32 = c + (u[1] & 0x3fU);
    *next_ix = ix + 2;
  } else if ((u[0] & 0xf0U) == 0xe0U) {
    c = (u[0] & 0x0fU) << 12;
    if ((u[1] & 0xc0U) != 0x80U) return false;
    c += (u[1] & 0x3fU) << 6;
    if ((u[2] & 0xc0U) != 0x80U) return false;
    *utf32 = c + (u[2] & 0x3fU);
    *next_ix = ix + 3;
  } else if ((u[0] & 0xf8U) == 0xf0U) {
    c = (u[0] & 0x07U) << 18;
    if ((u[1] & 0xc0U) != 0x80U) return false;
    c += (u[1] & 0x3fU) << 12;
    if ((u[2] & 0xc0U) != 0x80U) return false;
    c += (u[2] & 0x3fU) << 6;
    if ((u[3] & 0xc0U) != 0x80U) return false;
    c += (u[3] & 0x3fU);
    if ((c & 0xFFFFF800U) == 0xD800U) return false;
    *utf32 = c;
    *next_ix = ix + 4;
  } else return false;
  return true;
}

