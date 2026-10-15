/*
  Copyright 2026 Joel Svensson              svenssonjoel@yahoo.se

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

#ifndef TINYUTILS_H_
#define TINYUTILS_H_

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

static inline int MIN(int a, int b) {
  return a < b ? a : b;
}

static inline int MAX(int a, int b) {
  return a > b ? a : b;
}

static inline void swap_points(int *x0, int *y0, int *x1, int *y1) {
  int tx = *x0, ty = *y0;
  *x0 = *x1; *y0 = *y1;
  *x1 = tx;  *y1 = ty;
}

// Q24.8 slope of x as a linear function of y, from (xa,ya) to (xb,yb).
static inline int32_t tri_slope_fp(int32_t xa, int32_t xb, int32_t ya, int32_t yb) {
  return (xb - xa) * 256 / (yb - ya);
}

#ifdef __cplusplus
}
#endif
#endif
