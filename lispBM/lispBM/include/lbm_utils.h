/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se
    Copyright 2022 Benjamin Vedder

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

#ifndef LBM_UTILS_H_
#define LBM_UTILS_H_ 

#ifdef __cplusplus
extern "C" {
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#ifndef M_PI_2
#define M_PI_2 1.57079632679
#endif

#ifndef M_3PI_2
#define M_3PI_2 4.71238898038469
#endif

#define DEG2RAD_f(deg) ((deg) * (float)(M_PI / 180.0))
#define RAD2DEG_f(rad) ((rad) * (float)(180.0 / M_PI))

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

#define CMP(a,b) (((a) > (b)) - ((a) < (b)));

static inline bool str_eq(char *str1, char *str2) {
  if (*str1 != *str2) return false;
  if (*str1 == 0) return true;

  for (;;) {
    ++str1; ++str2;
    if (*str1 != *str2) return false;
    if (*str1 == 0) return true;
  }
}

#ifdef __cplusplus
}
#endif
#endif
