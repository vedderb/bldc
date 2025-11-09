/*
    Copyright 2025 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef LBM_COS_TABLE_H_
#define LBM_COS_TABLE_H_

#include <stdint.h>

// Cosine lookup table for a single quadrant (0° to 90°)
// 128 entries, values scaled 0-255 (representing 0.0 to 1.0)
// Can be used for both cosine and sine with appropriate indexing
static const uint8_t lbm_cos_tab_128[] =
  {
   255, 255, 255, 255, 254, 254, 254, 253, 253, 252, // 0 - 9
   251, 250, 250, 249, 248, 246, 245, 244, 243, 241, //10 - 19
   240, 238, 237, 235, 234, 232, 230, 228, 226, 224, //20 - 29
   222, 220, 218, 215, 213, 211, 208, 206, 203, 201, //30 - 39
   198, 196, 193, 190, 188, 185, 182, 179, 176, 173, //40 - 49
   170, 167, 165, 162, 158, 155, 152, 149, 146, 143, //50 - 59
   140, 137, 134, 131, 127, 124, 121, 118, 115, 112, //60 - 69
   109, 106, 103, 100, 97,  93,  90,  88,  85,  82,  //70 - 79
   79,  76,  73,  70,  67,  65,  62,  59,  57,  54,  //80 - 89
   52,  49,  47,  44,  42,  40,  37,  35,  33,  31,  //90 - 99
   29,  27,  25,  23,  21,  20,  18,  17,  15,  14,  //100 - 109
   12,  11,  10,  9,   7,   6,   5,   5,   4,   3,   //110 - 119
   2,   2,   1,   1,   1,   0,   0,   0              //120 - 127
};

#endif
