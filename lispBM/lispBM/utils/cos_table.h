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

#ifndef COS_TABLE_H_
#define COS_TABLE_H_


/* 
   The cosine table holds Q1.15 fixed point representations of a quadrant of cos.
   
   Q1.15 and QN.M in general means an integer type of N+M bits with N bits of 
   integer value and M bits of fractional precision. 
   
   in Q1.15 we have 1 bit at the integer level and it really just represents sign.
   0 - 32767 represents positive values 0.0 to 1.0 
   while 32768 - 65535 represents the negative range.

   Functions in this file contains the fixed point format it returns 
   as part of its name as qN_M. 

   gettingt the integer part (essentially floor) of a qN_M number 
   is done via a shift right by M:  (x >> M).
   (Note that >> must be an arithmetic shift on the target platform.
    so that 10000000b >> 7 = 11111111)
   
*/

#include <stdint.h>

static const int16_t cos_tab128[] =
  {
    32767,  32764,  32757,  32744,  32727,  32704,  32677,  32644,  32607,  32564, // 0 - 9
    32517,  32464,  32407,  32344,  32277,  32205,  32127,  32045,  31958,  31866, // 10 - 19
    31770,  31668,  31561,  31450,  31334,  31213,  31087,  30957,  30822,  30682, // 20 - 29
    30537,  30388,  30234,  30075,  29912,  29744,  29572,  29395,  29214,  29028, // 30 - 39
    28838,  28643,  28444,  28241,  28033,  27821,  27605,  27385,  27160,  26931, // 40 - 49
    26698,  26461,  26220,  25975,  25726,  25473,  25216,  24956,  24691,  24423, // 50 - 59
    24151,  23875,  23596,  23313,  23026,  22736,  22442,  22145,  21845,  21541, // 60 - 69
    21234,  20924,  20610,  20294,  19974,  19651,  19325,  18997,  18665,  18331, // 70 - 79
    17993,  17653,  17310,  16965,  16617,  16266,  15913,  15558,  15200,  14840, // 80 - 89
    14477,  14113,  13746,  13377,  13006,  12633,  12258,  11881,  11503,  11122, // 90 - 99
    10740,  10357,   9971,   9584,   9196,   8806,   8415,   8023,   7630,   7235, //100 - 109
     6839,   6442,   6044,   5646,   5246,   4845,   4444,   4042,   3640,   3237, //110 - 119
     2833,   2429,   2025,   1620,   1216,    810,    405,      0,                 //120 - 127
  };

#define TRIG_TAB_SIZE  128
#define TRIG_QUARTER   TRIG_TAB_SIZE          // 128 raw-index units per quadrant
#define TRIG_FULL_TURN (TRIG_TAB_SIZE * 4)    // 512 raw-index units per full turn

static inline int16_t cos_idx_q1_15(int idx) {
  idx &= (TRIG_FULL_TURN - 1);
  int i = idx & (TRIG_TAB_SIZE - 1);
  switch ((idx >> 7) & 3) {
  case 0:  return cos_tab128[i];
  case 1:  return (int16_t)(-cos_tab128[TRIG_TAB_SIZE - 1 - i]);
  case 2:  return (int16_t)(-cos_tab128[i]);
  default: return cos_tab128[TRIG_TAB_SIZE - 1 - i];
  }
}

static inline int16_t sin_idx_q1_15(int idx) {
  return cos_idx_q1_15(idx - TRIG_QUARTER);
}


static inline int16_t cos_q1_15(uint16_t phase_q9_7) {
  return cos_idx_q1_15(phase_q9_7 >> 7);
}

static inline int16_t sin_q1_15(uint16_t phase_q9_7) {
  return sin_idx_q1_15(phase_q9_7 >> 7);
}

static inline int16_t cos_lerp_q1_15(uint16_t phase_q9_7) {
  int idx  = phase_q9_7 >> 7;
  int frac = phase_q9_7 & 0x7F;
  int16_t a = cos_idx_q1_15(idx);
  int16_t b = cos_idx_q1_15(idx + 1);
  return (int16_t)(a + (((int32_t)(b - a) * frac) >> 7));
}

static inline int16_t sin_lerp_q1_15(uint16_t phase_q9_7) {
  return cos_lerp_q1_15((uint16_t)(phase_q9_7 - (TRIG_QUARTER << 7)));
}

static inline uint8_t cos_q0_8(int tab_pos) {
  int i = tab_pos <= 127 ? tab_pos : 128 - (tab_pos - 127);
  return (uint8_t)(cos_tab128[i] >> 7);
}

#endif
