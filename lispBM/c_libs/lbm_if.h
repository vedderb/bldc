/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Joel Svensson    svenssonjoel@yahoo.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef LBM_IF_H
#define LBM_IF_H

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

typedef uint32_t lbm_value;
typedef uint32_t lbm_type;

typedef uint32_t lbm_uint;
typedef int32_t  lbm_int;
typedef float    lbm_float;

typedef struct {
	lbm_type elt_type;        /// Type of elements: VAL_TYPE_FLOAT, U, I or CHAR
	lbm_uint size;            /// Number of elements
	lbm_uint *data;           /// pointer to lbm_memory array or C array.
} lbm_array_header_t;

typedef lbm_value (*extension_fptr)(lbm_value*,lbm_uint);

#define LBM_ADDRESS_SHIFT               2
#define LBM_VAL_SHIFT                   4

#define LBM_PTR_MASK                     0x00000001u
#define LBM_PTR_BIT                      0x00000001u
#define LBM_PTR_VAL_MASK                 0x03FFFFFCu
#define LBM_PTR_TYPE_MASK                0xFC000000u

#define LBM_POINTER_TYPE_FIRST           0x10000000u
#define LBM_TYPE_CONS                    0x10000000u
#define LBM_NON_CONS_POINTER_TYPE_FIRST  0x20000000u
#define LBM_TYPE_U32                     0x20000000u
#define LBM_TYPE_I32                     0x30000000u
#define LBM_TYPE_I64                     0x40000000u
#define LBM_TYPE_U64                     0x50000000u
#define LBM_TYPE_FLOAT                   0x60000000u
#define LBM_TYPE_DOUBLE                  0x70000000u
#define LBM_TYPE_ARRAY                   0xD0000000u
#define LBM_TYPE_REF                     0xE0000000u
#define LBM_TYPE_STREAM                  0xF0000000u
#define LBM_NON_CONS_POINTER_TYPE_LAST   0xF0000000u
#define LBM_POINTER_TYPE_LAST            0xF0000000u

#define LBM_GC_MASK                      0x00000002u
#define LBM_GC_MARKED                    0x00000002u

#define LBM_VAL_MASK                     0xFFFFFFF0u
#define LBM_VAL_TYPE_MASK                0x0000000Cu
                                                    //    gc ptr
#define LBM_TYPE_SYMBOL                  0x00000000u // 00  0   0
#define LBM_TYPE_CHAR                    0x00000004u // 01  0   0
#define LBM_TYPE_BYTE                    0x00000004u
#define LBM_TYPE_U                       0x00000008u // 10  0   0
#define LBM_TYPE_I                       0x0000000Cu // 11  0   0

// Default and fixed symbol ids
#define SYM_NIL           0x0
#define SYM_QUOTE         0x1
#define SYM_TRUE          0x2
#define SYM_IF            0x3
#define SYM_LAMBDA        0x4
#define SYM_CLOSURE       0x5
#define SYM_LET           0x6
#define SYM_DEFINE        0x7
#define SYM_PROGN         0x8
#define SYM_READ          0x9
#define SYM_READ_PROGRAM  0xA
#define SYM_DONTCARE      0xB
#define SYM_MATCH         0xC
#define SYM_SEND          0xD
#define SYM_RECEIVE       0xE
#define SYM_MACRO         0xF
#define SYM_MACRO_EXPAND  0x10
#define SYM_CALLCC        0x11
#define SYM_CONT          0x12
#define SYM_SETVAR        0x13

// 0x20 - 0x2F are errors
#define SYM_RERROR        0x20  /* READ ERROR */
#define SYM_TERROR        0x21  /* TYPE ERROR */
#define SYM_EERROR        0x22  /* EVAL ERROR */
#define SYM_MERROR        0x23
#define SYM_NOT_FOUND     0x24
#define SYM_DIVZERO       0x25
#define SYM_FATAL_ERROR   0x26 /* Runtime system is corrupt */
#define SYM_STACK_ERROR   0x27
#define SYM_RECOVERED     0x28

static inline lbm_value lbm_enc_cons_ptr(lbm_uint x) {
  return ((x << LBM_ADDRESS_SHIFT) | LBM_TYPE_CONS | LBM_PTR_BIT);
}

static inline lbm_uint lbm_dec_ptr(lbm_value p) {
  return ((LBM_PTR_VAL_MASK & p) >> LBM_ADDRESS_SHIFT);
}

static inline lbm_value lbm_set_ptr_type(lbm_value p, lbm_type t) {
  return ((LBM_PTR_VAL_MASK & p) | t | LBM_PTR_BIT);
}

static inline lbm_value lbm_enc_sym(lbm_uint s) {
  return (s << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL;
}

static inline lbm_value lbm_enc_i(lbm_int x) {
  return ((lbm_uint)x << LBM_VAL_SHIFT) | LBM_TYPE_I;
}

static inline lbm_value lbm_enc_u(lbm_uint x) {
  return (x << LBM_VAL_SHIFT) | LBM_TYPE_U;
}

static inline lbm_value lbm_enc_char(char x) {
  return ((lbm_uint)x << LBM_VAL_SHIFT) | LBM_TYPE_CHAR;
}

static inline lbm_int lbm_dec_i(lbm_value x) {
  return (lbm_int)x >> LBM_VAL_SHIFT;
}

static inline lbm_uint lbm_dec_u(lbm_value x) {
  return x >> LBM_VAL_SHIFT;
}

static inline char lbm_dec_char(lbm_value x) {
  return (char)(x >> LBM_VAL_SHIFT);
}

static inline lbm_uint lbm_dec_sym(lbm_value x) {
  return x >> LBM_VAL_SHIFT;
}

static inline lbm_type lbm_type_of(lbm_value x) {
  return (x & LBM_PTR_MASK) ? (x & LBM_PTR_TYPE_MASK) : (x & LBM_VAL_TYPE_MASK);
}

static inline bool lbm_is_ptr(lbm_value x) {
  return (x & LBM_PTR_MASK);
}

static inline bool lbm_is_list(lbm_value x) {
  return (lbm_type_of(x) == LBM_TYPE_CONS);
}

static inline bool lbm_is_number(lbm_value x) {
  lbm_uint t = lbm_type_of(x);
  return ((t == LBM_TYPE_I) ||
          (t == LBM_TYPE_U) ||
          (t == LBM_TYPE_CHAR) ||
          (t == LBM_TYPE_I32) ||
          (t == LBM_TYPE_U32) ||
          (t == LBM_TYPE_I64) ||
          (t == LBM_TYPE_U64) ||
          (t == LBM_TYPE_FLOAT) ||
          (t == LBM_TYPE_DOUBLE));
}

static inline bool lbm_is_char(lbm_value x) {
  lbm_uint t = lbm_type_of(x);
  return (t == LBM_TYPE_CHAR);
}

#endif  // LBM_IF_H

