/*
    Copyright 2023 Joel Svensson    svenssonjoel@yahoo.se

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

#ifndef LBM_FLAT_VALUE_H_
#define LBM_FLAT_VALUE_H_

#include <heap.h>
#include <symrepr.h>
#include <lbm_memory.h>

typedef struct {
  uint8_t *buf;
  size_t   buf_size;
  uint32_t buf_pos;
} lbm_flat_value_t;
                              // Arity   
#define S_CONS            0x1 // 2      car, cdr
#define S_SYM_VALUE       0x2 // 1      value
#define S_BYTE_VALUE      0x3
#define S_I_VALUE         0x4
#define S_U_VALUE         0x5
#define S_I32_VALUE       0x6
#define S_U32_VALUE       0x7
#define S_FLOAT_VALUE     0x8
#define S_I64_VALUE       0x9
#define S_U64_VALUE       0xA
#define S_DOUBLE_VALUE    0xB
#define S_LBM_ARRAY       0xC // 3      size, type, ptr

bool lbm_start_flatten(lbm_flat_value_t *v, size_t buffer_size);
bool lbm_finish_flatten(lbm_flat_value_t *v);
bool f_cons(lbm_flat_value_t *v);
bool f_sym(lbm_flat_value_t *v, lbm_uint sym);
bool f_i(lbm_flat_value_t *v, lbm_int i);
bool f_b(lbm_flat_value_t *v, uint8_t b);
bool f_i32(lbm_flat_value_t *v, int32_t w);
bool f_u32(lbm_flat_value_t *v, uint32_t w);
bool f_float(lbm_flat_value_t *v, float f);
bool f_i64(lbm_flat_value_t *v, int64_t w);
bool f_u64(lbm_flat_value_t *v, uint64_t w);
bool f_lbm_array(lbm_flat_value_t *v, uint32_t num_elts, lbm_type t, uint8_t *data);

/** Unflatten a flat value stored in an lbm_memory array onto the heap
 *  
 *  \param v Flat value to unflatten. 
 *  \param res Pointer to where the result lbm_value should be stored. 
 *  \return True on success and false otherwise.
 */ 
bool lbm_unflatten_value(lbm_flat_value_t *v, lbm_value *res);
#endif
