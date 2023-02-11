/*
    Copyright 2019 Joel Svensson        svenssonjoel@yahoo.se

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
/** \file fundamental.h
 *
 *  Implementation of the built in functions of the lispbm language (such as +, -, ... ).
 *
 */

#ifndef _FUNDAMENTAL_H_
#define _FUNDAMENTAL_H_

#include <eval_cps.h>

#ifdef __cplusplus
extern "C" {
#endif

  void fundamental_add(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_sub(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_mul(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_div(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_mod(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_numeq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_num_not_eq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_lt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_gt(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_leq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_geq(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_gc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_self(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_set_mailbox_size(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_cons(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_car(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_list(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_append(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_undefine(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_array_read(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_array_write(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_array_create(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_array_size(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_array_clear(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_symbol_to_string(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_string_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_symbol_to_uint(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_uint_to_symbol(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_set_car(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_set_cdr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_set_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_assoc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_acons(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_set_assoc(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_cossa(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_ix(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_i(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_i32(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_u(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_u32(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_float(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_i64(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_u64(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_double(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_to_byte(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_shl(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_shr(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_bitwise_and(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_bitwise_or(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_bitwise_xor(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_bitwise_not(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_custom_destruct(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_type_of(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_list_length(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_range(lbm_value *args, lbm_uint nargs, eval_context_t *ctx);
  void fundamental_reg_event_handler(lbm_value *args, lbm_uint argn, eval_context_t *ctx);

  bool struct_eq(lbm_value a, lbm_value b);
#ifdef __cplusplus
}
#endif
#endif


