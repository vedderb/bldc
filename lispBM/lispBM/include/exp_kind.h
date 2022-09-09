/*
    Copyright 2020, 2021, 2022   Joel Svensson      svenssonjoel@yahoo.se

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
/** \file exp_kind.h */

#ifndef _EXP_KIND_H_
#define _EXP_KIND_H_

#include "heap.h"
#include "symrepr.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 *  A set of classes of expressions.
 *  The classification is useful for deciding upon evaluation pattern to apply
 */
typedef enum {
  EXP_KIND_ERROR,     /**< EXP_KIND_ERROR */
  EXP_SELF_EVALUATING,/**< EXP_SELF_EVALUATING */
  EXP_VARIABLE,       /**< EXP_VARIABLE */
  EXP_QUOTED,         /**< EXP_QUOTED */
  EXP_DEFINE,         /**< EXP_DEFINE */
  EXP_LAMBDA,         /**< EXP_LAMBDA */
  EXP_IF,             /**< EXP_IF */
  EXP_PROGN,          /**< EXP_PROGN */
  EXP_NO_ARGS,        /**< EXP_NO_ARGS */
  EXP_APPLICATION,    /**< EXP_APPLICATION */
  EXP_LET,            /**< EXP_LET */
  EXP_AND,            /**< EXP_AND */
  EXP_OR              /**< EXP_OR */
} lbm_exp_kind;

/** Classify an expression according the lbm_exp_kind classes.
 *
 * \param exp Expression to classify.
 * \return Class that expression belongs to.
 */
static inline lbm_exp_kind lbm_exp_kind_of(lbm_value exp) {

  switch (lbm_type_of(exp)) {
  case LBM_TYPE_SYMBOL:
    if (!lbm_is_special(exp))
      return EXP_VARIABLE;
    // fall through
  case LBM_TYPE_FLOAT:
  case LBM_TYPE_U32:
  case LBM_TYPE_I32:
  case LBM_TYPE_I:
  case LBM_TYPE_U:
  case LBM_TYPE_CHAR:
  case LBM_TYPE_ARRAY:
    return EXP_SELF_EVALUATING;
  case LBM_TYPE_CONS: {
    lbm_value head = lbm_car(exp);
    if (lbm_type_of(head) == LBM_TYPE_SYMBOL) {
      lbm_uint sym_id = lbm_dec_sym(head);
      switch(sym_id){
      case SYM_AND:    return EXP_AND;
      case SYM_OR:     return EXP_OR;
      case SYM_QUOTE:  return EXP_QUOTED;
      case SYM_DEFINE: return EXP_DEFINE;
      case SYM_PROGN:  return EXP_PROGN;
      case SYM_LAMBDA: return EXP_LAMBDA;
      case SYM_IF:     return EXP_IF;
      case SYM_LET:    return EXP_LET;
      default: break;
      }
    } // end if symbol

    if (lbm_type_of(lbm_cdr(exp)) == LBM_TYPE_SYMBOL &&
        lbm_dec_sym(lbm_cdr(exp)) == SYM_NIL) {
      return EXP_NO_ARGS;
    } else {
      return EXP_APPLICATION;
    }
  } // end case PTR_TYPE_CONS:
  }
  return EXP_KIND_ERROR;
}

#ifdef __cplusplus
}
#endif
#endif
