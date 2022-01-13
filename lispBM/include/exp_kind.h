/*
    Copyright 2020, 2021     Joel Svensson      svenssonjoel@yahoo.se

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

#ifndef _EXP_KIND_H_
#define _EXP_KIND_H_

#include "heap.h"
#include "symrepr.h"

typedef enum {
  EXP_KIND_ERROR,
  EXP_SELF_EVALUATING,
  EXP_VARIABLE,
  EXP_QUOTED,
  EXP_DEFINE,
  EXP_LAMBDA,
  EXP_IF,
  EXP_PROGN,
  EXP_NO_ARGS,
  EXP_APPLICATION,
  EXP_LET,
  EXP_AND,
  EXP_OR
} exp_kind;

static inline exp_kind exp_kind_of(VALUE exp) {

  switch (type_of(exp)) {
  case VAL_TYPE_SYMBOL:
    if (!is_special(exp))
      return EXP_VARIABLE;
    // fall through
  case PTR_TYPE_BOXED_F:
  case PTR_TYPE_BOXED_U:
  case PTR_TYPE_BOXED_I:
  case VAL_TYPE_I:
  case VAL_TYPE_U:
  case VAL_TYPE_CHAR:
  case PTR_TYPE_ARRAY:
    return EXP_SELF_EVALUATING;
  case PTR_TYPE_CONS: {
    VALUE head = car(exp);
    if (type_of(head) == VAL_TYPE_SYMBOL) {
      UINT sym_id = dec_sym(head);
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

    if (type_of(cdr(exp)) == VAL_TYPE_SYMBOL &&
        dec_sym(cdr(exp)) == SYM_NIL) {
      return EXP_NO_ARGS;
    } else {
      return EXP_APPLICATION;
    }
  } // end case PTR_TYPE_CONS:
  }
  return EXP_KIND_ERROR;
}

#endif
