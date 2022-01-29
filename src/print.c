/*
    Copyright 2018, 2020, 2021, 2022 Joel Svensson    svenssonjoel@yahoo.se
                                2022 Benjamin Vedder

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

#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "print.h"
#include "heap.h"
#include "symrepr.h"
#include "lispbm_types.h"
#include "stack.h"

#define PRINT_STACK_SIZE 128 /* 1 KB */

#define PRINT          1
#define PRINT_SPACE    2
#define START_LIST     3
#define CONTINUE_LIST  4
#define END_LIST       5
#define PRINT_DOT      6

static lbm_value stack_storage[PRINT_STACK_SIZE];

const char *failed_str = "Error: print failed\n";

int lbm_print_value(char *buf,unsigned int len, lbm_value t) {

  lbm_stack_t s;
  lbm_stack_create(&s, stack_storage, PRINT_STACK_SIZE);

  int r = 0;
  unsigned int n = 0;
  unsigned int offset = 0;
  const char *str_ptr;
  int res;

  lbm_push_u32_2(&s, t, PRINT);

  while (!lbm_stack_is_empty(&s) && offset <= len - 5) {

    lbm_value curr;
    lbm_uint  instr;
    lbm_pop_u32(&s, &instr);

    switch(instr) {

    case START_LIST: {
      res = 1;
      lbm_pop_u32(&s, &curr);

      r = snprintf(buf + offset, len - offset, "(");
      if ( r >= 0 ) {
        n = (unsigned int) r;
      } else {
        snprintf(buf, len, "%s", failed_str);
        return -1;
      }

      offset += n;
      lbm_value car_val = lbm_car(curr);
      lbm_value cdr_val = lbm_cdr(curr);

      if (lbm_type_of(cdr_val) == LBM_PTR_TYPE_CONS) {
        res &= lbm_push_u32(&s, cdr_val);
        res &= lbm_push_u32(&s, CONTINUE_LIST);
      } else if (lbm_type_of(cdr_val) == LBM_VAL_TYPE_SYMBOL &&
                 lbm_dec_sym(cdr_val) == SYM_NIL) {
        res &= lbm_push_u32(&s, END_LIST);
      } else {
        res &= lbm_push_u32(&s, END_LIST);
        res &= lbm_push_u32(&s, cdr_val);
        res &= lbm_push_u32(&s, PRINT);
        res &= lbm_push_u32(&s, PRINT_DOT);
      }
      res &= lbm_push_u32(&s, car_val);
      res &= lbm_push_u32(&s, PRINT);

      if (!res) {
        snprintf(buf, len, "Error: Out of print stack\n");
        return -1;
      }

      break;
    }
    case CONTINUE_LIST: {

      res = 1;
      lbm_pop_u32(&s, &curr);

      if (lbm_type_of(curr) == LBM_VAL_TYPE_SYMBOL &&
          lbm_dec_sym(curr) == SYM_NIL) {
        break;
      }

      lbm_value car_val = lbm_car(curr);
      lbm_value cdr_val = lbm_cdr(curr);

      r = snprintf(buf + offset, len - offset, " ");
      if ( r > 0) {
        n = (unsigned int) r;
      } else {
        snprintf(buf, len, "%s", failed_str);
        return -1;
      }
      offset += n;

      if (lbm_type_of(cdr_val) == LBM_PTR_TYPE_CONS) {
        res &= lbm_push_u32(&s, cdr_val);
        res &= lbm_push_u32(&s, CONTINUE_LIST);
      } else if (lbm_type_of(cdr_val) == LBM_VAL_TYPE_SYMBOL &&
                  lbm_dec_sym(cdr_val) == SYM_NIL) {
        res &= lbm_push_u32(&s, END_LIST);
      } else {
        res &= lbm_push_u32(&s, END_LIST);
        res &= lbm_push_u32(&s, cdr_val);
        res &= lbm_push_u32(&s, PRINT);
        res &= lbm_push_u32(&s, PRINT_DOT);
      }
      res &= lbm_push_u32(&s, car_val);
      res &= lbm_push_u32(&s, PRINT);
      if (!res) {
        snprintf(buf, len, "Error: Out of print stack\n");
        return -1;
      }
      break;
    }
    case END_LIST:
      r = snprintf(buf + offset, len - offset, ")");
      if ( r > 0) {
        n = (unsigned int) r;
      } else {
        snprintf(buf, len, "%s", failed_str);
        return -1;
      }
      offset += n;
      break;

    case PRINT_SPACE:
      r = snprintf(buf + offset, len - offset, " ");
      if ( r > 0) {
        n = (unsigned int) r;
      } else {
        snprintf(buf, len, "%s", failed_str);
        return -1;
      }

      offset += n;
      break;
    case PRINT_DOT: /* space dot space */
      r = snprintf(buf + offset, len - offset, " . ");
      if (r > 0) {
        n = (unsigned int) r;
      } else {
        snprintf(buf, len, "%s", failed_str);
        return -1;
      }
      offset +=n;
      break;

    case PRINT:

      lbm_pop_u32(&s, &curr);

      switch(lbm_type_of(curr)) {

      case LBM_PTR_TYPE_CONS:{
        res = 1;
        res &= lbm_push_u32(&s, curr);
        res &= lbm_push_u32(&s, START_LIST);
        if (!res) {
          snprintf(buf, len, "Error: Out of print stack\n");
          return -1;
        }
        break;
      }

      case LBM_PTR_TYPE_REF:
        r = snprintf(buf + offset, len - offset, "_ref_");
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;

      case LBM_PTR_TYPE_BOXED_F: {
        lbm_value uv = lbm_car(curr);
        float v;
        memcpy(&v, &uv, sizeof(float)); // = *(float*)(&uv);
        r = snprintf(buf + offset, len - offset, "{%"PRI_FLOAT"}", (double)v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_PTR_TYPE_BOXED_U: {
        lbm_value v = lbm_car(curr);
        r = snprintf(buf + offset, len - offset, "{%"PRI_UINT"}", v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_PTR_TYPE_BOXED_I: {
        int32_t v = (int32_t)lbm_car(curr);
        r = snprintf(buf + offset, len - offset, "{%"PRI_INT"}", v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_PTR_TYPE_ARRAY: {
        lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(curr);
        switch (array->elt_type){
        case LBM_VAL_TYPE_CHAR:
          r = snprintf(buf + offset, len - offset, "\"%s\"", (char *)(array)+8);
          if ( r > 0) {
            n = (unsigned int) r;
          } else {
            snprintf(buf, len, "%s", failed_str);
            return -1;
          }
          offset += n;
          break;
          break;
        default:
          snprintf(buf, len, "Error: Array type not supported\n");
          return -1;
        }
        break;
      }
      case LBM_PTR_TYPE_STREAM: {

        r = snprintf(buf + offset, len - offset, "~STREAM~");
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }
      case LBM_VAL_TYPE_SYMBOL:
        str_ptr = lbm_get_name_by_symbol(lbm_dec_sym(curr));
        if (str_ptr == NULL) {

          snprintf(buf, len, "Error: Symbol not in table %"PRI_UINT"", lbm_dec_sym(curr));
          return -1;
        }
        r = snprintf(buf + offset, len - offset, "%s", str_ptr);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break; //Break VAL_TYPE_SYMBOL

      case LBM_VAL_TYPE_I:
        r = snprintf(buf + offset, len - offset, "%"PRI_INT"", lbm_dec_i(curr));
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;

      case LBM_VAL_TYPE_U:
        r = snprintf(buf + offset, len - offset, "%"PRI_UINT"", lbm_dec_u(curr));
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;

      case LBM_VAL_TYPE_CHAR:
        r = snprintf(buf + offset, len - offset, "\\#%c", lbm_dec_char(curr));
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;

      default:
        snprintf(buf, len, "Error: print does not recognize type of value: %"PRIx32"", curr);
        return -1;
        break;
      } // Switch type of curr
      break; // case PRINT

    default:
      snprintf(buf, len, "Error: Corrupt print stack!");
      return -1;
    }// Switch instruction
  }//While not empty stack


  if (!lbm_stack_is_empty(&s)) {
    snprintf(buf + (len - 5), 4, "...");
    buf[len-1] = 0;
    return (int)len;
  }


  return (int)n;
}

