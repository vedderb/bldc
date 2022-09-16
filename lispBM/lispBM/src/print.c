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
#include <lbm_types.h>
#include <lbm_custom_type.h>

#include "print.h"
#include "heap.h"
#include "symrepr.h"
#include "stack.h"

#define PRINT          1
#define PRINT_SPACE    2
#define START_LIST     3
#define CONTINUE_LIST  4
#define END_LIST       5
#define PRINT_DOT      6

static lbm_stack_t print_stack = { NULL, 0, 0, 0};
static bool print_has_stack = false;

const char *failed_str = "Error: print failed\n";

int lbm_print_init(lbm_uint *print_stack_storage, lbm_uint print_stack_size) {

  if (!print_stack_storage || print_stack_size == 0)
    return 0;

  if (lbm_stack_create(&print_stack, print_stack_storage, print_stack_size)) {
    print_has_stack = true;
    return 1;
  }
  return 0;
}

int lbm_print_value(char *buf,unsigned int len, lbm_value t) {

  int r = 0;
  unsigned int n = 0;
  unsigned int offset = 0;
  const char *str_ptr;
  int res;

  lbm_stack_clear(&print_stack);
  lbm_push_2(&print_stack, t, PRINT);

  while (!lbm_stack_is_empty(&print_stack) && offset <= len - 5) {

    lbm_value curr;
    lbm_uint  instr;
    lbm_pop(&print_stack, &instr);

    switch(instr) {

    case START_LIST: {
      res = 1;
      lbm_pop(&print_stack, &curr);

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

      if (lbm_type_of(cdr_val) == LBM_TYPE_CONS) {
        res &= lbm_push(&print_stack, cdr_val);
        res &= lbm_push(&print_stack, CONTINUE_LIST);
      } else if (lbm_type_of(cdr_val) == LBM_TYPE_SYMBOL &&
                 lbm_dec_sym(cdr_val) == SYM_NIL) {
        res &= lbm_push(&print_stack, END_LIST);
      } else {
        res &= lbm_push(&print_stack, END_LIST);
        res &= lbm_push(&print_stack, cdr_val);
        res &= lbm_push(&print_stack, PRINT);
        res &= lbm_push(&print_stack, PRINT_DOT);
      }
      res &= lbm_push(&print_stack, car_val);
      res &= lbm_push(&print_stack, PRINT);

      if (!res) {
        snprintf(buf, len, "Error: Out of print stack\n");
        return -1;
      }

      break;
    }
    case CONTINUE_LIST: {

      res = 1;
      lbm_pop(&print_stack, &curr);

      if (lbm_type_of(curr) == LBM_TYPE_SYMBOL &&
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

      if (lbm_type_of(cdr_val) == LBM_TYPE_CONS) {
        res &= lbm_push(&print_stack, cdr_val);
        res &= lbm_push(&print_stack, CONTINUE_LIST);
      } else if (lbm_type_of(cdr_val) == LBM_TYPE_SYMBOL &&
                  lbm_dec_sym(cdr_val) == SYM_NIL) {
        res &= lbm_push(&print_stack, END_LIST);
      } else {
        res &= lbm_push(&print_stack, END_LIST);
        res &= lbm_push(&print_stack, cdr_val);
        res &= lbm_push(&print_stack, PRINT);
        res &= lbm_push(&print_stack, PRINT_DOT);
      }
      res &= lbm_push(&print_stack, car_val);
      res &= lbm_push(&print_stack, PRINT);
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

      lbm_pop(&print_stack, &curr);

      switch(lbm_type_of(curr)) {

      case LBM_TYPE_CONS:{
        res = 1;
        res &= lbm_push(&print_stack, curr);
        res &= lbm_push(&print_stack, START_LIST);
        if (!res) {
          snprintf(buf, len, "Error: Out of print stack\n");
          return -1;
        }
        break;
      }

      case LBM_TYPE_REF:
        r = snprintf(buf + offset, len - offset, "_ref_");
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;

      case LBM_TYPE_FLOAT: {
        float v = lbm_dec_float(curr);
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

      case LBM_TYPE_DOUBLE: {
        double v = lbm_dec_double(curr);
        r = snprintf(buf + offset, len - offset, "{%lf}", (double)v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_TYPE_U32: {
        uint32_t v = lbm_dec_u32(curr);
        r = snprintf(buf + offset, len - offset, "{%"PRIu32"}", v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_TYPE_U64: {
        uint64_t v = lbm_dec_u64(curr);
        r = snprintf(buf + offset, len - offset, "{%"PRIu64"}", v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_TYPE_I32: {
        int32_t v = lbm_dec_i32(curr);
        r = snprintf(buf + offset, len - offset, "{%"PRId32"}", v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_TYPE_I64: {
        int64_t v = lbm_dec_i64(curr);
        r = snprintf(buf + offset, len - offset, "{%"PRId64"}", v);
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;
      }

      case LBM_TYPE_ARRAY: {
        lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(curr);
        switch (array->elt_type){
        case LBM_TYPE_CHAR:
          r = snprintf(buf + offset, len - offset, "\"%.*s\"", (int)array->size, (char *)array->data);
          if ( r > 0) {
            n = (unsigned int) r;
          } else {
            snprintf(buf, len, "%s", failed_str);
            return -1;
          }
          offset += n;
          break;
        default:
          r = snprintf(buf + offset, len - offset, "{");
          if (r == 1) {
            offset += 1;
          } else {
            snprintf(buf, len, "%s", failed_str);
            return -1;
          }
          for (unsigned int i = 0; i < array->size; i ++) {
            switch(array->elt_type) {
            case LBM_TYPE_I32:
              r = snprintf(buf+offset, len - offset, "%"PRIi32"%s", (int32_t)array->data[i], i == array->size - 1 ? "" : ", ");
              break;
            case LBM_TYPE_U32:
              r = snprintf(buf+offset, len - offset, "%"PRIu32"%s", (uint32_t)array->data[i], i == array->size - 1 ? "" : ", ");
              break;
            case LBM_TYPE_FLOAT: {
              float f_val;
              memcpy(&f_val, &array->data[i], sizeof(float));
              r = snprintf(buf+offset, len - offset, "%"PRI_FLOAT"%s",(double)f_val, i == array->size - 1 ? "" : ", ");
            } break;
            default:
              break;
            }
            if (r > 0) {
              offset += (unsigned int)r;
            } else {
              snprintf(buf, len, "%s", failed_str);
              return -1;
            }
          }
          snprintf(buf + offset, len - offset, "}");
          offset ++;
          break;
        }
        break;
      }
      case LBM_TYPE_CHANNEL: {

        r = snprintf(buf + offset, len - offset, "~CHANNEL~");
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
      } break;
      case LBM_TYPE_CUSTOM: {
        lbm_uint *custom = (lbm_uint*)lbm_car(curr);
        if (custom[CUSTOM_TYPE_DESCRIPTOR]) {
          r = snprintf(buf + offset, len - offset, "%s", (char*)custom[CUSTOM_TYPE_DESCRIPTOR]);
        } else {
          r = snprintf(buf + offset, len - offset, "Unspecified_Custom_Type");
        }
	if (r > 0) {
	  n = (unsigned int) r;
	} else {
	  snprintf(buf, len, "%s", failed_str);
          return -1;
	}
        offset += n;
      } break;
      case LBM_TYPE_SYMBOL:
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

      case LBM_TYPE_I:
        r = snprintf(buf + offset, len - offset, "%"PRI_INT"", lbm_dec_i(curr));
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;

      case LBM_TYPE_U:
        r = snprintf(buf + offset, len - offset, "%"PRI_UINT"", lbm_dec_u(curr));
        if ( r > 0) {
          n = (unsigned int) r;
        } else {
          snprintf(buf, len, "%s", failed_str);
          return -1;
        }
        offset += n;
        break;

      case LBM_TYPE_CHAR:
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
        snprintf(buf, len, "Error: print does not recognize type %"PRI_HEX" of value: %"PRI_HEX"", lbm_type_of(curr), curr);
        return -1;
        break;
      } // Switch type of curr
      break; // case PRINT

    default:
      snprintf(buf, len, "Error: Corrupt print stack!");
      return -1;
    }// Switch instruction
  }//While not empty stack

  if (!lbm_stack_is_empty(&print_stack)) {
    snprintf(buf + (len - 5), 4, "...");
    buf[len-1] = 0;
    return (int)len;
  }
  return (int)n;
}

