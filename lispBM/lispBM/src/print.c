/*
    Copyright 2018, 2020 - 2024      Joel Svensson    svenssonjoel@yahoo.se
                           2022      Benjamin Vedder

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
#include <ctype.h>
#include <inttypes.h>
#include <lbm_types.h>
#include <lbm_custom_type.h>

#include "print.h"
#include "heap.h"
#include "symrepr.h"
#include "stack.h"
#include "lbm_channel.h"

#define PRINT          1
#define PRINT_SPACE    2
#define START_LIST     3
#define CONTINUE_LIST  4
#define END_LIST       5
#define PRINT_DOT      6

static lbm_stack_t print_stack = { NULL, 0, 0, 0};
static bool print_has_stack = false;

const char *failed_str = "Error: print failed\n";

bool lbm_value_is_printable_string(lbm_value v, char **str) {
  bool is_a_string = false;
  if (lbm_is_array_r(v)) {
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(v);

    is_a_string = true;
    // TODO: Potential null deref.
    //       Highly unlikely that array is a recognizable NULL though.
    //       If it is incorrect, it is most likely arbitrary.
    char *c_data = (char *)array->data;
    if (array->size == 1) {
      *str = c_data;
      return c_data[0] == 0;
    }
    unsigned int i;
    for (i = 0; i < array->size; i ++) {
      if (c_data[i] == 0 && i > 0) break;
      if (!isprint((unsigned char)c_data[i]) && !iscntrl((unsigned char)c_data[i])) {
        is_a_string = false;
        break;
      }
    }

    if (i == array->size) i--;
    if (i > 0 && c_data[i] != 0) is_a_string = false;
    if (is_a_string) {
      *str = (char*)array->data;
    }
  }
  return is_a_string;
}


int lbm_print_init(lbm_uint print_stack_size) {

  if (print_stack_size == 0)
    return 0;

  lbm_uint *print_stack_storage = (lbm_uint*)lbm_malloc(print_stack_size * sizeof(lbm_uint));
  if (!print_stack_storage) return 0;

  if (lbm_stack_create(&print_stack, print_stack_storage, print_stack_size)) {
    print_has_stack = true;
    return 1;
  }
  return 0;
}

#define EMIT_BUFFER_SIZE 30

#define EMIT_FAILED -1
#define EMIT_OK      0

int print_emit_string(lbm_char_channel_t *chan, char* str) {
  if (str == NULL) return EMIT_FAILED;
  while (*str != 0) {
    int r = lbm_channel_write(chan, *str);
    str++;
    if (r != CHANNEL_SUCCESS) return EMIT_FAILED;
  }
  return EMIT_OK;
}

int print_emit_char(lbm_char_channel_t *chan, char c) {

  int r = lbm_channel_write(chan, c);
  if (r != CHANNEL_SUCCESS) return EMIT_FAILED;
  return EMIT_OK;
}


int emit_escape(lbm_char_channel_t *chan, char c) {
  switch(c) {
  case '"': return print_emit_string(chan, "\\\"");
  case '\n': return print_emit_string(chan, "\\n");
  case '\r': return print_emit_string(chan, "\\r");
  case '\t': return print_emit_string(chan, "\\t");
  case '\\': return print_emit_string(chan, "\\\\");
  default:
    return print_emit_char(chan, c);
  }
}

int print_emit_string_value(lbm_char_channel_t *chan, char* str) {
  if (str == NULL) return EMIT_FAILED;
  while (*str != 0) {
    int r = emit_escape(chan, *str++);
    if (r != EMIT_OK) return r;
  }
  return EMIT_OK;
}

int print_emit_symbol(lbm_char_channel_t *chan, lbm_value sym) {
  char *str_ptr = (char*)lbm_get_name_by_symbol(lbm_dec_sym(sym));
  return print_emit_string(chan, str_ptr);
}

int print_emit_i(lbm_char_channel_t *chan, lbm_int v) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%"PRI_INT, v);
  return print_emit_string(chan, buf);
}

int print_emit_u(lbm_char_channel_t *chan, lbm_uint v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%"PRI_UINT"%s", v, ps ? "u" : "");
  return print_emit_string(chan, buf);
}

int print_emit_byte(lbm_char_channel_t *chan, uint8_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%u%s", v, ps ? "b" : "");
  return print_emit_string(chan, buf);
}

int print_emit_float(lbm_char_channel_t *chan, float v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%"PRI_FLOAT"%s", (double)v, ps ? "f32" : "");
  return print_emit_string(chan, buf);
}

int print_emit_double(lbm_char_channel_t *chan, double v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%lf%s", v, ps ? "f64" : "");
  return print_emit_string(chan, buf);
}

int print_emit_u32(lbm_char_channel_t *chan, uint32_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRIu32"%s", v, ps ? "u32" : "");
  return print_emit_string(chan, buf);
}

int print_emit_i32(lbm_char_channel_t *chan, int32_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRId32"%s", v, ps ? "i32" : "");
  return print_emit_string(chan, buf);
}

int print_emit_u64(lbm_char_channel_t *chan, uint64_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRIu64"%s", v, ps ? "u64" : "");
  return print_emit_string(chan, buf);
}

int print_emit_i64(lbm_char_channel_t *chan, int64_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRId64"%s", v, ps ? "i64" : "");
  return print_emit_string(chan, buf);
}

int print_emit_continuation(lbm_char_channel_t *chan, lbm_value v) {
  char buf[EMIT_BUFFER_SIZE];
  lbm_uint cont = (v & ~LBM_CONTINUATION_INTERNAL) >> LBM_ADDRESS_SHIFT;
  snprintf(buf, EMIT_BUFFER_SIZE, "CONT[" "%"PRI_UINT"]", cont);
  return print_emit_string(chan, buf);
}

int print_emit_custom(lbm_char_channel_t *chan, lbm_value v) {
  lbm_uint *custom = (lbm_uint*)lbm_car(v);
  int r;
  if (custom && custom[CUSTOM_TYPE_DESCRIPTOR]) {
    r = print_emit_string(chan, (char*)custom[CUSTOM_TYPE_DESCRIPTOR]);
  } else {
    r = print_emit_string(chan, "Unspecified_Custom_Type");
  }
  return r;
}

int print_emit_channel(lbm_char_channel_t *chan, lbm_value v) {
  (void) v;
  return print_emit_string(chan, "~CHANNEL~");
}

int print_emit_array_data(lbm_char_channel_t *chan, lbm_array_header_t *array) {

  int r = print_emit_char(chan, '[');

  if (r == EMIT_OK) {

    for (unsigned int i = 0; i < array->size; i ++) {

      char *c_data = (char*)array->data;
      r = print_emit_byte(chan, (uint8_t)c_data[i], false);

      if (r == EMIT_OK && i != array->size - 1) {
        r = print_emit_char(chan, ' ');
      }
    }

    if (r != EMIT_OK) return r;
    return print_emit_char(chan, ']');
  }
  return r;
}

int print_emit_array(lbm_char_channel_t *chan, lbm_value v) {

  char *str;

  if (lbm_value_is_printable_string(v, &str)) {
    int r = print_emit_char(chan, '"');
    if (r == EMIT_OK) {
      r = print_emit_string_value(chan, str);
      if (r == EMIT_OK) {
        r = print_emit_char(chan, '"');
      }
    }
    return r;
  }

  lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(v);
  return print_emit_array_data(chan, array);
}


int lbm_print_internal(lbm_char_channel_t *chan, lbm_value v) {

  lbm_stack_clear(&print_stack);
  lbm_push_2(&print_stack, v, PRINT);
  bool chan_full = false;
  lbm_value curr;
  lbm_uint instr;
  int r = EMIT_FAILED;

  while (!lbm_stack_is_empty(&print_stack) && !chan_full) {
    lbm_pop(&print_stack, &instr);
    switch (instr) {
    case START_LIST: {
      lbm_pop(&print_stack, &curr);
      r = print_emit_char(chan, '(');
      if (r != EMIT_OK) return r;
      lbm_value car_val = lbm_car(curr);
      lbm_value cdr_val = lbm_cdr(curr);
      int res = 1;
      if (lbm_type_of(cdr_val) == LBM_TYPE_CONS ||
          lbm_type_of(cdr_val) == (LBM_TYPE_CONS | LBM_PTR_TO_CONSTANT_BIT)) {
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
        return EMIT_FAILED;
      }
      break;
    }
    case CONTINUE_LIST: {
      int res = 1;
      lbm_pop(&print_stack, &curr);

      if (lbm_type_of(curr) == LBM_TYPE_SYMBOL &&
          lbm_dec_sym(curr) == SYM_NIL) {
        break;
      }

      lbm_value car_val = lbm_car(curr);
      lbm_value cdr_val = lbm_cdr(curr);

      r = print_emit_char(chan, ' ');
      if (r != EMIT_OK) {
        return r;
      }
      if (lbm_type_of(cdr_val) == LBM_TYPE_CONS ||
          lbm_type_of(cdr_val) == (LBM_TYPE_CONS | LBM_PTR_TO_CONSTANT_BIT)) {
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
        return EMIT_FAILED;
      }
      break;
    }
    case END_LIST:
      r = print_emit_char(chan, ')');
      if (r != EMIT_OK) return r;
      break;
    case PRINT_SPACE:
      r = print_emit_char(chan, ' ');
      if (r != EMIT_OK) return r;
      break;
    case PRINT_DOT:
      r = print_emit_string(chan, " . ");
      if (r != EMIT_OK) return r;
      break;
    case PRINT:
      lbm_pop(&print_stack, &curr);

      lbm_type t = lbm_type_of(curr);
      if (lbm_is_ptr(curr))
          t = t & LBM_PTR_TO_CONSTANT_MASK; // print constants normally
      
      switch(t) {
        //case LBM_TYPE_CONS_CONST: /* fall through */
      case LBM_TYPE_CONS: {
        int res = lbm_push_2(&print_stack, curr, START_LIST);
        if (!res) {
          print_emit_string(chan," ...");
          return EMIT_OK;
        }
        break;
      }
      case LBM_TYPE_SYMBOL:
        r = print_emit_symbol(chan, curr);
        break;
      case LBM_TYPE_I:
        r = print_emit_i(chan, lbm_dec_i(curr));
        break;
      case LBM_TYPE_U:
        r = print_emit_u(chan, lbm_dec_u(curr), true);
        break;
      case LBM_TYPE_CHAR:
        r = print_emit_byte(chan, (uint8_t)lbm_dec_char(curr), true);
        break;
      case LBM_TYPE_FLOAT:
        r = print_emit_float(chan, lbm_dec_float(curr), true);
        break;
      case LBM_TYPE_DOUBLE:
        r = print_emit_double(chan, lbm_dec_double(curr), true);
        break;
      case LBM_TYPE_U32:
        r = print_emit_u32(chan, lbm_dec_u32(curr), true);
        break;
      case LBM_TYPE_I32:
        r = print_emit_i32(chan, lbm_dec_i32(curr), true);
        break;
      case LBM_TYPE_U64:
        r = print_emit_u64(chan, lbm_dec_u64(curr), true);
        break;
      case LBM_TYPE_I64:
        r = print_emit_i64(chan, lbm_dec_i64(curr), true);
        break;
      case LBM_CONTINUATION_INTERNAL_TYPE:
        r = print_emit_continuation(chan, curr);
        break;
      case LBM_TYPE_CUSTOM:
        r = print_emit_custom(chan, curr);
        break;
      case LBM_TYPE_CHANNEL:
        r = print_emit_channel(chan, curr);
        break;
      case LBM_TYPE_ARRAY:
        r = print_emit_array(chan, curr);
        break;
      default:
        return EMIT_FAILED;
      }
    }
  }
  return r;
}

int lbm_print_value(char *buf, unsigned int len, lbm_value v) {

  lbm_string_channel_state_t st;
  lbm_char_channel_t chan;

  memset(buf, 0, len);
  lbm_create_string_char_channel_size(&st, &chan, buf, len);
  if (lbm_print_internal(&chan,v) == EMIT_OK)
    return 1;
  return 0;
}
