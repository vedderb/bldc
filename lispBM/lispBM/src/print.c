/*
    Copyright 2018, 2020 - 2025      Joel Svensson    svenssonjoel@yahoo.se
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
#define START_ARRAY    7
#define CONTINUE_ARRAY 8
#define END_ARRAY      9

static lbm_stack_t print_stack = { NULL, 0, 0};
static bool print_has_stack = false;

const char *failed_str = "Error: print failed\n";

static int push_n(lbm_stack_t *s, lbm_uint *values, lbm_uint n) {
  if (s->sp + n < s->size) {
    for (lbm_uint i = 0; i < n; i ++) {
      s->data[s->sp+i] = values[i];
    }
    s->sp+=n;
    return 1;
  }
  return 0;
}

bool lbm_value_is_printable_string(lbm_value v, char **str) {
  bool is_a_string = false;
  if (lbm_is_array_r(v)) {    
    lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(v);
    // TODO: Potential null deref.
    //       Highly unlikely that array is a recognizable NULL though.
    //       If it is incorrect, it is most likely arbitrary.
    char *c_data = (char *)array->data;
    unsigned int i = 0;
    if (array->size >= 1 && c_data[0] != 0) { // nonzero length and ix 0 is not 0
      is_a_string = true;
      for (i = 0; i < array->size; i ++) {
	if (c_data[i] == 0) break;
	if (!isprint((unsigned char)c_data[i]) && ((c_data[i] < 8) || c_data[i] > 13)) {
	  is_a_string = false;
	  break;
	}
      }
    }
    if (i != array->size-1 && c_data[i-1] != 0) is_a_string = false;
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

static int print_emit_string(lbm_char_channel_t *chan, char* str) {
  if (str == NULL) return EMIT_FAILED;
  while (*str != 0) {
    int r = lbm_channel_write(chan, *str);
    str++;
    if (r != CHANNEL_SUCCESS) return EMIT_FAILED;
  }
  return EMIT_OK;
}

static int print_emit_char(lbm_char_channel_t *chan, char c) {

  int r = lbm_channel_write(chan, c);
  if (r != CHANNEL_SUCCESS) return EMIT_FAILED;
  return EMIT_OK;
}


static int emit_escape(lbm_char_channel_t *chan, char c) {
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

static int print_emit_string_value(lbm_char_channel_t *chan, char* str) {
  if (str == NULL) return EMIT_FAILED;
  while (*str != 0) {
    int r = emit_escape(chan, *str++);
    if (r != EMIT_OK) return r;
  }
  return EMIT_OK;
}

static int print_emit_symbol(lbm_char_channel_t *chan, lbm_value sym) {
  char *str_ptr = (char*)lbm_get_name_by_symbol(lbm_dec_sym(sym));
  return print_emit_string(chan, str_ptr);
}

static int print_emit_i(lbm_char_channel_t *chan, lbm_int v) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%"PRI_INT, v);
  return print_emit_string(chan, buf);
}

static int print_emit_u(lbm_char_channel_t *chan, lbm_uint v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%"PRI_UINT"%s", v, ps ? "u" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_byte(lbm_char_channel_t *chan, uint8_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%u%s", v, ps ? "b" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_float(lbm_char_channel_t *chan, float v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%"PRI_FLOAT"%s", (double)v, ps ? "f32" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_double(lbm_char_channel_t *chan, double v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf, EMIT_BUFFER_SIZE, "%lf%s", v, ps ? "f64" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_u32(lbm_char_channel_t *chan, uint32_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRIu32"%s", v, ps ? "u32" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_i32(lbm_char_channel_t *chan, int32_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRId32"%s", v, ps ? "i32" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_u64(lbm_char_channel_t *chan, uint64_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRIu64"%s", v, ps ? "u64" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_i64(lbm_char_channel_t *chan, int64_t v, bool ps) {
  char buf[EMIT_BUFFER_SIZE];
  snprintf(buf,EMIT_BUFFER_SIZE, "%"PRId64"%s", v, ps ? "i64" : "");
  return print_emit_string(chan, buf);
}

static int print_emit_continuation(lbm_char_channel_t *chan, lbm_value v) {
  char buf[EMIT_BUFFER_SIZE];
  lbm_uint cont = (v & ~LBM_CONTINUATION_INTERNAL) >> LBM_ADDRESS_SHIFT;
  snprintf(buf, EMIT_BUFFER_SIZE, "CONT[" "%"PRI_UINT"]", cont);
  return print_emit_string(chan, buf);
}

static int print_emit_custom(lbm_char_channel_t *chan, lbm_value v) {
  lbm_uint *custom = (lbm_uint*)lbm_car(v);
  int r; // NULL checks works against SYM_NIL. 
  if (custom && custom[CUSTOM_TYPE_DESCRIPTOR]) {
    r = print_emit_string(chan, (char*)custom[CUSTOM_TYPE_DESCRIPTOR]);
  } else {
    r = print_emit_string(chan, "INVALID_CUSTOM_TYPE");
  }
  return r;
}

static int print_emit_defrag_mem(lbm_char_channel_t *chan, lbm_value v) {
  (void) v;
  return print_emit_string(chan, "DM");
}

static int print_emit_channel(lbm_char_channel_t *chan, lbm_value v) {
  (void) v;
  return print_emit_string(chan, "~CHANNEL~");
}

static int print_emit_array_data(lbm_char_channel_t *chan, lbm_array_header_t *array) {

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

static int print_emit_bytearray(lbm_char_channel_t *chan, lbm_value v) {
  int r = 0;
  char *str;
  if (lbm_is_array_r(v)) {
    if (lbm_value_is_printable_string(v, &str)) {
      r = print_emit_char(chan, '"');
      if (r == EMIT_OK) {
        r = print_emit_string_value(chan, str);
        if (r == EMIT_OK) {
          r = print_emit_char(chan, '"');
        }
      }
    } else {
      lbm_array_header_t *array = (lbm_array_header_t*)lbm_car(v);
      r=  print_emit_array_data(chan, array);
    }
  } else {
    r = print_emit_string(chan, "[INVALID_ARRAY]");
  }
  return r;
}


static int lbm_print_internal(lbm_char_channel_t *chan, lbm_value v) {

  lbm_stack_clear(&print_stack);
  lbm_value start_print[2] = {v, PRINT};
  push_n(&print_stack, start_print, 2);
  bool chan_full = false;
  lbm_value curr;
  lbm_uint instr;
  int r = EMIT_FAILED;

  while (!lbm_stack_is_empty(&print_stack) && !chan_full) {
    lbm_pop(&print_stack, &instr);
    switch (instr) {
    case START_ARRAY: {
      lbm_pop(&print_stack, &curr);
      int res = 1;
      r = print_emit_char(chan, '[');
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(curr);
      lbm_uint size = arr->size / sizeof(lbm_value);
      lbm_value *arrdata = (lbm_value*)arr->data;
      if (size >= 1) {
        lbm_value continuation[5] =
          {1,  // next index
           (lbm_uint) arr,
           CONTINUE_ARRAY,
           arrdata[0], // first elt
           PRINT};
        res = res && push_n(&print_stack, continuation, 5);
      } else {
        res = res && lbm_push(&print_stack, END_LIST);
      }
      if (!res) {
        return EMIT_FAILED;
      }
      break;
    }
    case CONTINUE_ARRAY: {
      lbm_uint arr_ptr;
      lbm_array_header_t *arr;
      lbm_uint ix;
      int res = 1;
      lbm_pop_2(&print_stack, &arr_ptr, &ix);
      arr = (lbm_array_header_t *)arr_ptr;
      lbm_value *arrdata = (lbm_value*)arr->data;
      if (ix < (arr->size / sizeof(lbm_value))) {
        r = print_emit_char(chan, ' ');
        lbm_value continuation[5] =
          {ix + 1,
           (lbm_uint) arr,
           CONTINUE_ARRAY,
           arrdata[ix],
           PRINT};
        res = res && push_n(&print_stack, continuation, 5);
      } else {
        res = res && lbm_push(&print_stack, END_ARRAY);
      }
      if (!res) {
        return EMIT_FAILED;
      }
      break;
    }
    case END_ARRAY: {
      r = print_emit_char(chan, ']');
      break;
    }
    case START_LIST: {
      lbm_pop(&print_stack, &curr);
      r = print_emit_char(chan, '(');
      if (r != EMIT_OK) return r;
      lbm_value car_val = lbm_car(curr);
      lbm_value cdr_val = lbm_cdr(curr);
      int res = 1;
      if (lbm_type_of(cdr_val) == LBM_TYPE_CONS ||
          lbm_type_of(cdr_val) == (LBM_TYPE_CONS | LBM_PTR_TO_CONSTANT_BIT)) {
        lbm_value cont[2] = {cdr_val, CONTINUE_LIST};
        res = res && push_n(&print_stack, cont, 2);
      } else if (lbm_type_of(cdr_val) == LBM_TYPE_SYMBOL &&
                 cdr_val == ENC_SYM_NIL) {
        res = res && lbm_push(&print_stack, END_LIST);
      } else {
        lbm_value cont[4] = {END_LIST, cdr_val, PRINT, PRINT_DOT};
        res = res && push_n(&print_stack, cont, 4);
      }
      lbm_value cont[2] = {car_val, PRINT};
      res = res && push_n(&print_stack, cont,2);
      if (!res) {
        return EMIT_FAILED;
      }
      break;
    }
    case CONTINUE_LIST: {
      int res = 1;
      lbm_pop(&print_stack, &curr);

      if (lbm_type_of(curr) == LBM_TYPE_SYMBOL &&
          curr == ENC_SYM_NIL) {
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
        lbm_value cont[2] = {cdr_val, CONTINUE_LIST};
        res = res && push_n(&print_stack, cont, 2);
      } else if (lbm_type_of(cdr_val) == LBM_TYPE_SYMBOL &&
                  cdr_val == ENC_SYM_NIL) {
        res = res && lbm_push(&print_stack, END_LIST);
      } else {
        lbm_value cont[4] = {END_LIST, cdr_val, PRINT, PRINT_DOT};
        res = res && push_n(&print_stack, cont, 4);
      }
      lbm_value cont[2] = {car_val, PRINT};
      res = res && push_n(&print_stack, cont, 2);
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
      case LBM_TYPE_CONS: {
        lbm_value cont[2] = {curr, START_LIST};
        int res = push_n(&print_stack, cont, 2);
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
        r = print_emit_bytearray(chan, curr);
        break;
      case LBM_TYPE_DEFRAG_MEM:
	r = print_emit_defrag_mem(chan, curr);
	break;
      case LBM_TYPE_LISPARRAY: {
        lbm_value cont[2] = {curr, START_ARRAY};
        int res = push_n(&print_stack, cont, 2);
        if (!res) {
          print_emit_string(chan, " ...");
          return EMIT_OK;
        }
        break;
      }
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
