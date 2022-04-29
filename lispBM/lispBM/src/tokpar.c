/*
    Copyright 2019, 2021, 2022 Joel Svensson  svenssonjoel@yahoo.se

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

#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <lbm_memory.h>
#include <lbm_types.h>
#include <string.h>
#include <stdlib.h>

#include "tokpar.h"
#include "symrepr.h"
#include "heap.h"
#include "compression.h"
#include "qq_expand.h"
#include "env.h"

#define NOTOKEN         0u

#define TOKOPENPAR      1u      // "("
#define TOKCLOSEPAR     2u      // ")"
#define TOKQUOTE        3u      // "'"
#define TOKSYMBOL       4u      // "foo"
#define TOKINT          5u      // "42", "42i28"
#define TOKUINT         6u      // "42u28"
#define TOKBOXEDINT     7u      // "42i32"
#define TOKBOXEDUINT    8u      // "42u32"
#define TOKBOXEDFLOAT   9u      // "42.0"
#define TOKSTRING       10u     // "\"Hello\""
#define TOKCHAR         11u     // "\\#c"
#define TOKBACKQUOTE    12u     // "`"
#define TOKCOMMA        13u     // ","
#define TOKCOMMAAT      14u     // ",@"
#define TOKDOT          15u     // "."
#define TOKDONTCARE     16u     // "_"

#define TOKMATCHANY     17u
#define TOKMATCHI28     18u
#define TOKMATCHU28     19u
#define TOKMATCHU32     20u
#define TOKMATCHI32     21u
#define TOKMATCHFLOAT   22u
#define TOKMATCHCONS    23u

#define TOKOPENBRACK    30u     // "["
#define TOKCLOSEBRACK   31u     // "]"

#define TOKENIZER_ERROR 1024u
#define TOKENIZER_END   2048u

#define TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH 256

static char sym_str[TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH];

static void clear_sym_str(void) {
  memset(sym_str,0,TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH);
}

#define TOK_TYPE_NONE 0
#define TOK_TYPE_BYTE 1
#define TOK_TYPE_I    2
#define TOK_TYPE_U    3
#define TOK_TYPE_I32  4
#define TOK_TYPE_U32  5
#define TOK_TYPE_I64  6
#define TOK_TYPE_U64  7
#define TOK_TYPE_FLOAT  8
#define TOK_TYPE_DOUBLE 9

typedef struct {
  int type;
  uint64_t value;
  bool negative;
} token_int;

typedef struct token_float {
  int type;
  double value;
  bool negative;
} token_float;

typedef struct {
  const char *str;
  uint32_t  token;
  uint32_t len;
} matcher;

#define NUM_FIXED_SIZE_TOKENS 17
const matcher match_table[NUM_FIXED_SIZE_TOKENS] = {
  {"(", TOKOPENPAR, 1},
  {")", TOKCLOSEPAR, 1},
  {"[", TOKOPENBRACK, 1},
  {"]", TOKCLOSEBRACK, 1},
  {".", TOKDOT, 1},
  {"_", TOKDONTCARE, 1},
  {"'", TOKQUOTE, 1},
  {"`", TOKBACKQUOTE, 1},
  {",@", TOKCOMMAAT, 2},
  {",", TOKCOMMA, 1},
  {"?i", TOKMATCHI28, 2},
  {"?u", TOKMATCHU28, 2},
  {"?u32", TOKMATCHU32, 4},
  {"?i32", TOKMATCHI32, 4},
  {"?float", TOKMATCHFLOAT, 6},
  {"?cons", TOKMATCHCONS, 5},
  {"?", TOKMATCHANY, 1}
};

bool more(lbm_tokenizer_char_stream_t *str) {
  return str->more(str);
}

char get(lbm_tokenizer_char_stream_t *str) {
  return str->get(str);
}

char peek(lbm_tokenizer_char_stream_t *str, unsigned int n) {
  return str->peek(str,n);
}

void drop(lbm_tokenizer_char_stream_t *str, unsigned int n) {
  str->drop(str,n);
}


uint32_t tok_match_fixed_size_tokens(lbm_tokenizer_char_stream_t *str) {

  for (int i = 0; i < NUM_FIXED_SIZE_TOKENS; i ++) {
    uint32_t tok_len = match_table[i].len;
    const char *match_str = match_table[i].str;
    uint32_t tok = match_table[i].token;

    uint32_t char_pos;
    for (char_pos = 0; char_pos < tok_len; char_pos ++) {
      if (peek(str,char_pos) != match_str[char_pos]) break;
    }
    if (char_pos == tok_len) { //match
      drop(str,tok_len);
      return tok;
    }
  }
  return NOTOKEN;
}

bool symchar0(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/=<>#";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

bool symchar(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/=<>!?";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

int tok_symbol(lbm_tokenizer_char_stream_t *str) {

  if (!symchar0(peek(str,0)))  return 0;

  unsigned int i = 0;
  unsigned int len = 1;
  int n = 0;

  while (symchar((peek(str,len)))) {
    len++;
  }

  if (len > TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH)
    return -1; /* TODO: specific error code that can be presented to user */

  clear_sym_str();

  int c = 0;

  for (i = 0; i < len; i ++) {
    c = tolower(get(str));
    if (c >= 0 && c <= 255) {
      sym_str[i] = (char)c;
      n++;
    } else {
      return -1;
    }
  }
  return (int)n;
}

static char translate_escape_char(char c) {
  switch(c) {
  case '\\': return '\\';
  case 'n': return '\n';
  case 'r': return '\r';
  case 't': return '\t';
  case '\"': return '\"';
  default: return '\\';
  }
}

int tok_string(lbm_tokenizer_char_stream_t *str) {

  unsigned int i = 0;
  int n = 0;
  unsigned int len = 0;
  if (!(peek(str,0) == '\"')) return 0;

  get(str); // remove the " char
  n++;

  // compute length of string

  char c;
  do {
    c = peek(str,len);
    if (c == '\\') {
      len +=2;
    } else {
      len ++;
    }
  } while (c != 0 &&
           c != '\"');
  len = len -1;

  if (len > TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH)
    return -1; /* TODO: specific error code that can be presented to user */

  // str ends before tokenized string is closed.
  if ((peek(str,len)) != '\"') {
    return 0;
  }

  clear_sym_str();

  for (i = 0; i < len; i ++) {
    c = get(str);
    if (c == '\\') {
      if (i + 1 < len) {
        char escaped = get(str);
        c = translate_escape_char(escaped);
        len-=1;
      }
    }
    sym_str[i] = c;
    n++;
  }

  get(str);  // throw away the "
  return (int)(n+1);
}

int tok_char(lbm_tokenizer_char_stream_t *str, char *res) {

  int count = 0;
  if (peek(str,0) == '\\' &&
      peek(str,1) == '#' &&
      peek(str,2) == 'n' &&
      peek(str,3) == 'e' &&
      peek(str,4) == 'w' &&
      peek(str,5) == 'l' &&
      peek(str,6) == 'i' &&
      peek(str,7) == 'n' &&
      peek(str,8) == 'e') {
    *res = '\n';
    drop(str,9);
    count = 9;
  } else if (peek(str,0) == '\\' &&
             peek(str,1) == '#' &&
             isgraph(peek(str,2))) {
    *res = peek(str,2);
    drop(str,3);
    count = 3;
  }
  return count;
}

int tok_D(lbm_tokenizer_char_stream_t *str, token_float *result) {

  unsigned int n = 0;
  unsigned int m = 0;
  char fbuf[128];
  bool valid_num = false;

  result->type = TOK_TYPE_FLOAT;

  if (peek(str, 0) == '-') {
    n = 1;
    result->negative = true;
  }

  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  if ( peek(str,n) == '.') n++;
  else return 0;

  if ( !(peek(str,n) >= '0' && peek(str,n) <= '9')) return 0;
  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  unsigned int drop_extra = 0;
  if ((peek(str,n) == 'f' &&
       peek(str,n+1) == '6' &&
       peek(str,n+2) == '4')) {
    result->type = TOK_TYPE_DOUBLE;
    drop_extra = 3;
  }

  if ((result->negative && n > 1) ||
      (!result->negative && n > 0)) valid_num = true;

  if (n > 127) return 0;
  else m = n;
  if(valid_num) {
    unsigned int i;
    for (i = 0; i < m; i ++) {
      fbuf[i] = get(str);
    }

    drop(str,drop_extra);
    fbuf[i] = 0;
    result->value = (double)strtod(fbuf, NULL);
    return (int)n;
  }
  return 0;
}

void clean_whitespace(lbm_tokenizer_char_stream_t *str) {

  bool clean_whitespace = true;
  while ( clean_whitespace ){
    if ( peek(str,0) == ';' ) {
      while ( more(str) && peek(str, 0) != '\n') {
        drop(str,1);
      }
    } else if ( isspace(peek(str,0))) {
      drop(str,1);
    } else {
      clean_whitespace = false;
    }
  }
}

int tok_integer(lbm_tokenizer_char_stream_t *str, token_int *result ) {

  uint64_t acc = 0;
  unsigned int n = 0;
  bool valid_num = false;

  result->negative = false;
  if (peek(str, 0) == '-') {
    n = 1;
    result->negative = true;
  }

   // Check if hex notation is used
  if (peek(str,n) == '0' &&
      (peek(str,n+1) == 'x' || peek(str,n+1) == 'X')) {
    n+= 2;
    while ( (peek(str,n) >= '0' && peek(str,n) <= '9') ||
            (peek(str,n) >= 'a' && peek(str,n) <= 'f') ||
            (peek(str,n) >= 'A' && peek(str,n) <= 'F')){
      uint32_t val; /* values between 0 and 16 */
      if (peek(str,n) >= 'a' && peek(str,n) <= 'f') {
        val = 10 + (uint32_t)(peek(str,n) - 'a');
      } else if (peek(str,n) >= 'A' && peek(str,n) <= 'F') {
        val = 10 + (uint32_t)(peek(str,n) - 'A');
      } else {
        val = (uint32_t)peek(str,n) - '0';
      }
      acc = (acc * 0x10) + val;
      n++;
    }
  } else {
    while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
      acc = (acc*10) + (uint32_t)(peek(str,n) - '0');
      n++;
    }
  }

  unsigned int drop_type_str = 0;
  if (peek(str,n) == 'u' &&
      peek(str,n+1) == '6' &&
      peek(str,n+2) == '4') {
    drop_type_str = 3;
    result->type = TOK_TYPE_U64;
  } else if (peek(str,n) == 'i' &&
             peek(str,n+1) == '6' &&
             peek(str,n+2) == '4') {
    drop_type_str = 3;
    result->type = TOK_TYPE_I64;
  } else if (peek(str,n) == 'u' &&
             peek(str,n+1) == '3' &&
             peek(str,n+2) == '2') {
    drop_type_str = 3;
    result->type = TOK_TYPE_U32;
  } else if (peek(str,n) == 'i' &&
             peek(str,n+1) == '3' &&
             peek(str,n+2) == '2') {
    drop_type_str = 3;
    result->type = TOK_TYPE_I32;
  } else if (peek(str,n) == 'i') {
    drop_type_str = 1;
    result->type = TOK_TYPE_I;
  } else if (peek(str,n) == 'u') {
    drop_type_str = 1;
    result->type = TOK_TYPE_U;
  } else if (peek(str,n) == 'b') {
    drop_type_str = 1;
    result->type = TOK_TYPE_BYTE;
  } else {
    result->type = TOK_TYPE_I;
  }

  if ((result->negative && n > 1) ||
      (!result->negative && n > 0)) valid_num = true;

  if (valid_num) {
    drop(str,n + drop_type_str);
    result->value = acc;
    return (int)n; /*check that isnt so high that it becomes a negative number when casted */
  }
  return 0;
}


bool parse_array(lbm_tokenizer_char_stream_t *str, lbm_uint initial_size, lbm_value *res) {

  lbm_type t = LBM_TYPE_BYTE; // default

  int n = 0;
  clean_whitespace(str);
  if (!more(str)) {
    return false;
  }

  n = tok_symbol(str);

  if (n > 0) {
    if (strncmp(sym_str, "type-i32", (uint32_t)n) == 0) {
      t = LBM_TYPE_I32;
    } else if (strncmp(sym_str, "type-u32", (uint32_t)n) == 0) {
      t = LBM_TYPE_U32;
    } else if (strncmp(sym_str, "type-float", (uint32_t)n) == 0) {
      t = LBM_TYPE_FLOAT;
    } else if (strncmp(sym_str, "type-byte", (uint32_t)n) == 0) {
      t = LBM_TYPE_BYTE;
      initial_size = sizeof(lbm_uint) * initial_size;
    }
  } else {
    t = LBM_TYPE_BYTE;
    initial_size = sizeof(lbm_uint) * initial_size;
  }

  lbm_value array;
  if (!lbm_heap_allocate_array(&array, initial_size, t)) {
    return false;
  }
  lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(array);

  bool done = false;

  lbm_uint ix = 0;

  while (!done) {
    clean_whitespace(str);
    if (!more(str)) {
      lbm_heap_explicit_free_array(array);
      return false;
    }

    switch(tok_match_fixed_size_tokens(str)) {
    case TOKCLOSEBRACK:
      done = true;
      break;
    case NOTOKEN:
      break;
    default:
      lbm_heap_explicit_free_array(array);
      return false;
    }

    n = 0;
    //float f_val;

    token_int i_val;
    token_float f_val;

    if (!done) {
      switch (t) {
      case LBM_TYPE_BYTE:
        n = tok_integer(str, &i_val);
        if (n) ((uint8_t*)arr->data)[ix] = (uint8_t)(i_val.negative ? -i_val.value : i_val.value);
        break;
      case LBM_TYPE_I32:
        n = tok_integer(str, &i_val);
        if (n) arr->data[ix] = (uint32_t)(i_val.negative ? -i_val.value : i_val.value);
        break;
      case LBM_TYPE_U32:
        n = tok_integer(str, &i_val);
        if (n) arr->data[ix] = (uint32_t)(i_val.negative ? -i_val.value : i_val.value);
        break;
      case LBM_TYPE_FLOAT: {
        n = tok_D(str, &f_val);
        float f = (float)f_val.value;
        if (n) memcpy(&arr->data[ix], (uint32_t*)&f, sizeof(float));
      }break;
      }
      if (n == 0) {
        lbm_heap_explicit_free_array(array);
        return false;
      }
    }
    ix++;
  }

  lbm_uint array_size = ix - 1;

  // Calculate array size in number of words
  if (t == LBM_TYPE_BYTE) {
    if (array_size % 4) {
      array_size = (array_size / 4) + 1;
    } else {
      array_size = array_size / 4;
    }
  }

  lbm_memory_shrink((lbm_uint*)arr->data, array_size);
  arr->size = ix - 1;
  *res = array;
  return true;
}

lbm_value lbm_get_next_token(lbm_tokenizer_char_stream_t *str) {

  char c_val;
  int n = 0;

  if (!more(str)) {
    return lbm_enc_sym(SYM_TOKENIZER_DONE);
  }

  // Eat whitespace and comments.
  clean_whitespace(str);

  // Check for end of string again
  if (!more(str)) {
    return lbm_enc_sym(SYM_TOKENIZER_DONE);
  }

  lbm_value res = lbm_enc_sym(SYM_RERROR);
  uint32_t match;
  match = tok_match_fixed_size_tokens(str);
  if (match > 0) {
    switch (match) {
    case TOKOPENPAR:
      res = lbm_enc_sym(SYM_OPENPAR);
      break;
    case TOKCLOSEPAR:
      res = lbm_enc_sym(SYM_CLOSEPAR);
      break;
    case TOKDOT:
      res = lbm_enc_sym(SYM_DOT);
      break;
    case TOKDONTCARE:
      res = lbm_enc_sym(SYM_DONTCARE);
      break;
    case TOKQUOTE:
      res = lbm_enc_sym(SYM_QUOTE_IT);
      break;
    case TOKBACKQUOTE:
      res = lbm_enc_sym(SYM_BACKQUOTE);
      break;
    case TOKCOMMAAT:
      res = lbm_enc_sym(SYM_COMMAAT);
      break;
    case TOKCOMMA:
      res = lbm_enc_sym(SYM_COMMA);
      break;
    case TOKMATCHI28:
      res = lbm_enc_sym(SYM_MATCH_I);
      break;
    case TOKMATCHU28:
      res = lbm_enc_sym(SYM_MATCH_U);
      break;
    case TOKMATCHI32:
      res = lbm_enc_sym(SYM_MATCH_I32);
      break;
    case TOKMATCHU32:
      res = lbm_enc_sym(SYM_MATCH_U32);
      break;
    case TOKMATCHFLOAT:
      res = lbm_enc_sym(SYM_MATCH_FLOAT);
      break;
    case TOKMATCHCONS:
      res = lbm_enc_sym(SYM_MATCH_CONS);
      break;
    case TOKMATCHANY:
      res = lbm_enc_sym(SYM_MATCH_ANY);
      break;
    case TOKOPENBRACK: {
      lbm_uint num_free = lbm_memory_longest_free();
      lbm_uint initial_size = (lbm_uint)((float)num_free * 0.9);

      if (initial_size == 0) {
        res = lbm_enc_sym(SYM_MERROR);
        break;
      }

      lbm_value array;
      if (parse_array(str, initial_size, &array)) {
        res = array;
      } else {
        res = lbm_enc_sym(SYM_RERROR);
      }
    } break;
    case TOKCLOSEBRACK:
      res = lbm_enc_sym(SYM_RERROR); // a closing bracket without matching open.
    default:
      break;
    }
    return res;
  }

  n = tok_string(str);
  if (n >= 2) {
    // TODO: Proper error checking here!
    // TODO: Check if anything has to be allocated for the empty string
    lbm_heap_allocate_array(&res, (unsigned int)(n-2)+1, LBM_TYPE_CHAR);
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    char *data = (char *)arr->data;
    memset(data, 0, (unsigned int)((n-2)+1) * sizeof(char));
    memcpy(data, sym_str, (unsigned int)(n - 2) * sizeof(char));
    return res;
  } else if (n < 0) {
    // The string is too long error
    return res;
  }

  token_float f_val;

  if (tok_D(str, &f_val)) {
    switch (f_val.type) {
    case TOK_TYPE_FLOAT:
      return lbm_enc_float((float)f_val.value);
    case TOK_TYPE_DOUBLE:
      return lbm_enc_double(f_val.value);
    }
  }

  token_int int_result;

  if (tok_integer(str, &int_result)) {

    switch (int_result.type) {
    case TOK_TYPE_BYTE:
      return lbm_enc_char((char)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOK_TYPE_I:
      return lbm_enc_i((lbm_int)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOK_TYPE_U:
      return lbm_enc_u((lbm_uint)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOK_TYPE_I32:
      return lbm_enc_i32((lbm_int)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOK_TYPE_U32:
      return lbm_enc_u32((lbm_uint)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOK_TYPE_I64:
      return lbm_enc_i64((int64_t)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOK_TYPE_U64:
      return lbm_enc_u64((uint64_t)(int_result.negative ? -int_result.value : int_result.value));
      break;
    default:
      return lbm_enc_sym(SYM_RERROR);
      break;
    }
  }

  n = tok_symbol(str);
  if (n > 0) {

    lbm_uint symbol_id;

    if (lbm_get_symbol_by_name(sym_str, &symbol_id)) {
      res = lbm_enc_sym(symbol_id);
    }
    else {
      int r = 0;
      if (sym_str[0] == '#') {
        r = lbm_add_variable_symbol(sym_str, &symbol_id);
      } else {
        r = lbm_add_symbol(sym_str, &symbol_id);
      }
      if (r) {
        res = lbm_enc_sym(symbol_id);
      } else {
        res = lbm_enc_sym(SYM_RERROR);
      }
    }
    return res;
  } else if (n < 0) {
    // Symbol string is too long error
    return res;
  }

  if (tok_char(str, &c_val)) {
    return lbm_enc_char(c_val);
  }

  return res;
}

bool more_string(lbm_tokenizer_char_stream_t *str) {
  lbm_tokenizer_string_state_t *s =
    (lbm_tokenizer_string_state_t *)str->state;
  if ( s->pos > strlen(s->str)) {
    return false;
  } else {
    return s->str[s->pos] != 0;
  }
}

char get_string(lbm_tokenizer_char_stream_t *str) {
  lbm_tokenizer_string_state_t *s =
    (lbm_tokenizer_string_state_t *)str->state;
  char c = s->str[s->pos];
  s->pos = s->pos + 1;
  return c;
}

char peek_string(lbm_tokenizer_char_stream_t *str, unsigned int n) {
  lbm_tokenizer_string_state_t *s =
    (lbm_tokenizer_string_state_t *)str->state;
  // TODO error checking ?? how ?
  char c = s->str[s->pos + n];
  return c;
}

void drop_string(lbm_tokenizer_char_stream_t *str, unsigned int n) {
  lbm_tokenizer_string_state_t *s =
    (lbm_tokenizer_string_state_t *)str->state;
  s->pos = s->pos + n;
}

void lbm_create_char_stream_from_string(lbm_tokenizer_string_state_t *state,
                                        lbm_tokenizer_char_stream_t *char_stream,
                                        const char *string){
  state->str = string;
  state->pos = 0;

  char_stream->state = state;
  char_stream->more  = more_string;
  char_stream->peek  = peek_string;
  char_stream->drop  = drop_string;
  char_stream->get   = get_string;
}

/* VALUE tokpar_parse(tokenizer_char_stream_t *char_stream) { */

/*   return tokpar_parse_program(char_stream); */
/* } */
