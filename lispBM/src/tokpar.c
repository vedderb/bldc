/*
    Copyright 2019, 2021 Joel Svensson  svenssonjoel@yahoo.se

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
#include <string.h>
#include <stdlib.h>

#include "tokpar.h"
#include "symrepr.h"
#include "heap.h"
#include "lispbm_types.h"
#include "compression.h"
#include "qq_expand.h"
#include "lispbm_memory.h"
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
#define TOKMATCHFLOAT   20u
#define TOKMATCHCONS    21u

#define TOKENIZER_ERROR 1024u
#define TOKENIZER_END   2048u

#define TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH 256

static char sym_str[TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH];

static void clear_sym_str(void) {
  memset(sym_str,0,TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH);
}

typedef struct {

  unsigned int type;

  unsigned int text_len;
  union {
    char  c;
    char  *text;
    lbm_int   i;
    lbm_uint  u;
    lbm_float f;
  }data;
} token;

typedef struct {
  const char *str;
  uint32_t  token;
  uint32_t len;
} matcher;

#define NUM_FIXED_SIZE_TOKENS 13
const matcher match_table[NUM_FIXED_SIZE_TOKENS] = {
  {"(", TOKOPENPAR, 1},
  {")", TOKCLOSEPAR, 1},
  {".", TOKDOT, 1},
  {"_", TOKDONTCARE, 1},
  {"'", TOKQUOTE, 1},
  {"`", TOKBACKQUOTE, 1},
  {",@", TOKCOMMAAT, 2},
  {",", TOKCOMMA, 1},
  {"?i28", TOKMATCHI28, 4},
  {"?u28", TOKMATCHU28, 4},
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
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/=<>";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

bool symchar(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/=<>";

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

int tok_string(lbm_tokenizer_char_stream_t *str) {

  unsigned int i = 0;
  int n = 0;
  unsigned int len = 0;
  if (!(peek(str,0) == '\"')) return 0;

  get(str); // remove the " char
  n++;

  // compute length of string
  while (peek(str,len) != 0 &&
         peek(str,len) != '\"') {
    len++;
  }

  if (len > TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH)
    return -1; /* TODO: specific error code that can be presented to user */

  // str ends before tokenized string is closed.
  if ((peek(str,len)) != '\"') {
    return 0;
  }

  clear_sym_str();

  for (i = 0; i < len; i ++) {
    sym_str[i] = get(str);
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

int tok_i(lbm_tokenizer_char_stream_t *str, lbm_int *res) {

  lbm_int acc = 0;
  unsigned int n = 0;
  bool negative = false;
  bool valid_num = false;

  if (peek(str, 0) == '-') {
    n = 1;
    negative = true;
  }

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (peek(str,n) - '0');
    n++;
  }
  if ((negative && n > 1) ||
      (!negative && n > 0)) valid_num = true;

  // Not needed if strict adherence to ordering of calls to tokenizers.
  if (peek(str,n) == 'U' ||
      peek(str,n) == 'u' ||
      peek(str,n) == '.' ||
      peek(str,n) == 'I') return 0;

  if (valid_num) {
    drop(str,n);
    *res = negative ? -acc : acc;
    return (int)n; /*check that isnt so high that it becomes a negative number when casted */
  }
  return 0;
}

int tok_I(lbm_tokenizer_char_stream_t *str, lbm_int *res) {
  lbm_int acc = 0;
  unsigned int n = 0;
  bool negative = false;
  bool valid_num = false;

  if (peek(str, 0) == '-') {
    n = 1;
    negative = true;
  }

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (peek(str,n) - '0');
    n++;
  }
  if ((negative && n > 1) ||
      (!negative && n > 0)) valid_num = true;

  if (peek(str,n) == 'i' &&
      peek(str,n+1) == '3' &&
      peek(str,n+2) == '2' &&
      valid_num) {
    *res = negative ? -acc : acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_u(lbm_tokenizer_char_stream_t *str, lbm_uint *res) {
  lbm_uint acc = 0;
  unsigned int n = 0;
  bool negative = false;
  bool valid_num = false;

  if (peek(str, 0) == '-') {
    n = 1;
    negative = true;
  }

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (lbm_uint)(peek(str,n) - '0');
    n++;
  }
  if ((negative && n > 1) ||
      (!negative && n > 0)) valid_num = true;

  if (peek(str,n) == 'u' &&
      peek(str,n+1) == '2' &&
      peek(str,n+2) == '8' &&
      valid_num) {
    *res = negative ? -acc : acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_U(lbm_tokenizer_char_stream_t *str, lbm_uint *res) {
  lbm_uint acc = 0;
  unsigned int n = 0;
  bool negative = false;
  bool valid_num = false;

  if (peek(str, 0) == '-') {
    n = 1;
    negative = true;
  }

  // Check if hex notation is used
  if (peek(str,0) == '0' &&
      (peek(str,1) == 'x' || peek(str,1) == 'X')) {
    n+= 2;
    while ( (peek(str,n) >= '0' && peek(str,n) <= '9') ||
            (peek(str,n) >= 'a' && peek(str,n) <= 'f') ||
            (peek(str,n) >= 'A' && peek(str,n) <= 'F')){
      lbm_uint val;
      if (peek(str,n) >= 'a' && peek(str,n) <= 'f') {
        val = 10 + (lbm_uint)(peek(str,n) - 'a');
      } else if (peek(str,n) >= 'A' && peek(str,n) <= 'F') {
        val = 10 + (lbm_uint)(peek(str,n) - 'A');
      } else {
        val = (lbm_uint)peek(str,n) - '0';
      }
      acc = (acc * 0x10) + val;
      n++;
    }
    if ((negative && n > 1) ||
        (!negative && n > 0)) valid_num = true;

    if (valid_num) {
      drop(str,n);
      *res = negative ? -acc : acc;
      return (int)n; /*check that isnt so high that it becomes a negative number when casted */
    }
  }

  // check if nonhex
  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (lbm_uint)(peek(str,n) - '0');
    n++;
  }
  if ((negative && n > 1) ||
      (!negative && n > 0)) valid_num = true;

  if (peek(str,n) == 'u' &&
      peek(str,n+1) == '3' &&
      peek(str,n+2) == '2' &&
      valid_num) {
    *res = negative ? -acc : acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_F(lbm_tokenizer_char_stream_t *str, lbm_float *res) {

  unsigned int n = 0;
  unsigned int m = 0;
  char fbuf[128];
  bool negative = false;
  bool valid_num = false;

  if (peek(str, 0) == '-') {
    n = 1;
    negative = true;
  }

  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  if ( peek(str,n) == '.') n++;
  else return 0;

  if ( !(peek(str,n) >= '0' && peek(str,n) <= '9')) return 0;
  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  if ((negative && n > 1) ||
      (!negative && n > 0)) valid_num = true;

  if (n > 127) m = 127;
  else m = n;
  if(valid_num) {
    unsigned int i;
    for (i = 0; i < m; i ++) {
      fbuf[i] = get(str);
    }
    
    fbuf[i] = 0;
    *res = (float)strtod(fbuf, NULL);
    return (int)n;
  }
  return 0;
}


lbm_value lbm_get_next_token(lbm_tokenizer_char_stream_t *str) {

  lbm_int i_val;
  lbm_uint u_val;
  char c_val;
  lbm_float f_val;
  int n = 0;

  if (!more(str)) {
    return lbm_enc_sym(SYM_TOKENIZER_DONE);
  }

  // Eat whitespace and comments.
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
      res = lbm_enc_sym(SYM_QUOTE);
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
      res = lbm_enc_sym(SYM_MATCH_I28);
      break;
    case TOKMATCHU28:
      res = lbm_enc_sym(SYM_MATCH_U28);
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
    default:
      break;
    }
    return res;
  }

  n = tok_string(str);
  if (n >= 2) {
    // TODO: Proper error checking here!
    lbm_heap_allocate_array(&res, (unsigned int)(n-2)+1, LBM_VAL_TYPE_CHAR);
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    char *data = (char *)arr + 8;
    memset(data, 0, (unsigned int)((n-2)+1) * sizeof(char));
    memcpy(data, sym_str, (unsigned int)(n - 2) * sizeof(char));
    return res;
  } else if (n < 0) {
    // The string is too long error
    return res;
  }

  if (tok_F(str, &f_val)) {
    // Will be SYM_MERROR in case of full heap
    return lbm_enc_F(f_val);
  }

  if (tok_U(str, &u_val)) {
    // Will be SYM_MERROR in case of full heap
    return lbm_enc_U(u_val);
  }

  if (tok_u(str, &u_val)) {
    return lbm_enc_u(u_val);
  }

  if (tok_I(str, &i_val)) {
    return lbm_enc_I(i_val);
  }

  // Shortest form of integer match. Move to last in chain of numerical tokens.
  if (tok_i(str, &i_val)) {
    return lbm_enc_i(i_val);
  }

  n = tok_symbol(str);
  if (n > 0) {

    lbm_uint symbol_id;

    if (lbm_get_symbol_by_name(sym_str, &symbol_id)) {
      res = lbm_enc_sym(symbol_id);
    }
    else if (lbm_add_symbol(sym_str, &symbol_id)) {
      res = lbm_enc_sym(symbol_id);
    } else {
      res = lbm_enc_sym(SYM_RERROR);
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
  return s->str[s->pos] != 0;
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
                                           char *string){
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

