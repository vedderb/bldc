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
//#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lbm_memory.h"
#include "lbm_types.h"
#include "lbm_channel.h"
#include "tokpar.h"
#include "symrepr.h"
#include "heap.h"
#include "qq_expand.h"
#include "env.h"

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
#define TOKMATCHU64     24u
#define TOKMATCHI64     25u
#define TOKMATCHDOUBLE  26u

#define TOKOPENBRACK    30u     // "["
#define TOKCLOSEBRACK   31u     // "]"

#define TOKTYPEBYTE     34u
#define TOKTYPEI        35u
#define TOKTYPEU        36u
#define TOKTYPEI32      37u
#define TOKTYPEU32      38u
#define TOKTYPEI64      39u
#define TOKTYPEU64      40u
#define TOKTYPEF32      41u
#define TOKTYPEF64      42u

#define TOKENIZER_ERROR 1024u
#define TOKENIZER_END   2048u

#define TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH 256


// Tokenizer return values
// > 0 : Successfully found token
// = 0 : Tokenizer can definitely not create a token
// = -1 : Tokenizer does not know if it can or cannot create a token yet.
// = -2 : Tokenizer was reading a string but ran out of space (for example).
//        This is an error!

#define TOKENIZER_NO_TOKEN   0
#define TOKENIZER_NEED_MORE -1
#define TOKENIZER_STRING_ERROR -2
#define TOKENIZER_IMPOSSIBLE -3

static char sym_str[TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH];

static void clear_sym_str(void) {
  memset(sym_str,0,TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH);
}

typedef struct {
  uint32_t type;
  uint64_t value;
  bool negative;
} token_int;

typedef struct token_float {
  uint32_t type;
  double value;
  bool negative;
} token_float;

typedef struct {
  const char *str;
  uint32_t token;
  uint32_t len;
} matcher;

#define NUM_FIXED_SIZE_TOKENS 21
const matcher fixed_size_tokens[NUM_FIXED_SIZE_TOKENS] = {
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
  {"?", TOKMATCHANY, 1}
};

#define NUM_TYPE_QUALIFIERS 9
const matcher type_qual_table[NUM_TYPE_QUALIFIERS] = {
  {"f64", TOKTYPEF64,  3},
  {"f32", TOKTYPEF32,  3},
  {"i64", TOKTYPEI64,  3},
  {"u64", TOKTYPEU64,  3},
  {"i32", TOKTYPEI32,  3},
  {"u32", TOKTYPEU32,  3},
  {"i"  , TOKTYPEI,    1},
  {"u"  , TOKTYPEU,    1},
  {"b"  , TOKTYPEBYTE, 1}
};

int tok_match_fixed_size_tokens(lbm_char_channel_t *chan, const matcher *m, unsigned int start_pos, unsigned int num, uint32_t *res) {

  for (unsigned int i = 0; i < num; i ++) {
    uint32_t tok_len = m[i].len;
    const char *match_str = m[i].str;
    uint32_t tok = m[i].token;
    char c;
    int char_pos;
    int r;
    for (char_pos = 0; char_pos < (int)tok_len; char_pos ++) {
      r = lbm_channel_peek(chan,(unsigned int)char_pos + start_pos, &c);
      if (r == CHANNEL_SUCCESS) {
        if (c != match_str[char_pos]) break;
      } else if (r == CHANNEL_MORE ) {
        return TOKENIZER_NEED_MORE;
      } else {
        break;
      }
    }

    if (char_pos == (int)tok_len) { //match
      *res = tok;
      return (int)tok_len;
    }
  }
  return TOKENIZER_NO_TOKEN;
}

bool symchar0(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/=<>#!";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

bool symchar(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/=<>!?_";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

int tok_symbol(lbm_char_channel_t *chan) {

  char c;
  int r = 0;

  r = lbm_channel_peek(chan, 0, &c);
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  if (r == CHANNEL_END)  return TOKENIZER_NO_TOKEN;
  if (r == CHANNEL_SUCCESS && !symchar0(c)) {
    return TOKENIZER_IMPOSSIBLE;
  }
  clear_sym_str();
  sym_str[0] = (char)tolower(c);

  int len = 1;

  r = lbm_channel_peek(chan,(unsigned int)len, &c);
  while (r == CHANNEL_SUCCESS && symchar(c)) {
    c = (char)tolower(c);
    if (len < TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH) {
      sym_str[len] = (char)c;
    }
    len ++;
    r = lbm_channel_peek(chan,(unsigned int)len, &c);
  }
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  return len;
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

int tok_string(lbm_char_channel_t *chan, unsigned int *string_len) {

  unsigned int n = 0;
  unsigned int len = 0;
  char c;
  int r = 0;
  bool encode = false;

  r = lbm_channel_peek(chan,0,&c);
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  else if (r == CHANNEL_END) return TOKENIZER_NO_TOKEN;

  if (c != '\"') return TOKENIZER_NO_TOKEN;;

  n++;

  memset(sym_str, 0 , TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH);

  // read string into buffer
  r = lbm_channel_peek(chan,n,&c);
  while (r == CHANNEL_SUCCESS && (c != '\"' || (c == '\"' && encode)) &&
         len < TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH) {
    if (c == '\\') encode = true;
    else {
      sym_str[len] = encode ? translate_escape_char(c) : c ;
      len++;
      encode = false;
    }
    n ++;
    r = lbm_channel_peek(chan, n, &c);
  }

  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  if (c != '\"') return TOKENIZER_STRING_ERROR;

  *string_len = len;
  n ++;
  return (int)n;
}

int tok_char(lbm_char_channel_t *chan, char *res) {

  char c;
  int r;

  r = lbm_channel_peek(chan, 0, &c);
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  if (r == CHANNEL_END)  return TOKENIZER_NO_TOKEN;

  if (c != '\\') return TOKENIZER_IMPOSSIBLE;

  r = lbm_channel_peek(chan, 1, &c);
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  if (r == CHANNEL_END)  return TOKENIZER_NO_TOKEN;

  if (c != '#') return TOKENIZER_IMPOSSIBLE;

  r = lbm_channel_peek(chan, 2, &c);
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  if (r == CHANNEL_END)  return TOKENIZER_NO_TOKEN;

  *res = c;
  return 3;
}

int tok_D(lbm_char_channel_t *chan, token_float *result) {

  unsigned int n = 0;
  char fbuf[128];
  char c;
  bool valid_num = false;
  int res;

  memset(fbuf, 0, 128);

  result->type = TOKTYPEF32;
  result->negative = false;

  res = lbm_channel_peek(chan, 0, &c);
  if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
  if (c == '-') {
    n = 1;
    fbuf[0] = '-';
    result->negative = true;
  }

  res = lbm_channel_peek(chan, n, &c);
  if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
  while (c >= '0' && c <= '9') {
    fbuf[n] = c;
    n++;
    res = lbm_channel_peek(chan, n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    if (res == CHANNEL_END) break;
  }

  if (c == '.') {
    fbuf[n] = c;
    n ++;
  }

  else return TOKENIZER_NO_TOKEN;

  res = lbm_channel_peek(chan,n, &c);
  if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
  if (!(c >= '0' && c <= '9')) return TOKENIZER_NO_TOKEN;

  while (c >= '0' && c <= '9') {
    fbuf[n] = c;
    n++;
    res = lbm_channel_peek(chan, n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    if (res == CHANNEL_END) break;
  }

  if (c == 'e') {
    fbuf[n] = c;
    n++;
    res = lbm_channel_peek(chan,n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
    if (!((c >= '0' && c <= '9') || c == '-')) return TOKENIZER_NO_TOKEN;

    while ((c >= '0' && c <= '9') || c == '-') {
      fbuf[n] = c;
      n++;
      res = lbm_channel_peek(chan, n, &c);
      if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
      if (res == CHANNEL_END) break;
    }
  }

  uint32_t tok_res;
  int type_len = tok_match_fixed_size_tokens(chan, type_qual_table, n, NUM_TYPE_QUALIFIERS, &tok_res);

  if (type_len == TOKENIZER_NEED_MORE) return type_len;
  if (type_len == TOKENIZER_NO_TOKEN) {
    result->type = TOKTYPEF32;
  } else {
    result->type = tok_res;
  }

  if ((result->negative && n > 1) ||
      (!result->negative && n > 0)) valid_num = true;

  if (n > 127) {
    return 0;
  }

  if(valid_num) {
    result->value = (double)strtod(fbuf,NULL);
    return (int)n + type_len;
  }
  return 0;
}

bool clean_whitespace(lbm_char_channel_t *chan) {

  bool cleaning_whitespace = true;
  char c;
  int r;

  while (cleaning_whitespace) {

    if (lbm_channel_comment(chan)) {
      while (true) {
        r = lbm_channel_peek(chan, 0, &c);
        if (r == CHANNEL_END) {
          lbm_channel_set_comment(chan, false);
          cleaning_whitespace = false;
          break;
        }
        if (r == CHANNEL_MORE) {
          return false;
        }
        lbm_channel_drop(chan,1);
        if (c == '\n') {
          lbm_channel_set_comment(chan, false);
          break;
        }
      }
    }

    do {
      r = lbm_channel_peek(chan, 0, &c);
      if (r == CHANNEL_MORE) {
        return false;
      } else if (r == CHANNEL_END) {
        return true;
      }
      if (c == ';') {
        lbm_channel_set_comment(chan, true);
        break;
      }
      if (isspace(c)) {
        lbm_channel_drop(chan,1);
      } else {
        cleaning_whitespace = false;
      }

    } while (cleaning_whitespace);
  }
  return true;
}

int tok_integer(lbm_char_channel_t *chan, token_int *result ) {
  uint64_t acc = 0;
  unsigned int n = 0;
  bool valid_num = false;
  char c;
  int res;

  result->type = TOKTYPEI;
  result-> negative = false;
  res = lbm_channel_peek(chan, 0, &c);
  if (res == CHANNEL_MORE) {
    return TOKENIZER_NEED_MORE;
  } else if (res == CHANNEL_END) {
    return 0;
  }
  if (c == '-') {
    n = 1;
    result->negative = true;
  }

  bool hex = false;
  res = lbm_channel_peek(chan, n, &c);
  if (res == CHANNEL_SUCCESS && c == '0') {
    res = lbm_channel_peek(chan, n + 1, &c);
    if ( res == CHANNEL_SUCCESS && (c == 'x' || c == 'X')) {
      hex = true;
    } else if (res == CHANNEL_MORE) {
      return TOKENIZER_NEED_MORE;
    }
  } else if (res == CHANNEL_MORE) {
    return TOKENIZER_NEED_MORE;
  }

  if (hex) {
    n += 2;

    res = lbm_channel_peek(chan,n, &c);

    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    else if (res == CHANNEL_END) return 0;

    while ((c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F')) {
      uint32_t val; /* values between 0 and 16 */
      if (c >= 'a' && c <= 'f') {
        val = 10 + (uint32_t)c - 'a';
      } else if (c >= 'A' && c <= 'F') {
        val = 10 + (uint32_t)(c - 'A');
      } else {
        val = (uint32_t)c - '0';
      }
      acc = (acc * 0x10) + val;
      n++;
      res = lbm_channel_peek(chan, n, &c);
      if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
      if (res == CHANNEL_END) break;

    }
  } else {
    res = lbm_channel_peek(chan, n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    while (c >= '0' && c <= '9') {
      acc = (acc*10) + (uint32_t)(c - '0');
      n++;
      res = lbm_channel_peek(chan, n, &c);
      if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
      if (res == CHANNEL_END)  break;
    }
  }

  if (n == 0) return 0;

  uint32_t tok_res;
  int type_len = tok_match_fixed_size_tokens(chan, type_qual_table, n, NUM_TYPE_QUALIFIERS, &tok_res);

  if (type_len == TOKENIZER_NEED_MORE) return type_len;
  if (type_len != TOKENIZER_NO_TOKEN) {
    result->type = tok_res;
  }

  if ((result->negative && n > 1) ||
      (!result->negative && n > 0)) valid_num = true;

  if (valid_num) {
    //lbm_channel_drop(chan,n + drop_type_str);
    result->value = acc;
    return (int)n + type_len;
  }
  return 0;
}

lbm_value lbm_get_next_token(lbm_char_channel_t *chan, bool peek) {

  char c_val;
  int n = 0;

  if (!lbm_channel_more(chan) && lbm_channel_is_empty(chan)) {
    return lbm_enc_sym(SYM_TOKENIZER_DONE);
  }

  // Eat whitespace and comments.
  if (!clean_whitespace(chan)) {
    return lbm_enc_sym(SYM_TOKENIZER_WAIT);
  }

  // Check for end of string again
  if (!lbm_channel_more(chan) && lbm_channel_is_empty(chan)) {
    return lbm_enc_sym(SYM_TOKENIZER_DONE);
  }

  lbm_value res = lbm_enc_sym(SYM_TOKENIZER_RERROR);
  uint32_t match;
  n = tok_match_fixed_size_tokens(chan,
                                  fixed_size_tokens,
                                  0,
                                  NUM_FIXED_SIZE_TOKENS,
                                  &match);
  if (n > 0) {

    if (!peek) {
      if (!lbm_channel_drop(chan, (unsigned int)n)) {
        // Really should not happen (bug in channel implementation)
      }
    }
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
    case TOKMATCHANY:
      res = lbm_enc_sym(SYM_MATCH_ANY);
      break;
    case TOKOPENBRACK:
      res = lbm_enc_sym(SYM_OPENBRACK);
      break;
    case TOKCLOSEBRACK:
      res = lbm_enc_sym(SYM_CLOSEBRACK);
      break;
      //res = lbm_enc_sym(SYM_RERROR); // a closing bracket without matching open.
    default:
      break;
    }
    return res;
  } else if (n == TOKENIZER_NEED_MORE) {
    return lbm_enc_sym(SYM_TOKENIZER_WAIT);
  }

  unsigned int string_len = 0;
  n = tok_string(chan, &string_len);
  if (n >= 2) {
    if (!peek) lbm_channel_drop(chan, (unsigned int)n);
    // TODO: Proper error checking here!
    // TODO: Check if anything has to be allocated for the empty string
    if (!lbm_heap_allocate_array(&res, (unsigned int)(string_len+1), LBM_TYPE_CHAR)) {
      // Should really be a tokenizer memory error.
      // GC should run and tokenizer be retried.
      // Needs some thinking on how to do that.
      return lbm_enc_sym(TOKENIZER_ERROR);
    }
    lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(res);
    char *data = (char *)arr->data;
    memset(data, 0, (string_len+1) * sizeof(char));
    memcpy(data, sym_str, string_len * sizeof(char));
    return res;
  } else if (n == TOKENIZER_NEED_MORE) {
    return lbm_enc_sym(SYM_TOKENIZER_WAIT);
  } else if (n == TOKENIZER_STRING_ERROR) {
    return ENC_SYM_TOKENIZER_RERROR;
  }

  token_float f_val;

  n = tok_D(chan, &f_val);
  if (n > 0) {
    if (!peek) lbm_channel_drop(chan, (unsigned int)n);
    switch (f_val.type) {
    case TOKTYPEF32:
      return lbm_enc_float((float)f_val.value);
    case TOKTYPEF64:
      return lbm_enc_double(f_val.value);
    }
  } else if ( n == TOKENIZER_NEED_MORE) {
    return lbm_enc_sym(SYM_TOKENIZER_WAIT);
  }

  token_int int_result;

  n = tok_integer(chan, &int_result);
  if (n > 0) {
    if (!peek) lbm_channel_drop(chan, (unsigned int)n);

    switch (int_result.type) {
    case TOKTYPEBYTE:
      return lbm_enc_char((char)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEI:
      return lbm_enc_i((lbm_int)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEU:
      return lbm_enc_u((lbm_uint)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEI32:
      return lbm_enc_i32((lbm_int)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEU32:
      return lbm_enc_u32((lbm_uint)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEI64:
      return lbm_enc_i64((int64_t)(int_result.negative ? -int_result.value : int_result.value));
      break;
    case TOKTYPEU64:
      return lbm_enc_u64((uint64_t)(int_result.negative ? -int_result.value : int_result.value));
      break;
    default:
      return ENC_SYM_TOKENIZER_RERROR;
      break;
    }
  } else if (n == TOKENIZER_NEED_MORE) {
    return lbm_enc_sym(SYM_TOKENIZER_WAIT);
  }

  n = tok_symbol(chan);
  if (n > 0) {

    if (!peek) lbm_channel_drop(chan,(unsigned int)n);

    lbm_uint symbol_id;

    if (lbm_get_symbol_by_name(sym_str, &symbol_id)) {
      res = lbm_enc_sym(symbol_id);
    }
    else {
      int r = 0;
      if (strncmp(sym_str,"ext-",4) == 0) {
        r = lbm_add_extension_symbol(sym_str, &symbol_id);
      } else if (sym_str[0] == '#') {
        r = lbm_add_variable_symbol(sym_str, &symbol_id);
      } else {
        r = lbm_add_symbol(sym_str, &symbol_id);
      }
      if (r) {
        res = lbm_enc_sym(symbol_id);
      } else {
        res = ENC_SYM_TOKENIZER_RERROR;
      }
    }
    return res;
  } else if (n == TOKENIZER_NEED_MORE) {
    return lbm_enc_sym(SYM_TOKENIZER_WAIT);
  }

  n = tok_char(chan, &c_val);
  if (n > 0) {
    if (!peek) lbm_channel_drop(chan,(unsigned int)n);
    return lbm_enc_char(c_val);
  } else if (n == TOKENIZER_NEED_MORE) {
    return lbm_enc_sym(SYM_TOKENIZER_WAIT);
  }

  return ENC_SYM_TOKENIZER_RERROR;
}

