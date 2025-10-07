/*
    Copyright 2019, 2021, 2022, 2025 Joel Svensson  svenssonjoel@yahoo.se

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
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lbm_memory.h"
#include "lbm_types.h"
#include "lbm_channel.h"
#include "tokpar.h"
#include "symrepr.h"
#include "heap.h"
#include "env.h"

// +1 to ensure there is always a zero at last ix
char tokpar_sym_str[TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH+1];

typedef struct {
  const char *str;
  uint16_t token;
  uint16_t len;
} matcher;

/*
  \#\0 -> 0                 ; NUL
  \#\a -> 7                 ; bell character, BEL
  \#\b -> 8                 ; backspace, BS
  \#\t -> 9                 ; tab, TAB
  \#\n -> 10                ; newline
  \#\v -> 11                ; vertical tab
  \#\f -> 12                ; formfeed character
  \#\r -> 13                ; carriage return, RET
  \#\e -> 27                ; escape character, ESC
  \#\s -> 32                ; space character, SPC
  \#\" -> 34                ; double quote
  \#\\ -> 92                ; backslash character, \
  \#\d -> 127               ; delete character, DEL
*/

static inline signed char translate_escape_char(char c) {
  switch (c) {
    case '0': return '\0';
    case 'a': return '\a';
    case 'b': return '\b';
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
    case 'e': return 27;
    case 's': return 32;
    case '"': return '\"';
    case '\\': return '\\';
    case 'd': return 127;
    default: return -1;
  }
}

#define NUM_FIXED_SIZE_TOKENS 18
const matcher fixed_size_tokens[NUM_FIXED_SIZE_TOKENS] = {
  {"(", TOKOPENPAR, 1},
  {")", TOKCLOSEPAR, 1},
  {"[|", TOKOPENARRAY, 2},
  {"[", TOKOPENBRACK, 1},
  {"]", TOKCLOSEBRACK, 1},
  {".", TOKDOT, 1},
  {"_", TOKDONTCARE, 1},
  {"'", TOKQUOTE, 1},
  {"`", TOKBACKQUOTE, 1},
  {",@", TOKCOMMAAT, 2},
  {",", TOKCOMMA, 1},
  {"?", TOKMATCHANY, 1},
  {"{", TOKOPENCURL, 1},
  {"}", TOKCLOSECURL, 1},
  {"|]", TOKCLOSEARRAY, 2},
  {"@const-start", TOKCONSTSTART, 12},
  {"@const-end", TOKCONSTEND, 10},
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

static int tok_match_fixed_size_tokens(lbm_char_channel_t *chan, const matcher *m, unsigned int start_pos, unsigned int num, uint32_t *res) {

  for (unsigned int i = 0; i < num; i ++) {
    uint32_t tok_len = m[i].len;
    const char *match_str = m[i].str;
    char c;
    int char_pos;
    for (char_pos = 0; char_pos < (int)tok_len; char_pos ++) {
      int r = lbm_channel_peek(chan,(unsigned int)char_pos + start_pos, &c);
      if (r == CHANNEL_SUCCESS) {
        if (c != match_str[char_pos]) break;
      } else if (r == CHANNEL_MORE ) {
        return TOKENIZER_NEED_MORE;
      } else {
        break;
      }
    }

    if (char_pos == (int)tok_len) { //match
      *res = m[i].token;
      return (int)tok_len;
    }
  }
  return TOKENIZER_NO_TOKEN;
}

int tok_syntax(lbm_char_channel_t *chan, uint32_t *res) {
  return tok_match_fixed_size_tokens(chan, fixed_size_tokens, 0, NUM_FIXED_SIZE_TOKENS, res);
}

static bool alpha_char(char c) {
  return ((c >= 'a' && c <= 'z') ||
          (c >= 'A' && c <= 'Z'));
}

static bool num_char(char c) {
  return (c >= '0' && c <= '9');
}

static bool symchar0(char c) {
  const char *allowed = "+-*/=<>#!";

  if (alpha_char(c)) return true;
  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i]) return true;
    i ++;
  }
  return false;
}

static bool symchar(char c) {
  const char *allowed = "+-*/=<>!?_";

  if (alpha_char(c) || num_char(c)) return true;
  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i]) return true;
    i++;
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
    return TOKENIZER_NO_TOKEN;
  }
  memset(tokpar_sym_str,0,TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH+1);
  tokpar_sym_str[0] = (c >= 'A' && c <= 'Z') ? c + 32 : c; // locale independent ASCII only tolower.

  int len = 1;

  r = lbm_channel_peek(chan,(unsigned int)len, &c);
  while (r == CHANNEL_SUCCESS && symchar(c)) {
    c = (c >= 'A' && c <= 'Z') ? c + 32 : c; // locale independent ASCII only tolower.
    if (len < TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH) {
      tokpar_sym_str[len] = (char)c;
    } else {
      return TOKENIZER_SYMBOL_ERROR;
    }
    len ++;
    r = lbm_channel_peek(chan,(unsigned int)len, &c);
  }
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  tokpar_sym_str[len] = 0;
  return len;
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

  memset(tokpar_sym_str,0,TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH+1);

  // read string into buffer
  r = lbm_channel_peek(chan,n,&c);
  while (r == CHANNEL_SUCCESS && (c != '\"' || encode) &&
	 len < TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH) {
    if (c == '\\' && !encode) {
      encode = true;
    } else {
      if (encode) {
        signed char result = translate_escape_char(c);
        if (result == -1) {
          return TOKENIZER_STRING_ERROR;
        } else {
          tokpar_sym_str[len] = result; 
        }
      } else {
        tokpar_sym_str[len] = c;
      }
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

  if (c != '\\') return TOKENIZER_NO_TOKEN;

  r = lbm_channel_peek(chan, 1, &c);
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  if (r == CHANNEL_END)  return TOKENIZER_NO_TOKEN;

  if (c != '#') return TOKENIZER_NO_TOKEN;

  r = lbm_channel_peek(chan, 2, &c);
  if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  if (r == CHANNEL_END)  return TOKENIZER_NO_TOKEN;

  if (c == '\\') {
    r = lbm_channel_peek(chan, 3, &c);
    if (r == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    if (r == CHANNEL_END)  return TOKENIZER_NO_TOKEN;
    
    signed char result = translate_escape_char(c);
    if (result != -1) {
      *res = result;
      return 4;
    } else {
      return TOKENIZER_CHAR_ERROR;
    }
  }
  *res = c;
  return 3;
}


#define TD_BUF_SIZE 128

#define FBUF_ADD(X,N) if ((N) < TD_BUF_SIZE) { fbuf[(N)] = (X); N++; } else goto tok_double_no_tok;
int tok_double(lbm_char_channel_t *chan, token_float *result) {

  unsigned int n = 0;
  char fbuf[TD_BUF_SIZE];
  char c;
  bool valid_num = false;
  int res;

  memset(fbuf, 0, TD_BUF_SIZE);

  result->type = TOKTYPEF32;
  result->negative = false;

  res = lbm_channel_peek(chan, n, &c);
  if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
  if (c == '-') {
    FBUF_ADD('-', n);
    result->negative = true;
  }

  res = lbm_channel_peek(chan, n, &c);
  if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
  while (c >= '0' && c <= '9') {
    FBUF_ADD(c, n);
    res = lbm_channel_peek(chan, n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    if (res == CHANNEL_END) break;
  }

  if (c == '.') {
    FBUF_ADD(c, n);
  }
  else return TOKENIZER_NO_TOKEN;

  res = lbm_channel_peek(chan,n, &c);
  if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
  else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
  if (!(c >= '0' && c <= '9')) return TOKENIZER_NO_TOKEN;

  while (c >= '0' && c <= '9') {
    FBUF_ADD(c, n);
    res = lbm_channel_peek(chan, n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    if (res == CHANNEL_END) break;
  }

  if (c == 'e') {
    FBUF_ADD(c, n);
    res = lbm_channel_peek(chan,n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
    if (!((c >= '0' && c <= '9') || c == '-')) return TOKENIZER_NO_TOKEN;

    if (c == '-') {
      FBUF_ADD(c, n);
    }
    res = lbm_channel_peek(chan,n, &c);
    if (res == CHANNEL_MORE) return TOKENIZER_NEED_MORE;
    else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;
    while ((c >= '0' && c <= '9')) {
      FBUF_ADD(c,n);
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

  if(valid_num) {
    result->value = (double)strtod(fbuf,NULL);
    return (int)n + type_len;
  }

 tok_double_no_tok:
  return TOKENIZER_NO_TOKEN;
}

bool tok_clean_whitespace(lbm_char_channel_t *chan) {

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

int tok_integer(lbm_char_channel_t *chan, token_int *result) {
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
    return TOKENIZER_NO_TOKEN;
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
    else if (res == CHANNEL_END) return TOKENIZER_NO_TOKEN;

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

  if (n == 0 || (hex && n == 2)) return TOKENIZER_NO_TOKEN;

  uint32_t tok_res;
  int type_len = tok_match_fixed_size_tokens(chan, type_qual_table, n, NUM_TYPE_QUALIFIERS, &tok_res);

  if (type_len == TOKENIZER_NEED_MORE) return type_len;
  if (type_len != TOKENIZER_NO_TOKEN) {
    result->type = tok_res;
  }

  if ((result->negative && n > 1) ||
      !result->negative) valid_num = true;

  if (valid_num) {
    result->value = acc;
    return (int)n + type_len;
  }
  return TOKENIZER_NO_TOKEN;
}
