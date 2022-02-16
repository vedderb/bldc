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
    INT   i;
    UINT  u;
    FLOAT f;
  }data;
} token;


typedef struct {
  char *str;
  unsigned int pos;
} tokenizer_state;

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

bool more(tokenizer_char_stream str) {
  return str.more(str);
}

char get(tokenizer_char_stream str) {
  return str.get(str);
}

char peek(tokenizer_char_stream str, unsigned int n) {
  return str.peek(str,n);
}

void drop(tokenizer_char_stream str, unsigned int n) {
  str.drop(str,n);
}


uint32_t tok_match_fixed_size_tokens(tokenizer_char_stream str) {

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

int tok_symbol(tokenizer_char_stream str) {

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

int tok_string(tokenizer_char_stream str) {

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

int tok_char(tokenizer_char_stream str, char *res) {

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

int tok_i(tokenizer_char_stream str, INT *res) {

  INT acc = 0;
  unsigned int n = 0;

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (peek(str,n) - '0');
    n++;
  }

  // Not needed if strict adherence to ordering of calls to tokenizers.
  if (peek(str,n) == 'U' ||
      peek(str,n) == 'u' ||
      peek(str,n) == '.' ||
      peek(str,n) == 'I') return 0;

  drop(str,n);
  *res = acc;
  return (int)n; /*check that isnt so high that it becomes a negative number when casted */
}

int tok_I(tokenizer_char_stream str, INT *res) {
  INT acc = 0;
  unsigned int n = 0;

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (peek(str,n) - '0');
    n++;
  }

  if (peek(str,n) == 'i' &&
      peek(str,n+1) == '3' &&
      peek(str,n+2) == '2') {
    *res = acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_u(tokenizer_char_stream str, UINT *res) {
  UINT acc = 0;
  unsigned int n = 0;

  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (UINT)(peek(str,n) - '0');
    n++;
  }

  if (peek(str,n) == 'u' &&
      peek(str,n+1) == '2' &&
      peek(str,n+2) == '8' ) {
    *res = acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_U(tokenizer_char_stream str, UINT *res) {
  UINT acc = 0;
  unsigned int n = 0;

  // Check if hex notation is used
  if (peek(str,0) == '0' &&
      (peek(str,1) == 'x' || peek(str,1) == 'X')) {
    n+= 2;
    while ( (peek(str,n) >= '0' && peek(str,n) <= '9') ||
            (peek(str,n) >= 'a' && peek(str,n) <= 'f') ||
            (peek(str,n) >= 'A' && peek(str,n) <= 'F')){
      UINT val;
      if (peek(str,n) >= 'a' && peek(str,n) <= 'f') {
        val = 10 + (UINT)(peek(str,n) - 'a');
      } else if (peek(str,n) >= 'A' && peek(str,n) <= 'F') {
        val = 10 + (UINT)(peek(str,n) - 'A');
      } else {
        val = (UINT)peek(str,n) - '0';
      }
      acc = (acc * 0x10) + val;
      n++;
    }
    *res = acc;
    drop(str,n);
    return (int)n;
  }

  // check if nonhex
  while ( peek(str,n) >= '0' && peek(str,n) <= '9' ){
    acc = (acc*10) + (UINT)(peek(str,n) - '0');
    n++;
  }

  if (peek(str,n) == 'u' &&
      peek(str,n+1) == '3' &&
      peek(str,n+2) == '2') {
    *res = acc;
    drop(str,n+3);
    return (int)(n+3);
  }
  return 0;
}

int tok_F(tokenizer_char_stream str, FLOAT *res) {

  unsigned int n = 0;
  unsigned int m = 0;
  char fbuf[128];

  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  if ( peek(str,n) == '.') n++;
  else return 0;

  if ( !(peek(str,n) >= '0' && peek(str,n) <= '9')) return 0;
  while ( peek(str,n) >= '0' && peek(str,n) <= '9') n++;

  if (n > 127) m = 127;
  else m = n;

  unsigned int i;
  for (i = 0; i < m; i ++) {
    fbuf[i] = get(str);
  }

  fbuf[i] = 0;
  *res = (float)strtod(fbuf, NULL);
  return (int)n;
}


token next_token(tokenizer_char_stream str) {
  token t;

  INT i_val;
  UINT u_val;
  char c_val;
  FLOAT f_val;
  int n = 0;

  if (!more(str)) {
    t.type = TOKENIZER_END;
    return t;
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
    t.type = TOKENIZER_END;
    return t;
  }

  uint32_t match;;
  match = tok_match_fixed_size_tokens(str);
  if (match > 0) {
    t.type = match;
    return t;
  }

  n = tok_symbol(str);
  if (n > 0) {
    t.text_len = (unsigned int)n;
    t.type = TOKSYMBOL;
    return t;
  } else if (n < 0) {
    t.type = TOKENIZER_ERROR;
    return t;
  }

  if (tok_char(str, &c_val)) {
    t.data.c = c_val;
    t.type = TOKCHAR;
    return t;
  }

  n = tok_string(str);
  if (n >= 2) {
    t.text_len = (unsigned int)n - 2;
    t.type = TOKSTRING;
    return t;
  } else if (n < 0) {
    t.type = TOKENIZER_ERROR;
    return t;
  }

  if (tok_F(str, &f_val)) {
    t.data.f = f_val;
    t.type = TOKBOXEDFLOAT;
    return t;
  }

  if (tok_U(str, &u_val)) {
    t.data.u = u_val;
    t.type = TOKBOXEDUINT;
    return t;
  }

  if (tok_u(str, &u_val)) {
    t.data.u = u_val;
    t.type = TOKUINT;
    return t;
  }

  if (tok_I(str, &i_val)) {
    t.data.i = i_val;
    t.type = TOKBOXEDINT;
    return t;
  }

  // Shortest form of integer match. Move to last in chain of numerical tokens.
  if (tok_i(str, &i_val)) {
    t.data.i = i_val;
    t.type = TOKINT;
    return t;
  }

  t.type = TOKENIZER_ERROR;
  return t;
}

VALUE parse_sexp(token tok, tokenizer_char_stream str);
VALUE parse_sexp_list(token tok, tokenizer_char_stream str);

VALUE tokpar_parse_program(tokenizer_char_stream str) {
  token tok = next_token(str);
  VALUE head;
  VALUE tail;

  if (tok.type == TOKENIZER_ERROR) {
    return enc_sym(SYM_RERROR);
  }

  if (tok.type == TOKENIZER_END) {
    return enc_sym(SYM_NIL);
  }

  head = parse_sexp(tok, str);
  tail = tokpar_parse_program(str);

  return cons(head, tail);
}

VALUE parse_sexp(token tok, tokenizer_char_stream str) {

  VALUE v;
  token t;

  switch (tok.type) {
  case TOKENIZER_END:
    return enc_sym(SYM_RERROR);
  case TOKENIZER_ERROR:
    return enc_sym(SYM_RERROR);
  case TOKOPENPAR:
    t = next_token(str);
    return parse_sexp_list(t,str);
  case TOKDONTCARE:
    return enc_sym(SYM_DONTCARE);
  case TOKMATCHANY:
    return enc_sym(SYM_MATCH_ANY);
  case TOKMATCHI28:
    return enc_sym(SYM_MATCH_I28);
  case TOKMATCHU28:
    return enc_sym(SYM_MATCH_U28);
  case TOKMATCHFLOAT:
    return enc_sym(SYM_MATCH_FLOAT);
  case TOKMATCHCONS:
    return enc_sym(SYM_MATCH_CONS);
  case TOKSYMBOL: {
    UINT symbol_id;

    if (symrepr_lookup(sym_str, &symbol_id)) {
      v = enc_sym(symbol_id);
    }
    else if (symrepr_addsym(sym_str, &symbol_id)) {
      v = enc_sym(symbol_id);
    } else {
      v = enc_sym(SYM_RERROR);
    }
    return v;
  }
  case TOKSTRING: {
    heap_allocate_array(&v, tok.text_len+1, VAL_TYPE_CHAR);
    array_header_t *arr = (array_header_t*)car(v);
    char *data = (char *)arr + 8;
    memset(data, 0, (tok.text_len+1) * sizeof(char));
    memcpy(data, sym_str, tok.text_len * sizeof(char));
    return v;
  }
  case TOKINT:
    return enc_i(tok.data.i);
  case TOKUINT:
    return enc_u(tok.data.u);
  case TOKCHAR:
    return enc_char(tok.data.c);
  case TOKBOXEDINT:
    return set_ptr_type(cons((VALUE)tok.data.i, enc_sym(SYM_BOXED_I_TYPE)), PTR_TYPE_BOXED_I);
  case TOKBOXEDUINT:
    return set_ptr_type(cons(tok.data.u, enc_sym(SYM_BOXED_U_TYPE)), PTR_TYPE_BOXED_U);
  case TOKBOXEDFLOAT:
    return set_ptr_type(cons(tok.data.u, enc_sym(SYM_BOXED_F_TYPE)), PTR_TYPE_BOXED_F);
  case TOKQUOTE: {
    t = next_token(str);
    VALUE quoted = parse_sexp(t, str);
    if (type_of(quoted) == VAL_TYPE_SYMBOL &&
        dec_sym(quoted) == SYM_RERROR) return quoted;
    return cons(enc_sym(SYM_QUOTE), cons (quoted, enc_sym(SYM_NIL)));
  }
  case TOKBACKQUOTE: {
    t = next_token(str);
    VALUE quoted = parse_sexp(t, str);
    if (type_of(quoted) == VAL_TYPE_SYMBOL &&
        dec_sym(quoted) == SYM_RERROR) return quoted;
    VALUE expanded = qq_expand(quoted);
    if (type_of(expanded) == VAL_TYPE_SYMBOL &&
        symrepr_is_error(dec_sym(expanded))) return expanded;
    return expanded;
  }
  case TOKCOMMAAT: {
    t = next_token(str);
    VALUE splice = parse_sexp(t, str);
    if (type_of(splice) == VAL_TYPE_SYMBOL &&
        dec_sym(splice) == SYM_RERROR) return splice;
    return cons(enc_sym(SYM_COMMAAT), cons (splice, enc_sym(SYM_NIL)));
  }
  case TOKCOMMA: {
    t = next_token(str);
    VALUE unquoted = parse_sexp(t, str);
    if (type_of(unquoted) == VAL_TYPE_SYMBOL &&
        dec_sym(unquoted) == SYM_RERROR) return unquoted;
    return cons(enc_sym(SYM_COMMA), cons (unquoted, enc_sym(SYM_NIL)));
  }
  }
  return enc_sym(SYM_RERROR);
}

VALUE parse_sexp_list(token tok, tokenizer_char_stream str) {

  token t;
  VALUE head;
  VALUE tail;

  switch (tok.type) {
  case TOKENIZER_END:
    return enc_sym(SYM_RERROR);
  case TOKENIZER_ERROR:
    return enc_sym(SYM_RERROR);
  case TOKCLOSEPAR:
    return enc_sym(SYM_NIL);
  default:
    head = parse_sexp(tok, str);
    t = next_token(str);

    if (t.type == TOKDOT) {
      t = next_token(str);
      tail = parse_sexp(t, str);
      t = next_token(str);
      if (t.type != TOKCLOSEPAR) {
        return enc_sym(SYM_RERROR);
      }

    } else {
      tail = parse_sexp_list(t, str);
    }
    if ((type_of(head) == VAL_TYPE_SYMBOL &&
         dec_sym(head) == SYM_RERROR ) ||
        (type_of(tail) == VAL_TYPE_SYMBOL &&
         dec_sym(tail) == SYM_RERROR )) return enc_sym(SYM_RERROR);
    return cons(head, tail);
  }
  return enc_sym(SYM_RERROR);
}

bool more_string(tokenizer_char_stream str) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  return s->str[s->pos] != 0;
}

char get_string(tokenizer_char_stream str) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  char c = s->str[s->pos];
  s->pos = s->pos + 1;
  return c;
}

char peek_string(tokenizer_char_stream str, unsigned int n) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  // TODO error checking ?? how ?
  char c = s->str[s->pos + n];
  return c;
}

void drop_string(tokenizer_char_stream str, unsigned int n) {
  tokenizer_state *s = (tokenizer_state*)str.state;
  s->pos = s->pos + n;
}

VALUE tokpar_parse(char *string) {

  tokenizer_state ts;
  ts.str = string;
  ts.pos = 0;

  tokenizer_char_stream str;
  str.state = &ts;
  str.more = more_string;
  str.peek = peek_string;
  str.drop = drop_string;
  str.get  = get_string;

  return tokpar_parse_program(str);
}

