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

static bool stack_ok = true;

#ifdef TOKPAR_CHECK_STACK
#define CHECK_STACK() if (!TOKPAR_CHECK_STACK()) {stack_ok = false;} if (!stack_ok) {return enc_sym(SYM_STACK_ERROR);}
#else
#define CHECK_STACK()
#endif

typedef struct {
  const char *str;
  uint32_t  token;
  uint32_t len;
} matcher;

typedef struct {
  unsigned int type;
  unsigned int text_len;
  union {
    char  c;
    char  *text;
    INT   i;
    UINT  u;
    FLOAT f;
  } data;
} token;

typedef struct {
  token tok;
  char *str;
  unsigned int pos;
  bool (*more)(void);
  char (*get)(void);
  char (*peek)(unsigned int n);
  void (*drop)(unsigned int n);
} parser_state;

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

static parser_state ts;

bool more_local(void) {
  return ts.str[ts.pos] != 0;
}

char get_local(void) {
  return ts.str[ts.pos++];
}

char peek_local(unsigned int n) {
  return ts.str[ts.pos + n];
}

void drop_local(unsigned int n) {
  ts.pos = ts.pos + n;
}

#define more() ts.more()
#define get() ts.get()
#define peek(n) ts.peek(n)
#define drop(n) ts.drop(n)

static uint32_t tok_match_fixed_size_tokens(void) {
  for (int i = 0; i < NUM_FIXED_SIZE_TOKENS; i ++) {
    uint32_t tok_len = match_table[i].len;
    const char *match_str = match_table[i].str;
    uint32_t tok = match_table[i].token;

    uint32_t char_pos;
    for (char_pos = 0; char_pos < tok_len; char_pos ++) {
      if (peek(char_pos) != match_str[char_pos]) break;
    }
    if (char_pos == tok_len) { //match
      drop(tok_len);
      return tok;
    }
  }
  return NOTOKEN;
}

static bool symchar0(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/=<>";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

static bool symchar(char c) {
  const char *allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/=<>";

  int i = 0;
  while (allowed[i] != 0) {
    if (c == allowed[i++]) return true;
  }
  return false;
}

static int tok_symbol(void) {
  if (!symchar0(peek(0)))  return 0;

  unsigned int i = 0;
  unsigned int len = 1;
  int n = 0;

  while (symchar((peek(len)))) {
    len++;
  }

  if (len > TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH)
    return -1; /* TODO: specific error code that can be presented to user */

  clear_sym_str();

  int c = 0;

  for (i = 0; i < len; i ++) {
    c = tolower(get());
    if (c >= 0 && c <= 255) {
      sym_str[i] = (char)c;
      n++;
    } else {
      return -1;
    }
  }
  return (int)n;
}

static int tok_string(void) {
  unsigned int i = 0;
  int n = 0;
  unsigned int len = 0;
  if (!(peek(0) == '\"')) return 0;

  get(); // remove the " char
  n++;

  // compute length of string
  while (peek(len) != 0 &&
         peek(len) != '\"') {
    len++;
  }

  if (len > TOKENIZER_MAX_SYMBOL_AND_STRING_LENGTH)
    return -1; /* TODO: specific error code that can be presented to user */

  // str ends before tokenized string is closed.
  if ((peek(len)) != '\"') {
    return 0;
  }

  clear_sym_str();

  for (i = 0; i < len; i ++) {
    sym_str[i] = get();
    n++;
  }

  get();  // throw away the "
  return (int)(n+1);
}

static int tok_char(char *res) {
  int count = 0;
  if (peek(0) == '\\' &&
      peek(1) == '#' &&
      peek(2) == 'n' &&
      peek(3) == 'e' &&
      peek(4) == 'w' &&
      peek(5) == 'l' &&
      peek(6) == 'i' &&
      peek(7) == 'n' &&
      peek(8) == 'e') {
    *res = '\n';
    drop(9);
    count = 9;
  } else if (peek(0) == '\\' &&
             peek(1) == '#' &&
             isgraph(peek(2))) {
    *res = peek(2);
    drop(3);
    count = 3;
  }
  return count;
}

static int tok_i(INT *res) {
  INT acc = 0;
  unsigned int n = 0;

  while ( peek(n) >= '0' && peek(n) <= '9' ) {
    acc = (acc*10) + (peek(n) - '0');
    n++;
  }

  // Not needed if strict adherence to ordering of calls to tokenizers.
  if (peek(n) == 'U' ||
      peek(n) == 'u' ||
      peek(n) == '.' ||
      peek(n) == 'I') return 0;

  drop(n);
  *res = acc;
  return (int)n; /*check that isnt so high that it becomes a negative number when casted */
}

static int tok_I(INT *res) {
  INT acc = 0;
  unsigned int n = 0;

  while ( peek(n) >= '0' && peek(n) <= '9' ) {
    acc = (acc*10) + (peek(n) - '0');
    n++;
  }

  if (peek(n) == 'i' &&
      peek(n+1) == '3' &&
      peek(n+2) == '2') {
    *res = acc;
    drop(n+3);
    return (int)(n+3);
  }
  return 0;
}

static int tok_u(UINT *res) {
  UINT acc = 0;
  unsigned int n = 0;

  while ( peek(n) >= '0' && peek(n) <= '9' ){
    acc = (acc*10) + (UINT)(peek(n) - '0');
    n++;
  }

  if (peek(n) == 'u' &&
      peek(n+1) == '2' &&
      peek(n+2) == '8' ) {
    *res = acc;
    drop(n+3);
    return (int)(n+3);
  }
  return 0;
}

static int tok_U(UINT *res) {
  UINT acc = 0;
  unsigned int n = 0;

  // Check if hex notation is used
  if (peek(0) == '0' &&
      (peek(1) == 'x' || peek(1) == 'X')) {
    n+= 2;
    while ( (peek(n) >= '0' && peek(n) <= '9') ||
            (peek(n) >= 'a' && peek(n) <= 'f') ||
            (peek(n) >= 'A' && peek(n) <= 'F')){
      UINT val;
      if (peek(n) >= 'a' && peek(n) <= 'f') {
        val = 10 + (UINT)(peek(n) - 'a');
      } else if (peek(n) >= 'A' && peek(n) <= 'F') {
        val = 10 + (UINT)(peek(n) - 'A');
      } else {
        val = (UINT)peek(n) - '0';
      }
      acc = (acc * 0x10) + val;
      n++;
    }
    *res = acc;
    drop(n);
    return (int)n;
  }

  // check if nonhex
  while ( peek(n) >= '0' && peek(n) <= '9' ){
    acc = (acc*10) + (UINT)(peek(n) - '0');
    n++;
  }

  if (peek(n) == 'u' &&
      peek(n+1) == '3' &&
      peek(n+2) == '2') {
    *res = acc;
    drop(n+3);
    return (int)(n+3);
  }
  return 0;
}

static int tok_F(FLOAT *res) {
  unsigned int n = 0;
  unsigned int m = 0;

  while ( peek(n) >= '0' && peek(n) <= '9') n++;

  if ( peek(n) == '.') n++;
  else return 0;

  if ( !(peek(n) >= '0' && peek(n) <= '9')) return 0;
  while ( peek(n) >= '0' && peek(n) <= '9') n++;

  if (n > 127) m = 127;
  else m = n;

  unsigned int i;
  char fbuf[m + 1];
  for (i = 0; i < m; i ++) {
    fbuf[i] = get();
  }

  fbuf[i] = 0;
  *res = (float)strtod(fbuf, NULL);
  return (int)n;
}


static token next_token(void) {
  token t;

  if (!more()) {
    t.type = TOKENIZER_END;
    return t;
  }

  // Eat whitespace and comments.
  bool clean_whitespace = true;
  while ( clean_whitespace ){
    if ( peek(0) == ';' ) {
      while ( more() && peek( 0) != '\n') {
        drop(1);
      }
    } else if ( isspace(peek(0))) {
      drop(1);
    } else {
      clean_whitespace = false;
    }
  }

  // Check for end of string again
  if (!more()) {
    t.type = TOKENIZER_END;
    return t;
  }

  uint32_t match;;
  match = tok_match_fixed_size_tokens();
  if (match > 0) {
    t.type = match;
    return t;
  }

  int n = tok_symbol();
  if (n > 0) {
    t.text_len = (unsigned int)n;
    t.type = TOKSYMBOL;
    return t;
  } else if (n < 0) {
    t.type = TOKENIZER_ERROR;
    return t;
  }

  char c_val;
  if (tok_char(&c_val)) {
    t.data.c = c_val;
    t.type = TOKCHAR;
    return t;
  }

  n = tok_string();
  if (n >= 2) {
    t.text_len = (unsigned int)n - 2;
    t.type = TOKSTRING;
    return t;
  } else if (n < 0) {
    t.type = TOKENIZER_ERROR;
    return t;
  }

  FLOAT f_val;
  if (tok_F(&f_val)) {
    t.data.f = f_val;
    t.type = TOKBOXEDFLOAT;
    return t;
  }

  UINT u_val;
  if (tok_U(&u_val)) {
    t.data.u = u_val;
    t.type = TOKBOXEDUINT;
    return t;
  }

  if (tok_u(&u_val)) {
    t.data.u = u_val;
    t.type = TOKUINT;
    return t;
  }

  INT i_val;
  if (tok_I(&i_val)) {
    t.data.i = i_val;
    t.type = TOKBOXEDINT;
    return t;
  }

  // Shortest form of integer match. Move to last in chain of numerical tokens.
  if (tok_i(&i_val)) {
    t.data.i = i_val;
    t.type = TOKINT;
    return t;
  }

  t.type = TOKENIZER_ERROR;
  return t;
}

static VALUE tokpar_parse_program(void);
static VALUE parse_sexp(void);
static VALUE parse_sexp_list(void);

static VALUE tokpar_parse_program(void) {
  CHECK_STACK();

  ts.tok = next_token();

  if (ts.tok.type == TOKENIZER_ERROR) {
    return enc_sym(SYM_RERROR);
  }

  if (ts.tok.type == TOKENIZER_END) {
    return enc_sym(SYM_NIL);
  }

  return cons(parse_sexp(), tokpar_parse_program());
}

static VALUE parse_sexp(void) {
  CHECK_STACK();

  switch (ts.tok.type) {
  case TOKENIZER_END:
    return enc_sym(SYM_RERROR);
  case TOKENIZER_ERROR:
    return enc_sym(SYM_RERROR);
  case TOKOPENPAR: {
    ts.tok = next_token();
    return parse_sexp_list();
  }
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
    VALUE v;
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
    VALUE v;
    heap_allocate_array(&v, ts.tok.text_len+1, VAL_TYPE_CHAR);
    array_header_t *arr = (array_header_t*)car(v);
    char *data = (char *)arr + 8;
    memset(data, 0, (ts.tok.text_len+1) * sizeof(char));
    memcpy(data, sym_str, ts.tok.text_len * sizeof(char));
    return v;
  }
  case TOKINT:
    return enc_i(ts.tok.data.i);
  case TOKUINT:
    return enc_u(ts.tok.data.u);
  case TOKCHAR:
    return enc_char(ts.tok.data.c);
  case TOKBOXEDINT:
    return set_ptr_type(cons((VALUE)ts.tok.data.i, enc_sym(SYM_BOXED_I_TYPE)), PTR_TYPE_BOXED_I);
  case TOKBOXEDUINT:
    return set_ptr_type(cons(ts.tok.data.u, enc_sym(SYM_BOXED_U_TYPE)), PTR_TYPE_BOXED_U);
  case TOKBOXEDFLOAT:
    return set_ptr_type(cons(ts.tok.data.u, enc_sym(SYM_BOXED_F_TYPE)), PTR_TYPE_BOXED_F);
  case TOKQUOTE: {
    ts.tok = next_token();
    VALUE quoted = parse_sexp();
    if (type_of(quoted) == VAL_TYPE_SYMBOL &&
        dec_sym(quoted) == SYM_RERROR) return quoted;
    return cons(enc_sym(SYM_QUOTE), cons (quoted, enc_sym(SYM_NIL)));
  }
  case TOKBACKQUOTE: {
    ts.tok = next_token();
    VALUE quoted = parse_sexp();
    if (type_of(quoted) == VAL_TYPE_SYMBOL &&
        dec_sym(quoted) == SYM_RERROR) return quoted;
    VALUE expanded = qq_expand(quoted);
    if (type_of(expanded) == VAL_TYPE_SYMBOL &&
        symrepr_is_error(dec_sym(expanded))) return expanded;
    return expanded;
  }
  case TOKCOMMAAT: {
    ts.tok = next_token();
    VALUE splice = parse_sexp();
    if (type_of(splice) == VAL_TYPE_SYMBOL &&
        dec_sym(splice) == SYM_RERROR) return splice;
    return cons(enc_sym(SYM_COMMAAT), cons (splice, enc_sym(SYM_NIL)));
  }
  case TOKCOMMA: {
    ts.tok = next_token();
    VALUE unquoted = parse_sexp();
    if (type_of(unquoted) == VAL_TYPE_SYMBOL &&
        dec_sym(unquoted) == SYM_RERROR) return unquoted;
    return cons(enc_sym(SYM_COMMA), cons (unquoted, enc_sym(SYM_NIL)));
  }
  }
  return enc_sym(SYM_RERROR);
}

static VALUE parse_sexp_list(void) {
  CHECK_STACK();

  switch (ts.tok.type) {
  case TOKENIZER_END:
    return enc_sym(SYM_RERROR);
  case TOKENIZER_ERROR:
    return enc_sym(SYM_RERROR);
  case TOKCLOSEPAR:
    return enc_sym(SYM_NIL);
  default: {
    VALUE head = parse_sexp();
    ts.tok = next_token();

    VALUE tail;
    if (ts.tok.type == TOKDOT) {
      ts.tok = next_token();
      tail = parse_sexp();
      ts.tok = next_token();
      if (ts.tok.type != TOKCLOSEPAR) {
        return enc_sym(SYM_RERROR);
      }

    } else {
      tail = parse_sexp_list();
    }

    if ((type_of(head) == VAL_TYPE_SYMBOL &&
         dec_sym(head) == SYM_RERROR ) ||
        (type_of(tail) == VAL_TYPE_SYMBOL &&
         dec_sym(tail) == SYM_RERROR )) return enc_sym(SYM_RERROR);
    return cons(head, tail);
  }
  }
  return enc_sym(SYM_RERROR);
}

VALUE tokpar_parse(char *string) {
  stack_ok = true;

  ts.str = string;
  ts.pos = 0;
  ts.more = more_local;
  ts.get = get_local;
  ts.peek = peek_local;
  ts.drop = drop_local;

  VALUE v = tokpar_parse_program();
  CHECK_STACK();

  return v;
}

VALUE tokpar_parse_stream(
    bool (*more)(void),
    char (*get)(void),
    char (*peek)(unsigned int n),
    void (*drop)(unsigned int n)) {

  stack_ok = true;

  ts.str = 0;
  ts.pos = 0;
  ts.more = more;
  ts.get = get;
  ts.peek = peek;
  ts.drop = drop;

  VALUE v = tokpar_parse_program();
  CHECK_STACK();

  return v;
}
