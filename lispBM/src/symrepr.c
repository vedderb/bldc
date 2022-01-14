/*
    Copyright 2018, 2021 Joel Svensson  svenssonjoel@yahoo.se

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

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include "symrepr.h"
#include "lispbm_memory.h"

#define NUM_SPECIAL_SYMBOLS 79

#define NAME   0
#define ID     1
#define NEXT   2

typedef struct {
  const char *name;
  const UINT id;
} special_sym;

special_sym const special_symbols[NUM_SPECIAL_SYMBOLS] =  {
  {"nil"        , SYM_NIL},
  {"quote"      , SYM_QUOTE},
  {"t"          , SYM_TRUE},
  {"if"         , SYM_IF},
  {"lambda"     , SYM_LAMBDA},
  {"closure"    , SYM_CLOSURE},
  {"let"        , SYM_LET},
  {"define"     , SYM_DEFINE},
  {"progn"      , SYM_PROGN},
  //{"bquote"     , SYM_BACKQUOTE},
  {"comma"      , SYM_COMMA},
  {"splice"     , SYM_COMMAAT},
  {"match"      , SYM_MATCH},
  {"_"          , SYM_DONTCARE},
  {"send"       , SYM_SEND},
  {"recv"       , SYM_RECEIVE},
  {"?"          , SYM_MATCH_ANY},
  {"?i28"       , SYM_MATCH_I28},
  {"?u28"       , SYM_MATCH_U28},
  {"?float"     , SYM_MATCH_FLOAT},
  {"?cons"      , SYM_MATCH_CONS},

  // Special symbols with unparseable names
  {"no_match"           , SYM_NO_MATCH},
  {"read_error"         , SYM_RERROR},
  {"type_error"         , SYM_TERROR},
  {"eval_error"         , SYM_EERROR},
  {"out_of_memory"      , SYM_MERROR},
  {"fatal_error"        , SYM_FATAL_ERROR},
  {"division_by_zero"   , SYM_DIVZERO},
  {"sym_array"          , SYM_ARRAY_TYPE},
  {"sym_boxed_i"        , SYM_BOXED_I_TYPE},
  {"sym_boxed_u"        , SYM_BOXED_U_TYPE},
  {"sym_boxed_f"        , SYM_BOXED_F_TYPE},
  {"sym_ref"            , SYM_REF_TYPE},
  {"sym_recovered"      , SYM_RECOVERED},
  {"sym_bytecode"       , SYM_BYTECODE_TYPE},
  {"sym_nonsense"       , SYM_NONSENSE},
  {"variable_not_bound" , SYM_NOT_FOUND},

  // special symbols with parseable names
  {"type-list"        , SYM_TYPE_LIST},
  {"type-i28"         , SYM_TYPE_I28},
  {"type-u28"         , SYM_TYPE_U28},
  {"type-float"       , SYM_TYPE_FLOAT},
  {"type-i32"         , SYM_TYPE_I32},
  {"type-u32"         , SYM_TYPE_U32},
  {"type-array"       , SYM_TYPE_ARRAY},
  {"type-symbol"      , SYM_TYPE_SYMBOL},
  {"type-char"        , SYM_TYPE_CHAR},
  {"type-ref"         , SYM_TYPE_REF},

  // Fundamental operations
  {"+"              , SYM_ADD},
  {"-"              , SYM_SUB},
  {"*"              , SYM_MUL},
  {"/"              , SYM_DIV},
  {"mod"            , SYM_MOD},
  {"="              , SYM_EQ},
  {"<"              , SYM_LT},
  {">"              , SYM_GT},
  {"eval"           , SYM_EVAL},
  {"and"            , SYM_AND},
  {"or"             , SYM_OR},
  {"not"            , SYM_NOT},
  {"yield"          , SYM_YIELD},
  {"wait"           , SYM_WAIT},
  {"spawn"          , SYM_SPAWN},
  {"num-eq"         , SYM_NUMEQ},
  {"car"            , SYM_CAR},
  {"cdr"            , SYM_CDR},
  {"cons"           , SYM_CONS},
  {"list"           , SYM_LIST},
  {"append"         , SYM_APPEND},
  {"array-read"     , SYM_ARRAY_READ},
  {"array-write"    , SYM_ARRAY_WRITE},
  {"array-create"   , SYM_ARRAY_CREATE},
  {"type-of"        , SYM_TYPE_OF},
  {"sym-to-str"     , SYM_SYMBOL_TO_STRING},
  {"str-to-sym"     , SYM_STRING_TO_SYMBOL},
  {"sym-to-u"       , SYM_SYMBOL_TO_UINT},
  {"u-to-sym"       , SYM_UINT_TO_SYMBOL},
  {"mk-sym-indirect", SYM_MK_SYMBOL_INDIRECT},
  {"set-car"        , SYM_SET_CAR},
  {"set-cdr"        , SYM_SET_CDR},
  {"is-fundamental" , SYM_IS_FUNDAMENTAL}
};


static uint32_t *symlist = NULL;
static UINT next_symbol_id = 0;

bool symrepr_init(void) {
  symlist = NULL;
  next_symbol_id = 0;
  return true;
}

void symrepr_del(void) {

  uint32_t *curr = symlist;
  while (curr) {
    uint32_t *tmp = curr;
    curr = (uint32_t*)curr[NEXT];
    memory_free((uint32_t*)tmp[NAME]);
    memory_free(tmp);
  }
}

const char *lookup_symrepr_name_memory(UINT id) {

  uint32_t *curr = symlist;
  while (curr) {
    if (id == curr[ID]) {
      return (const char *)curr[NAME];
    }
    curr = (uint32_t*)curr[NEXT];
  }
  return NULL;
}

// Lookup symbol name given a symbol id
const char *symrepr_lookup_name(UINT id) {
  if (id < MAX_SPECIAL_SYMBOLS) {
    for (int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
      if (id == special_symbols[i].id) {
        return (special_symbols[i].name);
      }
    }
  }
  return lookup_symrepr_name_memory(id);
}

// Lookup symbol id given symbol name
int symrepr_lookup(char *name, UINT* id) {

  // loop through special symbols
  for (int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
    if (strcmp(name, special_symbols[i].name) == 0) {
      *id = special_symbols[i].id;
      return 1;
    }
  }

  uint32_t *curr = symlist;
  while (curr) {
    char *str = (char*)curr[NAME];
    if (strcmp(name, str) == 0) {
      *id = curr[ID];
      return 1;
    }
    curr = (uint32_t*)curr[NEXT];
  }
  return 0;
}

int symrepr_addsym(char *name, UINT* id) {
  size_t  n = 0;

  n = strlen(name) + 1;
  if (n == 1) return 0; // failure if empty symbol

  uint32_t *m = memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  char *symbol_name_storage = NULL;;
  if (n % 4 == 0) {
    symbol_name_storage = (char *)memory_allocate(n / 4);
  } else {
    symbol_name_storage = (char *)memory_allocate(n / 4 + 1);
  }

  if (symbol_name_storage == NULL) {
    memory_free(m);
    return 0;
  }

  strcpy(symbol_name_storage, name);

  m[NAME] = (uint32_t)symbol_name_storage;

  if (symlist == NULL) {
    m[NEXT] = (uint32_t) NULL;
    symlist = m;
  } else {
    m[NEXT] = (uint32_t) symlist;
    symlist = m;
  }
  m[ID] = MAX_SPECIAL_SYMBOLS + next_symbol_id++;
  *id = m[ID];
  return 1;
}

// Same as above, but assume that the name pointer stays valid
int symrepr_addsym_const(char *name, UINT* id) {
  if (strlen(name) == 0) return 0; // failure if empty symbol

  uint32_t *m = memory_allocate(3);

  if (m == NULL) {
    return 0;
  }

  m[NAME] = (uint32_t)name;

  if (symlist == NULL) {
    m[NEXT] = (uint32_t) NULL;
    symlist = m;
  } else {
    m[NEXT] = (uint32_t) symlist;
    symlist = m;
  }
  m[ID] = MAX_SPECIAL_SYMBOLS + next_symbol_id++;
  *id = m[ID];
  return 1;
}

unsigned int symrepr_size(void) {

  unsigned int n = 0;
  uint32_t *curr = symlist;

  while (curr) {
    // up to 3 extra bytes are used for string storage if length is not multiple of 4
    size_t s = strlen((char *)curr[NAME]);
    s ++;
    n += s % 4;
    n += 12; // sizeof the node in the linked list
    curr = (uint32_t *)curr[NEXT];
  }
  return n;
}
