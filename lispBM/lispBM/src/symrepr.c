/*
    Copyright 2018, 2021, 2022, 2024, 2025 Joel Svensson  svenssonjoel@yahoo.se
              2025 Rasmus Söderhielm rasmus.soderhielm@gmail.com

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


#include <lbm_memory.h>
#include <heap.h>
#include "symrepr.h"
#include "extensions.h"
#include "lbm_utils.h"
#include "lbm_image.h"

#define NUM_SPECIAL_SYMBOLS (sizeof(special_symbols) / sizeof(special_sym))
#define NAME   0
#define ID     1
#define NEXT   2

typedef struct {
  const char *name;
  const lbm_uint id;
} special_sym;

special_sym const special_symbols[] =  {
  {"nil"        , SYM_NIL},
  {"quote"      , SYM_QUOTE},
  {"t"          , SYM_TRUE},
  {"if"         , SYM_IF},
  {"cond"       , SYM_COND},
  {"lambda"     , SYM_LAMBDA},
  {"closure"    , SYM_CLOSURE},
  {"let"        , SYM_LET},
  {"define"     , SYM_DEFINE},
  {"progn"      , SYM_PROGN},
  {"read"       , SYM_READ},
  {"read-program" , SYM_READ_PROGRAM},
  {"read-eval-program", SYM_READ_AND_EVAL_PROGRAM},
  {"match"        , SYM_MATCH},
  {"_"            , SYM_DONTCARE},
  {"send"         , SYM_SEND},
  {"recv"         , SYM_RECEIVE},
  {"recv-to"      , SYM_RECEIVE_TIMEOUT},
  {"macro"        , SYM_MACRO},
  {"call-cc"      , SYM_CALLCC},
  {"continuation" , SYM_CONT},
  {"var"          , SYM_PROGN_VAR},
  {"timeout"      , SYM_TIMEOUT},

  {"set"          , SYM_SETVAR},
  {"setq"         , SYM_SETQ},
  {"move-to-flash", SYM_MOVE_TO_FLASH},
  {"exit-ok"      , SYM_EXIT_OK},
  {"exit-error"   , SYM_EXIT_ERROR},
  {"map"          , SYM_MAP},
  {"reverse"      , SYM_REVERSE},
  {"flatten"      , SYM_FLATTEN},
  {"unflatten"    , SYM_UNFLATTEN},
  {"kill"         , SYM_KILL},
  {"sleep"        , SYM_SLEEP},
  {"merge"        , SYM_MERGE},
  {"sort"         , SYM_SORT},
  {"gc"           , SYM_PERFORM_GC},
  {"loop"         , SYM_LOOP},
  {"trap"         , SYM_TRAP},
  {"rest-args"    , SYM_REST_ARGS},
  {"rotate"       , SYM_ROTATE},
  {"call-cc-unsafe", SYM_CALL_CC_UNSAFE},
  {"apply"        , SYM_APPLY},

  // pattern matching
  {"?"          , SYM_MATCH_ANY},

  // Error symbols with parsable names
  {"no_match"           , SYM_NO_MATCH},
  {"read_error"         , SYM_RERROR},
  {"type_error"         , SYM_TERROR},
  {"eval_error"         , SYM_EERROR},
  {"out_of_memory"      , SYM_MERROR},
  {"fatal_error"        , SYM_FATAL_ERROR},
  {"out_of_stack"       , SYM_STACK_ERROR},
  {"division_by_zero"   , SYM_DIVZERO},
  {"variable_not_bound" , SYM_NOT_FOUND},
  {"flash_full"         , SYM_ERROR_FLASH_HEAP_FULL},

  // Special symbols with unparsable names
  {"$barray"         , SYM_ARRAY_TYPE},
  {"$raw_i"          , SYM_RAW_I_TYPE},
  {"$raw_u"          , SYM_RAW_U_TYPE},
  {"$raw_f"          , SYM_RAW_F_TYPE},
  {"$ind_i"          , SYM_IND_I_TYPE},
  {"$ind_u"          , SYM_IND_U_TYPE},
  {"$ind_f"          , SYM_IND_F_TYPE},
  {"$channel"        , SYM_CHANNEL_TYPE},
  {"$recovered"      , SYM_RECOVERED},
  {"$placeholder"    , SYM_PLACEHOLDER},
  {"$custom"         , SYM_CUSTOM_TYPE},
  {"$array"          , SYM_LISPARRAY_TYPE},
  {"$nonsense"       , SYM_NONSENSE},
  {"$dm-array"       , SYM_DEFRAG_ARRAY_TYPE},
  {"$dm"             , SYM_DEFRAG_MEM_TYPE},

  // tokenizer symbols with unparsable names
  {"[openpar]"        , SYM_OPENPAR},
  {"[closepar]"       , SYM_CLOSEPAR},
  {"[backquote]"      , SYM_BACKQUOTE},
  {"[comma]"          , SYM_COMMA},
  {"[commaat]"        , SYM_COMMAAT},
  {"[dot]"            , SYM_DOT},
  {"[done]"           , SYM_TOKENIZER_DONE},
  {"[quote_it]"       , SYM_QUOTE_IT},
  {"[colon]"          , SYM_COLON},
  {"[wait]"           , SYM_TOKENIZER_WAIT},
  {"[openbrack]"      , SYM_OPENBRACK},
  {"[closebrack]"     , SYM_CLOSEBRACK},
  {"[rerror]"         , SYM_TOKENIZER_RERROR},
  {"[appcont]"        , SYM_APP_CONT},
  {"[openarr]"        , SYM_OPENARRAY},
  {"[closearr]"       , SYM_CLOSEARRAY},

  // special symbols with parseable names
  {"type-list"        , SYM_TYPE_LIST},
  {"type-i"           , SYM_TYPE_I},
  {"type-u"           , SYM_TYPE_U},
  {"type-float"       , SYM_TYPE_FLOAT},
  {"type-i32"         , SYM_TYPE_I32},
  {"type-u32"         , SYM_TYPE_U32},
  {"type-double"      , SYM_TYPE_DOUBLE},
  {"type-i64"         , SYM_TYPE_I64},
  {"type-u64"         , SYM_TYPE_U64},
  {"type-array"       , SYM_TYPE_ARRAY},
  {"type-symbol"      , SYM_TYPE_SYMBOL},
  {"type-char"        , SYM_TYPE_CHAR},
  {"type-byte"        , SYM_TYPE_BYTE},
  {"type-channel"     , SYM_TYPE_CHANNEL},
  {"type-lisparray"   , SYM_TYPE_LISPARRAY},
  {"type-dm"          , SYM_TYPE_DEFRAG_MEM},
  {"type-custom"      , SYM_TYPE_CUSTOM},

  // Fundamental operations
  {"+"                , SYM_ADD},
  {"-"                , SYM_SUB},
  {"*"                , SYM_MUL},
  {"/"                , SYM_DIV},
  {"//"               , SYM_INT_DIV},
  {"mod"              , SYM_MOD},
  {"="                , SYM_NUMEQ},
  {"!="               , SYM_NUM_NOT_EQ},
  {"<"                , SYM_LT},
  {">"                , SYM_GT},
  {"<="               , SYM_LEQ},
  {">="               , SYM_GEQ},
  {"eval"             , SYM_EVAL},
  {"eval-program"     , SYM_EVAL_PROGRAM},
  {"and"              , SYM_AND},
  {"or"               , SYM_OR},
  {"not"              , SYM_NOT},
  {"yield"            , SYM_YIELD},
  {"wait"             , SYM_WAIT},
  {"spawn"            , SYM_SPAWN},
  {"atomic"           , SYM_ATOMIC},
  {"self"             , SYM_SELF},
  {"spawn-trap"       , SYM_SPAWN_TRAP},
  {"set-mailbox-size" , SYM_SET_MAILBOX_SIZE},
  {"eq"               , SYM_EQ},
  {"not-eq"           , SYM_NOT_EQ},
  {"car"              , SYM_CAR},
  {"cdr"              , SYM_CDR},
  {"cons"             , SYM_CONS},
  {"list"             , SYM_LIST},
  {"append"           , SYM_APPEND},
  {"undefine"         , SYM_UNDEFINE},
  {"bufcreate"        , SYM_BYTEARRAY_CREATE},
  {"type-of"          , SYM_TYPE_OF},
  {"sym2str"          , SYM_SYMBOL_TO_STRING},
  {"str2sym"          , SYM_STRING_TO_SYMBOL},
  {"sym2u"            , SYM_SYMBOL_TO_UINT},
  {"u2sym"            , SYM_UINT_TO_SYMBOL},
  {"setcar"           , SYM_SET_CAR},
  {"setcdr"           , SYM_SET_CDR},
  {"setix"            , SYM_SET_IX},
  {"length"           , SYM_LIST_LENGTH},
  {"range"            , SYM_RANGE},
  {"member"           , SYM_MEMBER},

  {"assoc"          , SYM_ASSOC}, // lookup an association
  {"cossa"          , SYM_COSSA}, // lookup an association "backwards"
  {"acons"          , SYM_ACONS}, // Add to alist
  {"setassoc"       , SYM_SET_ASSOC}, // Change association

  {"shl"            , SYM_SHL},
  {"shr"            , SYM_SHR},
  {"bitwise-and"    , SYM_BITWISE_AND},
  {"bitwise-or"     , SYM_BITWISE_OR},
  {"bitwise-xor"    , SYM_BITWISE_XOR},
  {"bitwise-not"    , SYM_BITWISE_NOT},

  {"custom-destruct", SYM_CUSTOM_DESTRUCT},

  {"to-i"           , SYM_TO_I},
  {"to-i32"         , SYM_TO_I32},
  {"to-u"           , SYM_TO_U},
  {"to-u32"         , SYM_TO_U32},
  {"to-float"       , SYM_TO_FLOAT},
  {"to-i64"         , SYM_TO_I64},
  {"to-u64"         , SYM_TO_U64},
  {"to-double"      , SYM_TO_DOUBLE},
  {"to-byte"        , SYM_TO_BYTE},

  {"event-register-handler", SYM_REG_EVENT_HANDLER},
  {"take"           , SYM_TAKE},
  {"drop"           , SYM_DROP},
  {"mkarray"        , SYM_MKARRAY},

  {"dm-create"      , SYM_DM_CREATE},
  {"dm-alloc"       , SYM_DM_ALLOC},

  {"list?"          , SYM_IS_LIST},
  {"number?"        , SYM_IS_NUMBER},
  {"string?"        , SYM_IS_STRING},
  {"constant?"      , SYM_IS_CONSTANT},

  // fast access in list
  {"ix"             , SYM_IX},

  {"identity"       , SYM_IDENTITY},
  {"array"          , SYM_ARRAY},

  // aliases
  {"first"          , SYM_CAR},
  {"rest"           , SYM_CDR},
  {"fn"             , SYM_LAMBDA},
  {"def"            , SYM_DEFINE},
  {"true"           , SYM_TRUE},
  {"false"          , SYM_NIL},
  {"setvar"         , SYM_SETVAR},
  {"type-f32"       , SYM_TYPE_FLOAT},
  {"type-f64"       , SYM_TYPE_DOUBLE},
  {"array-create"   , SYM_BYTEARRAY_CREATE},

};

static lbm_uint *symlist = NULL;
static lbm_uint next_symbol_id = RUNTIME_SYMBOLS_START;
static lbm_uint symbol_table_size_list = 0;
static lbm_uint symbol_table_size_list_flash = 0;
static lbm_uint symbol_table_size_strings = 0;
static lbm_uint symbol_table_size_strings_flash = 0;

// When rebooting an image...
void lbm_symrepr_set_symlist(lbm_uint *ls) {
  symlist = ls;
}


lbm_uint lbm_symrepr_get_next_id(void) {
  return next_symbol_id;
}

void lbm_symrepr_set_next_id(lbm_uint id) {
  next_symbol_id = id;
}

bool lbm_symrepr_init(void) {
  symlist = NULL;
  next_symbol_id = RUNTIME_SYMBOLS_START;
  symbol_table_size_list = 0;
  symbol_table_size_list_flash = 0;
  symbol_table_size_strings = 0;
  symbol_table_size_strings_flash = 0;
  return true;
}

void lbm_symrepr_name_iterator(symrepr_name_iterator_fun f) {

  lbm_uint *curr = symlist;
  while (curr) {
    f((const char *)curr[NAME]);
    curr = (lbm_uint *)curr[NEXT];
  }
}

const char *lookup_symrepr_name_memory(lbm_uint id) {

  lbm_uint *curr = symlist;
  while (curr) {
    if (id == curr[ID]) {
      return (const char *)curr[NAME];
    }
    curr = (lbm_uint*)curr[NEXT];
  }
  return NULL;
}

// Lookup symbol name given a symbol id
const char *lbm_get_name_by_symbol(lbm_uint id) {
  lbm_uint sym_kind = SYMBOL_KIND(id);
  switch (sym_kind) {
  case SYMBOL_KIND_SPECIAL:  /* fall through */
  case SYMBOL_KIND_FUNDAMENTAL:
  case SYMBOL_KIND_APPFUN:
    for (unsigned int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
      if (id == special_symbols[i].id) {
        return (special_symbols[i].name);
      }
    }
    return NULL;
    break;
  case SYMBOL_KIND_EXTENSION: {
    lbm_uint ext_id = id - EXTENSION_SYMBOLS_START;
    if (ext_id < lbm_get_max_extensions()) {
      return extension_table[ext_id].name;
    }
    return NULL;
  } break;
  default:
    return lookup_symrepr_name_memory(id);
  }
}

lbm_uint *lbm_get_symbol_list_entry_by_name(char *name) {
  lbm_uint *curr = symlist;
  while (curr) {
    char *str = (char*)curr[NAME];
    if (str_eq(name, str)) {
      return (lbm_uint *)curr;
    }
    curr = (lbm_uint*)curr[NEXT];
  }
  return NULL;
}

// Lookup symbol id given symbol name
int lbm_get_symbol_by_name(char *name, lbm_uint* id) {

  // loop through special symbols
  for (unsigned int i = 0; i < NUM_SPECIAL_SYMBOLS; i ++) {
    if (str_eq(name, (char *)special_symbols[i].name)) {
      *id = special_symbols[i].id;
      return 1;
    }
   }

  // loop through extensions
  for (unsigned int i = 0; i < lbm_get_max_extensions(); i ++) {
    if (extension_table[i].name && str_eq(name, extension_table[i].name)) {
      *id = EXTENSION_SYMBOLS_START + i;
      return 1;
    }
  }

  lbm_uint *curr = symlist;
  while (curr) {
    char *str = (char*)curr[NAME];
    if (str_eq(name, str)) {
      *id = curr[ID];
      return 1;
    }
    curr = (lbm_uint*)curr[NEXT];
  }
  return 0;
}

extern lbm_flash_status lbm_write_const_array_padded(uint8_t *data, lbm_uint n, lbm_uint *res);

bool store_symbol_name_flash(char *name, lbm_uint *res) {
  size_t n = strlen(name) + 1;
  if (n == 1) return 0; // failure if empty symbol

  lbm_uint alloc_size;
  if (n % sizeof(lbm_uint) == 0) {
    alloc_size = n/(sizeof(lbm_uint));
  } else {
    alloc_size = (n/(sizeof(lbm_uint))) + 1;
  }

  lbm_uint symbol_addr = 0;
  lbm_flash_status s = lbm_write_const_array_padded((uint8_t*)name, n, &symbol_addr);
  if (s != LBM_FLASH_WRITE_OK || symbol_addr == 0) {
    return false;
  }
  symbol_table_size_strings_flash += alloc_size;
  *res = symbol_addr;
  return true;
}

// Symbol table
// non-const name copied into symbol-table-entry:
// Entry
//   |
//   [name-ptr | symbol-id | next-ptr | name n-bytes]
//       |                             /
//        ------------points here -----
//
// const name referenced by symbol-table-entry:
// Entry
//   |
//   [name-ptr | symbol-id | next-ptr]
//       |
//        [name n-bytes]
//

int lbm_add_symbol_base(char *name, lbm_uint *id) {
  lbm_uint symbol_name_storage;
  if (!store_symbol_name_flash(name, &symbol_name_storage)) return 0;
  lbm_uint *new_symlist = lbm_image_add_symbol((char*)symbol_name_storage, next_symbol_id, (lbm_uint)symlist);
  if (!new_symlist) {
    return 0;
  }
  symlist = new_symlist;
  *id = next_symbol_id ++;
  return 1;
}

int lbm_add_symbol(char *name, lbm_uint* id) {
  lbm_uint sym_id;
  if (!lbm_get_symbol_by_name(name, &sym_id)) {
    return lbm_add_symbol_base(name, id);
  } else {
    *id = sym_id;
    return 1;
  }
  return 0;
}

// on Linux, win, etc a const string may not be at
// the same address between runs.
int lbm_add_symbol_const_base(char *name, lbm_uint* id, bool link) {
  lbm_uint symbol_name_storage = (lbm_uint)name;
  lbm_uint *new_symlist;
  if (link) {
    new_symlist = lbm_image_add_and_link_symbol((char*)symbol_name_storage, next_symbol_id, (lbm_uint)symlist, id);
  } else {
    new_symlist = lbm_image_add_symbol((char*)symbol_name_storage, next_symbol_id, (lbm_uint)symlist);
  }
  if (new_symlist) {
    symlist = new_symlist;
    *id = next_symbol_id ++;
    return 1;
  }
  return 0;
}

int lbm_add_symbol_const(char *name, lbm_uint* id) {
  lbm_uint sym_id;
  if (!lbm_get_symbol_by_name(name, &sym_id)) {
    return lbm_add_symbol_const_base(name, id, true);
  } else {
    *id = sym_id;
    return 1;
  }
  return 0;
}

int lbm_str_to_symbol(char *name, lbm_uint *sym_id) {
  if (lbm_get_symbol_by_name(name, sym_id))
    return 1;
  else if (lbm_add_symbol(name, sym_id))
    return 1;
  return 0;
}

lbm_uint lbm_get_symbol_table_size(void) {
  return (symbol_table_size_list +symbol_table_size_strings);
}

lbm_uint lbm_get_symbol_table_size_flash(void) {
  return (symbol_table_size_list_flash +
          symbol_table_size_strings_flash) * sizeof(lbm_uint);
}

lbm_uint lbm_get_symbol_table_size_names(void) {
  return symbol_table_size_strings; // Bytes already
}

lbm_uint lbm_get_symbol_table_size_names_flash(void) {
  return symbol_table_size_strings_flash * sizeof(lbm_uint);
}

bool lbm_symbol_in_flash(char *str) {
  return !lbm_memory_ptr_inside((lbm_uint*)str);
}

bool lbm_symbol_list_entry_in_flash(char *str) {
  lbm_uint *entry = lbm_get_symbol_list_entry_by_name(str);
  return (entry == NULL || !lbm_memory_ptr_inside(entry));
}
