/*
    Copyright 2018, 2021, 2022 Joel Svensson   svenssonjoel@yahoo.se
                          2022 Benjamin Vedder

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

/** \file symrepr.h
 *  symrepr.h implements the symbol table and symbol creation and lookup functions.
 *
 *  The symbol table is implemented as a linked list in the arrays and symbols
 *  memory defined in lispbm_memory.h. So lbm_memory_init must be run before
 *  the symbol table is initialized and used.
 *
 */

#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h>
#include <stdbool.h>

#include "lbm_types.h"

// Default and fixed symbol ids
#define SYM_NIL           0x0
#define SYM_QUOTE         0x1
#define SYM_TRUE          0x2
#define SYM_IF            0x3
#define SYM_LAMBDA        0x4
#define SYM_CLOSURE       0x5
#define SYM_LET           0x6
#define SYM_RERROR        0x7   /* READ ERROR */
#define SYM_TERROR        0x8   /* TYPE ERROR */
#define SYM_EERROR        0x9   /* EVAL ERROR */
#define SYM_MERROR        0xA
#define SYM_DIVZERO       0xB
#define SYM_FATAL_ERROR   0xC  /* Runtime system is corrupt */
#define SYM_STACK_ERROR   0xD
#define SYM_DEFINE        0xE
#define SYM_PROGN         0xF
#define SYM_READ          0x10
#define SYM_READ_PROGRAM  0x11
#define SYM_DONTCARE      0x12
#define SYM_MATCH         0x13
#define SYM_SEND          0x14
#define SYM_RECEIVE       0x15

#define SYM_ARRAY_TYPE     0x20
#define SYM_BOXED_I_TYPE   0x21
#define SYM_BOXED_U_TYPE   0x22
#define SYM_BOXED_F_TYPE   0x23
#define SYM_REF_TYPE       0x24
#define SYM_RECOVERED      0x26
#define SYM_BYTECODE_TYPE  0x27
#define SYM_NONSENSE       0x28
#define SYM_NOT_FOUND      0x29
#define SYM_NO_MATCH       0x2A
#define SYM_MATCH_ANY      0x2B
#define SYM_MATCH_I28      0x2C
#define SYM_MATCH_U28      0x2D
#define SYM_MATCH_U32      0x2E
#define SYM_MATCH_I32      0x2F
#define SYM_MATCH_FLOAT    0x30
#define SYM_MATCH_CONS     0x31

// Type identifying symbols
#define SYM_TYPE_LIST      0x50
#define SYM_TYPE_I28       0x51
#define SYM_TYPE_U28       0x52
#define SYM_TYPE_FLOAT     0x53
#define SYM_TYPE_I32       0x54
#define SYM_TYPE_U32       0x55
#define SYM_TYPE_ARRAY     0x56
#define SYM_TYPE_SYMBOL    0x57
#define SYM_TYPE_CHAR      0x58
#define SYM_TYPE_REF       0x59
#define SYM_TYPE_STREAM    0x5A

//Relevant for the tokenizer
#define SYM_OPENPAR        0x70
#define SYM_CLOSEPAR       0x71
#define SYM_BACKQUOTE      0x72
#define SYM_COMMA          0x73
#define SYM_COMMAAT        0x74
#define SYM_TOKENIZER_DONE 0x75
#define SYM_DOT            0x76

// Fundamental Operations
#define FUNDAMENTALS_START      0x100
#define SYM_ADD                 0x100
#define SYM_SUB                 0x101
#define SYM_MUL                 0x102
#define SYM_DIV                 0x103
#define SYM_MOD                 0x104
#define SYM_EQ                  0x105
#define SYM_NUMEQ               0x106
#define SYM_LT                  0x107
#define SYM_GT                  0x108
#define SYM_EVAL                0x109
#define SYM_EVAL_PROGRAM        0x10A

#define SYM_AND                 0x110
#define SYM_OR                  0x111
#define SYM_NOT                 0x112

#define SYM_YIELD               0x113
#define SYM_WAIT                0x114
#define SYM_SPAWN               0x115

#define SYM_CONS                0x120
#define SYM_CAR                 0x121
#define SYM_CDR                 0x122
#define SYM_LIST                0x123
#define SYM_APPEND              0x124

#define SYM_ARRAY_READ          0x130
#define SYM_ARRAY_WRITE         0x131
//#define SYM_ARRAY_CREATE        0x132

#define SYM_SYMBOL_TO_STRING    0x140
#define SYM_STRING_TO_SYMBOL    0x141
#define SYM_SYMBOL_TO_UINT      0x142
#define SYM_UINT_TO_SYMBOL      0x143
#define SYM_SET_CAR             0x145
#define SYM_SET_CDR             0x146

#define SYM_IS_FUNDAMENTAL      0x150

#define SYM_IX                  0x151
#define SYM_ENCODE_I32          0x152
#define SYM_ENCODE_U32          0x153
#define SYM_ENCODE_FLOAT        0x154
#define SYM_DECODE              0x155

//#define SYM_STREAM_GET          0x160
//#define SYM_STREAM_MORE         0x161
//#define SYM_STREAM_PEEK         0x162
//#define SYM_STREAM_DROP         0x163
//#define SYM_STREAM_PUT          0x164

#define SYM_TYPE_OF             0x200
#define FUNDAMENTALS_END        0x200

#define MAX_SPECIAL_SYMBOLS 4096 // 12bits (highest id allowed is 0xFFFF)

/** Initialize the symbol table.
 *
 * \return 1
 */
extern int lbm_symrepr_init(void);
/** Add a symbol to the symbol table. The symbol name string is copied to arrays and symbols memory.
 *
 * \param name String representation of the symbol.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
extern int lbm_add_symbol(char *name, lbm_uint *id);
/** Add a symbol to the symbol table. The name is assumed to be a statically allocated string.
 *
 * \param name Statically allocated name string.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
extern int lbm_add_symbol_const(char *name, lbm_uint *id);
/** Look up an id from the symbol table given a name.
 *
 * \param name Name string to look up.
 * \param id Resulting id is returned through this argument.
 * \return 1 on success (name was found) and 0 for failure.
 */
extern int lbm_get_symbol_by_name(char *name, lbm_uint *id);
/** Look up a symbol name from the symbol table given an id.
 *
 * \param id The id to look up in the symbol table.
 * \return pointer to the name string if success otherwise NULL.
 */
extern const char* lbm_get_name_by_symbol(lbm_uint id);

/**
 *
 * \return The amount of space occupied by the symbol table in bytes.
 */
extern unsigned int lbm_get_symbol_table_size(void);

static inline bool lbm_is_error(lbm_uint symrep){
  return (symrep == SYM_RERROR ||
          symrep == SYM_TERROR ||
          symrep == SYM_RERROR ||
          symrep == SYM_MERROR ||
          symrep == SYM_EERROR ||
          symrep == SYM_FATAL_ERROR);
}


#endif
