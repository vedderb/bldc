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
#define SYM_DEFINE        0x7
#define SYM_PROGN         0x8
#define SYM_READ          0x9
#define SYM_READ_PROGRAM  0xA
#define SYM_DONTCARE      0xB
#define SYM_MATCH         0xC
#define SYM_SEND          0xD
#define SYM_RECEIVE       0xE
#define SYM_MACRO         0xF
#define SYM_MACRO_EXPAND  0x10
#define SYM_CALLCC        0x11
#define SYM_CONT          0x12
#define SYM_SETVAR        0x13

// 0x20 - 0x2F are errors
#define SYM_RERROR        0x20  /* READ ERROR */
#define SYM_TERROR        0x21  /* TYPE ERROR */
#define SYM_EERROR        0x22  /* EVAL ERROR */
#define SYM_MERROR        0x23
#define SYM_NOT_FOUND     0x24
#define SYM_DIVZERO       0x25
#define SYM_FATAL_ERROR   0x26 /* Runtime system is corrupt */
#define SYM_STACK_ERROR   0x27
#define SYM_RECOVERED     0x28


#define TYPE_CLASSIFIER_STARTS 0x30
#define SYM_ARRAY_TYPE     0x30
#define SYM_RAW_I_TYPE     0x31
#define SYM_RAW_U_TYPE     0x32
#define SYM_RAW_F_TYPE     0x33
#define SYM_IND_I_TYPE     0x34
#define SYM_IND_U_TYPE     0x35
#define SYM_IND_F_TYPE     0x36
#define SYM_STREAM_TYPE    0x37
#define SYM_BYTECODE_TYPE  0x38
#define TYPE_CLASSIFIER_ENDS 0x38
#define SYM_NONSENSE       0x39

#define SYM_NO_MATCH       0x3A
#define SYM_MATCH_ANY      0x3B
#define SYM_MATCH_I        0x3C
#define SYM_MATCH_U        0x3D
#define SYM_MATCH_U32      0x3E
#define SYM_MATCH_I32      0x3F
#define SYM_MATCH_FLOAT    0x40
#define SYM_MATCH_CONS     0x41

// Type identifying symbols
#define SYM_TYPE_LIST      0x50
#define SYM_TYPE_I         0x51
#define SYM_TYPE_U         0x52
#define SYM_TYPE_FLOAT     0x53
#define SYM_TYPE_I32       0x54
#define SYM_TYPE_U32       0x55
#define SYM_TYPE_DOUBLE    0x56
#define SYM_TYPE_I64       0x57
#define SYM_TYPE_U64       0x58
#define SYM_TYPE_ARRAY     0x59
#define SYM_TYPE_SYMBOL    0x5A
#define SYM_TYPE_CHAR      0x5B
#define SYM_TYPE_BYTE      0x5C
#define SYM_TYPE_REF       0x5D
#define SYM_TYPE_STREAM    0x5E

//Relevant for the tokenizer
#define SYM_OPENPAR        0x70
#define SYM_CLOSEPAR       0x71
#define SYM_BACKQUOTE      0x72
#define SYM_COMMA          0x73
#define SYM_COMMAAT        0x74
#define SYM_TOKENIZER_DONE 0x75
#define SYM_DOT            0x76
#define SYM_QUOTE_IT       0x77

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
#define SYM_LEQ                 0x109
#define SYM_GEQ                 0x10A
#define SYM_EVAL                0x10B
#define SYM_EVAL_PROGRAM        0x10C
#define SYM_PERFORM_GC          0x10D

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
#define SYM_ARRAY_CREATE        0x132
#define SYM_ARRAY_SIZE          0x133

#define SYM_SYMBOL_TO_STRING    0x140
#define SYM_STRING_TO_SYMBOL    0x141
#define SYM_SYMBOL_TO_UINT      0x142
#define SYM_UINT_TO_SYMBOL      0x143
#define SYM_SET_CAR             0x145
#define SYM_SET_CDR             0x146
#define SYM_SET_IX              0x147

#define SYM_ASSOC               0x148
#define SYM_ACONS               0x149
#define SYM_SET_ASSOC           0x14A

#define SYM_IS_FUNDAMENTAL      0x150

#define SYM_IX                  0x151
#define SYM_ENCODE_I32          0x152
#define SYM_ENCODE_U32          0x153
#define SYM_ENCODE_FLOAT        0x154
#define SYM_DECODE              0x155

#define SYM_TO_I                0x160
#define SYM_TO_I32              0x161
#define SYM_TO_U                0x162
#define SYM_TO_U32              0x163
#define SYM_TO_FLOAT            0x164
#define SYM_TO_I64              0x165
#define SYM_TO_U64              0x166
#define SYM_TO_DOUBLE           0x167
#define SYM_TO_BYTE             0x168

//#define SYM_STREAM_GET          0x160
//#define SYM_STREAM_MORE         0x161
//#define SYM_STREAM_PEEK         0x162
//#define SYM_STREAM_DROP         0x163
//#define SYM_STREAM_PUT          0x164

#define SYM_SHL                 0x170
#define SYM_SHR                 0x171
#define SYM_BITWISE_AND         0x172
#define SYM_BITWISE_OR          0x173
#define SYM_BITWISE_XOR         0x174
#define SYM_BITWISE_NOT         0x175

#define SYM_TYPE_OF             0x200
#define FUNDAMENTALS_END        0x200

#define SPECIAL_SYMBOLS_START    0
#define SPECIAL_SYMBOLS_END      0xFFFF
#define EXTENSION_SYMBOLS_START  0x10000
#define EXTENSION_SYMBOLS_END    0x1FFFF
#define VARIABLE_SYMBOLS_START   0x20000
#define VARIABLE_SYMBOLS_END     0x2FFFF
#define RUNTIME_SYMBOLS_START    0x30000
#define MAX_SYMBOL_VALUE 0x0FFFFFFF


typedef void (*symrepr_name_iterator_fun)(const char *);


/** Initialize the symbol table.
 *
 * \return 1
 */
extern int lbm_symrepr_init(void);
/** Iterate over all symbol names as strings
 *
 * \param symrepr_name_iterator_fun function taking a string
 */
extern void lbm_symrepr_name_iterator(symrepr_name_iterator_fun f);
/** Add a symbol to the symbol table. The symbol name string is copied to arrays and symbols memory.
 *
 * \param name String representation of the symbol.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
extern int lbm_add_symbol(char *name, lbm_uint *id);
/** Add a variable-symbol to the symbol table. The symbol name string is copied to arrays and symbols memory.
 *
 * \param name String representation of the symbol.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
extern int lbm_add_variable_symbol(char *name, lbm_uint* id);
/** Add a symbol to the symbol table. The name is assumed to be a statically allocated string.
 *
 * \param name Statically allocated name string.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
extern int lbm_add_symbol_const(char *name, lbm_uint *id);
/** Add an extension symbol to the symbol table.
 *  The name is assumed to be statically allocated.
 *
 * \param name Statically allocated name string.
 * \param id Resulting id is returned through this argument.
 * \return 1 for success and 0 for failure.
 */
extern int lbm_add_extension_symbol_const(char *name, lbm_uint* id);
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

extern int lbm_get_num_variables(void);

/**
 *
 * \return The amount of space occupied by the symbol table in bytes.
 */
extern lbm_uint lbm_get_symbol_table_size(void);

#endif
