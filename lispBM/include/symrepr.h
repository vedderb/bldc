/*
    Copyright 2018 2021 Joel Svensson   svenssonjoel@yahoo.se

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

#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <stdint.h>
#include <stdbool.h>

#include "lispbm_types.h"

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
#define SYM_DEFINE        0xD
#define SYM_PROGN         0xE
//#define SYM_BACKQUOTE     0xF
#define SYM_COMMA         0x10
#define SYM_COMMAAT       0x11
#define SYM_DONTCARE      0x12
#define SYM_MATCH         0x13
#define SYM_SEND          0x14
#define SYM_RECEIVE       0x15

#define SYM_ARRAY_TYPE     0x20
#define SYM_BOXED_I_TYPE   0x21
#define SYM_BOXED_U_TYPE   0x22
#define SYM_BOXED_F_TYPE   0x23
#define SYM_REF_TYPE       0x24
#define SYM_STREAM_TYPE    0x25
#define SYM_RECOVERED      0x26
#define SYM_BYTECODE_TYPE  0x27
#define SYM_NONSENSE       0x28
#define SYM_NOT_FOUND      0x29
#define SYM_NO_MATCH       0x2A
#define SYM_MATCH_ANY      0x2B
#define SYM_MATCH_I28      0x2C
#define SYM_MATCH_U28      0x2D
#define SYM_MATCH_FLOAT    0x2E
#define SYM_MATCH_CONS     0x2F

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

#define SYM_SYMBOL_TO_STRING    0x140
#define SYM_STRING_TO_SYMBOL    0x141
#define SYM_SYMBOL_TO_UINT      0x142
#define SYM_UINT_TO_SYMBOL      0x143
#define SYM_MK_SYMBOL_INDIRECT  0x144
#define SYM_SET_CAR             0x145
#define SYM_SET_CDR             0x146

#define SYM_IS_FUNDAMENTAL      0x150

#define SYM_TYPE_OF             0x200
#define FUNDAMENTALS_END        0x200

#define MAX_SPECIAL_SYMBOLS 4096 // 12bits (highest id allowed is 0xFFFF)

extern int symrepr_addsym(char *, UINT*);
int symrepr_addsym_const(char *name, UINT* id);
extern bool symrepr_init(void);
extern int symrepr_lookup(char *, UINT*);
extern const char* symrepr_lookup_name(UINT);
extern void symrepr_del(void);

extern unsigned int symrepr_size(void);

static inline bool symrepr_is_error(UINT symrep){
  return (symrep == SYM_RERROR ||
          symrep == SYM_TERROR ||
          symrep == SYM_RERROR ||
          symrep == SYM_MERROR ||
          symrep == SYM_EERROR ||
          symrep == SYM_FATAL_ERROR);
}


#endif
