/*
    Copyright 2022 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef LBM_DEFINES_H_
#define LBM_DEFINES_H_


/* ------------------------------------------------------------
   Types
   ------------------------------------------------------------ */

#ifndef LBM64

#define LBM_POINTER_TYPE_FIRST           0x10000000u
#define LBM_TYPE_CONS                    0x10000000u
#define LBM_NON_CONS_POINTER_TYPE_FIRST  0x20000000u
#define LBM_TYPE_U32                     0x28000000u
#define LBM_TYPE_I32                     0x38000000u
#define LBM_TYPE_I64                     0x48000000u
#define LBM_TYPE_U64                     0x58000000u
#define LBM_TYPE_FLOAT                   0x68000000u
#define LBM_TYPE_DOUBLE                  0x78000000u
#define LBM_TYPE_ARRAY                   0x80000000u
#define LBM_TYPE_REF                     0x90000000u
#define LBM_TYPE_CHANNEL                 0xA0000000u
#define LBM_TYPE_CUSTOM                  0xB0000000u
#define LBM_NON_CONS_POINTER_TYPE_LAST   0xB0000000u
#define LBM_POINTER_TYPE_LAST            0xB0000000u


#define LBM_GC_MASK                      0x00000002u
#define LBM_GC_MARKED                    0x00000002u

#define LBM_VAL_MASK                     0xFFFFFFF0u
#define LBM_VAL_TYPE_MASK                0x0000000Cu
                                                     //    gc ptr
#define LBM_TYPE_SYMBOL                  0x00000000u // 00  0   0
#define LBM_TYPE_CHAR                    0x00000004u // 01  0   0
#define LBM_TYPE_BYTE                    0x00000004u
#define LBM_TYPE_U                       0x00000008u // 10  0   0
#define LBM_TYPE_I                       0x0000000Cu // 11  0   0

#else /* 64 bit Version */

#define LBM_POINTER_TYPE_FIRST           (lbm_uint)0x1000000000000000
#define LBM_TYPE_CONS                    (lbm_uint)0x1000000000000000
#define LBM_NON_CONS_POINTER_TYPE_FIRST  (lbm_uint)0x2000000000000000
#define LBM_TYPE_U64                     (lbm_uint)0x2000000000000000
#define LBM_TYPE_I64                     (lbm_uint)0x3000000000000000
#define LBM_TYPE_DOUBLE                  (lbm_uint)0x4000000000000000
#define LBM_TYPE_ARRAY                   (lbm_uint)0x5000000000000000
#define LBM_TYPE_REF                     (lbm_uint)0x6000000000000000
#define LBM_TYPE_CHANNEL                 (lbm_uint)0x7000000000000000
#define LBM_TYPE_CUSTOM                  (lbm_uint)0x8000000000000000
#define LBM_NON_CONS_POINTER_TYPE_LAST   (lbm_uint)0x8000000000000000
#define LBM_POINTER_TYPE_LAST            (lbm_uint)0x8000000000000000

#define LBM_GC_MASK                      (lbm_uint)0x2
#define LBM_GC_MARKED                    (lbm_uint)0x2

/* 8 - 2 free bits to encode type information into */
#define LBM_VAL_MASK                    (lbm_uint)0xFFFFFFFFFFFFFF00
#define LBM_VAL_TYPE_MASK               (lbm_uint)0xFC
//    gc ptr
#define LBM_TYPE_SYMBOL                 (lbm_uint)0x0 // 00 00 00  0   0
#define LBM_TYPE_CHAR                   (lbm_uint)0x4 // 00 00 01  0   0
#define LBM_TYPE_BYTE                   (lbm_uint)0x4
#define LBM_TYPE_U                      (lbm_uint)0x8 // 00 00 10  0   0
#define LBM_TYPE_I                      (lbm_uint)0xC // 00 00 11  0   0
#define LBM_TYPE_U32                    (lbm_uint)0x14// 00 01 01  0   0
#define LBM_TYPE_I32                    (lbm_uint)0x18// 00 01 10  0   0
#define LBM_TYPE_FLOAT                  (lbm_uint)0x1C// 00 01 11  0   0

#endif
/* ------------------------------------------------------------
   Built in symbols
   ------------------------------------------------------------ */

// Default and fixed symbol ids
#define SYM_NIL           0x0
#define SYM_TRUE          0x2
#define SYM_DONTCARE      0x9

// Consecutive value symbols for lookup-application
#define APPLY_FUNS_START  0x10
#define SYM_SETVAR        0x10
#define SYM_READ          0x11
#define SYM_READ_PROGRAM  0x12
#define SYM_SPAWN         0x13
#define SYM_SPAWN_TRAP    0x14
#define SYM_YIELD         0x15
#define SYM_WAIT          0x16
#define SYM_EVAL          0x17
#define SYM_EVAL_PROGRAM  0x18
#define SYM_SEND          0x19
#define SYM_EXIT_OK       0x1A
#define SYM_EXIT_ERROR    0x1B
#define APPLY_FUNS_END    0x1B



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
#define SYM_CHANNEL_TYPE   0x37
#define SYM_BYTECODE_TYPE  0x38
#define SYM_CUSTOM_TYPE    0x39
#define TYPE_CLASSIFIER_ENDS 0x39
#define SYM_NONSENSE       0x3A

#define SYM_NO_MATCH       0x40
#define SYM_MATCH_ANY      0x41
#define SYM_MATCH_I        0x42
#define SYM_MATCH_U        0x43
#define SYM_MATCH_U32      0x44
#define SYM_MATCH_I32      0x45
#define SYM_MATCH_FLOAT    0x46
#define SYM_MATCH_CONS     0x47
#define SYM_MATCH_U64      0x48
#define SYM_MATCH_I64      0x49
#define SYM_MATCH_DOUBLE   0x4A

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
#define SYM_TYPE_CHANNEL   0x5E

//Relevant for the tokenizer
#define SYM_OPENPAR          0x70
#define SYM_CLOSEPAR         0x71
#define SYM_BACKQUOTE        0x72
#define SYM_COMMA            0x73
#define SYM_COMMAAT          0x74
#define SYM_TOKENIZER_DONE   0x75
#define SYM_DOT              0x76
#define SYM_QUOTE_IT         0x77
#define SYM_COLON            0x78
#define SYM_TOKENIZER_WAIT   0x79
#define SYM_OPENBRACK        0x80
#define SYM_CLOSEBRACK       0x81
#define SYM_TOKENIZER_RERROR 0x82


// Built in special forms - consecutive for lookup-application
#define SPECIAL_FORMS_START     0x100
#define SYM_QUOTE               0x100
#define SYM_DEFINE              0x101
#define SYM_PROGN               0x102
#define SYM_LAMBDA              0x103
#define SYM_IF                  0x104
#define SYM_LET                 0x105
#define SYM_AND                 0x106
#define SYM_OR                  0x107
#define SYM_MATCH               0x108
#define SYM_RECEIVE             0x109
#define SYM_CALLCC              0x10A
#define SYM_ATOMIC              0x10B
#define SYM_MACRO               0x10C
#define SYM_CONT                0x10D
#define SYM_CLOSURE             0x10E
#define SPECIAL_FORMS_END       0x10E

// Fundamental Operations
// Consecutive values for lookup-application
#define FUNDAMENTALS_START      0x200
#define SYM_ADD                 0x200
#define SYM_SUB                 0x201
#define SYM_MUL                 0x202
#define SYM_DIV                 0x203
#define SYM_MOD                 0x204
#define SYM_EQ                  0x205
#define SYM_NUMEQ               0x206
#define SYM_LT                  0x207
#define SYM_GT                  0x208
#define SYM_LEQ                 0x209
#define SYM_GEQ                 0x20A
#define SYM_NOT                 0x20B
#define SYM_PERFORM_GC          0x20C
#define SYM_SELF                0x20D
#define SYM_SET_MAILBOX_SIZE    0x20E
#define SYM_CONS                0x20F
#define SYM_CAR                 0x210
#define SYM_CDR                 0x211
#define SYM_LIST                0x212
#define SYM_APPEND              0x213
#define SYM_UNDEFINE            0x214
#define SYM_ARRAY_READ          0x215
#define SYM_ARRAY_WRITE         0x216
#define SYM_ARRAY_CREATE        0x217
#define SYM_ARRAY_SIZE          0x218
#define SYM_ARRAY_CLEAR         0x219
#define SYM_SYMBOL_TO_STRING    0x21A
#define SYM_STRING_TO_SYMBOL    0x21B
#define SYM_SYMBOL_TO_UINT      0x21C
#define SYM_UINT_TO_SYMBOL      0x21D
#define SYM_SET_CAR             0x21E
#define SYM_SET_CDR             0x21F
#define SYM_SET_IX              0x220
#define SYM_ASSOC               0x221
#define SYM_ACONS               0x222
#define SYM_SET_ASSOC           0x223
#define SYM_COSSA               0x224
#define SYM_IS_FUNDAMENTAL      0x225
#define SYM_IX                  0x226
#define SYM_ENCODE_I32          0x227
#define SYM_ENCODE_U32          0x228
#define SYM_ENCODE_FLOAT        0x229
#define SYM_DECODE              0x22A
#define SYM_TO_I                0x22B
#define SYM_TO_I32              0x22C
#define SYM_TO_U                0x22D
#define SYM_TO_U32              0x22E
#define SYM_TO_FLOAT            0x22F
#define SYM_TO_I64              0x230
#define SYM_TO_U64              0x231
#define SYM_TO_DOUBLE           0x232
#define SYM_TO_BYTE             0x233
#define SYM_SHL                 0x234
#define SYM_SHR                 0x235
#define SYM_BITWISE_AND         0x236
#define SYM_BITWISE_OR          0x237
#define SYM_BITWISE_XOR         0x238
#define SYM_BITWISE_NOT         0x239
#define SYM_CUSTOM_DESTRUCT     0x23A /* run the destructor of a custom type */
#define SYM_TYPE_OF             0x23B
#define FUNDAMENTALS_END        0x23B

#define SPECIAL_SYMBOLS_START    0
#define SPECIAL_SYMBOLS_END      0xFFFF
#define EXTENSION_SYMBOLS_START  0x10000
#define EXTENSION_SYMBOLS_END    0x1FFFF
#define VARIABLE_SYMBOLS_START   0x20000
#define VARIABLE_SYMBOLS_END     0x2FFFF
#define RUNTIME_SYMBOLS_START    0x30000
#define MAX_SYMBOL_VALUE 0x0FFFFFFF

/* ------------------------------------------------------------
   Encoded Symbols
   ------------------------------------------------------------ */

#define ENC_SYM_STACK_ERROR ((SYM_STACK_ERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_MERROR      ((SYM_MERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_RERROR      ((SYM_RERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_EERROR      ((SYM_EERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TERROR      ((SYM_TERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_FATAL_ERROR ((SYM_FATAL_ERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_DIVZERO     ((SYM_DIVZERO << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_NOT_FOUND   ((SYM_NOT_FOUND << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_NONSENSE    ((SYM_NONSENSE << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_NO_MATCH    ((SYM_NO_MATCH << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_NIL         ((SYM_NIL << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_RECOVERED   ((SYM_RECOVERED << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TRUE        ((SYM_TRUE << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_ARRAY_TYPE  ((SYM_ARRAY_TYPE << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_READ        ((SYM_READ << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_READ_PROGRAM ((SYM_READ_PROGRAM << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_EVAL        ((SYM_EVAL << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_EVAL_PROGRAM ((SYM_EVAL_PROGRAM << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_CONT        ((SYM_CONT << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_CLOSURE     ((SYM_CLOSURE << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_MACRO       ((SYM_MACRO << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_SETVAR      ((SYM_SETVAR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_EXIT_OK     ((SYM_EXIT_OK << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_EXIT_ERROR  ((SYM_EXIT_ERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_SPAWN       ((SYM_SPAWN << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_YIELD       ((SYM_YIELD << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_WAIT        ((SYM_WAIT << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_SEND        ((SYM_SEND << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_SPAWN_TRAP  ((SYM_SPAWN_TRAP << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_CONS        ((SYM_CONS << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_CAR         ((SYM_CAR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_CDR         ((SYM_CDR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_LIST        ((SYM_LIST << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_APPEND      ((SYM_APPEND << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_QUOTE       ((SYM_QUOTE << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_COMMA       ((SYM_COMMA << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_COMMAAT     ((SYM_COMMAAT << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_CLOSEPAR    ((SYM_CLOSEPAR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TOKENIZER_DONE ((SYM_TOKENIZER_DONE << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TOKENIZER_RERROR ((SYM_TOKENIZER_RERROR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_TYPE_LIST   ((SYM_TYPE_LIST << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_ARRAY  ((SYM_TYPE_ARRAY << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_I32    ((SYM_TYPE_I32 << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_U32    ((SYM_TYPE_U32 << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_FLOAT  ((SYM_TYPE_FLOAT << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_I64    ((SYM_TYPE_I64 << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_U64    ((SYM_TYPE_U64 << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_DOUBLE ((SYM_TYPE_DOUBLE << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_I      ((SYM_TYPE_I << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_U      ((SYM_TYPE_U << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_CHAR   ((SYM_TYPE_CHAR << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)
#define ENC_SYM_TYPE_SYMBOL ((SYM_TYPE_SYMBOL << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#define ENC_SYM_COLON       ((SYM_COLON << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL)

#endif
