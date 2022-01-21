/*
    Copyright 2018 , 2022 Joel Svensson        svenssonjoel@yahoo.se

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

#ifndef HEAP_H_
#define HEAP_H_

#include <string.h>
#include "lispbm_types.h"
#include "symrepr.h"
#include "streams.h"

/*
Planning for a more space efficient heap representation.
TODO: Need to find a good reference to read up on this.
      - List based heap
      - Easy to implement and somewhat efficient

0000 0000  Size    Free bits
003F FFFF  4MB      10
007F FFFF  8MB       9
00FF FFFF  16MB      8
01FF FFFF  32MB      7
03FF FFFF  64MB      6         * Kind of heap size I am looking for
07FF FFFF  128MB     5
0FFF FFFF  256MB     4
1FFF FFFF  512MB     3


--- May 9 2021 ---
Actually now I am much more interested in way smaller memories ;)

0000 0000  Size    Free bits
0000 0FFF   4KB     20          |
0000 1FFF   8KB     19          |
0000 3FFF  16KB     18          |
0000 7FFF  32KB     17          |
0000 FFFF  64KB     16          |
0001 FFFF  128KB    15          |
0003 FFFF  256KB    14          | - This range is very interesting.
0007 FFFF  512KB    13
000F FFFF  1MB      12
001F FFFF  2MB      11
003F FFFF  4MB      10
007F FFFF  8MB       9
00FF FFFF  16MB      8
01FF FFFF  32MB      7
03FF FFFF  64MB      6
07FF FFFF  128MB     5
0FFF FFFF  256MB     4
1FFF FFFF  512MB     3

Those are the kind of platforms that are fun... so a bunch of
wasted bits in heap pointers if we run on small MCUs.

-----------------

it is also the case that not all addresses will be used if all "cells" are
of the same size, 8 bytes...

value 0: 0000 0000
value 1: 0000 0008
value 3: 0000 0010
value 4: 0000 0018

Means bits 0,1,2 will always be empty in a valid address.

Cons cells also need to be have room for 2 pointers. So each allocated cell from
memory should be 8bytes.

Things that needs to be represented within these bits:

 - GC MARK one per cell
 - TYPE: type of CAR and type of cons

Types I would want:
 - Full 32bit integer. Does not leave room for identification of type
 - Float values.  Same problem


Free bits in pointers 64MB heap:
31 30 29 28 27 26                               2 1 0
0  0  0  0  0  0  XX XXXX XXXX XXXX XXXX XXXX X 0 0 0


Information needed for each cell:
 Meaning  |   bits total | bits per car | bits per cdr
 GC mark  |    2         |   1          |   1          - only one of them will be used (the other is wasted)   
 Type     |    2x        |   x          |   x
 Ptr/!ptr |    2         |   1          |   1


Types (unboxed):
 - Symbols
 - 28bit integer   ( will need signed shift right functionality )
 - 28bit unsigned integer
 - Character

If four types is all that should be possible (unboxed). then 2 bits are needed to differentiate.  
2 + 1 + 1 = 4 => 28bits for data.

bit 0: ptr/!ptr
bit 1: gc
bit 2-3: type (if not ptr)
bit 3 - 24 ptr (if ptr)
bit 4 - 31 value (if value)

An unboxed value can occupy a car or cdr field in a cons cell.

types (boxed) extra information in pointer to cell can contain information
 - 32 bit integer
 - 32 bit unsigned integer
 - 32 bit float

boxed representation:
  [ptr| cdr]
    |
  [Value | Aux + GC_MARK]

Kinds of pointers:
 - Pointer to cons cell.
 - Pointer to unboxed value  (fixnums not in a list, I hope this is so rare that it can be removed )  
   - integer
   - unsigned integer
   - symbol
   - float
 - Pointer to boxed value.
   - 32 bit integer
   - 32 bit unsigned integer
   - 32 bit float
 - (Maybe something else ? Vectors/strings allocated in memory not occupied by heap?)
   - vector of int
   - vector of uint
   - vector of float
   - vector of double
   - String

13 pointer"types" -> needs 4 bits
for 64MB heap there are 6 free bits. So with this scheme going to 128MB or 256MB heap 
is also possible

 a pointer to some off heap vector/string could be represented by

 [ptr | cdr]
   |
 [full pointer | Aux + GC_MARK]
   |
 [VECTOR]

Aux bits could be used for storing vector size. Up to 30bits should be available there
>> This is problematic. Now the information that something is a vector is split up 
>> between 2 cons cells. This means GC needs both of these intact to be able to make 
>> proper decision.
>> Will try to resolve this by adding some special symbols. But these must be symbols 
>> that cannot occur normally in programs. Then an array could be:

 [Full pointer | ARRAY_SYM + GC_MARK]
     |
 [VECTOR]

>> Boxed values same treatment as above.
>> TODO: Could this be simpler?

[ VALUE | TYPE_SYM + GC_MARK]


0000 00XX XXXX XXXX XXXX XXXX XXXX X000   : 0x03FF FFF8
1111 AA00 0000 0000 0000 0000 0000 0000   : 0xFC00 0000 (AA bits left unused for now, future heap growth?)
 */

#define LBM_CONS_CELL_SIZE              8
#define LBM_ADDRESS_SHIFT               3
#define LBM_VAL_SHIFT                   4

#define LBM_PTR_MASK                    0x00000001u
#define LBM_PTR_BIT                     0x00000001u
#define LBM_PTR_VAL_MASK                0x03FFFFF8u
#define LBM_PTR_TYPE_MASK               0xFC000000u

#define LBM_PTR_TYPE_CONS               0x10000000u
#define LBM_PTR_TYPE_BOXED_U            0x20000000u
#define LBM_PTR_TYPE_BOXED_I            0x30000000u
#define LBM_PTR_TYPE_BOXED_F            0x40000000u

#define LBM_PTR_TYPE_ARRAY              0xD0000000u
#define LBM_PTR_TYPE_REF                0xE0000000u
#define LBM_PTR_TYPE_STREAM             0xF0000000u

#define LBM_GC_MASK                     0x00000002u
#define LBM_GC_MARKED                   0x00000002u

#define LBM_VAL_MASK                    0xFFFFFFF0u
#define LBM_VAL_TYPE_MASK               0x0000000Cu
                                                //    gc ptr
#define LBM_VAL_TYPE_SYMBOL             0x00000000u // 00  0   0
#define LBM_VAL_TYPE_CHAR               0x00000004u // 01  0   0
#define LBM_VAL_TYPE_U                  0x00000008u // 11  0   0
#define LBM_VAL_TYPE_I                  0x0000000Cu // 10  0   0

typedef struct {
  lbm_value car;
  lbm_value cdr;
} lbm_cons_t;

typedef struct {
  lbm_cons_t  *heap;
  bool  malloced;           // allocated by heap_init
  lbm_value freelist;           // list of free cons cells.

  unsigned int heap_size;          // In number of cells.
  unsigned int heap_bytes;         // In bytes.

  unsigned int num_alloc;          // Number of cells allocated.
  unsigned int num_alloc_arrays;   // Number of arrays allocated.

  unsigned int gc_num;             // Number of times gc has been performed.
  unsigned int gc_marked;          // Number of cells marked by mark phase.
  unsigned int gc_recovered;       // Number of cells recovered by sweep phase.
  unsigned int gc_recovered_arrays;// Number of arrays recovered by sweep.
} lbm_heap_state_t;

typedef struct {
  lbm_type elt_type;            // Type of elements: VAL_TYPE_FLOAT, U, I or CHAR
  uint32_t size;            // Number of elements
} lbm_array_header_t;

extern int lbm_heap_init(lbm_cons_t *addr, unsigned int num_cells);
extern unsigned int lbm_heap_num_free(void);
extern unsigned int lbm_heap_num_allocated(void);
extern unsigned int lbm_heap_size(void);
extern lbm_value lbm_heap_allocate_cell(lbm_type type);
extern unsigned int lbm_heap_size_bytes(void);

extern char *lbm_dec_str(lbm_value);
extern lbm_stream_t *lbm_dec_stream(lbm_value val);
extern lbm_uint lbm_dec_as_u(lbm_value);
extern lbm_int lbm_dec_as_i(lbm_value);
extern lbm_float lbm_dec_as_f(lbm_value);

extern lbm_value lbm_cons(lbm_value car, lbm_value cdr);
extern lbm_value lbm_car(lbm_value cons);
extern lbm_value lbm_cdr(lbm_value cons);
extern bool lbm_set_car(lbm_value c, lbm_value v);
extern bool lbm_set_cdr(lbm_value c, lbm_value v);

// List functions
extern unsigned int lbm_list_length(lbm_value c);
extern lbm_value lbm_list_reverse(lbm_value list);
extern lbm_value lbm_list_copy(lbm_value list);
extern lbm_value lbm_list_append(lbm_value list1, lbm_value list2);


// State and statistics
extern void lbm_get_heap_state(lbm_heap_state_t *);

// Garbage collection
extern int lbm_perform_gc(lbm_value env);
extern int lbm_perform_gc_aux(lbm_value env, lbm_value env2, lbm_value exp, lbm_value exp2, lbm_value exp3, lbm_uint *aux_data, unsigned int aux_size);
extern void lbm_gc_state_inc(void);
extern int lbm_gc_mark_freelist(void);
extern int lbm_gc_mark_phase(lbm_value v);
extern int lbm_gc_mark_aux(lbm_uint *data, unsigned int n);
extern int lbm_gc_sweep_phase(void);


// Array functionality
extern int lbm_heap_allocate_array(lbm_value *res, unsigned int size, lbm_type type);

static inline lbm_type lbm_type_of(lbm_value x) {
  return (x & LBM_PTR_MASK) ? (x & LBM_PTR_TYPE_MASK) : (x & LBM_VAL_TYPE_MASK);
}

static inline bool lbm_is_ptr(lbm_value x) {
  return (x & LBM_PTR_MASK);
}

static inline lbm_value lbm_enc_cons_ptr(lbm_uint x) {
  return ((x << LBM_ADDRESS_SHIFT) | LBM_PTR_TYPE_CONS | LBM_PTR_BIT);
}

static inline lbm_uint lbm_dec_ptr(lbm_value p) {
  return ((LBM_PTR_VAL_MASK & p) >> LBM_ADDRESS_SHIFT);
}

static inline lbm_value lbm_set_ptr_type(lbm_value p, lbm_type t) {
  return (LBM_PTR_VAL_MASK & p) | t | LBM_PTR_BIT;
}

static inline lbm_value lbm_enc_sym(uint32_t s) {
  return (s << LBM_VAL_SHIFT) | LBM_VAL_TYPE_SYMBOL;
}

static inline lbm_value lbm_enc_i(lbm_int x) {
  return ((lbm_uint)x << LBM_VAL_SHIFT) | LBM_VAL_TYPE_I;
}

static inline lbm_value lbm_enc_u(lbm_uint x) {
  return (x << LBM_VAL_SHIFT) | LBM_VAL_TYPE_U;
}

static inline lbm_value lbm_enc_I(lbm_int x) {
  lbm_value i = lbm_cons((lbm_uint)x, lbm_enc_sym(SYM_BOXED_I_TYPE));
  if (lbm_type_of(i) == LBM_VAL_TYPE_SYMBOL) return i;
  return lbm_set_ptr_type(i, LBM_PTR_TYPE_BOXED_I);
}

static inline lbm_value lbm_enc_U(lbm_uint x) {
  lbm_value u = lbm_cons(x, lbm_enc_sym(SYM_BOXED_U_TYPE));
  if (lbm_type_of(u) == LBM_VAL_TYPE_SYMBOL) return u;
  return lbm_set_ptr_type(u, LBM_PTR_TYPE_BOXED_U);
}

static inline lbm_value lbm_enc_F(lbm_float x) {
  lbm_uint t;
  memcpy(&t, &x, sizeof(float));
  lbm_value f = lbm_cons(t, lbm_enc_sym(SYM_BOXED_F_TYPE));
  if (lbm_type_of(f) == LBM_VAL_TYPE_SYMBOL) return f;
  return lbm_set_ptr_type(f, LBM_PTR_TYPE_BOXED_F);
}

static inline lbm_value lbm_enc_char(char x) {
  return ((lbm_uint)x << LBM_VAL_SHIFT) | LBM_VAL_TYPE_CHAR;
}

static inline lbm_int lbm_dec_i(lbm_value x) {
  return (lbm_int)x >> LBM_VAL_SHIFT;
}

static inline lbm_uint lbm_dec_u(lbm_value x) {
  return x >> LBM_VAL_SHIFT;
}

static inline char lbm_dec_char(lbm_value x) {
  return (char)(x >> LBM_VAL_SHIFT);
}

static inline lbm_uint lbm_dec_sym(lbm_value x) {
  return x >> LBM_VAL_SHIFT;
}

static inline lbm_float lbm_dec_F(lbm_value x) { // Use only when knowing that x is a VAL_TYPE_F
  lbm_float f_tmp;
  lbm_uint tmp = lbm_car(x);
  memcpy(&f_tmp, &tmp, sizeof(lbm_float));
  return f_tmp;
}

static inline lbm_uint lbm_dec_U(lbm_value x) {
  return lbm_car(x);
}

static inline lbm_int lbm_dec_I(lbm_value x) {
  return (lbm_int)lbm_car(x);
}

static inline lbm_value lbm_set_gc_mark(lbm_value x) {
  return x | LBM_GC_MARKED;
}

static inline lbm_value lbm_clr_gc_mark(lbm_value x) {
  return x & ~LBM_GC_MASK;
}

static inline bool lbm_get_gc_mark(lbm_value x) {
  return x & LBM_GC_MASK;
}

static inline bool lbm_is_number(lbm_value x) {
  lbm_uint t = lbm_type_of(x);
  return ((t == LBM_VAL_TYPE_I) ||
          (t == LBM_VAL_TYPE_U) ||
          (t == LBM_PTR_TYPE_BOXED_I) ||
          (t == LBM_PTR_TYPE_BOXED_U) ||
          (t == LBM_PTR_TYPE_BOXED_F));
}

static inline bool lbm_is_special(lbm_value symrep) {
  return ((lbm_type_of(symrep) == LBM_VAL_TYPE_SYMBOL) &&
          (lbm_dec_sym(symrep) < MAX_SPECIAL_SYMBOLS));
}

static inline bool lbm_is_fundamental(lbm_value symrep) {
  return ((lbm_type_of(symrep) == LBM_VAL_TYPE_SYMBOL)  &&
          (lbm_dec_sym(symrep) >= FUNDAMENTALS_START) &&
          (lbm_dec_sym(symrep) <= FUNDAMENTALS_END));
}

static inline bool lbm_is_closure(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_PTR_TYPE_CONS) &&
          (lbm_type_of(lbm_car(exp)) == LBM_VAL_TYPE_SYMBOL) &&
          (lbm_dec_sym(lbm_car(exp)) == SYM_CLOSURE));
}

static inline bool lbm_is_match_binder(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_PTR_TYPE_CONS) &&
          (lbm_type_of(lbm_car(exp)) == LBM_VAL_TYPE_SYMBOL) &&
          ((lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_ANY) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_I28) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_U28) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_FLOAT) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_CONS)));
}

static inline bool lbm_is_symbol(lbm_value exp) {
  return (lbm_type_of(exp) == LBM_VAL_TYPE_SYMBOL);
}

static inline bool lbm_is_symbol_nil(lbm_value exp) {
  return (lbm_is_symbol(exp) && lbm_dec_sym(exp) == SYM_NIL);
}

static inline bool lbm_is_symbol_eval(lbm_value exp) {
  return (lbm_is_symbol(exp) && lbm_dec_sym(exp) == SYM_EVAL);
}

static inline bool lbm_is_symbol_merror(lbm_value exp) {
  return (lbm_is_symbol(exp) && lbm_dec_sym(exp) == SYM_MERROR);
}
#endif
