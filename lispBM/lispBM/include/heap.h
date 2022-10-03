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
/** \file heap.h */

#ifndef HEAP_H_
#define HEAP_H_

#include <string.h>

#include "lbm_types.h"
#include "symrepr.h"
#include "stack.h"
#include "lbm_memory.h"
#include "lbm_defines.h"
#include "lbm_channel.h"

#ifdef __cplusplus
extern "C" {
#endif

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
#ifndef LBM64

#define LBM_ADDRESS_SHIFT               2
#define LBM_VAL_SHIFT                   4

#define LBM_PTR_MASK                     0x00000001u
#define LBM_PTR_BIT                      0x00000001u
#define LBM_PTR_VAL_MASK                 0x03FFFFFCu
#define LBM_PTR_TYPE_MASK                0xFC000000u

#else /* 64 bit Version */

#define LBM_ADDRESS_SHIFT                2
#define LBM_VAL_SHIFT                    8

#define LBM_PTR_MASK                     (lbm_uint)0x1
#define LBM_PTR_BIT                      (lbm_uint)0x1
#define LBM_PTR_VAL_MASK                 (lbm_uint)0x03FFFFFFFFFFFFFC
#define LBM_PTR_TYPE_MASK                (lbm_uint)0xFC00000000000000

#endif

/** Struct representing a heap cons-cell.
 *
 */
typedef struct {
  lbm_value car;
  lbm_value cdr;
} lbm_cons_t;

/**
 *  Heap state
 */
typedef struct {
  lbm_cons_t *heap;
  lbm_value freelist;          // list of free cons cells.
  lbm_stack_t gc_stack;

  lbm_uint heap_size;          // In number of cells.
  lbm_uint heap_bytes;         // In bytes.

  lbm_uint num_alloc;          // Number of cells allocated.
  lbm_uint num_alloc_arrays;   // Number of arrays allocated.

  lbm_uint gc_num;             // Number of times gc has been performed.
  lbm_uint gc_marked;          // Number of cells marked by mark phase.
  lbm_uint gc_recovered;       // Number of cells recovered by sweep phase.
  lbm_uint gc_recovered_arrays;// Number of arrays recovered by sweep.
  lbm_uint gc_least_free;      // The smallest length of the freelist.
  lbm_uint gc_last_free;       // Number of elements on the freelist
                               // after most recent GC.

  lbm_uint gc_time_acc;
  lbm_uint gc_min_duration;
  lbm_uint gc_max_duration;
} lbm_heap_state_t;

extern lbm_heap_state_t lbm_heap_state;

/**
 *  The header portion of an array stored in array and symbol memory.
 */
typedef struct {
  lbm_type elt_type;        /// Type of elements: VAL_TYPE_FLOAT, U, I or CHAR
  lbm_uint size;            /// Number of elements
  lbm_uint *data;           /// pointer to lbm_memory array or C array.
} lbm_array_header_t;

/** Initialize heap storage.
 * \param addr Pointer to an array of lbm_cons_t elements. This array must at least be aligned 4.
 * \param num_cells Number of lbm_cons_t elements in the array.
 * \param gc_stack_storage uint32_t pointer to space to use as "recursion" stack for GC
 * \param gc_stack_size Size of the gc_stack in number of words.
 * \return 1 on success or 0 for failure.
 */
int lbm_heap_init(lbm_cons_t *addr, lbm_uint num_cells,
                         lbm_uint *gc_stack_storage, lbm_uint gc_stack_size);

/** Add GC time statistics to heap_stats
 *
 * \param dur Duration as reported by the timestamp callback.
 */
void lbm_heap_new_gc_time(lbm_uint dur);
/** Add a new free_list length to the heap_stats.
 *  Calculates a new freelist length and updates
 *  the GC statistics.
 */
void lbm_heap_new_freelist_length(void);
/** Check how many lbm_cons_t cells are on the free-list
 *
 * \return Number of free lbm_cons_t cells.
 */
lbm_uint lbm_heap_num_free(void);
/** Check how many lbm_cons_t cells are allocated.
 *
 * \return  Number of lbm_cons_t cells that are currently allocated.
 */
lbm_uint lbm_heap_num_allocated(void);
/** Size of the heap in number of lbm_cons_t cells.
 *
 * \return Size of the heap in number of lbm_cons_t cells.
 */
lbm_uint lbm_heap_size(void);
/** Size of the heap in bytes.
 *
 * \return Size of heap in bytes.
 */
lbm_uint lbm_heap_size_bytes(void);
/** Allocate an lbm_cons_t cell from the heap.
 *
 * \param type A type that can be encoded onto the cell (most often LBM_PTR_TYPE_CONS).
 * \return An lbm_value referring to a cons_cell or enc_sym(SYM_MERROR) in case the heap is full.
 */
lbm_value lbm_heap_allocate_cell(lbm_type type);

/** Decode an lbm_value representing a string into a C string
 *
 * \param val Value
 * \return String or NULL if the value does not encode a string.
 */
char *lbm_dec_str(lbm_value val);
/** Decode an lbm_value representing a char channel into an lbm_char_channel_t pointer.
 *
 * \param val Value
 * \return A pointer to an lbm_char_channel_t or NULL if the value does not encode a channel.
 */
lbm_char_channel_t *lbm_dec_channel(lbm_value val);
/** Decode an lbm_value representing a custom type into a lbm_uint value.
 *
 * \param val Value.
 * \return The custom type payload.
 */
lbm_uint lbm_dec_custom(lbm_value val);
/** Decode a numerical value as if it is char
 *
 * \param val Value to decode
 * \return The value encoded in val casted to a char. Returns 0 if val does not encode a number.
 */
char lbm_dec_as_char(lbm_value a);
/** Decode a numerical value as if it is unsigned
 *
 * \param val Value to decode
 * \return The value encoded in val casted to an unsigned int. Returns 0 if val does not encode a number.
 */
uint32_t lbm_dec_as_u32(lbm_value val);
/** Decode a numerical value as a signed integer.
 *
 * \param val Value to decode
 * \return The value encoded in val casted to a signed int. Returns 0 if val does not encode a number.
 */
int32_t lbm_dec_as_i32(lbm_value val);
/** Decode a numerical value as a float.
 *
 * \param val Value to decode.
 * \return The value encoded in val casted to a float. Returns 0 if val does not encode a number.
 */
float lbm_dec_as_float(lbm_value val);
/** Decode a numerical value as if it is a 64bit unsigned
 *
 * \param val Value to decode
 * \return The value encoded in val casted to an unsigned int. Returns 0 if val does not encode a number.
 */
uint64_t lbm_dec_as_u64(lbm_value val);
/** Decode a numerical value as a 64bit signed integer.
 *
 * \param val Value to decode
 * \return The value encoded in val casted to a signed int. Returns 0 if val does not encode a number.
 */
int64_t lbm_dec_as_i64(lbm_value val);
/** Decode a numerical value as a float.
 *
 * \param val Value to decode.
 * \return The value encoded in val casted to a float. Returns 0 if val does not encode a number.
 */
double lbm_dec_as_double(lbm_value val);

lbm_uint lbm_dec_raw(lbm_value v);
/** Allocates an lbm_cons_t cell from the heap and populates it.
 *
 * \param car The value to put in the car field of the allocated lbm_cons_t.
 * \param cdr The value to put in the cdr field of the allocated lbm_cons_t.
 * \return A value referencing the lbm_cons_t or enc_sym(SYM_MERROR) if heap is full.
 */
lbm_value lbm_cons(lbm_value car, lbm_value cdr);

/** Accesses the car field of an lbm_cons_t.
 *
 * \param cons Value
 * \return The car field of the lbm_cons_t if cons is a reference to a heap cell.
 * If cons is nil, the return value is nil. If the value
 * is not cons or nil, the return value is enc_sym(SYM_TERROR) for type error.
 */
lbm_value lbm_car(lbm_value cons);
/** Accesses the car of the cdr of an cons cell
 *
 * \param c Value
 * \return the cdr field or type error.
 */
lbm_value lbm_cadr(lbm_value c);
/** Accesses the cdr field of an lbm_cons_t.
 *
 * \param cons Value
 * \return The cdr field of the lbm_cons_t if cons is a reference to a heap cell.
 * If cons is nil, the return value is nil. If the value
 * if not cons or nil, the return value is enc_sym(SYM_TERROR) for type error.
 */
lbm_value lbm_cdr(lbm_value cons);
/** Update the value stored in the car field of a heap cell.
 *
 * \param c Value referring to a heap cell.
 * \param v Value to replace the car field with.
 * \return 1 on success and 0 if the c value does not refer to a heap cell.
 */
int lbm_set_car(lbm_value c, lbm_value v);
/** Update the value stored in the cdr field of a heap cell.
 *
 * \param c Value referring to a heap cell.
 * \param v Value to replace the cdr field with.
 * \return 1 on success and 0 if the c value does not refer to a heap cell.
 */
int lbm_set_cdr(lbm_value c, lbm_value v);

// List functions
/** Calculate the length of a proper list
 * \warning This is a dangerous function that should be used carefully. Cyclic structures on the heap
 * may lead to the function not terminating.
 *
 * \param c A list
 * \return The length of the list. Unless the value is a cyclic structure on the heap, this function will terminate.
 */
unsigned int lbm_list_length(lbm_value c);
/** Reverse a proper list
 * \warning This is a dangerous function that should be used carefully. Cyclic structures on the heap
 * may lead to the function not terminating.
 *
 * \param list A list
 * \return The list reversed or enc_sym(SYM_MERROR) if heap is full.
 */
lbm_value lbm_list_reverse(lbm_value list);
/** Reverse a proper list destroying the original.
 * \warning This is a dangerous function that should be used carefully. Cyclic structures on the heap
 * may lead to the function not terminating.
 *
 * \param list A list
 * \return The list reversed
 */
lbm_value lbm_list_destructive_reverse(lbm_value list);
/** Copy a list
 * \warning This is a dangerous function that should be used carefully. Cyclic structures on the heap
 * may lead to the function not terminating.
 *
 * \param list A list.
 * \return Reversed list or enc_sym(SYM_MERROR) if heap is full.
 */
lbm_value lbm_list_copy(lbm_value list);

/** A destructive append of two lists
 *
 * \param list1 A list
 * \param list2 A list
 * \return list1 with list2 appended at the end.
 */
lbm_value lbm_list_append(lbm_value list1, lbm_value list2);


// State and statistics
/** Get a copy of the heap statistics structure.
 *
 * \param A pointer to an lbm_heap_state_t to populate
 * with the current statistics.
 */
void lbm_get_heap_state(lbm_heap_state_t *);

// Garbage collection
/** Increment the counter that is counting the number of times GC ran
 *
 */
void lbm_gc_state_inc(void);
/** Mark all heap cells that are on the free-list.
 *
 * \return 1 on success or 0 if the free-list is corrupted.
 */
int lbm_gc_mark_freelist(void);
/** Mark heap cells reachable from the lbm_value v.
 *
 * \param v Root node to start marking from.
 * \return 1 on success and 0 if the stack used internally is full.
 */
int lbm_gc_mark_phase(lbm_value v);
int lbm_gc_mark_phase2(lbm_value env);

/** Performs lbm_gc_mark_phase on all the values of an array.
 *
 * \param data Array of roots to traverse from.
 * \param n Number of elements in roots-array.
 * \return 1 on success or 0 for failure.
 */
int lbm_gc_mark_aux(lbm_uint *data, lbm_uint n);

/** Sweep up all non marked heap cells and place them on the free list.
 *
 * \return 1
 */
int lbm_gc_sweep_phase(void);

// Array functionality
/** Allocate an array in symbols and arrays memory (lispbm_memory.h)
 * and create a heap cell that refers to this array.
 * \param res The resulting lbm_value is returned through this argument.
 * \param size Array size in number of 32 bit words.
 * \param type The type information to encode onto the heap cell.
 * \return 1 for success of 0 for failure.
 */
int lbm_heap_allocate_array(lbm_value *res, lbm_uint size, lbm_type type);
/** Explicitly free an array.
 *  This function needs to be used with care and knowledge.
 * \param arr Array value.
 */
int lbm_heap_explicit_free_array(lbm_value arr);

/** Query the type information of a value.
 *
 * \param x Value to check the type of.
 * \return The type information.
 */
static inline lbm_type lbm_type_of(lbm_value x) {
  return (x & LBM_PTR_MASK) ? (x & LBM_PTR_TYPE_MASK) : (x & LBM_VAL_TYPE_MASK);
}

static inline lbm_value lbm_enc_cons_ptr(lbm_uint x) {
  return ((x << LBM_ADDRESS_SHIFT) | LBM_TYPE_CONS | LBM_PTR_BIT);
}

static inline lbm_uint lbm_dec_ptr(lbm_value p) {
  return ((LBM_PTR_VAL_MASK & p) >> LBM_ADDRESS_SHIFT);
}

static inline lbm_value lbm_set_ptr_type(lbm_value p, lbm_type t) {
  return ((LBM_PTR_VAL_MASK & p) | t | LBM_PTR_BIT);
}

static inline lbm_value lbm_enc_sym(lbm_uint s) {
  return (s << LBM_VAL_SHIFT) | LBM_TYPE_SYMBOL;
}

static inline lbm_value lbm_enc_i(lbm_int x) {
  return ((lbm_uint)x << LBM_VAL_SHIFT) | LBM_TYPE_I;
}

static inline lbm_value lbm_enc_u(lbm_uint x) {
  return (x << LBM_VAL_SHIFT) | LBM_TYPE_U;
}

/** Encode 32 bit integer into an lbm_value.
 * \param x Value to encode.
 * \return result encoded value.
 */
extern lbm_value lbm_enc_i32(int32_t x);

/** Encode 32 bit unsigned integer into an lbm_value.
 * \param x Value to encode.
 * \return result encoded value.
 */
extern lbm_value lbm_enc_u32(uint32_t x);

/** Encode a float into an lbm_value.
 * \param x float value to encode.
 * \return result encoded value.
 */
extern lbm_value lbm_enc_float(float x);

/** Encode a 64 bit integer into an lbm_value.
 * \param x 64 bit integer to encode.
 * \return result encoded value.
 */
extern lbm_value lbm_enc_i64(int64_t x);

/** Encode a 64 bit unsigned integer into an lbm_value.
 * \param x 64 bit unsigned integer to encode.
 * \return result encoded value.
 */
extern lbm_value lbm_enc_u64(uint64_t x);

/** Encode a double into an lbm_value.
 * \param x double to encode.
 * \return result encoded value.
 */
extern lbm_value lbm_enc_double(double x);

static inline lbm_value lbm_enc_char(char x) {
  return ((lbm_uint)x << LBM_VAL_SHIFT) | LBM_TYPE_CHAR;
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

/** Decode an lbm_value representing a float.
 * \param x Value to decode.
 * \return decoded float.
 */
extern float lbm_dec_float(lbm_value x);

/** Decode an lbm_value representing a double.
 * \param x Value to decode.
 * \return decoded float.
 */
extern double lbm_dec_double(lbm_value x);


static inline uint32_t lbm_dec_u32(lbm_value x) {
#ifndef LBM64
  return (uint32_t)lbm_car(x);
#else
   return (uint32_t)(x >> LBM_VAL_SHIFT);
#endif
}

/** Decode an lbm_value representing a 64 bit unsigned integer.
 * \param x Value to decode.
 * \return decoded uint64_t.
 */
extern uint64_t lbm_dec_u64(lbm_value x);

static inline int32_t lbm_dec_i32(lbm_value x) {
#ifndef LBM64
  return (int32_t)lbm_car(x);
#else
  return (int32_t)(x >> LBM_VAL_SHIFT);
#endif
}

/** Decode an lbm_value representing a 64 bit integert.
 * \param x Value to decode.
 * \return decoded int64_t.
 */
extern int64_t lbm_dec_i64(lbm_value x);

static inline lbm_value lbm_set_gc_mark(lbm_value x) {
  return x | LBM_GC_MARKED;
}

static inline lbm_value lbm_clr_gc_mark(lbm_value x) {
  return x & ~LBM_GC_MASK;
}

static inline bool lbm_get_gc_mark(lbm_value x) {
  return x & LBM_GC_MASK;
}

static inline bool lbm_is_ptr(lbm_value x) {
  return (x & LBM_PTR_MASK);
}

static inline bool lbm_is_cons(lbm_value x) {
  return (lbm_type_of(x) == LBM_TYPE_CONS);
}

/** Check if a value represents a number
 * \param x Value to check.
 * \return true is x represents a number and false otherwise.
 */
extern bool lbm_is_number(lbm_value x);

static inline bool lbm_is_array(lbm_value x) {
  return (lbm_type_of(x) == LBM_TYPE_ARRAY &&
          lbm_type_of(lbm_cdr(x)) == LBM_TYPE_SYMBOL &&
          lbm_dec_sym(lbm_cdr(x)) == SYM_ARRAY_TYPE);
}

/** Check if a value represents a byte array.
 * \param x Value to check.
 * \return true if x represents a byte array and false otherwise.
 */
extern bool lbm_is_byte_array(lbm_value x);

static inline bool lbm_is_channel(lbm_value x) {
  return (lbm_type_of(x) == LBM_TYPE_CHANNEL &&
          lbm_type_of(lbm_cdr(x)) == LBM_TYPE_SYMBOL &&
          lbm_dec_sym(lbm_cdr(x)) == SYM_CHANNEL_TYPE);
}
static inline bool lbm_is_char(lbm_value x) {
  return (lbm_type_of(x) == LBM_TYPE_CHAR);
}

static inline bool lbm_is_special(lbm_value symrep) {
  return ((lbm_type_of(symrep) == LBM_TYPE_SYMBOL) &&
          (lbm_dec_sym(symrep) < SPECIAL_SYMBOLS_END));
}

static inline bool lbm_is_fundamental(lbm_value symrep) {
  return ((lbm_type_of(symrep) == LBM_TYPE_SYMBOL)  &&
          (lbm_dec_sym(symrep) >= FUNDAMENTALS_START) &&
          (lbm_dec_sym(symrep) <= FUNDAMENTALS_END));
}

static inline bool lbm_is_closure(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_TYPE_CONS) &&
          (lbm_type_of(lbm_car(exp)) == LBM_TYPE_SYMBOL) &&
          (lbm_dec_sym(lbm_car(exp)) == SYM_CLOSURE));
}

static inline bool lbm_is_continuation(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_TYPE_CONS) &&
          (lbm_type_of(lbm_car(exp)) == LBM_TYPE_SYMBOL) &&
          (lbm_dec_sym(lbm_car(exp)) == SYM_CONT));
}

static inline bool lbm_is_macro(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_TYPE_CONS) &&
          (lbm_type_of(lbm_car(exp)) == LBM_TYPE_SYMBOL) &&
          (lbm_dec_sym(lbm_car(exp)) == SYM_MACRO));
}

static inline bool lbm_is_match_binder(lbm_value exp) {
  return ((lbm_type_of(exp) == LBM_TYPE_CONS) &&
          (lbm_type_of(lbm_car(exp)) == LBM_TYPE_SYMBOL) &&
          ((lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_ANY) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_I) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_U) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_I32) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_U32) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_FLOAT) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_I64) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_U64) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_DOUBLE) ||
           (lbm_dec_sym(lbm_car(exp)) == SYM_MATCH_CONS)));
}

static inline bool lbm_is_comma_qualified_symbol(lbm_value exp) {
  return (lbm_is_cons(exp) &&
          (lbm_type_of(lbm_car(exp)) == LBM_TYPE_SYMBOL) &&
          (lbm_dec_sym(lbm_car(exp)) == SYM_COMMA) &&
          (lbm_type_of(lbm_car(lbm_cdr(exp))) == LBM_TYPE_SYMBOL));
}

static inline bool lbm_is_symbol(lbm_value exp) {
  return (lbm_type_of(exp) == LBM_TYPE_SYMBOL);
}

static inline bool lbm_is_symbol_nil(lbm_value exp) {
  return (lbm_is_symbol(exp) && lbm_dec_sym(exp) == SYM_NIL);
}

static inline bool lbm_is_symbol_true(lbm_value exp) {
  return (lbm_is_symbol(exp) && lbm_dec_sym(exp) == SYM_TRUE);
}

static inline bool lbm_is_symbol_eval(lbm_value exp) {
  return (lbm_is_symbol(exp) && lbm_dec_sym(exp) == SYM_EVAL);
}

static inline bool lbm_is_symbol_merror(lbm_value exp) {
  return (lbm_is_symbol(exp) && lbm_dec_sym(exp) == SYM_MERROR);
}

static inline bool lbm_is_list(lbm_value x) {
  return (lbm_is_cons(x) || lbm_is_symbol_nil(x));
}

#ifndef LBM64
#define ERROR_SYMBOL_MASK 0xFFFFFF20
#else
#define ERROR_SYMBOL_MASK 0xFFFFFFFFFFFFFF20
#endif

/* all error signaling symbols are in the range 0x20 - 0x2F */
static inline bool lbm_is_error(lbm_value v){
  return (lbm_type_of(v) == LBM_TYPE_SYMBOL &&
          ((lbm_dec_sym(v) & ERROR_SYMBOL_MASK) == 0x20));
}

// ref_cell: returns a reference to the cell addressed by bits 3 - 26
//           Assumes user has checked that is_ptr was set
static inline lbm_cons_t* lbm_ref_cell(lbm_value addr) {
  return &lbm_heap_state.heap[lbm_dec_ptr(addr)];
  //  return (cons_t*)(heap_base + (addr & PTR_VAL_MASK));
}

#ifdef __cplusplus
}
#endif
#endif
