/*
    Copyright 2018, 2020, 2022 - 2025 Joel Svensson  svenssonjoel@yahoo.se
                          2022        Benjamin Vedder

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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <lbm_memory.h>
#include <lbm_custom_type.h>
#include <lbm_defrag_mem.h>
#include <lbm_image.h>

#include "heap.h"
#include "symrepr.h"
#include "stack.h"
#include "lbm_channel.h"
#include "platform_mutex.h"
#include "eval_cps.h"
#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif

static inline lbm_value lbm_set_gc_mark(lbm_value x) {
  return x | LBM_GC_MARKED;
}
static inline lbm_value lbm_clr_gc_mark(lbm_value x) {
  return x & ~LBM_GC_MASK;
}

static inline bool lbm_get_gc_mark(lbm_value x) {
  return x & LBM_GC_MASK;
}

static inline void gc_mark(lbm_value c) {
  //c must be a cons cell.
  lbm_cons_t *cell = lbm_ref_cell(c);
  cell->cdr = lbm_set_gc_mark(cell->cdr);
}

static inline bool gc_marked(lbm_value c) {
  lbm_cons_t *cell = lbm_ref_cell(c);
  return lbm_get_gc_mark(cell->cdr);
}

static inline void gc_clear_mark(lbm_value c) {
  //c must be a cons cell.
  lbm_cons_t *cell = lbm_ref_cell(c);
  cell->cdr = lbm_clr_gc_mark(cell->cdr);
}

// flag is the same bit as mark, but in car
static inline bool lbm_get_gc_flag(lbm_value x) {
  return x & LBM_GC_MARKED;
}

static inline lbm_value lbm_set_gc_flag(lbm_value x) {
  return x | LBM_GC_MARKED;
}

static inline lbm_value lbm_clr_gc_flag(lbm_value x) {
  return x & ~LBM_GC_MASK;
}


lbm_heap_state_t lbm_heap_state;

lbm_const_heap_t *lbm_const_heap_state;

lbm_cons_t *lbm_heaps[2] = {NULL, NULL};

static mutex_t lbm_const_heap_mutex;
static bool    lbm_const_heap_mutex_initialized = false;

static mutex_t lbm_mark_mutex;
static bool    lbm_mark_mutex_initialized = false;

#ifdef USE_GC_PTR_REV
void lbm_gc_lock(void) {
  mutex_lock(&lbm_mark_mutex);
}
void lbm_gc_unlock(void) {
  mutex_unlock(&lbm_mark_mutex);
}
#else
void lbm_gc_lock(void) {
}
void lbm_gc_unlock(void) {
}
#endif

/****************************************************/
/* ENCODERS DECODERS                                */

lbm_value lbm_enc_i32(int32_t x) {
#ifndef LBM64
  lbm_value i = lbm_cons((lbm_uint)x, ENC_SYM_RAW_I_TYPE);
  if (lbm_type_of(i) == LBM_TYPE_SYMBOL) return i;
  return lbm_set_ptr_type(i, LBM_TYPE_I32);
#else
  return (((lbm_uint)x) << LBM_VAL_SHIFT) | LBM_TYPE_I32;
#endif
}

lbm_value lbm_enc_u32(uint32_t x) {
#ifndef LBM64
  lbm_value u = lbm_cons(x, ENC_SYM_RAW_U_TYPE);
  if (lbm_type_of(u) == LBM_TYPE_SYMBOL) return u;
  return lbm_set_ptr_type(u, LBM_TYPE_U32);
#else
  return (((lbm_uint)x) << LBM_VAL_SHIFT) | LBM_TYPE_U32;
#endif
}

lbm_value lbm_enc_float(float x) {
#ifndef LBM64
  lbm_uint t;
  memcpy(&t, &x, sizeof(lbm_float));
  lbm_value f = lbm_cons(t, ENC_SYM_RAW_F_TYPE);
  if (lbm_type_of(f) == LBM_TYPE_SYMBOL) return f;
  return lbm_set_ptr_type(f, LBM_TYPE_FLOAT);
#else
  lbm_uint t = 0;
  memcpy(&t, &x, sizeof(float));
  return (((lbm_uint)t) << LBM_VAL_SHIFT) | LBM_TYPE_FLOAT;
#endif
}

#ifndef LBM64
static lbm_value enc_64_on_32(uint8_t *source, lbm_uint type_qual, lbm_uint type) {
  lbm_value res = lbm_cons(ENC_SYM_NIL,ENC_SYM_NIL);
  if (lbm_type_of(res) != LBM_TYPE_SYMBOL) {
    uint8_t* storage = lbm_malloc(sizeof(uint64_t));
    if (storage) {
      memcpy(storage,source, sizeof(uint64_t));
      lbm_set_car_and_cdr(res, (lbm_uint)storage,  type_qual);
      res = lbm_set_ptr_type(res, type);
    } else {
      res = ENC_SYM_MERROR;
    }
  }
  return res;
}
#endif

lbm_value lbm_enc_i64(int64_t x) {
#ifndef LBM64
  return enc_64_on_32((uint8_t *)&x, ENC_SYM_IND_I_TYPE, LBM_TYPE_I64);
#else
  lbm_value u = lbm_cons((uint64_t)x, ENC_SYM_RAW_I_TYPE);
  if (lbm_type_of(u) == LBM_TYPE_SYMBOL) return u;
  return lbm_set_ptr_type(u, LBM_TYPE_I64);
#endif
}

lbm_value lbm_enc_u64(uint64_t x) {
#ifndef LBM64
  return enc_64_on_32((uint8_t *)&x, ENC_SYM_IND_U_TYPE, LBM_TYPE_U64);
#else
  lbm_value u = lbm_cons(x, ENC_SYM_RAW_U_TYPE);
  if (lbm_type_of(u) == LBM_TYPE_SYMBOL) return u;
  return lbm_set_ptr_type(u, LBM_TYPE_U64);
#endif
}

lbm_value lbm_enc_double(double x) {
#ifndef LBM64
  return enc_64_on_32((uint8_t *)&x, ENC_SYM_IND_F_TYPE, LBM_TYPE_DOUBLE);
#else
  lbm_uint t;
  memcpy(&t, &x, sizeof(double));
  lbm_value f = lbm_cons(t, ENC_SYM_RAW_F_TYPE);
  if (lbm_type_of(f) == LBM_TYPE_SYMBOL) return f;
  return lbm_set_ptr_type(f, LBM_TYPE_DOUBLE);
#endif
}

// Type specific (as opposed to the dec_as_X) functions
// should only be run on values KNOWN to represent a value of the type
// that the decoder decodes.

float lbm_dec_float(lbm_value x) {
#ifndef LBM64
  float f_tmp;
  lbm_uint tmp = lbm_car(x);
  memcpy(&f_tmp, &tmp, sizeof(float));
  return f_tmp;
#else
  uint32_t tmp = (uint32_t)(x >> LBM_VAL_SHIFT);
  float f_tmp;
  memcpy(&f_tmp, &tmp, sizeof(float));
  return f_tmp;
#endif
}

double lbm_dec_double(lbm_value x) {
#ifndef LBM64
  double d = 0.0;
  if (lbm_is_ptr(x)) {
    uint32_t *data = (uint32_t*)lbm_ref_cell(x)->car;
    memcpy(&d, data, sizeof(double));
  }
  return d;
#else
  double f_tmp;
  lbm_uint tmp = lbm_car(x);
  memcpy(&f_tmp, &tmp, sizeof(double));
  return f_tmp;
#endif
}

uint64_t lbm_dec_u64(lbm_value x) {
#ifndef LBM64
  uint64_t u = 0;
  if (lbm_is_ptr(x)) {
    uint32_t *data = (uint32_t*)lbm_ref_cell(x)->car;
    memcpy(&u, data, 8);
  }
  return u;
#else
  return (uint64_t)lbm_car(x);
#endif
}

int64_t lbm_dec_i64(lbm_value x) {
#ifndef LBM64
  int64_t i = 0;
  if (lbm_is_ptr(x)) {
    uint32_t *data = (uint32_t*)lbm_ref_cell(x)->car;
    memcpy(&i, data, 8);
  }
  return i;
#else
  return (int64_t)lbm_car(x);
#endif
}

char *lbm_dec_str(lbm_value val) {
  char *res = 0;
  if (lbm_is_array_r(val)) {
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(val);
    if (array) {
      res = (char *)array->data;
    }
  }
  return res;
}

lbm_array_header_t *lbm_dec_array_r(lbm_value val) {
  lbm_array_header_t *array = NULL;
  if (lbm_is_array_r(val)) {
    array = (lbm_array_header_t *)lbm_car(val);
  }
  return array;
}

lbm_array_header_t *lbm_dec_array_rw(lbm_value val) {
  lbm_array_header_t *array = NULL;
  if (lbm_is_array_rw(val)) {
    array = (lbm_array_header_t *)lbm_car(val);
  }
  return array;
}

lbm_char_channel_t *lbm_dec_channel(lbm_value val) {
  lbm_char_channel_t *res = NULL;

  if (lbm_type_of(val) == LBM_TYPE_CHANNEL) {
    res = (lbm_char_channel_t *)lbm_car(val);
  }
  return res;
}

lbm_uint lbm_dec_custom(lbm_value val) {
  lbm_uint res = 0;
  if (lbm_type_of(val) == LBM_TYPE_CUSTOM) {
    res = (lbm_uint)lbm_car(val);
  }
  return res;
}

uint8_t lbm_dec_as_char(lbm_value a) {
  uint8_t r = 0;
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    r = (uint8_t)lbm_dec_char(a); break;
  case LBM_TYPE_I:
    r = (uint8_t)lbm_dec_i(a); break;
  case LBM_TYPE_U:
    r = (uint8_t)lbm_dec_u(a); break;
  case LBM_TYPE_I32:
    r = (uint8_t)lbm_dec_i32(a); break;
  case LBM_TYPE_U32:
    r = (uint8_t)lbm_dec_u32(a); break;
  case LBM_TYPE_FLOAT:
    r = (uint8_t)lbm_dec_float(a); break;
  case LBM_TYPE_I64:
    r = (uint8_t)lbm_dec_i64(a); break;
  case LBM_TYPE_U64:
    r = (uint8_t)lbm_dec_u64(a); break;
  case LBM_TYPE_DOUBLE:
    r = (uint8_t) lbm_dec_double(a); break;
  }
  return r;
}

uint32_t lbm_dec_as_u32(lbm_value a) {
  uint32_t r = 0;
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    r = (uint32_t)lbm_dec_char(a); break;
  case LBM_TYPE_I:
    r = (uint32_t)lbm_dec_i(a); break;
  case LBM_TYPE_U:
    r = (uint32_t)lbm_dec_u(a); break;
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_U32:
    r = (uint32_t)lbm_dec_u32(a); break;
  case LBM_TYPE_FLOAT:
    r = (uint32_t)lbm_dec_float(a); break;
  case LBM_TYPE_I64:
    r = (uint32_t)lbm_dec_i64(a); break;
  case LBM_TYPE_U64:
    r = (uint32_t)lbm_dec_u64(a); break;
  case LBM_TYPE_DOUBLE:
    r = (uint32_t)lbm_dec_double(a); break;
  }
  return r;
}

int32_t lbm_dec_as_i32(lbm_value a) {
  int32_t r = 0;
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    r = (int32_t)lbm_dec_char(a); break;
  case LBM_TYPE_I:
    r = (int32_t)lbm_dec_i(a); break;
  case LBM_TYPE_U:
    r = (int32_t)lbm_dec_u(a); break;
  case LBM_TYPE_I32:
    r = (int32_t)lbm_dec_i32(a); break;
  case LBM_TYPE_U32:
    r = (int32_t)lbm_dec_u32(a); break;
  case LBM_TYPE_FLOAT:
    r = (int32_t)lbm_dec_float(a); break;
  case LBM_TYPE_I64:
    r = (int32_t)lbm_dec_i64(a); break;
  case LBM_TYPE_U64:
    r = (int32_t)lbm_dec_u64(a); break;
  case LBM_TYPE_DOUBLE:
    r = (int32_t) lbm_dec_double(a); break;
  }
  return r;
}

int64_t lbm_dec_as_i64(lbm_value a) {
  int64_t r = 0;
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    r = (int64_t)lbm_dec_char(a); break;
  case LBM_TYPE_I:
    r = (int64_t)lbm_dec_i(a); break;
  case LBM_TYPE_U:
    r = (int64_t)lbm_dec_u(a); break;
  case LBM_TYPE_I32:
    r = (int64_t)lbm_dec_i32(a); break;
  case LBM_TYPE_U32:
    r = (int64_t)lbm_dec_u32(a); break;
  case LBM_TYPE_FLOAT:
    r = (int64_t)lbm_dec_float(a); break;
  case LBM_TYPE_I64:
    r = (int64_t)lbm_dec_i64(a); break;
  case LBM_TYPE_U64:
    r = (int64_t)lbm_dec_u64(a); break;
  case LBM_TYPE_DOUBLE:
    r = (int64_t) lbm_dec_double(a); break;
  }
  return r;
}

uint64_t lbm_dec_as_u64(lbm_value a) {
  uint64_t r = 0;
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    r = (uint64_t)lbm_dec_char(a); break;
  case LBM_TYPE_I:
    r = (uint64_t)lbm_dec_i(a); break;
  case LBM_TYPE_U:
    r = (uint64_t)lbm_dec_u(a); break;
  case LBM_TYPE_I32:
    r = (uint64_t)lbm_dec_i32(a); break;
  case LBM_TYPE_U32:
    r = (uint64_t)lbm_dec_u32(a); break;
  case LBM_TYPE_FLOAT:
    r = (uint64_t)lbm_dec_float(a); break;
  case LBM_TYPE_I64:
    r = (uint64_t)lbm_dec_i64(a); break;
  case LBM_TYPE_U64:
    r = (uint64_t)lbm_dec_u64(a); break;
  case LBM_TYPE_DOUBLE:
    r = (uint64_t)lbm_dec_double(a); break;
  }
  return r;
}

/* lbm_uint lbm_dec_as_uint(lbm_value a) { */
/*   lbm_uint r = 0; */
/*   switch (lbm_type_of_functional(a)) { */
/*   case LBM_TYPE_CHAR: */
/*     r = (lbm_uint)lbm_dec_char(a); break; */
/*   case LBM_TYPE_I: */
/*     r = (lbm_uint)lbm_dec_i(a); break; */
/*   case LBM_TYPE_U: */
/*     r = (lbm_uint)lbm_dec_u(a); break; */
/*   case LBM_TYPE_I32: */
/*     r = (lbm_uint)lbm_dec_i32(a); break; */
/*   case LBM_TYPE_U32: */
/*     r = (lbm_uint)lbm_dec_u32(a); break; */
/*   case LBM_TYPE_FLOAT: */
/*     r = (lbm_uint)lbm_dec_float(a); break; */
/*   case LBM_TYPE_I64: */
/*     r = (lbm_uint)lbm_dec_i64(a); break; */
/*   case LBM_TYPE_U64: */
/*     r = (lbm_uint) lbm_dec_u64(a); break; */
/*   case LBM_TYPE_DOUBLE: */
/*     r = (lbm_uint)lbm_dec_double(a); break; */
/*   } */
/*   return r; */
/* } */

/* lbm_int lbm_dec_as_int(lbm_value a) { */
/*   lbm_int r = 0; */
/*   switch (lbm_type_of_functional(a)) { */
/*   case LBM_TYPE_CHAR: */
/*     r = (lbm_int)lbm_dec_char(a); break; */
/*   case LBM_TYPE_I: */
/*     r = (lbm_int)lbm_dec_i(a); break; */
/*   case LBM_TYPE_U: */
/*     r = (lbm_int)lbm_dec_u(a); break; */
/*   case LBM_TYPE_I32: */
/*     r = (lbm_int)lbm_dec_i32(a); break; */
/*   case LBM_TYPE_U32: */
/*     r = (lbm_int)lbm_dec_u32(a); break; */
/*   case LBM_TYPE_FLOAT: */
/*     r = (lbm_int)lbm_dec_float(a); break; */
/*   case LBM_TYPE_I64: */
/*     r = (lbm_int)lbm_dec_i64(a); break; */
/*   case LBM_TYPE_U64: */
/*     r = (lbm_int)lbm_dec_u64(a); break; */
/*   case LBM_TYPE_DOUBLE: */
/*     r = (lbm_int)lbm_dec_double(a); break; */
/*   } */
/*   return r; */
/* } */

float lbm_dec_as_float(lbm_value a) {
  float r = 0;
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    r = (float)lbm_dec_char(a); break;
  case LBM_TYPE_I:
    r = (float)lbm_dec_i(a); break;
  case LBM_TYPE_U:
    r = (float)lbm_dec_u(a); break;
  case LBM_TYPE_I32:
    r = (float)lbm_dec_i32(a); break;
  case LBM_TYPE_U32:
    r = (float)lbm_dec_u32(a); break;
  case LBM_TYPE_FLOAT:
    r = (float)lbm_dec_float(a); break;
  case LBM_TYPE_I64:
    r = (float)lbm_dec_i64(a); break;
  case LBM_TYPE_U64:
    r = (float)lbm_dec_u64(a); break;
  case LBM_TYPE_DOUBLE:
    r = (float)lbm_dec_double(a); break;
  }
  return r;
}

double lbm_dec_as_double(lbm_value a) {
  double r = 0;
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    r = (double)lbm_dec_char(a); break;
  case LBM_TYPE_I:
    r = (double)lbm_dec_i(a); break;
  case LBM_TYPE_U:
    r = (double)lbm_dec_u(a); break;
  case LBM_TYPE_I32:
    r = (double)lbm_dec_i32(a); break;
  case LBM_TYPE_U32:
    r = (double)lbm_dec_u32(a); break;
  case LBM_TYPE_FLOAT:
    r = (double)lbm_dec_float(a); break;
  case LBM_TYPE_I64:
    r = (double)lbm_dec_i64(a); break;
  case LBM_TYPE_U64:
    r = (double)lbm_dec_u64(a); break;
  case LBM_TYPE_DOUBLE:
    r = (double)lbm_dec_double(a); break;
  }
  return r;
}

/****************************************************/
/* HEAP MANAGEMENT                                  */

static bool generate_freelist(size_t num_cells) {
  size_t i = 0;

  if (!lbm_heap_state.heap) return false;

  lbm_heap_state.freelist = lbm_enc_cons_ptr(0);

  lbm_cons_t *t;

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    t = lbm_ref_cell(lbm_enc_cons_ptr(i-1));
    t->car = ENC_SYM_RECOVERED;    // all cars in free list are "RECOVERED"
    t->cdr = lbm_enc_cons_ptr(i);
  }

  // Replace the incorrect pointer at the last cell.
  t = lbm_ref_cell(lbm_enc_cons_ptr(num_cells-1));
  t->cdr = ENC_SYM_NIL;

  return true;
}

void lbm_nil_freelist(void) {
  lbm_heap_state.freelist = ENC_SYM_NIL;
  lbm_heap_state.num_alloc = lbm_heap_state.heap_size;
}

static void heap_init_state(lbm_cons_t *addr, lbm_uint num_cells,
                            lbm_uint* gc_stack_storage, lbm_uint gc_stack_size) {
  lbm_heap_state.heap         = addr;
  lbm_heap_state.heap_bytes   = (unsigned int)(num_cells * sizeof(lbm_cons_t));
  lbm_heap_state.heap_size    = num_cells;

  lbm_stack_create(&lbm_heap_state.gc_stack, gc_stack_storage, gc_stack_size);

  lbm_heap_state.num_alloc           = 0;
  lbm_heap_state.num_alloc_arrays    = 0;
  lbm_heap_state.gc_num              = 0;
  lbm_heap_state.gc_marked           = 0;
  lbm_heap_state.gc_recovered        = 0;
  lbm_heap_state.gc_recovered_arrays = 0;
  lbm_heap_state.gc_least_free       = num_cells;
  lbm_heap_state.gc_last_free        = num_cells;
}

void lbm_heap_new_freelist_length(void) {
  lbm_uint l = lbm_heap_state.heap_size - lbm_heap_state.num_alloc;
  lbm_heap_state.gc_last_free = l;
  if (l < lbm_heap_state.gc_least_free)
    lbm_heap_state.gc_least_free = l;
}

bool lbm_heap_init(lbm_cons_t *addr, lbm_uint num_cells,
                  lbm_uint gc_stack_size) {

  if (((uintptr_t)addr % 8) != 0) return false;

  memset(addr,0, sizeof(lbm_cons_t) * num_cells);

  lbm_uint *gc_stack_storage = (lbm_uint*)lbm_malloc(gc_stack_size * sizeof(lbm_uint));
  if (gc_stack_storage == NULL) return 0;

  heap_init_state(addr, num_cells,
                  gc_stack_storage, gc_stack_size);

  lbm_heaps[0] = addr;

  return generate_freelist(num_cells);
}


lbm_value lbm_heap_allocate_cell(lbm_type ptr_type, lbm_value car, lbm_value cdr) {
  lbm_value r;
  lbm_value cell = lbm_heap_state.freelist;
  if (cell) {
    lbm_uint heap_ix = lbm_dec_ptr(cell);
    lbm_heap_state.freelist = lbm_heap_state.heap[heap_ix].cdr;
    lbm_heap_state.num_alloc++;
    lbm_heap_state.heap[heap_ix].car = car;
    lbm_heap_state.heap[heap_ix].cdr = cdr;
    r = lbm_set_ptr_type(cell, ptr_type);
  } else {
    r = ENC_SYM_MERROR;
  }
  return r;
}

lbm_value lbm_heap_allocate_list(lbm_uint n) {
  if (n == 0) return ENC_SYM_NIL;
  if (lbm_heap_num_free() < n) return ENC_SYM_MERROR;
  // Here the freelist is guaranteed to be a cons_cell.

  lbm_value curr = lbm_heap_state.freelist;
  lbm_value res  = curr;
  
  lbm_cons_t *c_cell = NULL;
  lbm_uint count = 0;
  do {
    c_cell = lbm_ref_cell(curr);
    c_cell->car = ENC_SYM_NIL;
    curr = c_cell->cdr;
    count ++;
  } while (count < n);
  lbm_heap_state.freelist = curr;
  c_cell->cdr = ENC_SYM_NIL;
  lbm_heap_state.num_alloc+=count;
  return res;
}

lbm_value lbm_heap_allocate_list_init_va(unsigned int n, va_list valist) {
  if (n == 0) return ENC_SYM_NIL;
  if (lbm_heap_num_free() < n) return ENC_SYM_MERROR;

  lbm_value curr = lbm_heap_state.freelist;
  lbm_value res  = curr;

  lbm_cons_t *c_cell = NULL;
  unsigned int count = 0;
  do {
    c_cell = lbm_ref_cell(curr);
    c_cell->car = va_arg(valist, lbm_value);
    curr = c_cell->cdr;
    count ++;
  } while (count < n);
  lbm_heap_state.freelist = curr;
  c_cell->cdr = ENC_SYM_NIL;
  lbm_heap_state.num_alloc+=count;
  return res;
}

lbm_value lbm_heap_allocate_list_init(unsigned int n, ...) {
    va_list valist;
    va_start(valist, n);
    lbm_value r = lbm_heap_allocate_list_init_va(n, valist);
    va_end(valist);
    return r;
}

lbm_uint lbm_heap_num_allocated(void) {
  return lbm_heap_state.num_alloc;
}
lbm_uint lbm_heap_size(void) {
  return lbm_heap_state.heap_size;
}

lbm_uint lbm_heap_size_bytes(void) {
  return lbm_heap_state.heap_bytes;
}

void lbm_get_heap_state(lbm_heap_state_t *res) {
  *res = lbm_heap_state;
}

lbm_uint lbm_get_gc_stack_max(void) {
  return lbm_get_max_stack(&lbm_heap_state.gc_stack);
}

lbm_uint lbm_get_gc_stack_size(void) {
  return lbm_heap_state.gc_stack.size;
}

static inline void value_assign(lbm_value *a, lbm_value b) {
  lbm_value a_old = *a & LBM_GC_MASK;
  *a = a_old | (b & ~LBM_GC_MASK);
}

#ifdef LBM_USE_GC_PTR_REV
/* ************************************************************
   Deutch-Schorr-Waite (DSW) pointer reversal GC for 2-ptr cells
   with a hack-solution for the lisp-array case (n-ptr cells).

   DSW visits each branch node 3 times compared to 2 times for
   the stack based recursive mark.
   Where the stack based recursive mark performs a stack push/pop,
   DSW rearranges the, current, prev, next and a ptr field on
   the heap.

   DSW changes the structure of the heap and it introduces an
   invalid pointer (LBM_PTR_NULL) temporarily during marking.
   Since the heap will be "messed up" while marking, a mutex
   is introuded to keep other processes out of the heap while
   marking.
*/

static int do_nothing(lbm_value v, bool shared, void *arg) {
  (void) v;
  (void) shared;
  (void) arg;
  return TRAV_FUN_SUBTREE_CONTINUE;
}

void lbm_gc_mark_phase(lbm_value root) {
    mutex_lock(&lbm_const_heap_mutex);
    lbm_ptr_rev_trav(do_nothing, root, NULL);
    mutex_unlock(&lbm_const_heap_mutex);
}

#else
/* ************************************************************
   Explicit stack "recursive" mark phase

   Trees are marked in a left subtree before rigth subtree, car first then cdr,
   way to favor lisp lists. This means that stack will grow slowly when
   marking right-leaning (cdr-recursive) data-structures while left-leaning
   (car-recursive) structures uses a lot of stack.

   Lisp arrays contain an extra book-keeping field to keep track
   of how far into the array the marking process has gone.

   TODO: DSW should be used as a last-resort if the GC stack is exhausted.
         If we use DSW as last-resort can we get away with a way smaller
         GC stack and unchanged performance (on sensible programs)?
*/

extern eval_context_t *ctx_running;
void lbm_gc_mark_phase(lbm_value root) {
  lbm_value t_ptr;
  lbm_stack_t *s = &lbm_heap_state.gc_stack;
  s->data[s->sp++] = root;

  while (!lbm_stack_is_empty(s)) {
    lbm_value curr;
    lbm_pop(s, &curr);

  mark_shortcut:

    if (!lbm_is_ptr(curr) ||
        (curr & LBM_PTR_TO_CONSTANT_BIT)) {
      continue;
    }

    lbm_cons_t *cell = &lbm_heap_state.heap[lbm_dec_ptr(curr)];

    if (lbm_get_gc_mark(cell->cdr)) {
      continue;
    }

    t_ptr = lbm_type_of(curr);

    // An array is marked in O(N) time using an additional 32bit
    // value per array that keeps track of how far into the array GC
    // has progressed.
    if (t_ptr == LBM_TYPE_LISPARRAY) {
      lbm_array_header_extended_t *arr = (lbm_array_header_extended_t*)cell->car;
      lbm_value *arrdata = (lbm_value *)arr->data;
      uint32_t index = arr->index;
      if (arr->size > 0) {
        lbm_push(s, curr); // put array back as bookkeeping.
        // Potential optimization.
        // 1. CONS pointers are set to curr and recurse.
        // 2. Any other ptr is marked immediately and index is increased.
        if (lbm_is_ptr(arrdata[index]) && ((arrdata[index] & LBM_PTR_TO_CONSTANT_BIT) == 0) &&
            !((arrdata[index] & LBM_CONTINUATION_INTERNAL) == LBM_CONTINUATION_INTERNAL)) {
          lbm_cons_t *elt = &lbm_heap_state.heap[lbm_dec_ptr(arrdata[index])];
          if (!lbm_get_gc_mark(elt->cdr)) {
            curr = arrdata[index];
            goto mark_shortcut;
          }
        }
        if (index < ((arr->size/(sizeof(lbm_value))) - 1)) {
          arr->index++;
          continue;
        }
        arr->index = 0;
        lbm_pop(s, &curr); // Remove array from GC stack as we are done marking it.
      }
      cell->cdr = lbm_set_gc_mark(cell->cdr);
      lbm_heap_state.gc_marked ++;
      continue;
    } else if (t_ptr == LBM_TYPE_CHANNEL) {
      cell->cdr = lbm_set_gc_mark(cell->cdr);
      lbm_heap_state.gc_marked ++;
      // TODO: Can channels be explicitly freed ?
      if (cell->car != ENC_SYM_NIL) {
        lbm_char_channel_t *chan = (lbm_char_channel_t *)cell->car;
        curr = chan->dependency;
        goto mark_shortcut;
      }
      continue;
    }

    cell->cdr = lbm_set_gc_mark(cell->cdr);
    lbm_heap_state.gc_marked ++;

    if (t_ptr == LBM_TYPE_CONS) {
      if (lbm_is_ptr(cell->cdr)) {
        if (!lbm_push(s, cell->cdr)) {
          lbm_critical_error();
          break;
        }
      }
      curr = cell->car;
      goto mark_shortcut; // Skip a push/pop
    }
  }
}
#endif

//Environments are proper lists with a 2 element list stored in each car.
void lbm_gc_mark_env(lbm_value env) {
  lbm_value curr = env;
  lbm_cons_t *c;

  while (lbm_is_ptr(curr)) {
    c = lbm_ref_cell(curr);
    c->cdr = lbm_set_gc_mark(c->cdr); // mark the environent list structure.
    lbm_cons_t *b = lbm_ref_cell(c->car);
    b->cdr = lbm_set_gc_mark(b->cdr); // mark the binding list head cell.
    lbm_gc_mark_phase(b->cdr);        // mark the bound object.
    lbm_heap_state.gc_marked +=2;
    curr = c->cdr;
  }
}


void lbm_gc_mark_aux(lbm_uint *aux_data, lbm_uint aux_size) {
  for (lbm_uint i = 0; i < aux_size; i ++) {
    if (lbm_is_ptr(aux_data[i])) {
      lbm_type pt_t = lbm_type_of(aux_data[i]);
      lbm_uint pt_v = lbm_dec_ptr(aux_data[i]);
      if( pt_t >= LBM_POINTER_TYPE_FIRST &&
          pt_t <= LBM_POINTER_TYPE_LAST &&
          pt_v < lbm_heap_state.heap_size) {
        lbm_gc_mark_phase(aux_data[i]);
      }
    }
  }
}

void lbm_gc_mark_roots(lbm_uint *roots, lbm_uint num_roots) {
  for (lbm_uint i = 0; i < num_roots; i ++) {
    lbm_gc_mark_phase(roots[i]);
  }
}

// Sweep moves non-marked heap objects to the free list.
int lbm_gc_sweep_phase(void) {
  unsigned int i = 0;
  lbm_cons_t *heap = (lbm_cons_t *)lbm_heap_state.heap;

  for (i = 0; i < lbm_heap_state.heap_size; i ++) {
    if ( lbm_get_gc_mark(heap[i].cdr)) {
      heap[i].cdr = lbm_clr_gc_mark(heap[i].cdr);
    } else {
      // Check if this cell is a pointer to an array
      // and free it.
      if (lbm_type_of(heap[i].cdr) == LBM_TYPE_SYMBOL) {
        switch(heap[i].cdr) {

        case ENC_SYM_IND_I_TYPE: /* fall through */
        case ENC_SYM_IND_U_TYPE:
        case ENC_SYM_IND_F_TYPE:
          lbm_memory_free((lbm_uint*)heap[i].car);
          break;
        case ENC_SYM_DEFRAG_LISPARRAY_TYPE: /* fall through */
        case ENC_SYM_DEFRAG_ARRAY_TYPE:
          lbm_defrag_mem_free((lbm_uint*)heap[i].car);
          break;
        case ENC_SYM_LISPARRAY_TYPE: /* fall through */
        case ENC_SYM_ARRAY_TYPE:{
          lbm_array_header_t *arr = (lbm_array_header_t*)heap[i].car;
          lbm_memory_free((lbm_uint *)arr->data);
          lbm_heap_state.gc_recovered_arrays++;
          lbm_memory_free((lbm_uint *)arr);
        } break;
        case ENC_SYM_CHANNEL_TYPE:{
          lbm_char_channel_t *chan = (lbm_char_channel_t*)heap[i].car;
          lbm_memory_free((lbm_uint*)chan->state);
          lbm_memory_free((lbm_uint*)chan);
        } break;
        case ENC_SYM_CUSTOM_TYPE: {
          lbm_uint *t = (lbm_uint*)heap[i].car;
          lbm_custom_type_destroy(t);
          lbm_memory_free(t);
          } break;
        case ENC_SYM_DEFRAG_MEM_TYPE: {
          lbm_uint *ptr = (lbm_uint *)heap[i].car;
          lbm_defrag_mem_destroy(ptr);
          } break;
        default:
          break;
        }
      }
      // create pointer to use as new freelist
      lbm_uint addr = lbm_enc_cons_ptr(i);

      // Clear the "freed" cell.
      heap[i].car = ENC_SYM_RECOVERED;
      heap[i].cdr = lbm_heap_state.freelist;
      lbm_heap_state.freelist = addr;
      lbm_heap_state.num_alloc --;
      lbm_heap_state.gc_recovered ++;
    }
  }
  return 1;
}

void lbm_gc_state_inc(void) {
  lbm_heap_state.gc_num ++;
  lbm_heap_state.gc_recovered = 0;
  lbm_heap_state.gc_marked = 0;
}

// construct, alter and break apart
lbm_value lbm_cons(lbm_value car, lbm_value cdr) {
  return lbm_heap_allocate_cell(LBM_TYPE_CONS, car, cdr);
}

lbm_value lbm_car(lbm_value c){
  if (lbm_is_ptr(c) ){
    lbm_cons_t *cell = lbm_ref_cell(c);
    return cell->car;
  }
  return c ? ENC_SYM_TERROR : c; //nil if c == nil
}

// TODO: Many comparisons "is this the nil symbol" can be
// streamlined a bit. NIL is 0 and cannot be confused with any other
// lbm_value.

lbm_value lbm_caar(lbm_value c) {
  lbm_value tmp = ENC_SYM_NIL;
  if (lbm_is_ptr(c)) {
    tmp = lbm_ref_cell(c)->car;
    if (lbm_is_ptr(tmp)) {
      return lbm_ref_cell(tmp)->car;
    }
  }
  return c || tmp ? ENC_SYM_TERROR : c; //nil if not something else
}


lbm_value lbm_cadr(lbm_value c) {
  lbm_value tmp = ENC_SYM_NIL;
  if (lbm_is_ptr(c)) {
    tmp = lbm_ref_cell(c)->cdr;
    if (lbm_is_ptr(tmp)) {
      return lbm_ref_cell(tmp)->car;
    }
  }
  return c || tmp ? ENC_SYM_TERROR : c;
}

lbm_value lbm_cdr(lbm_value c){
  if (lbm_is_ptr(c)) {
    lbm_cons_t *cell = lbm_ref_cell(c);
    return cell->cdr;
  }
  return c ? ENC_SYM_TERROR: c;
}

lbm_value lbm_cddr(lbm_value c) {
  if (lbm_is_ptr(c)) {
    lbm_value tmp = lbm_ref_cell(c)->cdr;
    if (lbm_is_ptr(tmp)) {
      return lbm_ref_cell(tmp)->cdr;
    }
  }
  return c ? ENC_SYM_TERROR : c;
}

int lbm_set_car(lbm_value c, lbm_value v) {
  int r = 0;

  if (lbm_type_of(c) == LBM_TYPE_CONS) {
    lbm_cons_t *cell = lbm_ref_cell(c);
    cell->car = v;
    r = 1;
  }
  return r;
}

int lbm_set_cdr(lbm_value c, lbm_value v) {
  int r = 0;
  if (lbm_is_cons_rw(c)){
    lbm_cons_t *cell = lbm_ref_cell(c);
    cell->cdr = v;
    r = 1;
  }
  return r;
}

int lbm_set_car_and_cdr(lbm_value c, lbm_value car_val, lbm_value cdr_val) {
  int r = 0;
  if (lbm_is_cons_rw(c)) {
    lbm_cons_t *cell = lbm_ref_cell(c);
    cell->car = car_val;
    cell->cdr = cdr_val;
    r = 1;
  }
  return r;
}

/* calculate length of a proper list */
lbm_uint lbm_list_length(lbm_value c) {
  lbm_uint len = 0;

  while (lbm_is_cons(c)){
    len ++;
    c = lbm_cdr(c);
  }
  return len;
}

lbm_value lbm_list_destructive_reverse(lbm_value list) {
  if (lbm_type_of(list) == LBM_TYPE_SYMBOL) {
    return list;
  }
  lbm_value curr = list;
  lbm_value last_cell = ENC_SYM_NIL;

  while (lbm_is_cons_rw(curr)) {
    lbm_value next = lbm_cdr(curr);
    lbm_set_cdr(curr, last_cell);
    last_cell = curr;
    curr = next;
  }
  return last_cell;
}


lbm_value lbm_list_copy(int *m, lbm_value list) {
  lbm_value curr = list;
  lbm_uint n = lbm_list_length(list);
  lbm_uint copy_n = n;
  if (*m >= 0 && (lbm_uint)*m < n) {
    copy_n = (lbm_uint)*m;
  } else if (*m == -1) {
    *m = (int)n; // TODO: smaller range in target variable.
  }
  if (copy_n == 0) return ENC_SYM_NIL;
  lbm_uint new_list = lbm_heap_allocate_list(copy_n);
  if (lbm_is_symbol(new_list)) return new_list;
  lbm_value curr_targ = new_list;

  while (lbm_is_cons(curr) && copy_n > 0) {
    lbm_value v = lbm_car(curr);
    lbm_set_car(curr_targ, v);
    curr_targ = lbm_cdr(curr_targ);
    curr = lbm_cdr(curr);
    copy_n --;
  }

  return new_list;
}

// Append for proper lists only
// Destructive update of list1.
lbm_value lbm_list_append(lbm_value list1, lbm_value list2) {

  if(lbm_is_list_rw(list1) &&
     lbm_is_list(list2)) {

    lbm_value curr = list1;
    while(lbm_type_of(lbm_cdr(curr)) == LBM_TYPE_CONS) {
      curr = lbm_cdr(curr);
    }
    if (lbm_is_symbol_nil(curr)) return list2;
    lbm_set_cdr(curr, list2);
    return list1;
  }
  return ENC_SYM_EERROR;
}

lbm_value lbm_list_drop(unsigned int n, lbm_value ls) {
  lbm_value curr = ls;
  while (lbm_type_of_functional(curr) == LBM_TYPE_CONS &&
         n > 0) {
    curr = lbm_cdr(curr);
    n --;
  }
  return curr;
}

lbm_value lbm_index_list(lbm_value l, int32_t n) {
  lbm_value curr = l;

  if (n < 0) {
    int32_t len = (int32_t)lbm_list_length(l);
    n = len + n;
    if (n < 0) return ENC_SYM_NIL;
  }

  while (lbm_is_cons(curr) &&
          n > 0) {
    curr = lbm_cdr(curr);
    n --;
  }
  if (lbm_is_cons(curr)) {
    return lbm_car(curr);
  } else {
    return ENC_SYM_NIL;
  }
}

// High-level arrays are just bytearrays but with a different tag and pointer type.
// These arrays will be inspected by GC and the elements of the array will be marked.

// Arrays are part of the heap module because their lifespan is managed
// by the garbage collector. The data in the array is not stored
// in the "heap of cons cells".
int lbm_heap_allocate_array_base(lbm_value *res, bool byte_array, lbm_uint size){

  lbm_uint tag = ENC_SYM_ARRAY_TYPE;
  lbm_uint type = LBM_TYPE_ARRAY;
  lbm_array_header_t *array = NULL;
  lbm_array_header_extended_t *ext_array = NULL;

  if (byte_array) {
    array = (lbm_array_header_t*)lbm_malloc(sizeof(lbm_array_header_t));
  } else {
    tag = ENC_SYM_LISPARRAY_TYPE;
    type = LBM_TYPE_LISPARRAY;
    size = sizeof(lbm_value) * size;
    array = (lbm_array_header_t*)lbm_malloc(sizeof(lbm_array_header_extended_t));
    ext_array = (lbm_array_header_extended_t*)array;
  }
  if (array) {
    if (!byte_array) ext_array->index = 0;

    array->data = NULL;
    array->size = size;
    if ( size > 0) {
      array->data = (lbm_uint*)lbm_malloc(size);
      if (array->data == NULL) {
        lbm_memory_free((lbm_uint*)array);
        goto allocate_array_merror;
      }
      // It is more important to zero out high-level arrays.
      // 0 is symbol NIL which is perfectly safe for the GC to inspect.
      memset(array->data, 0, size);
    }
    // allocating a cell for array's heap-presence
    lbm_value cell = lbm_heap_allocate_cell(type, (lbm_uint) array, tag);
    if (cell == ENC_SYM_MERROR) {
      lbm_memory_free((lbm_uint*)array->data);
      lbm_memory_free((lbm_uint*)array);
      goto allocate_array_merror;
    }
    *res = cell;
    lbm_heap_state.num_alloc_arrays ++;
    return 1;
  }
 allocate_array_merror:
  *res = ENC_SYM_MERROR;
  return 0;
}

int lbm_heap_allocate_array(lbm_value *res, lbm_uint size){
  return lbm_heap_allocate_array_base(res, true, size);
}

int lbm_heap_allocate_lisp_array(lbm_value *res, lbm_uint size) {
  return lbm_heap_allocate_array_base(res, false, size);
}

int lbm_lift_array(lbm_value *value, char *data, lbm_uint num_elt) {

  lbm_array_header_t *array = NULL;
  lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_CONS, ENC_SYM_NIL, ENC_SYM_ARRAY_TYPE);

  if (cell == ENC_SYM_MERROR) {
    *value = cell;
    return 0;
  }

  array = (lbm_array_header_t*)lbm_malloc(sizeof(lbm_array_header_t));

  if (array == NULL) {
    lbm_set_car_and_cdr(cell, ENC_SYM_NIL, ENC_SYM_NIL);
    *value = ENC_SYM_MERROR;
    return 0;
  }

  array->data = (lbm_uint*)data;
  array->size = num_elt;

  lbm_set_car(cell, (lbm_uint)array);

  cell = lbm_set_ptr_type(cell, LBM_TYPE_ARRAY);
  *value = cell;
  return 1;
}

lbm_int lbm_heap_array_get_size(lbm_value arr) {

  lbm_int r = -1;
  lbm_array_header_t *header = lbm_dec_array_r(arr);
  if (header) {
    r = (lbm_int)header->size;
  }
  return r;
}

const uint8_t *lbm_heap_array_get_data_ro(lbm_value arr) {
  uint8_t *r = NULL;
  lbm_array_header_t *header = lbm_dec_array_r(arr);
  if (header) {
    r = (uint8_t*)header->data;
  }
  return r;
}

/* Explicitly freeing an array.

   This is a highly unsafe operation and can only be safely
   used if the heap cell that points to the array has not been made
   accessible to the program.

   So This function can be used to free an array in case an array
   is being constructed and some error case appears while doing so
   If the array still have not become available it can safely be
   "explicitly" freed.

   The problem is that if the "array" heap-cell is made available to
   the program, this cell can easily be duplicated and we would have
   to search the entire heap to find all cells pointing to the array
   memory in question and "null"-them out before freeing the memory
*/

int lbm_heap_explicit_free_array(lbm_value arr) {

  int r = 0;
  if (lbm_is_array_rw(arr) && lbm_cdr(arr) == ENC_SYM_ARRAY_TYPE) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(arr);
    if (header == NULL) {
      return 0;
    }
    lbm_memory_free((lbm_uint*)header->data);
    lbm_memory_free((lbm_uint*)header);

    arr = lbm_set_ptr_type(arr, LBM_TYPE_CONS);
    lbm_set_car(arr, ENC_SYM_NIL);
    lbm_set_cdr(arr, ENC_SYM_NIL);
    r = 1;
  }

  return r;
}

static bool dummy_flash_write(lbm_uint ix, lbm_uint val) {
  (void)ix;
  (void)val;
  return false;
}

static const_heap_write_fun const_heap_write = dummy_flash_write;

int lbm_const_heap_init(const_heap_write_fun w_fun,
                        lbm_const_heap_t *heap,
                        lbm_uint *addr) {
  if (((uintptr_t)addr % 4) != 0) return 0;

  if (!lbm_const_heap_mutex_initialized) {
    mutex_init(&lbm_const_heap_mutex);
    lbm_const_heap_mutex_initialized = true;
  }

  if (!lbm_mark_mutex_initialized) {
    mutex_init(&lbm_mark_mutex);
    lbm_mark_mutex_initialized = true;
  }

  const_heap_write = w_fun;

  heap->heap = addr;
  heap->size = 0;
  heap->next = 0;

  lbm_const_heap_state = heap;
  // ref_cell views the lbm_uint array as an lbm_cons_t array
  lbm_heaps[1] = (lbm_cons_t*)addr;
  return 1;
}

lbm_flash_status lbm_allocate_const_cell(lbm_value *res) {
  lbm_flash_status r = LBM_FLASH_FULL;

  mutex_lock(&lbm_const_heap_mutex);
  // waste a cell if we have ended up unaligned after writing an array to flash.
  if (lbm_const_heap_state->next % 2 == 1) {
    lbm_const_heap_state->next++;
  }

  if (lbm_const_heap_state &&
      (lbm_const_heap_state->next+1) < (uint32_t)lbm_image_get_write_index()) {
    // A cons cell uses two words.
    lbm_value cell = lbm_const_heap_state->next;
    lbm_const_heap_state->next += 2;
    *res = (cell << LBM_ADDRESS_SHIFT) | LBM_PTR_BIT | LBM_TYPE_CONS | LBM_PTR_TO_CONSTANT_BIT;
    r = LBM_FLASH_WRITE_OK;
  }
  mutex_unlock(&lbm_const_heap_mutex);
  return r;
}

lbm_flash_status lbm_allocate_const_raw(lbm_uint nwords, lbm_uint *res) {
  lbm_flash_status r = LBM_FLASH_FULL;

  if (lbm_const_heap_state &&
      (lbm_const_heap_state->next + nwords) < (uint32_t)lbm_image_get_write_index()) {
    lbm_uint ix = lbm_const_heap_state->next;
    *res = (lbm_uint)&lbm_const_heap_state->heap[ix];
    lbm_const_heap_state->next += nwords;
    r = LBM_FLASH_WRITE_OK;
  }
  return r;
}

lbm_flash_status lbm_write_const_raw(lbm_uint *data, lbm_uint n, lbm_uint *res) {

  lbm_flash_status r = LBM_FLASH_FULL;

  if (lbm_const_heap_state &&
      (lbm_const_heap_state->next + n) < (uint32_t)lbm_image_get_write_index()) {
    lbm_uint ix = lbm_const_heap_state->next;

    for (unsigned int i = 0; i < n; i ++) {
      if (!const_heap_write(((lbm_uint*)data)[i],ix + i))
        return LBM_FLASH_WRITE_ERROR;
    }
    lbm_const_heap_state->next += n;
    *res = (lbm_uint)&lbm_const_heap_state->heap[ix];
    r = LBM_FLASH_WRITE_OK;
  }
  return r;
}

lbm_flash_status lbm_const_write(lbm_uint *tgt, lbm_uint val) {

  if (lbm_const_heap_state) {
    lbm_uint flash = (lbm_uint)lbm_const_heap_state->heap;
    lbm_uint ix = (((lbm_uint)tgt - flash) / sizeof(lbm_uint)); // byte address to ix
    if (const_heap_write(val, ix)) {
      return LBM_FLASH_WRITE_OK;
    }
    return LBM_FLASH_WRITE_ERROR;
  }
  return LBM_FLASH_FULL;
}

lbm_flash_status write_const_cdr(lbm_value cell, lbm_value val) {
  lbm_uint addr = lbm_dec_ptr(cell);
  if (const_heap_write(val, addr+1))
    return LBM_FLASH_WRITE_OK;
  return LBM_FLASH_WRITE_ERROR;
}

lbm_flash_status write_const_car(lbm_value cell, lbm_value val) {
  lbm_uint addr = lbm_dec_ptr(cell);
  if (const_heap_write(val, addr))
    return LBM_FLASH_WRITE_OK;
  return LBM_FLASH_WRITE_ERROR;
}

lbm_uint lbm_flash_memory_usage(void) {
  return lbm_const_heap_state->next;
}


// ////////////////////////////////////////////////////////////
// pointer reversal traversal
//
// Caveats:
//   * Structures on the constant heap cannot be traversed using
//     pointer reversal. If a dynamic structure is pointing into the
//     constant heap, the 'f' will be applied to the constant cons cell on
//     the border and then traversal will retreat.
//
//  * It may be impossible to detect all kinds of cycles
//    if attempting to use and restore the GC in a single pass
//    over the tree. To detect cycles all visited nodes must
//    remain detectable when traversing all branches!
//
//  * Potential fix is to run GC after a complete traversal of the
//    entire value in order to restore the GC bits.
//
//  * If we leave GC bits set when traversing values, we can use this
//    to detect cycles that happen in multiple steps accross values
//    in the environment.
//
//  * lbm_ptr_rev_trav with the "do_nothing" travfun is the same thing
//    as a GC mark phase! Maybe utilize this for code-size
//    purposes. This also increases the amount of testing the
//    ptr_rev_trav function is subjected to.

void lbm_ptr_rev_trav(trav_fun f, lbm_value v, void* arg) {

  lbm_value curr = v;
  lbm_value prev = lbm_enc_cons_ptr(LBM_PTR_NULL);
  while (true) {

    // Run leftwards and process conses until
    // hitting a leaf in the left direction.

    // If curr is marked here there is a cycle in the graph.
    // In case of a cycle or leaf, this first loop is exited.
    while (((lbm_is_cons_rw(curr)) ||
            (lbm_is_lisp_array_rw(curr))) && !gc_marked(curr)) {
      lbm_cons_t *cell = lbm_ref_cell(curr);
      if (lbm_is_cons(curr)) {
        // In-order traversal
        if (f(curr, false, arg) == TRAV_FUN_SUBTREE_DONE) {
          lbm_gc_mark_phase(curr);
          break;
        }
        gc_mark(curr);

        lbm_value next = 0;
        value_assign(&next, cell->car);
        value_assign(&cell->car, prev);
        value_assign(&prev, curr);
        value_assign(&curr, next);
      } else { // it is an array

        lbm_array_header_extended_t *arr = (lbm_array_header_extended_t*)cell->car;
        lbm_value *arr_data = (lbm_value *)arr->data;
        uint32_t index = arr->index;
        if (arr->size == 0) break;
        if (index == 0) { // index should only be 0 or there is a potential cycle
          if (f(curr, false, arg) == TRAV_FUN_SUBTREE_DONE) {
            lbm_gc_mark_phase(curr);
            break;
          }
          arr->index = 1;
          gc_mark(curr);

          lbm_value next = 0;
          value_assign(&next, arr_data[0]);
          value_assign(&arr_data[0], prev);
          value_assign(&prev, curr);
          value_assign(&curr, next);
        }
      }
    }
    // Currently there are a few different users of this traversal.
    // size, flatten and detect_sharing.
    // detect_sharing make use of the shared (true) argument in f(curr, true, arg)
    // while the other do not. detect_sharing also assumes it is run once per env item
    // while not resetting any GC-bits in between. This detects global sharing.

    if (lbm_is_ptr(curr) && gc_marked(curr)) {
      // gc bit set so this subtree is already traversed.
      // f is called with true to indicate visited node.
      // if this happens during a sharing discovery phase, curr will be added to sharing table.
      f(curr, true, arg);
      // In this case f should not be able to return subtree continue.
      // The only correct return value from f is PROCEED.
    } else if (!lbm_is_cons(curr) || // Found a leaf
               (curr & LBM_PTR_TO_CONSTANT_BIT)) {
      if (lbm_is_ptr(curr) && !(curr & LBM_PTR_TO_CONSTANT_BIT)) gc_mark(curr); // Mark it so that the mandatory GC does not swipe it.
      f(curr, false, arg);
    }

    // Now either prev has the "flag" set or it doesnt.
    // If the flag is set that means that the prev node
    // have had both its car and cdr visited. So that node is done!
    //
    // If the flag is not set, jump down to SWAP

    while ((lbm_is_cons(prev) &&
            (lbm_dec_ptr(prev) != LBM_PTR_NULL) && // is LBM_NULL a cons type?
            lbm_get_gc_flag(lbm_car(prev))) ||
           lbm_is_lisp_array_rw(prev)) {
      lbm_cons_t *cell = lbm_ref_cell(prev);
      if (lbm_is_cons(prev)) {

        // clear the flag
        // This means that we are done with a "CDR" child.
        // prev = [ a , b ][flag = 1]
        // =>
        // prev = [ a , b ][flag = 0]

        //gc_clear_mark(prev);
        cell->car = lbm_clr_gc_flag(cell->car);
        // Move on downwards until
        //   finding a cons cell without flag or NULL

        // curr = c
        // prev = [ a , b ][flag = 0]
        // =>
        // prev = [ a , c ][flag = 0]
        // curr = prev
        // prev = b

        lbm_value next = 0;
        value_assign(&next, cell->cdr);
        value_assign(&cell->cdr, curr);
        value_assign(&curr, prev);
        value_assign(&prev, next);
      } else { // is an array
        lbm_array_header_extended_t *arr = (lbm_array_header_extended_t*)cell->car;
        lbm_value *arr_data = (lbm_value *)arr->data;
        size_t arr_size = (size_t)arr->size / sizeof(lbm_value);
        lbm_value next = 0;
        if (arr->index == arr_size) {
          value_assign(&next, arr_data[arr->index-1]);
          value_assign(&arr_data[arr->index-1], curr);
          value_assign(&curr, prev);
          value_assign(&prev, next);
          arr->index = 0;
        } else {
          break;
        }
      }
    }

    // SWAP

    // if the prev node is NULL we have traced backwards all the
    // way back to where curr == v. Another alternative is that
    // the input v was an Atom.  We are done!
    if (lbm_is_ptr(prev) &&
        lbm_dec_ptr(prev) == LBM_PTR_NULL) {
      if (lbm_is_cons(curr)) {
        //gc_clear_mark(curr);
      }
      //done = true;
      break;
    }

    // if the prev node is not NULL then we should move
    // down to the prev node and start process its remaining child.
    else if (lbm_is_cons(prev)) {

      lbm_cons_t *cell = lbm_ref_cell(prev);
      lbm_value next = 0;


      //  prev = [ p , cdr ][flag = 0]
      //  =>
      //  prev = [ p , cdr ][flag = 1]

      cell->car = lbm_set_gc_flag(cell->car);

      // switch to processing the cdr field and set the flag.
      // curr = c
      // prev = [ a, b ][flag = 1]
      // =>
      // prev = [ c, a ][flag = 1]
      // curr = b

      value_assign(&next, cell->car);
      value_assign(&cell->car, curr);
      value_assign(&curr, cell->cdr);
      value_assign(&cell->cdr, next);
    } else if (lbm_is_lisp_array_rw(prev)) {
      lbm_cons_t *cell = lbm_ref_cell(prev);
      lbm_array_header_extended_t *arr = (lbm_array_header_extended_t*)cell->car;
      lbm_value *arr_data = (lbm_value *)arr->data;
      lbm_value next = 0;

      value_assign(&next, arr_data[arr->index-1]);
      value_assign(&arr_data[arr->index-1], curr);
      value_assign(&curr, arr_data[arr->index]);
      value_assign(&arr_data[arr->index], next);
      arr->index = arr->index + 1;
    }
  }
}
