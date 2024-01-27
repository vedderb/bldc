/*
    Copyright 2018, 2020, 2022, 2023 Joel Svensson  svenssonjoel@yahoo.se
                          2022       Benjamin Vedder

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
  lbm_value i = lbm_cons((lbm_uint)x, lbm_enc_sym(SYM_RAW_I_TYPE));
  if (lbm_type_of(i) == LBM_TYPE_SYMBOL) return i;
  return lbm_set_ptr_type(i, LBM_TYPE_I32);
#else
  return (((lbm_uint)x) << LBM_VAL_SHIFT) | LBM_TYPE_I32;
#endif
}

lbm_value lbm_enc_u32(uint32_t x) {
#ifndef LBM64
  lbm_value u = lbm_cons(x, lbm_enc_sym(SYM_RAW_U_TYPE));
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
  lbm_value f = lbm_cons(t, lbm_enc_sym(SYM_RAW_F_TYPE));
  if (lbm_type_of(f) == LBM_TYPE_SYMBOL) return f;
  return lbm_set_ptr_type(f, LBM_TYPE_FLOAT);
#else
  lbm_uint t = 0;
  memcpy(&t, &x, sizeof(float));
  return (((lbm_uint)t) << LBM_VAL_SHIFT) | LBM_TYPE_FLOAT;
#endif
}

lbm_value lbm_enc_i64(int64_t x) {
#ifndef LBM64
  lbm_value res = lbm_enc_sym(SYM_MERROR);
  lbm_uint* storage = lbm_memory_allocate(2);
  if (storage) {
    res = lbm_cons((lbm_uint)storage, lbm_enc_sym(SYM_IND_I_TYPE));
    if (lbm_type_of(res) != LBM_TYPE_SYMBOL) {
      memcpy(storage,&x, 8);
      res = lbm_set_ptr_type(res, LBM_TYPE_I64);
    }
  }
  return res;
#else
  lbm_value u = lbm_cons((uint64_t)x, lbm_enc_sym(SYM_RAW_I_TYPE));
  if (lbm_type_of(u) == LBM_TYPE_SYMBOL) return u;
  return lbm_set_ptr_type(u, LBM_TYPE_I64);
#endif
}

lbm_value lbm_enc_u64(uint64_t x) {
#ifndef LBM64
  lbm_value res = lbm_enc_sym(SYM_MERROR);
  uint8_t* storage = lbm_malloc(sizeof(uint64_t));
  if (storage) {
    res = lbm_cons((lbm_uint)storage, lbm_enc_sym(SYM_IND_U_TYPE));
    if (lbm_type_of(res) != LBM_TYPE_SYMBOL) {
      memcpy(storage,&x, sizeof(uint64_t));
      res = lbm_set_ptr_type(res, LBM_TYPE_U64);
    }
  }
  return res;
#else
  lbm_value u = lbm_cons(x, lbm_enc_sym(SYM_RAW_U_TYPE));
  if (lbm_type_of(u) == LBM_TYPE_SYMBOL) return u;
  return lbm_set_ptr_type(u, LBM_TYPE_U64);
#endif
}

lbm_value lbm_enc_double(double x) {
#ifndef LBM64
  lbm_value res = lbm_enc_sym(SYM_MERROR);
  lbm_uint* storage = lbm_memory_allocate(2);
  if (storage) {
    res = lbm_cons((lbm_uint)storage, lbm_enc_sym(SYM_IND_F_TYPE));
    if (lbm_type_of(res) != LBM_TYPE_SYMBOL) {
      memcpy(storage,&x, 8);
      res = lbm_set_ptr_type(res, LBM_TYPE_DOUBLE);
    }
  }
  return res;
#else
  lbm_uint t;
  memcpy(&t, &x, sizeof(double));
  lbm_value f = lbm_cons(t, lbm_enc_sym(SYM_RAW_F_TYPE));
  if (lbm_type_of(f) == LBM_TYPE_SYMBOL) return f;
  return lbm_set_ptr_type(f, LBM_TYPE_DOUBLE);
#endif
}

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
  double d;
  uint32_t *data = (uint32_t*)lbm_car(x);
  if (data == NULL) return 0; // no good way to report error from here currently.
  memcpy(&d, data, sizeof(double));
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
  uint64_t u;
  uint32_t *data = (uint32_t*)lbm_car(x);
  if (data == NULL) return 0;
  memcpy(&u, data, 8);
  return u;
#else
  return (uint64_t)lbm_car(x);
#endif
}

int64_t lbm_dec_i64(lbm_value x) {
#ifndef LBM64
  int64_t i;
  uint32_t *data = (uint32_t*)lbm_car(x);
  if (data == NULL) return 0;
  memcpy(&i, data, 8);
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

char lbm_dec_as_char(lbm_value a) {
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    return (char) lbm_dec_char(a);
  case LBM_TYPE_I:
    return (char) lbm_dec_i(a);
  case LBM_TYPE_U:
    return (char) lbm_dec_u(a);
  case LBM_TYPE_I32:
    return (char) lbm_dec_i32(a);
  case LBM_TYPE_U32:
    return (char) lbm_dec_u32(a);
  case LBM_TYPE_FLOAT:
    return (char)lbm_dec_float(a);
  case LBM_TYPE_I64:
    return (char) lbm_dec_i64(a);
  case LBM_TYPE_U64:
    return (char) lbm_dec_u64(a);
  case LBM_TYPE_DOUBLE:
    return (char) lbm_dec_double(a);
  }
  return 0;
}

uint32_t lbm_dec_as_u32(lbm_value a) {
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    return (uint32_t) lbm_dec_char(a);
  case LBM_TYPE_I:
    return (uint32_t) lbm_dec_i(a);
  case LBM_TYPE_U:
    return (uint32_t) lbm_dec_u(a);
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_U32:
    return (uint32_t) lbm_dec_u32(a);
  case LBM_TYPE_FLOAT:
    return (uint32_t)lbm_dec_float(a);
  case LBM_TYPE_I64:
    return (uint32_t) lbm_dec_i64(a);
  case LBM_TYPE_U64:
    return (uint32_t) lbm_dec_u64(a);
  case LBM_TYPE_DOUBLE:
    return (uint32_t) lbm_dec_double(a);
  }
  return 0;
}

uint64_t lbm_dec_as_u64(lbm_value a) {
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
    return (uint64_t) lbm_dec_char(a);
  case LBM_TYPE_I:
    return (uint64_t) lbm_dec_i(a);
  case LBM_TYPE_U:
    return lbm_dec_u(a);
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_U32:
    return (uint64_t) lbm_dec_u32(a);
  case LBM_TYPE_FLOAT:
    return (uint64_t)lbm_dec_float(a);
  case LBM_TYPE_I64:
  case LBM_TYPE_U64:
    return (uint64_t) lbm_dec_u64(a);
  case LBM_TYPE_DOUBLE:
    return (uint64_t) lbm_dec_double(a);
  }
  return 0;
}


int32_t lbm_dec_as_i32(lbm_value a) {
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
      return (int32_t) lbm_dec_char(a);
  case LBM_TYPE_I:
    return (int32_t) lbm_dec_i(a);
  case LBM_TYPE_U:
    return (int32_t) lbm_dec_u(a);
  case LBM_TYPE_I32:
  case LBM_TYPE_U32:
    return (int32_t) lbm_dec_i32(a);
  case LBM_TYPE_FLOAT:
    return (int32_t) lbm_dec_float(a);
  case LBM_TYPE_I64:
    return (int32_t) lbm_dec_i64(a);
  case LBM_TYPE_U64:
    return (int32_t) lbm_dec_u64(a);
  case LBM_TYPE_DOUBLE:
    return (int32_t) lbm_dec_double(a);

  }
  return 0;
}

int64_t lbm_dec_as_i64(lbm_value a) {
  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
      return (int64_t) lbm_dec_char(a);
  case LBM_TYPE_I:
    return lbm_dec_i(a);
  case LBM_TYPE_U:
    return (int64_t) lbm_dec_u(a);
  case LBM_TYPE_I32:
  case LBM_TYPE_U32:
    return (int64_t) lbm_dec_i32(a);
  case LBM_TYPE_FLOAT:
    return (int64_t) lbm_dec_float(a);
  case LBM_TYPE_I64:
  case LBM_TYPE_U64:
    return (int64_t) lbm_dec_i64(a);
  case LBM_TYPE_DOUBLE:
    return (int64_t) lbm_dec_double(a);
  }
  return 0;
}


float lbm_dec_as_float(lbm_value a) {

  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
      return (float) lbm_dec_char(a);
  case LBM_TYPE_I:
    return (float) lbm_dec_i(a);
  case LBM_TYPE_U:
    return (float) lbm_dec_u(a);
  case LBM_TYPE_I32:
    return (float) lbm_dec_i32(a);
  case LBM_TYPE_U32:
    return (float) lbm_dec_u32(a);
  case LBM_TYPE_FLOAT:
    return (float) lbm_dec_float(a);
  case LBM_TYPE_I64:
    return (float) lbm_dec_i64(a);
  case LBM_TYPE_U64:
    return (float) lbm_dec_u64(a);
  case LBM_TYPE_DOUBLE:
    return (float) lbm_dec_double(a);
  }
  return 0;
}

double lbm_dec_as_double(lbm_value a) {

  switch (lbm_type_of_functional(a)) {
  case LBM_TYPE_CHAR:
      return (double) lbm_dec_char(a);
  case LBM_TYPE_I:
    return (double) lbm_dec_i(a);
  case LBM_TYPE_U:
    return (double) lbm_dec_u(a);
  case LBM_TYPE_I32:
    return (double) lbm_dec_i32(a);
  case LBM_TYPE_U32:
    return (double) lbm_dec_u32(a);
  case LBM_TYPE_FLOAT:
    return (double) lbm_dec_float(a);
  case LBM_TYPE_I64:
    return (double) lbm_dec_i64(a);
  case LBM_TYPE_U64:
    return (double) lbm_dec_u64(a);
  case LBM_TYPE_DOUBLE:
    return (double) lbm_dec_double(a);
  }
  return 0;
}
/****************************************************/
/* IS                                               */

bool lbm_is_number(lbm_value x) {
  lbm_uint t = lbm_type_of(x);
  #ifndef LBM64
  return (t & 0xC || t & LBM_NUMBER_MASK);
  #else
  return (t & ((uint64_t)0x1C) || t & LBM_NUMBER_MASK);
  #endif
}

/****************************************************/
/* HEAP MANAGEMENT                                  */

static int generate_freelist(size_t num_cells) {
  size_t i = 0;

  if (!lbm_heap_state.heap) return 0;

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

  return 1;
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

int lbm_heap_init(lbm_cons_t *addr, lbm_uint num_cells,
                  lbm_uint gc_stack_size) {

  if (((uintptr_t)addr % 8) != 0) return 0;

  memset(addr,0, sizeof(lbm_cons_t) * num_cells);

  lbm_uint *gc_stack_storage = (lbm_uint*)lbm_malloc(gc_stack_size * sizeof(lbm_uint));
  if (gc_stack_storage == NULL) return 0;
  
  heap_init_state(addr, num_cells,
                  gc_stack_storage, gc_stack_size);

  lbm_heaps[0] = addr;

  return generate_freelist(num_cells);
}

lbm_uint lbm_heap_num_free(void) {
  return lbm_heap_state.heap_size - lbm_heap_state.num_alloc;
}

lbm_value lbm_heap_allocate_cell(lbm_type ptr_type, lbm_value car, lbm_value cdr) {

  lbm_value res;

  // it is a ptr replace freelist with cdr of freelist;
  res = lbm_heap_state.freelist;

  if (lbm_type_of(res) == LBM_TYPE_CONS) {
    lbm_uint heap_ix = lbm_dec_ptr(res);
    //lbm_cons_t *rc = lbm_ref_cell(res);
    lbm_heap_state.freelist = lbm_heap_state.heap[heap_ix].cdr;

    lbm_heap_state.num_alloc++;

    lbm_heap_state.heap[heap_ix].car = car;
    lbm_heap_state.heap[heap_ix].cdr = cdr;
    res = lbm_set_ptr_type(res, ptr_type);
    return res;
  }
  else if ((lbm_type_of(res) == LBM_TYPE_SYMBOL) &&
           (lbm_dec_sym(res) == SYM_NIL)) {
    // all is as it should be (but no free cells)
    return ENC_SYM_MERROR;
  }
  else {
    return ENC_SYM_FATAL_ERROR;
  }
}

lbm_value lbm_heap_allocate_list(lbm_uint n) {
  if (n == 0) return ENC_SYM_NIL;
  if (lbm_heap_num_free() < n) return ENC_SYM_MERROR;

  lbm_value curr = lbm_heap_state.freelist;
  lbm_value res  = curr;
  if (lbm_type_of(curr) == LBM_TYPE_CONS) {

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
  return ENC_SYM_FATAL_ERROR;
}

lbm_value lbm_heap_allocate_list_init_va(unsigned int n, va_list valist) {
  if (n == 0) return ENC_SYM_NIL;
  if (lbm_heap_num_free() < n) return ENC_SYM_MERROR;

  lbm_value curr = lbm_heap_state.freelist;
  lbm_value res  = curr;
  if (lbm_type_of(curr) == LBM_TYPE_CONS) {

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
  return ENC_SYM_FATAL_ERROR;
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
  return lbm_heap_state.gc_stack.max_sp;
}

lbm_uint lbm_get_gc_stack_size(void) {
  return lbm_heap_state.gc_stack.size;
}

#ifdef USE_GC_PTR_REV
static inline void value_assign(lbm_value *a, lbm_value b) {
  lbm_value a_old = *a & LBM_GC_MASK;
  *a = a_old | (b & ~LBM_GC_MASK);
}

void lbm_gc_mark_phase(lbm_value root) {
  bool work_to_do = true;

  if (!lbm_is_ptr(root)) return;

  mutex_lock(&lbm_const_heap_mutex);
  lbm_value curr = root;
  lbm_value prev = lbm_enc_cons_ptr(LBM_PTR_NULL);

  while (work_to_do) {
    // follow leftwards pointers
    while (lbm_is_ptr(curr) &&
           (lbm_dec_ptr(curr) != LBM_PTR_NULL) &&
           ((curr & LBM_PTR_TO_CONSTANT_BIT) == 0) &&
           !lbm_get_gc_mark(lbm_cdr(curr))) {
      // Mark the cell if not a constant cell
      lbm_cons_t *cell = lbm_ref_cell(curr);
      cell->cdr = lbm_set_gc_mark(cell->cdr);
      if (lbm_is_cons_rw(curr)) {
        lbm_value next = 0;
        value_assign(&next, cell->car);
        value_assign(&cell->car, prev);
        value_assign(&prev,curr);
        value_assign(&curr, next);
      }
      // Will jump out next iteration as gc mark is set in curr.
    }
    while (lbm_is_ptr(prev) &&
           (lbm_dec_ptr(prev) != LBM_PTR_NULL) &&
           lbm_get_gc_flag(lbm_car(prev)) ) {
      // clear the flag
      lbm_cons_t *cell = lbm_ref_cell(prev);
      cell->car = lbm_clr_gc_flag(cell->car);
      lbm_value next = 0;
      value_assign(&next, cell->cdr);
      value_assign(&cell->cdr, curr);
      value_assign(&curr, prev);
      value_assign(&prev, next);
    }
    if (lbm_is_ptr(prev) &&
        lbm_dec_ptr(prev) == LBM_PTR_NULL) {
      work_to_do = false;
    } else if (lbm_is_ptr(prev)) {
      // set the flag
      lbm_cons_t *cell = lbm_ref_cell(prev);
      cell->car = lbm_set_gc_flag(cell->car);
      lbm_value next = 0;
      value_assign(&next, cell->car);
      value_assign(&cell->car, curr);
      value_assign(&curr, cell->cdr);
      value_assign(&cell->cdr, next);
    }
  }
  mutex_unlock(&lbm_const_heap_mutex);
}

#else
extern eval_context_t *ctx_running;
void lbm_gc_mark_phase(lbm_value root) {

  lbm_stack_t *s = &lbm_heap_state.gc_stack;
  s->data[s->sp++] = root;

  while (!lbm_stack_is_empty(s)) {
    lbm_value curr;
    lbm_pop(s, &curr);

  mark_shortcut:
    if (!lbm_is_ptr(curr)) {
      continue;
    }

    if (curr & LBM_PTR_TO_CONSTANT_BIT) {
      // Go back and pop next root.
      // Constant values can only refer to non-constants by name.
      continue;
    }
    lbm_cons_t *cell = lbm_ref_cell(curr);

    lbm_uint gc_mark = lbm_get_gc_mark(cell->cdr);
    if (gc_mark) continue;
    lbm_heap_state.gc_marked ++;
    cell->cdr = lbm_set_gc_mark(cell->cdr);

    lbm_value t_ptr = lbm_type_of(curr);

    if (t_ptr >= LBM_NON_CONS_POINTER_TYPE_FIRST &&
        t_ptr <= LBM_NON_CONS_POINTER_TYPE_LAST) continue;

    if (cell->car == ENC_SYM_CONT) {
      lbm_value cont = cell->cdr;
      lbm_array_header_t *arr = (lbm_array_header_t*)lbm_car(cont);
      lbm_value *arrdata = (lbm_value *)arr->data;
      for (lbm_uint i = 0; i < arr->size / 4; i ++) {
        if (lbm_is_ptr(arrdata[i]) &&
            !((arrdata[i] & LBM_CONTINUATION_INTERNAL) == LBM_CONTINUATION_INTERNAL)) {
          if (!lbm_push (s, arrdata[i])) {
            lbm_critical_error();
          }
        }
      }
    }
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
#endif

// The free list should be a "proper list"
// Using a while loop to traverse over the cdrs
int lbm_gc_mark_freelist(void) {

  lbm_value curr;
  lbm_cons_t *t;
  lbm_value fl = lbm_heap_state.freelist;

  if (!lbm_is_ptr(fl)) {
    if (lbm_type_of(fl) == LBM_TYPE_SYMBOL &&
        fl == ENC_SYM_NIL){
      return 1; // Nothing to mark here
    } else {
      return 0;
    }
  }

  curr = fl;
  while (lbm_is_ptr(curr)){
    t = lbm_ref_cell(curr);
    t->cdr = lbm_set_gc_mark(t->cdr);
    curr = t->cdr;

    lbm_heap_state.gc_marked ++;
  }

  return 1;
}

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
        switch(lbm_dec_sym(heap[i].cdr)) {

        case SYM_IND_I_TYPE: /* fall through */
        case SYM_IND_U_TYPE:
        case SYM_IND_F_TYPE:
          lbm_memory_free((lbm_uint*)heap[i].car);
          break;
        case SYM_ARRAY_TYPE:{
          lbm_array_header_t *arr = (lbm_array_header_t*)heap[i].car;
          if (lbm_memory_ptr_inside((lbm_uint*)arr->data)) {
            lbm_memory_free((lbm_uint *)arr->data);
            lbm_heap_state.gc_recovered_arrays++;
          }
          lbm_memory_free((lbm_uint *)arr);
        } break;
        case SYM_CHANNEL_TYPE:{
          lbm_char_channel_t *chan = (lbm_char_channel_t*)heap[i].car;
          if (lbm_memory_ptr_inside((lbm_uint*)chan)) {
            lbm_memory_free((lbm_uint*)chan->state);
            lbm_memory_free((lbm_uint*)chan);
          }
        } break;
        case SYM_CUSTOM_TYPE: {
          lbm_uint *t = (lbm_uint*)heap[i].car;
          lbm_custom_type_destroy(t);
          lbm_memory_free(t);
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

  if (lbm_type_of(c) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(c) == SYM_NIL) {
    return ENC_SYM_NIL; // if nil, return nil.
  }

  return ENC_SYM_TERROR;
}

lbm_value lbm_caar(lbm_value c) {

  lbm_value tmp;

  if (lbm_is_ptr(c)) {
    tmp = lbm_ref_cell(c)->car;

    if (lbm_is_ptr(tmp)) {
      return lbm_ref_cell(tmp)->car;
    } else if (lbm_is_symbol(tmp) && lbm_dec_sym(tmp) == SYM_NIL) {
      return tmp;
    }
  } else if (lbm_is_symbol(c) && lbm_dec_sym(c) == SYM_NIL) {
    return c;
  }
  return ENC_SYM_TERROR;
}


lbm_value lbm_cadr(lbm_value c) {

  lbm_value tmp;

  if (lbm_is_ptr(c)) {
    tmp = lbm_ref_cell(c)->cdr;

    if (lbm_is_ptr(tmp)) {
      return lbm_ref_cell(tmp)->car;
    } else if (lbm_is_symbol(tmp) && lbm_dec_sym(tmp) == SYM_NIL) {
      return tmp;
    }
  } else if (lbm_is_symbol(c) && lbm_dec_sym(c) == SYM_NIL) {
    return c;
  }
  return ENC_SYM_TERROR;
}

lbm_value lbm_cdr(lbm_value c){

  if (lbm_type_of(c) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(c) == SYM_NIL) {
    return ENC_SYM_NIL; // if nil, return nil.
  }

  if (lbm_is_ptr(c)) {
    lbm_cons_t *cell = lbm_ref_cell(c);
    return cell->cdr;
  }
  return ENC_SYM_TERROR;
}

lbm_value lbm_cddr(lbm_value c) {

  if (lbm_is_ptr(c)) {
    lbm_value tmp = lbm_ref_cell(c)->cdr;
    if (lbm_is_ptr(tmp)) {
      return lbm_ref_cell(tmp)->cdr;
    }
  }
  if (lbm_is_symbol(c) && lbm_dec_sym(c) == SYM_NIL) {
    return ENC_SYM_NIL;
  }
  return ENC_SYM_TERROR;
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

/* calculate the length of a list and check that each element
   fullfills the predicate pred */
unsigned int lbm_list_length_pred(lbm_value c, bool *pres, bool (*pred)(lbm_value)) {
  bool res = true;
  unsigned int len = 0;

  while (lbm_is_cons(c)){
    len ++;
    res = res && pred(lbm_car(c));
    c = lbm_cdr(c);
  }
  *pres = res;
  return len;
}

/* reverse a proper list */
lbm_value lbm_list_reverse(lbm_value list) {
  if (lbm_type_of(list) == LBM_TYPE_SYMBOL) {
    return list;
  }

  lbm_value curr = list;

  lbm_value new_list = ENC_SYM_NIL;
  while (lbm_is_cons(curr)) {

    new_list = lbm_cons(lbm_car(curr), new_list);
    if (lbm_type_of(new_list) == LBM_TYPE_SYMBOL) {
      return ENC_SYM_MERROR;
    }
    curr = lbm_cdr(curr);
  }
  return new_list;
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


// Arrays are part of the heap module because their lifespan is managed
// by the garbage collector. The data in the array is not stored
// in the "heap of cons cells".
int lbm_heap_allocate_array(lbm_value *res, lbm_uint size){

  lbm_array_header_t *array = NULL;

  array = (lbm_array_header_t*)lbm_memory_allocate(sizeof(lbm_array_header_t) / sizeof(lbm_uint));

  if (array == NULL) {
    *res = ENC_SYM_MERROR;
    return 0;
  }

  array->data = (lbm_uint*)lbm_malloc(size);

  if (array->data == NULL) {
    lbm_memory_free((lbm_uint*)array);
    *res = ENC_SYM_MERROR;
    return 0;
  }
  memset(array->data, 0, size);
  array->size = size;

  // allocating a cell for array's heap-presence
  lbm_value cell  = lbm_heap_allocate_cell(LBM_TYPE_ARRAY, (lbm_uint) array, ENC_SYM_ARRAY_TYPE);

  *res = cell;

  if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) { // Out of heap memory
    lbm_memory_free((lbm_uint*)array->data);
    lbm_memory_free((lbm_uint*)array);
    return 0;
  }

  lbm_heap_state.num_alloc_arrays ++;

  return 1;
}

// Convert a C array into an lbm_array.
// if the array is in LBM_MEMORY, the lifetime will be managed by the GC.
int lbm_lift_array(lbm_value *value, char *data, lbm_uint num_elt) {

  lbm_array_header_t *array = NULL;
  lbm_value cell  = lbm_heap_allocate_cell(LBM_TYPE_CONS, ENC_SYM_NIL, ENC_SYM_ARRAY_TYPE);

  if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) { // Out of heap memory
    *value = cell;
    return 0;
  }

  array = (lbm_array_header_t*)lbm_malloc(sizeof(lbm_array_header_t));

  if (array == NULL) {
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

  int r = -1;
  if (lbm_is_array_r(arr)) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(arr);
    if (header == NULL) {
      return r;
    }
    r = (lbm_int)header->size;
  }
  return r;
}

const uint8_t *lbm_heap_array_get_data_ro(lbm_value arr) {
  uint8_t *r = NULL;
  if (lbm_is_array_r(arr)) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(arr);
    if (header == NULL) {
      return r;
    }
    r = (uint8_t*)header->data;
  }
  return r;
}

uint8_t *lbm_heap_array_get_data_rw(lbm_value arr) {
  uint8_t *r = NULL;
  if (lbm_is_array_rw(arr)) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(arr);
    if (header == NULL) {
      return r;
    }
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
  if (lbm_is_array_rw(arr)) {

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

lbm_uint lbm_size_of(lbm_type t) {
  lbm_uint s = 0;
  switch(t) {
  case LBM_TYPE_BYTE:
    s = 1;
    break;
  case LBM_TYPE_I: /* fall through */
  case LBM_TYPE_U:
  case LBM_TYPE_SYMBOL:
    s = sizeof(lbm_uint);
    break;
  case LBM_TYPE_I32: /* fall through */
  case LBM_TYPE_U32:
  case LBM_TYPE_FLOAT:
    s = 4;
    break;
  case LBM_TYPE_I64: /* fall through */
  case LBM_TYPE_U64:
  case LBM_TYPE_DOUBLE:
    s = 8;
    break;
  }
  return s;
}

static bool dummy_flash_write(lbm_uint ix, lbm_uint val) {
  (void)ix;
  (void)val;
  return false;
}

static const_heap_write_fun const_heap_write = dummy_flash_write;

int lbm_const_heap_init(const_heap_write_fun w_fun,
                        lbm_const_heap_t *heap,
                        lbm_uint *addr,
                        lbm_uint  num_words) {
  if (((uintptr_t)addr % 4) != 0) return 0;
  if ((num_words % 2) != 0) return 0;

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
  heap->size = num_words;
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
      (lbm_const_heap_state->next+1) < lbm_const_heap_state->size) {
    // A cons cell uses two words.
    lbm_value cell = lbm_const_heap_state->next;
    lbm_const_heap_state->next += 2;
    *res = (cell << LBM_ADDRESS_SHIFT) | LBM_PTR_BIT | LBM_TYPE_CONS | LBM_PTR_TO_CONSTANT_BIT;
    r = LBM_FLASH_WRITE_OK;
  }
  mutex_unlock(&lbm_const_heap_mutex);
  return r;
}

lbm_flash_status lbm_write_const_raw(lbm_uint *data, lbm_uint n, lbm_uint *res) {

  lbm_flash_status r = LBM_FLASH_FULL;

  if (lbm_const_heap_state &&
      (lbm_const_heap_state->next + n) < lbm_const_heap_state->size) {
    lbm_uint ix = lbm_const_heap_state->next;

    for (unsigned int i = 0; i < n; i ++) {
      if (!const_heap_write(ix + i, ((lbm_uint*)data)[i]))
        return LBM_FLASH_WRITE_ERROR;
    }
    lbm_const_heap_state->next += n;
    *res = (lbm_uint)&lbm_const_heap_state->heap[ix];
    r = LBM_FLASH_WRITE_OK;
  }
  return r;
}

lbm_flash_status write_const_cdr(lbm_value cell, lbm_value val) {
  lbm_uint addr = lbm_dec_ptr(cell);
  if (const_heap_write(addr+1, val))
    return LBM_FLASH_WRITE_OK;
  return LBM_FLASH_WRITE_ERROR;
}

lbm_flash_status write_const_car(lbm_value cell, lbm_value val) {
  lbm_uint addr = lbm_dec_ptr(cell);
  if (const_heap_write(addr, val))
    return LBM_FLASH_WRITE_OK;
  return LBM_FLASH_WRITE_ERROR;
}

lbm_uint lbm_flash_memory_usage(void) {
  return lbm_const_heap_state->next;
}
