/*
    Copyright 2018, 2020, 2022 Joel Svensson  svenssonjoel@yahoo.se
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
#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif

lbm_heap_state_t lbm_heap_state;


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
  uint32_t t;
  memcpy(&t, &x, sizeof(float)); /*TODO: Assumes something about storage here ?*/
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
  lbm_uint* storage = lbm_memory_allocate(2);
  if (storage) {
    res = lbm_cons((lbm_uint)storage, lbm_enc_sym(SYM_IND_U_TYPE));
    if (lbm_type_of(res) != LBM_TYPE_SYMBOL) {
      memcpy(storage,&x, 8);
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
  memcpy(&t, &x, sizeof(lbm_float));
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
  if (lbm_type_of(val) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(val);
    if (array) {
      if (array->elt_type == LBM_TYPE_CHAR) {
        res = (char *)array->data;
      }
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
  switch (lbm_type_of(a)) {
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
  switch (lbm_type_of(a)) {
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
  switch (lbm_type_of(a)) {
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
  switch (lbm_type_of(a)) {
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
  switch (lbm_type_of(a)) {
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

  switch (lbm_type_of(a)) {
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

  switch (lbm_type_of(a)) {
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
  return (t & 0xC || t & 0x08000000);

#else
  return ((t == LBM_TYPE_I) ||
          (t == LBM_TYPE_U) ||
          (t == LBM_TYPE_CHAR) ||
          (t == LBM_TYPE_I32) ||
          (t == LBM_TYPE_U32) ||
          (t == LBM_TYPE_I64) ||
          (t == LBM_TYPE_U64) ||
          (t == LBM_TYPE_FLOAT) ||
          (t == LBM_TYPE_DOUBLE));
#endif
}


bool lbm_is_byte_array(lbm_value x) {
  if (lbm_is_array(x)) {
    lbm_array_header_t *header = (lbm_array_header_t*)lbm_car(x);
    return (header != NULL && header->elt_type == LBM_TYPE_BYTE);
  }
  return false;
}



/****************************************************/
/* HEAP MANAGEMENT                                  */

static inline void set_gc_mark(lbm_cons_t *cell) {
  lbm_value cdr = cell->cdr;
  cell->cdr =  lbm_set_gc_mark(cdr);
}

static inline void clr_gc_mark(lbm_cons_t *cell) {
  lbm_value cdr = cell->cdr;
  cell->cdr = lbm_clr_gc_mark(cdr);
}

static inline bool get_gc_mark(lbm_cons_t* cell) {
  lbm_value cdr = cell->cdr;
  return lbm_get_gc_mark(cdr);
}

static int generate_freelist(size_t num_cells) {
  size_t i = 0;

  if (!lbm_heap_state.heap) return 0;

  lbm_heap_state.freelist = lbm_enc_cons_ptr(0);

  lbm_cons_t *t;

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    t = lbm_ref_cell(lbm_enc_cons_ptr(i-1));
    t->car = ENC_SYM_RECOVERED;    // all cars in free list are "RECOVERED"
    t->cdr =  lbm_enc_cons_ptr(i);
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
                            lbm_uint *gc_stack_storage, lbm_uint gc_stack_size) {
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

  lbm_heap_state.gc_time_acc = 0;
  lbm_heap_state.gc_max_duration = 0;
  lbm_heap_state.gc_min_duration = UINT32_MAX;
}

void lbm_heap_new_gc_time(lbm_uint dur) {
  lbm_heap_state.gc_time_acc += dur;
  if (dur > lbm_heap_state.gc_max_duration)
    lbm_heap_state.gc_max_duration = dur;
  if (dur < lbm_heap_state.gc_min_duration)
    lbm_heap_state.gc_min_duration = dur;
}

void lbm_heap_new_freelist_length(void) {
  lbm_uint l = lbm_heap_state.heap_size - lbm_heap_state.num_alloc;
  lbm_heap_state.gc_last_free = l;
  if (l < lbm_heap_state.gc_least_free)
    lbm_heap_state.gc_least_free = l;
}

int lbm_heap_init(lbm_cons_t *addr, lbm_uint num_cells,
                  lbm_uint *gc_stack_storage, lbm_uint gc_stack_size) {

  if (((uintptr_t)addr % 8) != 0) return 0;

  memset(addr,0, sizeof(lbm_cons_t) * num_cells);

  heap_init_state(addr, num_cells,
                  gc_stack_storage, gc_stack_size);

  return generate_freelist(num_cells);
}

lbm_uint lbm_heap_num_free(void) {
  return lbm_heap_state.heap_size - lbm_heap_state.num_alloc;
}

lbm_value lbm_heap_allocate_cell(lbm_type ptr_type) {

  lbm_value res;

  // it is a ptr replace freelist with cdr of freelist;
  res = lbm_heap_state.freelist;

  if (lbm_type_of(res) == LBM_TYPE_CONS) {
    lbm_heap_state.freelist = lbm_cdr(lbm_heap_state.freelist);

    lbm_heap_state.num_alloc++;

    // set some ok initial values (nil . nil)
    lbm_ref_cell(res)->car = ENC_SYM_NIL;
    lbm_ref_cell(res)->cdr = ENC_SYM_NIL;

    res = res | ptr_type;
    return res;
  }
  else if ((lbm_type_of(lbm_heap_state.freelist) == LBM_TYPE_SYMBOL) &&
           (lbm_dec_sym(lbm_heap_state.freelist) == SYM_NIL)) {
    // all is as it should be (but no free cells)
    return ENC_SYM_MERROR;
  }
  else {
    return ENC_SYM_FATAL_ERROR;
  }
}

lbm_value lbm_heap_allocate_list(unsigned int n) {
  if (n == 0) return ENC_SYM_NIL;
  if (lbm_heap_num_free() < n) return ENC_SYM_MERROR;

  lbm_value res = lbm_heap_state.freelist;
  if (lbm_type_of(res) == LBM_TYPE_CONS) {

    lbm_value curr = res;
    unsigned int count = 1;
    while (lbm_type_of(curr) == LBM_TYPE_CONS && count < n) {
      lbm_ref_cell(curr)->car = ENC_SYM_NIL;
      curr = lbm_cdr(curr);
      count ++;
    }
    lbm_set_car(curr, ENC_SYM_NIL);
    lbm_heap_state.freelist = lbm_cdr(curr);
    lbm_set_cdr(curr, ENC_SYM_NIL);
    lbm_heap_state.num_alloc+=count;
    return res;
  } else {
    return ENC_SYM_FATAL_ERROR;
  }
}

bool lbm_heap_allocate_list_init(lbm_value *ls, unsigned int n, ...) {
  if (n == 0) {
    *ls = ENC_SYM_NIL;
    return true;
  }
  if (lbm_heap_num_free() < n) return false;

  lbm_value res = lbm_heap_state.freelist;
  if (lbm_type_of(res) == LBM_TYPE_CONS) {
    va_list valist;
    va_start(valist, n);
    lbm_value curr = res;
    unsigned int count = 1;
    while (lbm_type_of(curr) == LBM_TYPE_CONS && count < n) {
      lbm_ref_cell(curr)->car = va_arg(valist, lbm_value);
      curr = lbm_cdr(curr);
      count ++;
    }
    lbm_set_car(curr, va_arg(valist, lbm_value));
    lbm_heap_state.freelist = lbm_cdr(curr);
    lbm_set_cdr(curr, ENC_SYM_NIL);
    lbm_heap_state.num_alloc+=count;
    va_end(valist);
    *ls = res;
    return true;
  }
  return false;
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

int lbm_gc_mark_phase(int num, ... ) { //lbm_value env) {

  lbm_stack_t *s = &lbm_heap_state.gc_stack;

  va_list valist;
  va_start(valist, num);
  lbm_value root;
  for (int i = 0; i < num; i++) {
      root = va_arg(valist, lbm_value);
      if (lbm_is_ptr(root)) {
        lbm_push(s, root);
      }
  }
  va_end(valist);
  int res = 1;

  while (!lbm_stack_is_empty(s)) {
    lbm_value curr;
    lbm_pop(s, &curr);

    if (!lbm_is_ptr(curr)) {
      continue;
    }

    // Circular object on heap, or visited..
    if (get_gc_mark(lbm_ref_cell(curr))) {
      continue;
    }

    // There is at least a pointer to one cell here. Mark it and add children to stack
    lbm_heap_state.gc_marked ++;

    set_gc_mark(lbm_ref_cell(curr));

    lbm_value t_ptr = lbm_type_of(curr);

    if (t_ptr >= LBM_NON_CONS_POINTER_TYPE_FIRST &&
        t_ptr <= LBM_NON_CONS_POINTER_TYPE_LAST) continue;

    res &= lbm_push(s, lbm_ref_cell(curr)->cdr);
    res &= lbm_push(s, lbm_ref_cell(curr)->car);

    if (!res) break;
  }

  return res;
}

// The free list should be a "proper list"
// Using a while loop to traverse over the cdrs
int lbm_gc_mark_freelist() {

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
    set_gc_mark(t);
    curr = t->cdr;

    lbm_heap_state.gc_marked ++;
  }

  return 1;
}

int lbm_gc_mark_aux(lbm_uint *aux_data, lbm_uint aux_size) {

  for (lbm_uint i = 0; i < aux_size; i ++) {
    if (lbm_is_ptr(aux_data[i])) {

      lbm_type pt_t = lbm_type_of(aux_data[i]);
      lbm_uint pt_v = lbm_dec_ptr(aux_data[i]);

      if( pt_t >= LBM_POINTER_TYPE_FIRST &&
          pt_t <= LBM_POINTER_TYPE_LAST &&
          pt_v < lbm_heap_state.heap_size) {
        lbm_gc_mark_phase(1,aux_data[i]);
      }
    }
  }
  return 1;
}


// Sweep moves non-marked heap objects to the free list.
int lbm_gc_sweep_phase(void) {
  unsigned int i = 0;
  lbm_cons_t *heap = (lbm_cons_t *)lbm_heap_state.heap;

  for (i = 0; i < lbm_heap_state.heap_size; i ++) {
    if ( get_gc_mark(&heap[i])) {
      clr_gc_mark(&heap[i]);
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
  lbm_value addr = lbm_heap_allocate_cell(LBM_TYPE_CONS);
  if ( lbm_is_ptr(addr)) {
    lbm_ref_cell(addr)->car =  car;
    lbm_ref_cell(addr)->cdr =  cdr;
  }

  // heap_allocate_cell returns MERROR if out of heap.
  return addr;
}

lbm_value lbm_car(lbm_value c){

  if (lbm_type_of(c) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(c) == SYM_NIL) {
    return ENC_SYM_NIL; // if nil, return nil.
  }

  if (lbm_is_ptr(c) ){
    lbm_cons_t *cell = lbm_ref_cell(c);
    return cell->car;
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
  if (lbm_type_of(c) == LBM_TYPE_CONS){
    lbm_cons_t *cell = lbm_ref_cell(c);
    cell->cdr = v;
    r = 1;
  }
  return r;
}

int lbm_set_car_and_cdr(lbm_value c, lbm_value car_val, lbm_value cdr_val) {
  int r = 0;
  if (lbm_type_of(c) == LBM_TYPE_CONS) {
    lbm_cons_t *cell = lbm_ref_cell(c);
    cell->car = car_val;
    cell->cdr = cdr_val;
    r = 1;
  }
  return r;
}

/* calculate length of a proper list */
unsigned int lbm_list_length(lbm_value c) {
  unsigned int len = 0;

  while (lbm_type_of(c) == LBM_TYPE_CONS){
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

  while (lbm_type_of(c) == LBM_TYPE_CONS){
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
  while (lbm_type_of(curr) == LBM_TYPE_CONS) {

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

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value next = lbm_cdr(curr);
    lbm_set_cdr(curr, last_cell);
    last_cell = curr;
    curr = next;
  }
  return last_cell;
}


lbm_value lbm_list_copy(lbm_value list) {
  // TODO: a more efficient approach
  lbm_value res = ENC_SYM_NIL;

  lbm_value curr = list;

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value c = lbm_cons (lbm_car(curr), res);
    if (lbm_type_of(c) == LBM_TYPE_SYMBOL) {
      return ENC_SYM_MERROR;
    }
    res = c;
    curr = lbm_cdr(curr);
  }

  return lbm_list_destructive_reverse(res);
}

// Append for proper lists only
// Destructive update of list1.
lbm_value lbm_list_append(lbm_value list1, lbm_value list2) {

  if (lbm_type_of(list1) != LBM_TYPE_CONS) {
    return list2;
  }
  if (lbm_type_of(list1) != LBM_TYPE_CONS) {
    return list1;
  }

  lbm_value curr = list1;
  while(lbm_type_of(lbm_cdr(curr)) == LBM_TYPE_CONS) {
    curr = lbm_cdr(curr);
  }
  lbm_set_cdr(curr, list2);
  return list1;
}

// Arrays are part of the heap module because their lifespan is managed
// by the garbage collector. The data in the array is not stored
// in the "heap of cons cells".
int lbm_heap_allocate_array(lbm_value *res, lbm_uint size, lbm_type type){

  lbm_array_header_t *array = NULL;
  // allocating a cell that will, to start with, be a cons cell.
  lbm_value cell  = lbm_heap_allocate_cell(LBM_TYPE_CONS);

  if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) { // Out of heap memory
    *res = cell;
    return 0;
  }

  lbm_uint allocate_size = 0;
  if (type == LBM_TYPE_CHAR) {
    if ( size % sizeof(lbm_uint) == 0) {
      #ifndef LBM64
      allocate_size = size >> 2;
      #else
      allocate_size = size >> 3;
      #endif
    } else {
      #ifndef LBM64
      allocate_size = (size >> 2) + 1;
      #else
      allocate_size = (size >> 3) + 1;
      #endif
    }
  }
#ifndef LBM64
  else if (type == LBM_TYPE_I64 ||
           type == LBM_TYPE_U64 ||
           type == LBM_TYPE_DOUBLE) {
    allocate_size = 2*size;
  }
#endif
  else {
    allocate_size = size;
  }

  array = (lbm_array_header_t*)lbm_memory_allocate(sizeof(lbm_array_header_t) / sizeof(lbm_uint));

  if (array == NULL) {
    *res = ENC_SYM_MERROR;
    return 0;
  }

  array->data = (lbm_uint*)lbm_memory_allocate(allocate_size);

  if (array->data == NULL) {
    lbm_memory_free((lbm_uint*)array);
    *res = ENC_SYM_MERROR;
    return 0;
  }
  memset(array->data, 0, allocate_size * sizeof(lbm_uint));

  array->elt_type = type;
  array->size = size;

  lbm_set_car(cell, (lbm_uint)array);
  lbm_set_cdr(cell, lbm_enc_sym(SYM_ARRAY_TYPE));

  cell = lbm_set_ptr_type(cell, LBM_TYPE_ARRAY);

  *res = cell;

  lbm_heap_state.num_alloc_arrays ++;

  return 1;
}

// Convert a C array into an lbm_array.
// if the array is in LBM_MEMORY, the lifetime will be managed by the GC.
int lbm_lift_array(lbm_value *value, char *data, lbm_type type, lbm_uint num_elt) {

  lbm_array_header_t *array = NULL;
  lbm_value cell  = lbm_heap_allocate_cell(LBM_TYPE_CONS);

  if (lbm_type_of(cell) == LBM_TYPE_SYMBOL) { // Out of heap memory
    *value = cell;
    return 0;
  }

  array = (lbm_array_header_t*)lbm_memory_allocate(sizeof(lbm_array_header_t) / 4);

  if (array == NULL) return 0;

  array->data = (lbm_uint*)data;
  array->elt_type = type;
  array->size = num_elt;

  lbm_set_car(cell, (lbm_uint)array);
  lbm_set_cdr(cell, lbm_enc_sym(SYM_ARRAY_TYPE));

  cell = lbm_set_ptr_type(cell, LBM_TYPE_ARRAY);
  *value = cell;
  return 1;
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
  if (lbm_is_array(arr)) {

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
