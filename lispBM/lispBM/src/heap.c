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
#include <inttypes.h>
#include <lbm_memory.h>

#include "heap.h"
#include "symrepr.h"
#include "stack.h"
#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif

lbm_heap_state_t lbm_heap_state;

static lbm_value        NIL;
static lbm_value        RECOVERED;

char *lbm_dec_str(lbm_value val) {
  char *res = 0;

  if (lbm_type_of(val) == LBM_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(val);

    if (array->elt_type == LBM_TYPE_CHAR) {
      res = (char *)array->data;
    }
  }
  return res;
}

lbm_stream_t *lbm_dec_stream(lbm_value val) {
  lbm_stream_t *res = 0;

  if (lbm_type_of(val) == LBM_TYPE_STREAM) {
    res = (lbm_stream_t *)lbm_car(val);
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

static inline lbm_value read_car(lbm_cons_t *cell) {
  return cell->car;
}

static inline lbm_value read_cdr(lbm_cons_t *cell) {
  return cell->cdr;
}

static inline void set_car_(lbm_cons_t *cell, lbm_value v) {
  cell->car = v;
}

static inline void set_cdr_(lbm_cons_t *cell, lbm_value v) {
  cell->cdr = v;
}

static inline void set_gc_mark(lbm_cons_t *cell) {
  lbm_value cdr = read_cdr(cell);
  set_cdr_(cell, lbm_set_gc_mark(cdr));
}

static inline void clr_gc_mark(lbm_cons_t *cell) {
  lbm_value cdr = read_cdr(cell);
  set_cdr_(cell, lbm_clr_gc_mark(cdr));
}

static inline bool get_gc_mark(lbm_cons_t* cell) {
  lbm_value cdr = read_cdr(cell);
  return lbm_get_gc_mark(cdr);
}

static inline void set_gc_flag(lbm_cons_t *cell) {
  lbm_value v = read_car(cell);
  set_car_(cell, lbm_set_gc_mark(v));
}

static inline void clr_gc_flag(lbm_cons_t *cell) {
  lbm_value v = read_car(cell);
  set_car_(cell, lbm_clr_gc_mark(v));
}

static inline bool get_gc_flag(lbm_cons_t* cell) {
  lbm_value v = read_car(cell);
  return lbm_get_gc_mark(v);
}


static int generate_freelist(size_t num_cells) {
  size_t i = 0;

  if (!lbm_heap_state.heap) return 0;

  lbm_heap_state.freelist = lbm_enc_cons_ptr(0);

  lbm_cons_t *t;

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    t = lbm_ref_cell(lbm_enc_cons_ptr(i-1));
    set_car_(t, RECOVERED);    // all cars in free list are "RECOVERED"
    set_cdr_(t, lbm_enc_cons_ptr(i));
  }

  // Replace the incorrect pointer at the last cell.
  t = lbm_ref_cell(lbm_enc_cons_ptr(num_cells-1));
  set_cdr_(t, NIL);

  return 1;
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

  NIL = lbm_enc_sym(SYM_NIL);
  RECOVERED = lbm_enc_sym(SYM_RECOVERED);

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

  if (!lbm_is_ptr(lbm_heap_state.freelist)) {
    // Free list not a ptr (should be Symbol NIL)
    if ((lbm_type_of(lbm_heap_state.freelist) == LBM_TYPE_SYMBOL) &&
        (lbm_dec_sym(lbm_heap_state.freelist) == SYM_NIL)) {
      // all is as it should be (but no free cells)
      return lbm_enc_sym(SYM_MERROR);
    } else {
      // something is most likely very wrong
      return lbm_enc_sym(SYM_FATAL_ERROR);
    }
  }

  // it is a ptr replace freelist with cdr of freelist;
  res = lbm_heap_state.freelist;

  if (lbm_type_of(res) != LBM_TYPE_CONS) {
    return lbm_enc_sym(SYM_FATAL_ERROR);
  }

  lbm_heap_state.freelist = lbm_cdr(lbm_heap_state.freelist);

  lbm_heap_state.num_alloc++;

  // set some ok initial values (nil . nil)
  set_car_(lbm_ref_cell(res), NIL);
  set_cdr_(lbm_ref_cell(res), NIL);

  // clear GC bit on allocated cell
  clr_gc_mark(lbm_ref_cell(res));

  res = res | ptr_type;
  return res;
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

int lbm_gc_mark_phase(lbm_value env) {

  lbm_stack_t *s = &lbm_heap_state.gc_stack;

  if (!lbm_is_ptr(env)) {
      return 1; // Nothing to mark here
  }

  lbm_push(s, env);
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
        fl == NIL){
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
        lbm_gc_mark_phase(aux_data[i]);
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
    if ( !get_gc_mark(&heap[i])){

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
        case SYM_STREAM_TYPE:{
          lbm_stream_t *stream = (lbm_stream_t*)heap[i].car;
          if (lbm_memory_ptr_inside((lbm_uint*)stream)) {
            lbm_memory_free((lbm_uint*)stream);
          }
        } break;
        default:
          break;
        }
      }

      // create pointer to use as new freelist
      lbm_uint addr = lbm_enc_cons_ptr(i);

      // Clear the "freed" cell.
      heap[i].car = RECOVERED;
      heap[i].cdr = lbm_heap_state.freelist;
      lbm_heap_state.freelist = addr;
      lbm_heap_state.num_alloc --;
      lbm_heap_state.gc_recovered ++;
    }
    clr_gc_mark(&heap[i]);
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
    set_car_(lbm_ref_cell(addr), car);
    set_cdr_(lbm_ref_cell(addr), cdr);
  }

  // heap_allocate_cell returns MERROR if out of heap.
  return addr;
}

lbm_value lbm_car(lbm_value c){

  if (lbm_type_of(c) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(c) == SYM_NIL) {
    return lbm_enc_sym(SYM_NIL); // if nil, return nil.
  }

  if (lbm_is_ptr(c) ){
    lbm_cons_t *cell = lbm_ref_cell(c);
    return read_car(cell);
  }
  return lbm_enc_sym(SYM_TERROR);
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
  return lbm_enc_sym(SYM_TERROR);
}

lbm_value lbm_cdr(lbm_value c){

  if (lbm_type_of(c) == LBM_TYPE_SYMBOL &&
      lbm_dec_sym(c) == SYM_NIL) {
    return lbm_enc_sym(SYM_NIL); // if nil, return nil.
  }

  if (lbm_is_ptr(c)) {
    lbm_cons_t *cell = lbm_ref_cell(c);
    return read_cdr(cell);
  }
  return lbm_enc_sym(SYM_TERROR);
}

int lbm_set_car(lbm_value c, lbm_value v) {
  int r = 0;
  if (lbm_type_of(c) == LBM_TYPE_CONS) {
    lbm_cons_t *cell = lbm_ref_cell(c);
    set_car_(cell,v);
    r = 1;
  }
  return r;
}

int lbm_set_cdr(lbm_value c, lbm_value v) {
  int r = 0;
  if (lbm_type_of(c) == LBM_TYPE_CONS){
    lbm_cons_t *cell = lbm_ref_cell(c);
    set_cdr_(cell,v);
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

/* reverse a proper list */
lbm_value lbm_list_reverse(lbm_value list) {
  if (lbm_type_of(list) == LBM_TYPE_SYMBOL) {
    return list;
  }

  lbm_value curr = list;

  lbm_value new_list = NIL;
  while (lbm_type_of(curr) == LBM_TYPE_CONS) {

    new_list = lbm_cons(lbm_car(curr), new_list);
    if (lbm_type_of(new_list) == LBM_TYPE_SYMBOL) {
      return lbm_enc_sym(SYM_MERROR);
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
  lbm_value last_cell = lbm_enc_sym(SYM_NIL);

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
  lbm_value res = NIL;

  lbm_value curr = list;

  while (lbm_type_of(curr) == LBM_TYPE_CONS) {
    lbm_value c = lbm_cons (lbm_car(curr), res);
    if (lbm_type_of(c) == LBM_TYPE_SYMBOL) {
      return lbm_enc_sym(SYM_MERROR);
    }
    res = c;
    curr = lbm_cdr(curr);
  }

  return lbm_list_reverse(res);
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
    *res = lbm_enc_sym(SYM_MERROR);
    return 0;
  }

  array->data = (lbm_uint*)lbm_memory_allocate(allocate_size);
  memset(array->data, 0, allocate_size * sizeof(lbm_uint));

  if (array->data == NULL) {
    lbm_memory_free((lbm_uint*)array);
    *res = lbm_enc_sym(SYM_MERROR);
    return 0;
  }

  array->elt_type = type;
  array->size = size;

  lbm_set_car(cell, (lbm_uint)array);
  lbm_set_cdr(cell, lbm_enc_sym(SYM_ARRAY_TYPE));

  cell = cell | LBM_TYPE_ARRAY;

  *res = cell;

  lbm_heap_state.num_alloc_arrays ++;

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

    lbm_memory_free((lbm_uint*)header->data);
    lbm_memory_free((lbm_uint*)header);

    arr = lbm_set_ptr_type(arr, LBM_TYPE_CONS);
    lbm_set_car(arr, lbm_enc_sym(SYM_NIL));
    lbm_set_cdr(arr, lbm_enc_sym(SYM_NIL));
    r = 1;
  }

  return r;
}
