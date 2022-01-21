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

#include "heap.h"
#include "symrepr.h"
#include "stack.h"
#include "lispbm_memory.h"
#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif

static lbm_heap_state_t heap_state;

static lbm_value        NIL;
static lbm_value        RECOVERED;

char *lbm_dec_str(lbm_value val) {
  char *res = 0;

  if (lbm_type_of(val) == LBM_PTR_TYPE_ARRAY) {
    lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(val);

    if (array->elt_type == LBM_VAL_TYPE_CHAR) {
      res = (char *)array + 8;
    }
  }
  return res;
}

lbm_stream_t *lbm_dec_stream(lbm_value val) {
  lbm_stream_t *res = 0;

  if (lbm_type_of(val) == LBM_PTR_TYPE_STREAM) {
    res = (lbm_stream_t *)lbm_car(val);
  }
  return res;
}


lbm_uint lbm_dec_as_u(lbm_value a) {
  lbm_uint tmp;
  lbm_float f_tmp;

  switch (lbm_type_of(a)) {
  case LBM_VAL_TYPE_I:
    return (lbm_uint) lbm_dec_i(a);
  case LBM_VAL_TYPE_U:
    return lbm_dec_u(a);
  case LBM_PTR_TYPE_BOXED_I: /* fall through */
  case LBM_PTR_TYPE_BOXED_U:
    return (lbm_uint)lbm_car(a);
  case LBM_PTR_TYPE_BOXED_F:
    tmp = lbm_car(a);
    memcpy(&f_tmp, &tmp, sizeof(lbm_float));
    return (lbm_uint)f_tmp;
  }
  return 0;
}

lbm_int lbm_dec_as_i(lbm_value a) {

  lbm_uint tmp;
  lbm_float f_tmp;

  switch (lbm_type_of(a)) {
  case LBM_VAL_TYPE_I:
    return lbm_dec_i(a);
  case LBM_VAL_TYPE_U:
    return (lbm_int) lbm_dec_u(a);
  case LBM_PTR_TYPE_BOXED_I:
  case LBM_PTR_TYPE_BOXED_U:
    return (lbm_int)lbm_car(a);
  case LBM_PTR_TYPE_BOXED_F:
    tmp = lbm_car(a);
    memcpy(&f_tmp, &tmp, sizeof(lbm_float));
    return (lbm_int)f_tmp;
  }
  return 0;
}

lbm_float lbm_dec_as_f(lbm_value a) {

  lbm_uint tmp;
  lbm_float f_tmp;

  switch (lbm_type_of(a)) {
  case LBM_VAL_TYPE_I:
    return (lbm_float) lbm_dec_i(a);
  case LBM_VAL_TYPE_U:
    return (lbm_float)lbm_dec_u(a);
  case LBM_PTR_TYPE_BOXED_I:
  case LBM_PTR_TYPE_BOXED_U:
    return (lbm_float)lbm_car(a);
  case LBM_PTR_TYPE_BOXED_F:
    tmp = lbm_car(a);
    memcpy(&f_tmp, &tmp, sizeof(lbm_float));
    return f_tmp;
  }
  return 0;
}


// ref_cell: returns a reference to the cell addressed by bits 3 - 26
//           Assumes user has checked that is_ptr was set
static inline lbm_cons_t* ref_cell(lbm_value addr) {
  return &heap_state.heap[lbm_dec_ptr(addr)];
  //  return (cons_t*)(heap_base + (addr & PTR_VAL_MASK));
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

static int generate_freelist(size_t num_cells) {
  size_t i = 0;

  if (!heap_state.heap) return 0;

  heap_state.freelist = lbm_enc_cons_ptr(0);

  lbm_cons_t *t;

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    t = ref_cell(lbm_enc_cons_ptr(i-1));
    set_car_(t, RECOVERED);    // all cars in free list are "RECOVERED"
    set_cdr_(t, lbm_enc_cons_ptr(i));
  }

  // Replace the incorrect pointer at the last cell.
  t = ref_cell(lbm_enc_cons_ptr(num_cells-1));
  set_cdr_(t, NIL);

  return 1;
}

static void heap_init_state(lbm_cons_t *addr, unsigned int num_cells, bool malloced) {
  heap_state.heap         = addr;
  heap_state.heap_bytes   = (unsigned int)(num_cells * sizeof(lbm_cons_t));
  heap_state.heap_size    = num_cells;
  heap_state.malloced = malloced;

  heap_state.num_alloc           = 0;
  heap_state.num_alloc_arrays    = 0;
  heap_state.gc_num              = 0;
  heap_state.gc_marked           = 0;
  heap_state.gc_recovered        = 0;
  heap_state.gc_recovered_arrays = 0;
}

int lbm_heap_init(lbm_cons_t *addr, unsigned int num_cells) {

  NIL = lbm_enc_sym(SYM_NIL);
  RECOVERED = lbm_enc_sym(SYM_RECOVERED);

  if (((uintptr_t)addr % 8) != 0) return 0;

  memset(addr,0, sizeof(lbm_cons_t) * num_cells);

  heap_init_state(addr, num_cells, false);

  return generate_freelist(num_cells);
}

unsigned int lbm_heap_num_free(void) {

  unsigned int count = 0;
  lbm_value curr = heap_state.freelist;

  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    curr = read_cdr(ref_cell(curr));
    count++;
  }
  // Prudence.
  if (!(lbm_type_of(curr) == LBM_VAL_TYPE_SYMBOL) &&
      curr == NIL){
    return 0;
  }
  return count;
}


lbm_value lbm_heap_allocate_cell(lbm_type ptr_type) {

  lbm_value res;

  if (!lbm_is_ptr(heap_state.freelist)) {
    // Free list not a ptr (should be Symbol NIL)
    if ((lbm_type_of(heap_state.freelist) == LBM_VAL_TYPE_SYMBOL) &&
        (heap_state.freelist == NIL)) {
      // all is as it should be (but no free cells)
      return lbm_enc_sym(SYM_MERROR);
    } else {
      // something is most likely very wrong
      return lbm_enc_sym(SYM_FATAL_ERROR);
    }
  }

  // it is a ptr replace freelist with cdr of freelist;
  res = heap_state.freelist;

  if (lbm_type_of(res) != LBM_PTR_TYPE_CONS) {
    return lbm_enc_sym(SYM_FATAL_ERROR);
  }

  heap_state.freelist = lbm_cdr(heap_state.freelist);

  heap_state.num_alloc++;

  // set some ok initial values (nil . nil)
  set_car_(ref_cell(res), NIL);
  set_cdr_(ref_cell(res), NIL);

  // clear GC bit on allocated cell
  clr_gc_mark(ref_cell(res));

  res = res | ptr_type;
  return res;
}

unsigned int lbm_heap_num_allocated(void) {
  return heap_state.num_alloc;
}
unsigned int lbm_heap_size(void) {
  return heap_state.heap_size;
}

unsigned int lbm_heap_size_bytes(void) {
  return heap_state.heap_bytes;
}

void lbm_get_heap_state(lbm_heap_state_t *res) {
  res->heap                = heap_state.heap;
  res->malloced            = heap_state.malloced;
  res->freelist            = heap_state.freelist;
  res->heap_size           = heap_state.heap_size;
  res->heap_bytes          = heap_state.heap_bytes;
  res->num_alloc           = heap_state.num_alloc;
  res->num_alloc_arrays    = heap_state.num_alloc_arrays;
  res->gc_num              = heap_state.gc_num;
  res->gc_marked           = heap_state.gc_marked;
  res->gc_recovered        = heap_state.gc_recovered;
  res->gc_recovered_arrays = heap_state.gc_recovered_arrays;
}

static lbm_value stack_storage[1024];

int lbm_gc_mark_phase(lbm_value env) {

  lbm_stack_t s;
  lbm_stack_create(&s, stack_storage, 1024);

  if (!lbm_is_ptr(env)) {
      return 1; // Nothing to mark here
  }

  lbm_push_u32(&s, env);

  while (!lbm_stack_is_empty(&s)) {
    lbm_value curr;
    int res = 1;
    lbm_pop_u32(&s, &curr);

    if (!lbm_is_ptr(curr)) {
      continue;
    }

    // Circular object on heap, or visited..
    if (get_gc_mark(ref_cell(curr))) {
      continue;
    }

    // There is at least a pointer to one cell here. Mark it and add children to stack
    heap_state.gc_marked ++;

    set_gc_mark(ref_cell(curr));

    lbm_value t_ptr = lbm_type_of(curr);

    if (t_ptr == LBM_PTR_TYPE_BOXED_I ||
        t_ptr == LBM_PTR_TYPE_BOXED_U ||
        t_ptr == LBM_PTR_TYPE_BOXED_F ||
        t_ptr == LBM_PTR_TYPE_ARRAY   ||
        t_ptr == LBM_PTR_TYPE_STREAM) {
      continue;
    }
    res &= lbm_push_u32(&s, lbm_cdr(curr));
    res &= lbm_push_u32(&s, lbm_car(curr));

    if (!res) return 0;
  }

  return 1;
}

// The free list should be a "proper list"
// Using a while loop to traverse over the cdrs
int lbm_gc_mark_freelist() {

  lbm_value curr;
  lbm_cons_t *t;
  lbm_value fl = heap_state.freelist;

  if (!lbm_is_ptr(fl)) {
    if (lbm_type_of(fl) == LBM_VAL_TYPE_SYMBOL &&
        fl == NIL){
      return 1; // Nothing to mark here
    } else {
      return 0;
    }
  }

  curr = fl;
  while (lbm_is_ptr(curr)){
     t = ref_cell(curr);
     set_gc_mark(t);
     curr = read_cdr(t);

     heap_state.gc_marked ++;
  }

  return 1;
}

int lbm_gc_mark_aux(lbm_uint *aux_data, unsigned int aux_size) {

  for (unsigned int i = 0; i < aux_size; i ++) {
    if (lbm_is_ptr(aux_data[i])) {

      lbm_type pt_t = lbm_type_of(aux_data[i]);
      lbm_uint pt_v = lbm_dec_ptr(aux_data[i]);

      if ( (pt_t == LBM_PTR_TYPE_CONS ||
            pt_t == LBM_PTR_TYPE_BOXED_I ||
            pt_t == LBM_PTR_TYPE_BOXED_U ||
            pt_t == LBM_PTR_TYPE_BOXED_F ||
            pt_t == LBM_PTR_TYPE_ARRAY ||
            pt_t == LBM_PTR_TYPE_REF ||
            pt_t == LBM_PTR_TYPE_STREAM) &&
           pt_v < heap_state.heap_size) {

        lbm_gc_mark_phase(aux_data[i]);
      }
    }
  }

  return 1;
}


// Sweep moves non-marked heap objects to the free list.
int lbm_gc_sweep_phase(void) {

  unsigned int i = 0;
  lbm_cons_t *heap = (lbm_cons_t *)heap_state.heap;

  for (i = 0; i < heap_state.heap_size; i ++) {
    if ( !get_gc_mark(&heap[i])){

      // Check if this cell is a pointer to an array
      // and free it.
      if (lbm_type_of(heap[i].cdr) == LBM_VAL_TYPE_SYMBOL &&
          lbm_dec_sym(heap[i].cdr) == SYM_ARRAY_TYPE) {
        lbm_array_header_t *arr = (lbm_array_header_t*)heap[i].car;
        lbm_memory_free((uint32_t *)arr);
        heap_state.gc_recovered_arrays++;
      }

      // create pointer to use as new freelist
      lbm_uint addr = lbm_enc_cons_ptr(i);

      // Clear the "freed" cell.
      heap[i].car = RECOVERED;
      heap[i].cdr = heap_state.freelist;
      heap_state.freelist = addr;

      heap_state.num_alloc --;
      heap_state.gc_recovered ++;
    }
    clr_gc_mark(&heap[i]);
  }
  return 1;
}

void lbm_gc_state_inc(void) {
  heap_state.gc_num ++;
  heap_state.gc_recovered = 0;
  heap_state.gc_marked = 0;
}


int lbm_perform_gc(lbm_value env) {
  lbm_gc_state_inc();

  lbm_gc_mark_freelist();
  lbm_gc_mark_phase(env);
  return lbm_gc_sweep_phase();
}

int heap_perform_gc_extra(lbm_value env, lbm_value env2, lbm_value exp, lbm_value exp2, lbm_value list) {
  lbm_gc_state_inc();

  lbm_gc_mark_freelist();
  lbm_gc_mark_phase(exp);
  lbm_gc_mark_phase(exp2);
  lbm_gc_mark_phase(env);
  lbm_gc_mark_phase(env2);
  lbm_gc_mark_phase(list);

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  return lbm_gc_sweep_phase();
}

int lbm_perform_gc_aux(lbm_value env, lbm_value env2, lbm_value exp, lbm_value exp2, lbm_value exp3, lbm_uint *aux_data, unsigned int aux_size) {
  lbm_gc_state_inc();

  lbm_gc_mark_freelist();
  lbm_gc_mark_phase(exp);
  lbm_gc_mark_phase(exp2);
  lbm_gc_mark_phase(exp3);
  lbm_gc_mark_phase(env);
  lbm_gc_mark_phase(env2);
  lbm_gc_mark_aux(aux_data, aux_size);

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  return lbm_gc_sweep_phase();
}

// construct, alter and break apart
lbm_value lbm_cons(lbm_value car, lbm_value cdr) {
  lbm_value addr = lbm_heap_allocate_cell(LBM_PTR_TYPE_CONS);
  if ( lbm_is_ptr(addr)) {
    set_car_(ref_cell(addr), car);
    set_cdr_(ref_cell(addr), cdr);
  }

  // heap_allocate_cell returns MERROR if out of heap.
  return addr;
}

lbm_value lbm_car(lbm_value c){

  if (lbm_type_of(c) == LBM_VAL_TYPE_SYMBOL &&
      c == NIL) {
    return c; // if nil, return nil.
  }

  if (lbm_is_ptr(c) ){
    lbm_cons_t *cell = ref_cell(c);
    return read_car(cell);
  }
  return lbm_enc_sym(SYM_TERROR);
}

lbm_value lbm_cdr(lbm_value c){

  if (lbm_type_of(c) == LBM_VAL_TYPE_SYMBOL &&
      c == NIL) {
    return c; // if nil, return nil.
  }

  if (lbm_type_of(c) == LBM_PTR_TYPE_CONS) {
    lbm_cons_t *cell = ref_cell(c);
    return read_cdr(cell);
  }
  return lbm_enc_sym(SYM_TERROR);
}

bool lbm_set_car(lbm_value c, lbm_value v) { // Todo: Where are these used?
                                 //   Can then return VALUE instead?
  bool r = false;
  if (lbm_is_ptr(c) && lbm_type_of(c) == LBM_PTR_TYPE_CONS) {
    lbm_cons_t *cell = ref_cell(c);
    set_car_(cell,v);
    r = true;
  }
  return r;
}

bool lbm_set_cdr(lbm_value c, lbm_value v) {
  bool r = false;
  if (lbm_type_of(c) == LBM_PTR_TYPE_CONS){
    lbm_cons_t *cell = ref_cell(c);
    set_cdr_(cell,v);
    r = true;
  }
  return r;
}

/* calculate length of a proper list */
unsigned int lbm_list_length(lbm_value c) {
  unsigned int len = 0;

  while (lbm_type_of(c) == LBM_PTR_TYPE_CONS){
    len ++;
    c = lbm_cdr(c);
  }
  return len;
}

/* reverse a proper list */
lbm_value lbm_list_reverse(lbm_value list) {
  if (lbm_type_of(list) == LBM_VAL_TYPE_SYMBOL &&
      list == NIL) {
    return list;
  }

  lbm_value curr = list;

  lbm_value new_list = NIL;
  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {

    new_list = lbm_cons(lbm_car(curr), new_list);
    if (lbm_type_of(new_list) == LBM_VAL_TYPE_SYMBOL) {
      return lbm_enc_sym(SYM_MERROR);
    }
    curr = lbm_cdr(curr);
  }
  return new_list;
}

lbm_value lbm_list_copy(lbm_value list) {
  // TODO: a more efficient approach
  lbm_value res = NIL;

  lbm_value curr = list;

  while (lbm_type_of(curr) == LBM_PTR_TYPE_CONS) {
    lbm_value c = lbm_cons (lbm_car(curr), res);
    if (lbm_type_of(c) == LBM_VAL_TYPE_SYMBOL) {
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

  if (lbm_type_of(list1) != LBM_PTR_TYPE_CONS) {
    return list2;
  }
  if (lbm_type_of(list1) != LBM_PTR_TYPE_CONS) {
    return list1;
  }

  lbm_value curr = list1;
  while(lbm_type_of(lbm_cdr(curr)) == LBM_PTR_TYPE_CONS) {
    curr = lbm_cdr(curr);
  }
  lbm_set_cdr(curr, list2);
  return list1;
}

// Arrays are part of the heap module because their lifespan is managed
// by the garbage collector. The data in the array is not stored
// in the "heap of cons cells".
int lbm_heap_allocate_array(lbm_value *res, unsigned int size, lbm_type type){

  lbm_array_header_t *array = NULL;
  // allocating a cell that will, to start with, be a cons cell.
  lbm_value cell  = lbm_heap_allocate_cell(LBM_PTR_TYPE_CONS);

  if (lbm_type_of(cell) == LBM_VAL_TYPE_SYMBOL) { // Out of heap memory
    *res = cell;
    return 0;
  }

  unsigned int allocate_size = 0;
  if (type == LBM_VAL_TYPE_CHAR) {
    if ( size % 4 == 0) {
      allocate_size = size >> 2;
    } else {
      allocate_size = (size >> 2) + 1;
    }
  } else {
    allocate_size = size;
  }

  array = (lbm_array_header_t*)lbm_memory_allocate(2 + allocate_size);

  if (array == NULL) return 0;

  array->elt_type = type;
  array->size = size;

  lbm_set_car(cell, (lbm_uint)array);
  lbm_set_cdr(cell, lbm_enc_sym(SYM_ARRAY_TYPE));

  cell = cell | LBM_PTR_TYPE_ARRAY;

  *res = cell;

  heap_state.num_alloc_arrays ++;

  return 1;
}
