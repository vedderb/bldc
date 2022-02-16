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

static heap_state_t heap_state;

static VALUE        NIL;
static VALUE        RECOVERED;

char *dec_str(VALUE val) {
  char *res = 0;

  if (type_of(val) == PTR_TYPE_ARRAY) {
    array_header_t *array = (array_header_t *)car(val);

    if (array->elt_type == VAL_TYPE_CHAR) {
      res = (char *)array + 8;
    }
  }
  return res;
}


UINT dec_as_u(VALUE a) {
  UINT tmp;
  FLOAT f_tmp;

  switch (type_of(a)) {
  case VAL_TYPE_I:
    return (UINT) dec_i(a);
  case VAL_TYPE_U:
    return dec_u(a);
  case PTR_TYPE_BOXED_I: /* fall through */
  case PTR_TYPE_BOXED_U:
    return (UINT)car(a);
  case PTR_TYPE_BOXED_F:
    tmp = car(a);
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    return (UINT)f_tmp;
  }
  return 0;
}

INT dec_as_i(VALUE a) {

  UINT tmp;
  FLOAT f_tmp;

  switch (type_of(a)) {
  case VAL_TYPE_I:
    return dec_i(a);
  case VAL_TYPE_U:
    return (INT) dec_u(a);
  case PTR_TYPE_BOXED_I:
  case PTR_TYPE_BOXED_U:
    return (INT)car(a);
  case PTR_TYPE_BOXED_F:
    tmp = car(a);
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    return (INT)f_tmp;
  }
  return 0;
}

FLOAT dec_as_f(VALUE a) {

  UINT tmp;
  FLOAT f_tmp;

  switch (type_of(a)) {
  case VAL_TYPE_I:
    return (FLOAT) dec_i(a);
  case VAL_TYPE_U:
    return (FLOAT)dec_u(a);
  case PTR_TYPE_BOXED_I:
  case PTR_TYPE_BOXED_U:
    return (FLOAT)car(a);
  case PTR_TYPE_BOXED_F:
    tmp = car(a);
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    return f_tmp;
  }
  return 0;
}


// ref_cell: returns a reference to the cell addressed by bits 3 - 26
//           Assumes user has checked that is_ptr was set
static inline cons_t* ref_cell(VALUE addr) {
  return &heap_state.heap[dec_ptr(addr)];
  //  return (cons_t*)(heap_base + (addr & PTR_VAL_MASK));
}

static inline VALUE read_car(cons_t *cell) {
  return cell->car;
}

static inline VALUE read_cdr(cons_t *cell) {
  return cell->cdr;
}

static inline void set_car_(cons_t *cell, VALUE v) {
  cell->car = v;
}

static inline void set_cdr_(cons_t *cell, VALUE v) {
  cell->cdr = v;
}

static inline void set_gc_mark(cons_t *cell) {
  VALUE cdr = read_cdr(cell);
  set_cdr_(cell, val_set_gc_mark(cdr));
}

static inline void clr_gc_mark(cons_t *cell) {
  VALUE cdr = read_cdr(cell);
  set_cdr_(cell, val_clr_gc_mark(cdr));
}

static inline bool get_gc_mark(cons_t* cell) {
  VALUE cdr = read_cdr(cell);
  return val_get_gc_mark(cdr);
}

static int generate_freelist(size_t num_cells) {
  size_t i = 0;

  if (!heap_state.heap) return 0;

  heap_state.freelist = enc_cons_ptr(0);

  cons_t *t;

  // Add all cells to free list
  for (i = 1; i < num_cells; i ++) {
    t = ref_cell(enc_cons_ptr(i-1));
    set_car_(t, RECOVERED);    // all cars in free list are "RECOVERED"
    set_cdr_(t, enc_cons_ptr(i));
  }

  // Replace the incorrect pointer at the last cell.
  t = ref_cell(enc_cons_ptr(num_cells-1));
  set_cdr_(t, NIL);

  return 1;
}

static void heap_init_state(cons_t *addr, unsigned int num_cells, bool malloced) {
  heap_state.heap         = addr;
  heap_state.heap_bytes   = (unsigned int)(num_cells * sizeof(cons_t));
  heap_state.heap_size    = num_cells;
  heap_state.malloced = malloced;

  heap_state.num_alloc           = 0;
  heap_state.num_alloc_arrays    = 0;
  heap_state.gc_num              = 0;
  heap_state.gc_marked           = 0;
  heap_state.gc_recovered        = 0;
  heap_state.gc_recovered_arrays = 0;
}

int heap_init(cons_t *addr, unsigned int num_cells) {

  NIL = enc_sym(SYM_NIL);
  RECOVERED = enc_sym(SYM_RECOVERED);

  if (((uintptr_t)addr % 8) != 0) return 0;

  memset(addr,0, sizeof(cons_t) * num_cells);

  heap_init_state(addr, num_cells, false);

  return generate_freelist(num_cells);
}

unsigned int heap_num_free(void) {

  unsigned int count = 0;
  VALUE curr = heap_state.freelist;

  while (type_of(curr) == PTR_TYPE_CONS) {
    curr = read_cdr(ref_cell(curr));
    count++;
  }
  // Prudence.
  if (!(type_of(curr) == VAL_TYPE_SYMBOL) &&
      curr == NIL){
    return 0;
  }
  return count;
}


VALUE heap_allocate_cell(TYPE ptr_type) {

  VALUE res;

  if (!is_ptr(heap_state.freelist)) {
    // Free list not a ptr (should be Symbol NIL)
    if ((type_of(heap_state.freelist) == VAL_TYPE_SYMBOL) &&
        (heap_state.freelist == NIL)) {
      // all is as it should be (but no free cells)
      return enc_sym(SYM_MERROR);
    } else {
      // something is most likely very wrong
      return enc_sym(SYM_FATAL_ERROR);
    }
  }

  // it is a ptr replace freelist with cdr of freelist;
  res = heap_state.freelist;

  if (type_of(res) != PTR_TYPE_CONS) {
    return enc_sym(SYM_FATAL_ERROR);
  }

  heap_state.freelist = cdr(heap_state.freelist);

  heap_state.num_alloc++;

  // set some ok initial values (nil . nil)
  set_car_(ref_cell(res), NIL);
  set_cdr_(ref_cell(res), NIL);

  // clear GC bit on allocated cell
  clr_gc_mark(ref_cell(res));

  res = res | ptr_type;
  return res;
}

unsigned int heap_num_allocated(void) {
  return heap_state.num_alloc;
}
unsigned int heap_size(void) {
  return heap_state.heap_size;
}

unsigned int heap_size_bytes(void) {
  return heap_state.heap_bytes;
}

void heap_get_state(heap_state_t *res) {
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

static VALUE stack_storage[1024];

int gc_mark_phase(VALUE env) {

  stack s;
  stack_create(&s, stack_storage, 1024);

  if (!is_ptr(env)) {
      return 1; // Nothing to mark here
  }

  push_u32(&s, env);

  while (!stack_is_empty(&s)) {
    VALUE curr;
    int res = 1;
    pop_u32(&s, &curr);

    if (!is_ptr(curr)) {
      continue;
    }

    // Circular object on heap, or visited..
    if (get_gc_mark(ref_cell(curr))) {
      continue;
    }

    // There is at least a pointer to one cell here. Mark it and add children to stack
    heap_state.gc_marked ++;

    set_gc_mark(ref_cell(curr));

    VALUE t_ptr = type_of(curr);

    if (t_ptr == PTR_TYPE_BOXED_I ||
        t_ptr == PTR_TYPE_BOXED_U ||
        t_ptr == PTR_TYPE_BOXED_F ||
        t_ptr == PTR_TYPE_ARRAY) {
      continue;
    }
    res &= push_u32(&s, cdr(curr));
    res &= push_u32(&s, car(curr));

    if (!res) return 0;
  }

  return 1;
}

// The free list should be a "proper list"
// Using a while loop to traverse over the cdrs
int gc_mark_freelist() {

  VALUE curr;
  cons_t *t;
  VALUE fl = heap_state.freelist;

  if (!is_ptr(fl)) {
    if (val_type(fl) == VAL_TYPE_SYMBOL &&
        fl == NIL){
      return 1; // Nothing to mark here
    } else {
      return 0;
    }
  }

  curr = fl;
  while (is_ptr(curr)){
     t = ref_cell(curr);
     set_gc_mark(t);
     curr = read_cdr(t);

     heap_state.gc_marked ++;
  }

  return 1;
}

int gc_mark_aux(UINT *aux_data, unsigned int aux_size) {

  for (unsigned int i = 0; i < aux_size; i ++) {
    if (is_ptr(aux_data[i])) {

      TYPE pt_t = ptr_type(aux_data[i]);
      UINT pt_v = dec_ptr(aux_data[i]);

      if ( (pt_t == PTR_TYPE_CONS ||
            pt_t == PTR_TYPE_BOXED_I ||
            pt_t == PTR_TYPE_BOXED_U ||
            pt_t == PTR_TYPE_BOXED_F ||
            pt_t == PTR_TYPE_ARRAY ||
            pt_t == PTR_TYPE_REF ||
            pt_t == PTR_TYPE_STREAM) &&
           pt_v < heap_state.heap_size) {

        gc_mark_phase(aux_data[i]);
      }
    }
  }

  return 1;
}


// Sweep moves non-marked heap objects to the free list.
int gc_sweep_phase(void) {

  unsigned int i = 0;
  cons_t *heap = (cons_t *)heap_state.heap;

  for (i = 0; i < heap_state.heap_size; i ++) {
    if ( !get_gc_mark(&heap[i])){

      // Check if this cell is a pointer to an array
      // and free it.
      if (type_of(heap[i].cdr) == VAL_TYPE_SYMBOL &&
          dec_sym(heap[i].cdr) == SYM_ARRAY_TYPE) {
        array_header_t *arr = (array_header_t*)heap[i].car;
        memory_free((uint32_t *)arr);
        heap_state.gc_recovered_arrays++;
      }

      // create pointer to use as new freelist
      UINT addr = enc_cons_ptr(i);

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

void gc_state_inc(void) {
  heap_state.gc_num ++;
  heap_state.gc_recovered = 0;
  heap_state.gc_marked = 0;
}


int heap_perform_gc(VALUE env) {
  gc_state_inc();

  gc_mark_freelist();
  gc_mark_phase(env);
  return gc_sweep_phase();
}

int heap_perform_gc_extra(VALUE env, VALUE env2, VALUE exp, VALUE exp2, VALUE list) {
  gc_state_inc();

  gc_mark_freelist();
  gc_mark_phase(exp);
  gc_mark_phase(exp2);
  gc_mark_phase(env);
  gc_mark_phase(env2);
  gc_mark_phase(list);

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  return gc_sweep_phase();
}

int heap_perform_gc_aux(VALUE env, VALUE env2, VALUE exp, VALUE exp2, VALUE exp3, UINT *aux_data, unsigned int aux_size) {
  gc_state_inc();

  gc_mark_freelist();
  gc_mark_phase(exp);
  gc_mark_phase(exp2);
  gc_mark_phase(exp3);
  gc_mark_phase(env);
  gc_mark_phase(env2);
  gc_mark_aux(aux_data, aux_size);

#ifdef VISUALIZE_HEAP
  heap_vis_gen_image();
#endif

  return gc_sweep_phase();
}

// construct, alter and break apart
VALUE cons(VALUE car, VALUE cdr) {
  VALUE addr = heap_allocate_cell(PTR_TYPE_CONS);
  if ( is_ptr(addr)) {
    set_car_(ref_cell(addr), car);
    set_cdr_(ref_cell(addr), cdr);
  }

  // heap_allocate_cell returns MERROR if out of heap.
  return addr;
}

VALUE car(VALUE c){

  if (type_of(c) == VAL_TYPE_SYMBOL &&
      c == NIL) {
    return c; // if nil, return nil.
  }

  if (is_ptr(c) ){
    cons_t *cell = ref_cell(c);
    return read_car(cell);
  }
  return enc_sym(SYM_TERROR);
}

VALUE cdr(VALUE c){

  if (type_of(c) == VAL_TYPE_SYMBOL &&
      c == NIL) {
    return c; // if nil, return nil.
  }

  if (type_of(c) == PTR_TYPE_CONS) {
    cons_t *cell = ref_cell(c);
    return read_cdr(cell);
  }
  return enc_sym(SYM_TERROR);
}

bool set_car(VALUE c, VALUE v) { // Todo: Where are these used?
                                 //   Can then return VALUE instead?
  bool r = false;
  if (is_ptr(c) && ptr_type(c) == PTR_TYPE_CONS) {
    cons_t *cell = ref_cell(c);
    set_car_(cell,v);
    r = true;
  }
  return r;
}

bool set_cdr(VALUE c, VALUE v) {
  bool r = false;
  if (type_of(c) == PTR_TYPE_CONS){
    cons_t *cell = ref_cell(c);
    set_cdr_(cell,v);
    r = true;
  }
  return r;
}

/* calculate length of a proper list */
unsigned int length(VALUE c) {
  unsigned int len = 0;

  while (type_of(c) == PTR_TYPE_CONS){
    len ++;
    c = cdr(c);
  }
  return len;
}

/* reverse a proper list */
VALUE reverse(VALUE list) {
  if (type_of(list) == VAL_TYPE_SYMBOL &&
      list == NIL) {
    return list;
  }

  VALUE curr = list;

  VALUE new_list = NIL;
  while (type_of(curr) == PTR_TYPE_CONS) {

    new_list = cons(car(curr), new_list);
    if (type_of(new_list) == VAL_TYPE_SYMBOL) {
      return enc_sym(SYM_MERROR);
    }
    curr = cdr(curr);
  }
  return new_list;
}

VALUE copy(VALUE list) {
  // TODO: a more efficient approach
  VALUE res = NIL;

  VALUE curr = list;

  while (type_of(curr) == PTR_TYPE_CONS) {
    VALUE c = cons (car(curr), res);
    if (type_of(c) == VAL_TYPE_SYMBOL) {
      return enc_sym(SYM_MERROR);
    }
    res = c;
    curr = cdr(curr);
  }

  return reverse(res);
}

// Arrays are part of the heap module because their lifespan is managed
// by the garbage collector. The data in the array is not stored
// in the "heap of cons cells".
int heap_allocate_array(VALUE *res, unsigned int size, TYPE type){

  array_header_t *array = NULL;
  // allocating a cell that will, to start with, be a cons cell.
  VALUE cell  = heap_allocate_cell(PTR_TYPE_CONS);

  if (type_of(cell) == VAL_TYPE_SYMBOL) { // Out of heap memory
    *res = cell;
    return 0;
  }

  unsigned int allocate_size = 0;
  if (type == VAL_TYPE_CHAR) {
    if ( size % 4 == 0) {
      allocate_size = size >> 2;
    } else {
      allocate_size = (size >> 2) + 1;
    }
  } else {
    allocate_size = size;
  }

  array = (array_header_t*)memory_allocate(2 + allocate_size);

  if (array == NULL) return 0;

  array->elt_type = type;
  array->size = size;

  set_car(cell, (UINT)array);
  set_cdr(cell, enc_sym(SYM_ARRAY_TYPE));

  cell = cell | PTR_TYPE_ARRAY;

  *res = cell;

  heap_state.num_alloc_arrays ++;

  return 1;
}
