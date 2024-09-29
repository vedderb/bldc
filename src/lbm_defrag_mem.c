/*
    Copyright 2024 Joel Svensson        svenssonjoel@yahoo.se

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

#include <lbm_defrag_mem.h>
#include <heap.h>
#include <lbm_memory.h>

static inline lbm_uint bs2ws(lbm_uint bs) {
  return bs % sizeof(lbm_uint) == 0 ? bs / sizeof(lbm_uint) : (bs / sizeof(lbm_uint)) + 1;
}

#define DEFRAG_MEM_HEADER_SIZE 2
#define DEFRAG_MEM_HEADER_BYTES  (2*sizeof(lbm_uint))
#define DEFRAG_MEM_DATA(X) &(X)[DEFRAG_MEM_HEADER_SIZE];
// length and flags.
// Currently only one flag that tells if we should do a compaction before allocation.
#define DEFRAG_MEM_SIZE(X) X[0]
#define DEFRAG_MEM_FLAGS(X) X[1]

#define DEFRAG_ALLOC_SIZE(X) X[0]
#define DEFRAG_ALLOC_DATA(X) X[1]
#define DEFRAG_ALLOC_CELLPTR(X) X[2]

lbm_value lbm_defrag_mem_create(lbm_uint nbytes) {
  lbm_value res = ENC_SYM_TERROR;
  lbm_uint nwords = bs2ws(nbytes); // multiple of 4.
  if (nwords > 0) {
    res = ENC_SYM_MERROR;
    lbm_uint *data = (lbm_uint*)lbm_malloc(DEFRAG_MEM_HEADER_BYTES + nwords * sizeof(lbm_uint));
    if (data) {
      memset((uint8_t*)data , 0, DEFRAG_MEM_HEADER_BYTES + nwords*sizeof(lbm_uint));
      data[0] = nwords;
      data[1] = 0;      //flags
      lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_DEFRAG_MEM, (lbm_uint)data, ENC_SYM_DEFRAG_MEM_TYPE);
      if (cell == ENC_SYM_MERROR) {
        lbm_free(data);
      } else {
        res = cell;
      }
    }
  }
  return res;
}

static void free_defrag_allocation(lbm_uint *allocation) {
  lbm_uint size = DEFRAG_ALLOC_SIZE(allocation); // array allocation is size in bytes
  // a defrag-mem allocation is always bigger than 0
  lbm_uint nwords = bs2ws(size) + 3;
  lbm_value cell_back_ptr = DEFRAG_ALLOC_CELLPTR(allocation);

  // I have a feeling that it should be impossible for the
  // cell to be recovered if we end up here.
  // if the cell is recovered, then the data should also have been
  // cleared in the defrag_mem.

  cell_back_ptr = lbm_set_ptr_type(cell_back_ptr, LBM_TYPE_CONS);
  bool marked = lbm_cdr(cell_back_ptr) & LBM_GC_MASK;
  lbm_value new_cdr = marked ? (ENC_SYM_NIL | LBM_GC_MARKED) : ENC_SYM_NIL;
  lbm_set_car_and_cdr(cell_back_ptr, ENC_SYM_NIL, new_cdr);
  // possible optimize, if not marked. dont bother setting anything.

  for (lbm_uint i = 0; i < nwords; i ++) {
    allocation[i] = 0;
  }
}

// Called by GC.
// As it is called by GC, gc bits may be set and needs to be
// honored!
void lbm_defrag_mem_destroy(lbm_uint *defrag_mem) {
  lbm_uint nwords = DEFRAG_MEM_SIZE(defrag_mem);
  lbm_uint *defrag_data = DEFRAG_MEM_DATA(defrag_mem);
  for (lbm_uint i = 0; i < nwords;) {
    lbm_uint a = defrag_data[i];
    if (a != 0) {
      lbm_uint *allocation = &defrag_data[i];
      lbm_uint alloc_words = 3 + bs2ws(DEFRAG_ALLOC_SIZE(allocation));
      free_defrag_allocation(allocation);
      i += alloc_words;
    }
    else i ++;
  }
  lbm_free(defrag_mem);
}


static void lbm_defrag_mem_defrag(lbm_uint *defrag_mem, lbm_uint bytes) {
  lbm_uint mem_size = ((lbm_uint*)defrag_mem)[0]; // mem size words
  lbm_uint *mem_data = DEFRAG_MEM_DATA(defrag_mem);
  lbm_uint hole_start = 0;

  lbm_uint until_size = bs2ws(bytes) + 3; // defrag until hole is this size or complete defrag.

  for (lbm_uint i = 0; i < mem_size; ) {
    // check if there is an allocation here
    if (mem_data[i] != 0) {
      lbm_uint *source = &mem_data[i];
      lbm_uint *target = &mem_data[hole_start];
      lbm_uint alloc_bytes = DEFRAG_ALLOC_SIZE(source);
      lbm_uint alloc_words = bs2ws(alloc_bytes);
      // move allocation into hole
      if (hole_start == i) {
        i += alloc_words+3;
        hole_start = i;
      } else {
        lbm_uint move_dist = i - hole_start;
        if (move_dist >= until_size) break;
        lbm_uint clear_ix = (hole_start + alloc_words + 3);
        memmove(target, source, (alloc_words + 3) * sizeof(lbm_uint));
        memset(&mem_data[clear_ix],0, move_dist* sizeof(lbm_uint));
        DEFRAG_ALLOC_DATA(target) = (lbm_uint)&target[3];
        lbm_value cell = DEFRAG_ALLOC_CELLPTR(target);

        lbm_set_car(cell,(lbm_uint)target);
        // move home and i forwards.
        // i can move to the original end of allocation.
        hole_start += alloc_words + 3;
        i += alloc_words + 3;
      }
    } else {
      // no allocation hole remains but i increments.
      i ++;
    }
  }
}

// Allocate an array from the defragable pool
// these arrays must be recognizable by GC so that
// gc can free them by performing a call into the defrag_mem api.
// At the same time they need to be just bytearrays..
// Allocation must always scan from start.
//

#define INIT 0
#define FREE_LEN 1

// An array allocated in defragmem has the following layout inside of the defrag mem
//
// [header (size, data-ptr) cell_back_ptr | data | padding ]

lbm_value lbm_defrag_mem_alloc_internal(lbm_uint *defrag_mem, lbm_uint bytes) {

  if (bytes == 0) return ENC_SYM_EERROR;
  lbm_value cell = lbm_heap_allocate_cell(LBM_TYPE_CONS, ENC_SYM_NIL, ENC_SYM_DEFRAG_ARRAY_TYPE);

  if (cell == ENC_SYM_MERROR) {
    return cell;
  }

  lbm_uint mem_size = DEFRAG_MEM_SIZE(defrag_mem);
  lbm_uint *mem_data = DEFRAG_MEM_DATA(defrag_mem);

  lbm_uint num_words = bs2ws(bytes);
  lbm_uint alloc_words = num_words + 3;

  uint8_t state = INIT;
  lbm_uint free_words = 0;
  lbm_uint free_start = 0;
  bool alloc_found = false;
  lbm_value res = ENC_SYM_MERROR;

  for (lbm_uint i = 0; i < mem_size;) {
    switch(state) {
    case INIT:
      if (mem_data[i] == 0) {
        free_start = i;
        free_words = 1;
        state = FREE_LEN;
        i++;
      } else {
        // jump to next spot
        i += bs2ws(mem_data[i]) + 3;
      }
      break;
    case FREE_LEN:
      if (mem_data[i] == 0) {
        free_words ++;
        if (free_words >= alloc_words) {
          alloc_found = true;
        } else {
          i ++;
        }
      } else {
        state = INIT;
        i++;
      }
      break;
    }
    if (alloc_found) break;
  }
  if (alloc_found) {
    lbm_uint *allocation = (lbm_uint*)&mem_data[free_start];
    DEFRAG_ALLOC_SIZE(allocation) = bytes;
    DEFRAG_ALLOC_DATA(allocation) = (lbm_uint)&allocation[3]; //data starts after back_ptr
    DEFRAG_ALLOC_CELLPTR(allocation) = cell;
    lbm_set_car(cell, (lbm_uint)allocation);
    cell = lbm_set_ptr_type(cell, LBM_TYPE_ARRAY);
    res = cell;
  } else {
    DEFRAG_MEM_FLAGS(defrag_mem) = 1;
    lbm_set_car_and_cdr(cell, ENC_SYM_NIL, ENC_SYM_NIL);
  }
  return res;
}

lbm_value lbm_defrag_mem_alloc(lbm_uint *defrag_mem, lbm_uint bytes) {

  lbm_value res = lbm_defrag_mem_alloc_internal(defrag_mem, bytes); // Try to allocate 
  if (lbm_is_symbol_merror(res)) {
    if (DEFRAG_MEM_FLAGS(defrag_mem)) { //if we already performed GC, then also defrag
      lbm_defrag_mem_defrag(defrag_mem, bytes);
      res = lbm_defrag_mem_alloc_internal(defrag_mem, bytes); // then try again
      DEFRAG_MEM_FLAGS(defrag_mem) = 0;
    }
  }
  return res;
}


// IF GC frees a defrag allocation, the cell pointing to it will also be destroyed by GC.

// is it guaranteed there are no copies of a heap-cell representing a defrag allocation.
// Same guarantee must hold for arrays in general or it would not be possible to explicitly free
// any array.

// At the time of free from GC all we have is the pointer.
void lbm_defrag_mem_free(lbm_uint* data) {
  lbm_uint nbytes = data[0];
  lbm_uint words_to_wipe = 3 + bs2ws(nbytes);
  for (lbm_uint i = 0; i < words_to_wipe; i ++) {
    data[i] = 0;
  }
}

