/*
    Copyright 2020, 2021, 2022 Joel Svensson  svenssonjoel@yahoo.se

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

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "lbm_memory.h"
#include "platform_mutex.h"

/* Status bit patterns */
#define FREE_OR_USED  0  //00b
#define END           1  //01b
#define START         2  //10b
#define START_END     3  //11b

/* States in memory_allocate state-machine*/
#define INIT                 0
#define FREE_LENGTH_CHECK    1
#define SKIP                 2
#define ALLOC_DONE           0xF00DF00D
#define ALLOC_FAILED         0xDEADBEAF

static lbm_uint *bitmap = NULL;
static lbm_uint *memory = NULL;
static lbm_uint memory_size;  // in 4 or 8 byte words depending on 32 or 64 bit platform
static lbm_uint bitmap_size;  // in 4 or 8 byte words
static lbm_uint memory_base_address = 0;
static mutex_t lbm_mem_mutex;
static bool    lbm_mem_mutex_initialized;

int lbm_memory_init(lbm_uint *data, lbm_uint data_size,
                    lbm_uint *bits, lbm_uint bits_size) {

  if (!lbm_mem_mutex_initialized) {
    mutex_init(&lbm_mem_mutex);
  }
  mutex_lock(&lbm_mem_mutex);
  int res = 0;
  if (data == NULL || bits == NULL) return 0;

  if (((lbm_uint)data % sizeof(lbm_uint) != 0) ||
      (data_size * 2) != (bits_size * sizeof(lbm_uint) * 8) ||
      data_size % 4 != 0 ||
      ((lbm_uint)bits % sizeof(lbm_uint) != 0) ||
      bits_size < 1 ||
      bits_size % 4 != 0) {
    // data is not aligned to sizeof lbm_uint
    // size is too small
    // or size is not a multiple of 4
  } else {

    bitmap = bits;
    bitmap_size = bits_size;

    for (lbm_uint i = 0; i < bitmap_size; i ++) {
      bitmap[i] = 0;
    }

    memory = data;
    memory_base_address = (lbm_uint)data;
    memory_size = data_size;
    res = 1;
  }
  mutex_unlock(&lbm_mem_mutex);
  return res;
}

static inline lbm_uint address_to_bitmap_ix(lbm_uint *ptr) {
  #ifndef LBM64
  return ((lbm_uint)ptr - memory_base_address) >> 2;
  #else
  return ((lbm_uint)ptr - memory_base_address) >> 3;
  #endif
}

lbm_int lbm_memory_address_to_ix(lbm_uint *ptr) {
  /* TODO: assuming that index
           will have more then enough room in the
           positive half of a 28bit integer */
  return (lbm_int)address_to_bitmap_ix(ptr);
}


static inline lbm_uint *bitmap_ix_to_address(lbm_uint ix) {
  return &((lbm_uint*)(memory_base_address))[ix];// + (ix << 2));
}

#ifndef LBM64
#define WORD_IX_SHIFT 5
#define WORD_MOD_MASK 0x1F
#define BITMAP_SIZE_SHIFT 4  // 16 statuses per bitmap word
#else
#define WORD_IX_SHIFT 6      // divide by 64
#define WORD_MOD_MASK 0x3F   // mod 64
#define BITMAP_SIZE_SHIFT 5  // times 32, 32 statuses per bitmap word
#endif

static inline lbm_uint status(lbm_uint i) {

  lbm_uint ix = i << 1;                      // * 2
  lbm_uint word_ix = ix >> WORD_IX_SHIFT;    // / 32
  lbm_uint bit_ix  = ix & WORD_MOD_MASK;              // % 32

  lbm_uint mask = ((lbm_uint)3) << bit_ix;       // 000110..0
  if (word_ix > bitmap_size) {
    return (lbm_uint)NULL;
  }
  return (bitmap[word_ix] & mask) >> bit_ix;
}

static inline void set_status(lbm_uint i, lbm_uint status) {
  lbm_uint ix = i << 1;          // * 2
  lbm_uint word_ix = ix >> WORD_IX_SHIFT;    // / 32
  lbm_uint bit_ix  = ix & WORD_MOD_MASK;  // % 32

  lbm_uint clr_mask = ~(((lbm_uint)3) << bit_ix);
  lbm_uint mask = status << bit_ix;

  bitmap[word_ix] &= clr_mask;
  bitmap[word_ix] |= mask;
}

lbm_uint lbm_memory_num_words(void) {
  return memory_size;
}

lbm_uint lbm_memory_num_free(void) {
  if (memory == NULL || bitmap == NULL) {
    return 0;
  }
  mutex_lock(&lbm_mem_mutex);
  unsigned int state = INIT;
  lbm_uint sum_length = 0;

  for (unsigned int i = 0; i < (bitmap_size << BITMAP_SIZE_SHIFT); i ++) {

    switch(status(i)) {
    case FREE_OR_USED:
      switch (state) {
      case INIT:
          state = FREE_LENGTH_CHECK;
          sum_length ++;
        break;
      case FREE_LENGTH_CHECK:
        sum_length ++;
        state = FREE_LENGTH_CHECK;
        break;
      case SKIP:
        break;
      }
      break;
    case END:
      state = INIT;
      break;
    case START:
      state = SKIP;
      break;
    case START_END:
      state = INIT;
      break;
    default:
      mutex_unlock(&lbm_mem_mutex);
      return 0;
      break;
    }
  }
  mutex_unlock(&lbm_mem_mutex);
  return sum_length;
}

lbm_uint lbm_memory_longest_free(void) {
  if (memory == NULL || bitmap == NULL) {
    return 0;
  }
  mutex_lock(&lbm_mem_mutex);
  unsigned int state = INIT;
  lbm_uint max_length = 0;

  lbm_uint curr_length = 0;
  for (unsigned int i = 0; i < (bitmap_size << BITMAP_SIZE_SHIFT); i ++) {

    switch(status(i)) {
    case FREE_OR_USED:
      switch (state) {
      case INIT:
        curr_length = 1;
        if (curr_length > max_length) max_length = curr_length;
        state = FREE_LENGTH_CHECK;
        break;
      case FREE_LENGTH_CHECK:
        curr_length ++;
        if (curr_length > max_length) max_length = curr_length;
        state = FREE_LENGTH_CHECK;
        break;
      case SKIP:
        break;
      }
      break;
    case END:
      state = INIT;
      break;
    case START:
      state = SKIP;
      break;
    case START_END:
      state = INIT;
      break;
    default:
      mutex_unlock(&lbm_mem_mutex);
      return 0;
      break;
    }
  }
  mutex_unlock(&lbm_mem_mutex);
  return max_length;
}


lbm_uint *lbm_memory_allocate(lbm_uint num_words) {

  if (memory == NULL || bitmap == NULL) {
    return NULL;
  }

  mutex_lock(&lbm_mem_mutex);

  lbm_uint start_ix = 0;
  lbm_uint end_ix = 0;
  lbm_uint free_length = 0;
  unsigned int state = INIT;

  for (unsigned int i = 0; i < (bitmap_size << BITMAP_SIZE_SHIFT); i ++) {
    if (state == ALLOC_DONE) break;

    switch(status(i)) {
    case FREE_OR_USED:
      switch (state) {
      case INIT:
        start_ix = i;
        if (num_words == 1) {
          end_ix = i;
          state = ALLOC_DONE;
        } else {
          state = FREE_LENGTH_CHECK;
          free_length = 1;
        }
        break;
      case FREE_LENGTH_CHECK:
        free_length ++;
        if (free_length == num_words) {
          end_ix = i;
          state = ALLOC_DONE;
        } else {
          state = FREE_LENGTH_CHECK;
        }
        break;
      case SKIP:
        break;
      }
      break;
    case END:
      state = INIT;
      break;
    case START:
      state = SKIP;
      break;
    case START_END:
      state = INIT;
      break;
    default: // error case
      mutex_unlock(&lbm_mem_mutex);
      return NULL;
    }
  }

  if (state == ALLOC_DONE) {
    if (start_ix == end_ix) {
      set_status(start_ix, START_END);
    } else {
      set_status(start_ix, START);
      set_status(end_ix, END);
    }
    mutex_unlock(&lbm_mem_mutex);
    return bitmap_ix_to_address(start_ix);
  }
  mutex_unlock(&lbm_mem_mutex);
  return NULL;
}

int lbm_memory_free(lbm_uint *ptr) {

  int r = 0;
  if (lbm_memory_ptr_inside(ptr)) {
    mutex_lock(&lbm_mem_mutex);
    lbm_uint ix = address_to_bitmap_ix(ptr);

    switch(status(ix)) {
    case START:
      set_status(ix, FREE_OR_USED);
      for (lbm_uint i = ix; i < (bitmap_size << BITMAP_SIZE_SHIFT); i ++) {
        if (status(i) == END) {
          set_status(i, FREE_OR_USED);
          r = 1;
          break;
        }
      }
      break;
    case START_END:
      set_status(ix, FREE_OR_USED);
      r = 1;
      break;
    default:
      break;
    }
    mutex_unlock(&lbm_mem_mutex);
  }
  return r;
}
//Malloc/free like interface
void* lbm_malloc(size_t size) {
  lbm_uint alloc_size;
  if (size % sizeof(lbm_uint) == 0) {
    alloc_size = size / (sizeof(lbm_uint));
  } else {
    alloc_size = (size / (sizeof(lbm_uint))) + 1;
  }

  return lbm_memory_allocate(alloc_size);
}

void lbm_free(void *ptr) {
  lbm_memory_free(ptr);
}

int lbm_memory_shrink(lbm_uint *ptr, lbm_uint n) {
  lbm_uint ix = address_to_bitmap_ix(ptr);

  mutex_lock(&lbm_mem_mutex);
  if (status(ix) != START) {
    mutex_unlock(&lbm_mem_mutex);
    return 0; // ptr does not point to the start of an allocated range.
  }
  if (status(ix) == START_END) {
    mutex_unlock(&lbm_mem_mutex);
    return 0; // Cannot shrink a 1 element allocation
  }

  bool done = false;
  unsigned int i = 0;
  for (i = 0; i < ((bitmap_size << BITMAP_SIZE_SHIFT) - ix); i ++) {
    if (status(ix+i) == END && i < n) {
      mutex_unlock(&lbm_mem_mutex);
      return 0; // cannot shrink allocation to a larger size
    }
    switch(status(ix+i)) {
    case START:
      break;
    case END:
      break;
    case START_END:
      break;
    case FREE_OR_USED:
      break;
    default:
      break;
    }

    if (i == (n-1)) {
      if (status(ix+i) == END ||
          status(ix+i) == START_END) {
        done = true;
      }
      if (i == 0) {
        set_status(ix+i, START_END);
      }
      else {
        set_status(ix+i, END);
      }
      break;
    }
  }

  if (!done) {
    i++; // move to next position, prev position should be END or START_END
    for (;i < ((bitmap_size << BITMAP_SIZE_SHIFT) - ix); i ++)
      if (status(ix+i) == END) {
        set_status(ix+i, FREE_OR_USED);
        break;
      }
  }
  mutex_unlock(&lbm_mem_mutex);
  return 1;
}

int lbm_memory_ptr_inside(lbm_uint *ptr) {
  return ((lbm_uint)ptr >= (lbm_uint)memory &&
          (lbm_uint)ptr < (lbm_uint)memory + (memory_size * sizeof(lbm_uint)));
}
