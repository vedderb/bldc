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

#include "lispbm_memory.h"

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

static uint32_t *bitmap = NULL;
static uint32_t *memory = NULL;
static uint32_t memory_size;  // in 4 byte words
static uint32_t bitmap_size;  // in 4 byte words
static unsigned int memory_base_address = 0;

int memory_init(uint32_t *data, uint32_t data_size,
                uint32_t *bits, uint32_t bits_size) {

  if (data == NULL || bits == NULL) return 0;

  if (((unsigned int)data % 4 != 0) || data_size != 16 * bits_size || data_size % 4 != 0 ||
      ((unsigned int)bits % 4 != 0) || bits_size < 1 || bits_size % 4 != 0) {
    // data is not 4 byte aligned
    // size is too small
    // or size is not a multiple of 4
    return 0;
  }

  bitmap = bits;
  bitmap_size = bits_size;

  for (uint32_t i = 0; i < bitmap_size; i ++) {
    bitmap[i] = 0;
  }

  memory = data;
  memory_base_address = (unsigned int)data;
  memory_size = data_size;
  return 1;
}

static inline unsigned int address_to_bitmap_ix(uint32_t *ptr) {
  return ((unsigned int)ptr - memory_base_address) >> 2;
}

static inline uint32_t *bitmap_ix_to_address(unsigned int ix) {
  return (uint32_t*)(memory_base_address + (ix << 2));
}

static inline unsigned int status(unsigned int i) {

  unsigned int ix = i << 1;          // * 2
  unsigned int word_ix = ix >> 5;    // / 32
  unsigned int bit_ix  = ix & 0x1F;  // % 32

  uint32_t mask = ((uint32_t)3) << bit_ix;       // 000110..0
  return (bitmap[word_ix] & mask) >> bit_ix;
}

static inline void set_status(unsigned int i, uint32_t status) {
  unsigned int ix = i << 1;          // * 2
  unsigned int word_ix = ix >> 5;    // / 32
  unsigned int bit_ix  = ix & 0x1F;  // % 32

  uint32_t clr_mask = ~(((uint32_t)3) << bit_ix);
  uint32_t mask = status << bit_ix;
  bitmap[word_ix] &= clr_mask;
  bitmap[word_ix] |= mask;
}

uint32_t memory_num_words(void) {
  return memory_size;
}

uint32_t memory_num_free(void) {
  if (memory == NULL || bitmap == NULL) {
    return 0;
  }

  unsigned int state = INIT;
  uint32_t sum_length = 0;

  for (unsigned int i = 0; i < (bitmap_size << 4); i ++) {

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
      return 0;
      break;
    }
  }
  return sum_length;
}

uint32_t *memory_allocate(uint32_t num_words) {

  if (memory == NULL || bitmap == NULL) {
    return NULL;
  }

  uint32_t start_ix = 0;
  uint32_t end_ix = 0;
  uint32_t free_length = 0;
  unsigned int state = INIT;

  for (unsigned int i = 0; i < (bitmap_size << 4); i ++) {
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
    default:
      return NULL;
      break;
    }
  }

  if (state == ALLOC_DONE) {
    if (start_ix == end_ix) {
      set_status(start_ix, START_END);
    } else {
      set_status(start_ix, START);
      set_status(end_ix, END);
    }
    return bitmap_ix_to_address(start_ix);
  }
  return NULL;
}

int memory_free(uint32_t *ptr) {
  unsigned int ix = address_to_bitmap_ix(ptr);
  switch(status(ix)) {
  case START:
    set_status(ix, FREE_OR_USED);
    for (unsigned int i = ix; i < (bitmap_size << 4); i ++) {
      if (status(i) == END) {
        set_status(i, FREE_OR_USED);
        return 1;
      }
    }
    return 0;
  case START_END:
    set_status(ix, FREE_OR_USED);
    return 1;
  }

  return 0;
}
