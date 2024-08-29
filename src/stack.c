/*
    Copyright 2019, 2021, 2024 Joel Svensson  svenssonjoel@yahoo.se

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

#include <lbm_memory.h>
#include <lbm_types.h>
#include <string.h>

#include "stack.h"
#include "print.h"

#define STACK_UNUSED_BYTE 0x55
#ifndef LBM64
#define STACK_UNUSED_WORD 0x55555555
#else
#define STACK_UNUSED_WORD 0x5555555555555555
#endif

int lbm_stack_allocate(lbm_stack_t *s, lbm_uint stack_size) {
  int r = 0;
  s->data = lbm_memory_allocate(stack_size);
  if (s->data) {
    memset(s->data, STACK_UNUSED_BYTE, stack_size * sizeof(lbm_uint));
    s->sp = 0;
    s->size = stack_size;
    r = 1;
  }
  return r;
}

int lbm_stack_create(lbm_stack_t *s, lbm_uint* data, lbm_uint stack_size) {
  s->data = data;
  memset(s->data, STACK_UNUSED_BYTE, stack_size * sizeof(lbm_uint));
  s->sp = 0;
  s->size = stack_size;
  return 1;
}

lbm_uint lbm_get_max_stack(lbm_stack_t *s) {
  lbm_uint unused = 0;
  for (int i = (int)s->size-1 ; i >= 0; i --) {
    if (s->data[i] == STACK_UNUSED_WORD) {
      unused ++;
    } else {
      break;
    }
  }
  return s->size - unused;
}

void lbm_stack_free(lbm_stack_t *s) {
  if (s->data) {
    lbm_memory_free(s->data);
  }
}

void lbm_stack_clear(lbm_stack_t *s) {
  s->sp = 0;
}

int lbm_stack_drop(lbm_stack_t *s, lbm_uint n) {

  if (n > s->sp) return 0;

  s->sp -= n;
  return 1;
}

int lbm_push(lbm_stack_t *s, lbm_uint val) {
  int res = 1;
  if (s->sp == s->size) {
    return 0;
  }
  s->data[s->sp++] = val;
  return res;
}

int lbm_pop(lbm_stack_t *s, lbm_uint *val) {
  s->sp--;
  *val = s->data[s->sp];
  return 1;
}

int lbm_pop_2(lbm_stack_t *s, lbm_uint *r0, lbm_uint *r1) {
  s->sp--;
  *r0 = s->data[s->sp--];
  *r1 = s->data[s->sp];
  return 1;
}

int lbm_pop_3(lbm_stack_t *s, lbm_uint *r0, lbm_uint *r1, lbm_uint *r2) {
  s->sp--;
  *r0 = s->data[s->sp--];
  *r1 = s->data[s->sp--];
  *r2 = s->data[s->sp];
  return 1;
}

