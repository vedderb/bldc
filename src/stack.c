/*
    Copyright 2019, 2021 Joel Svensson  svenssonjoel@yahoo.se

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

int lbm_stack_allocate(lbm_stack_t *s, lbm_uint stack_size) {
  s->data = lbm_memory_allocate(stack_size);
  s->sp = 0;
  s->size = stack_size;
  s->max_sp = 0;

  if (s->data) return 1;
  return 0;
}

int lbm_stack_create(lbm_stack_t *s, lbm_uint* data, lbm_uint size) {
  s->data = data;
  s->sp = 0;
  s->size = size;
  s->max_sp = 0;
  return 1;
}

void lbm_stack_free(lbm_stack_t *s) {
  if (s->data) {
    lbm_memory_free(s->data);
  }
}

void lbm_stack_clear(lbm_stack_t *s) {
  s->sp = 0;
}

lbm_uint *lbm_get_stack_ptr(lbm_stack_t *s, lbm_uint n) {
  if (n > s->sp) return NULL;
  lbm_uint index = s->sp - n;
  return &s->data[index];
}

int lbm_stack_drop(lbm_stack_t *s, lbm_uint n) {

  if (n > s->sp) return 0;

  s->sp -= n;
  return 1;
}

lbm_uint *lbm_stack_reserve(lbm_stack_t *s, lbm_uint n) {

  if (s->sp + n >= s->size) {
    return NULL;
  }
  lbm_uint *ptr = &s->data[s->sp];
  s->sp += n;
  return ptr;
}

int lbm_push(lbm_stack_t *s, lbm_uint val) {
  int res = 1;
  if (s->sp == s->size) {
    return 0;
  }
  s->data[s->sp++] = val;
  if (s->sp > s->max_sp) s->max_sp = s->sp;
  return res;
}

int lbm_push_2(lbm_stack_t *s, lbm_uint v1, lbm_uint v2) {
  if (s->sp + 1 < s->size) {
    s->data[s->sp++] = v1;
    s->data[s->sp++] = v2;
    if (s->sp > s->max_sp) s->max_sp = s->sp;
    return 1;
  } else {
    return 0;
  }
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

