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

#include <string.h>

#include "stack.h"
#include "lispbm_types.h"
#include "print.h"
#include "lispbm_memory.h"

int lbm_stack_allocate(lbm_stack_t *s, unsigned int stack_size) {
  s->data = lbm_memory_allocate(stack_size);
  s->sp = 0;
  s->size = stack_size;
  s->max_sp = 0;

  if (s->data) return 1;
  return 0;
}

int lbm_stack_create(lbm_stack_t *s, lbm_uint* data, unsigned int size) {
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

int lbm_stack_clear(lbm_stack_t *s) {
  s->sp = 0;
  return 1;
}

lbm_uint *lbm_get_stack_ptr(lbm_stack_t *s, unsigned int n) {
  if (n > s->sp) return NULL;
  unsigned int index = s->sp - n;
  return &s->data[index];
}

int lbm_stack_drop(lbm_stack_t *s, unsigned int n) {

  if (n > s->sp) return 0;

  s->sp -= n;
  return 1;
}

int lbm_push_u32(lbm_stack_t *s, lbm_uint val) {
  int res = 1;
  if (s->sp == s->size) {
    return 0;
  }

  if (!res) return res;

  s->data[s->sp] = val;
  s->sp++;

  if (s->sp > s->max_sp) s->max_sp = s->sp;

  return res;
}

int lbm_pop_u32(lbm_stack_t *s, lbm_uint *val) {

  s->sp--;
  *val = s->data[s->sp];
  s->data[s->sp] = 0;
  return 1;
}
