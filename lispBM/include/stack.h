#ifndef STACK_H_
#define STACK_H_
/*
    Copyright 2019 Joel Svensson        svenssonjoel@yahoo.se

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
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "lispbm_types.h"

typedef struct {
  lbm_uint* data;
  unsigned int sp;
  unsigned int size;
  unsigned int max_sp;
} lbm_stack_t;

extern int lbm_stack_allocate(lbm_stack_t *s, unsigned int stack_size);
extern int lbm_stack_create(lbm_stack_t *s, lbm_uint* data, unsigned int size);
extern void lbm_stack_free(lbm_stack_t *s);
extern int lbm_stack_clear(lbm_stack_t *s);
extern lbm_uint *lbm_get_stack_ptr(lbm_stack_t *s, unsigned int n);
extern int lbm_stack_drop(lbm_stack_t *s, unsigned int n);
extern int lbm_push_u32(lbm_stack_t *s, lbm_uint val);
extern int lbm_pop_u32(lbm_stack_t *s, lbm_uint *val);

static inline int lbm_stack_is_empty(lbm_stack_t *s) {
  if (s->sp == 0) return 1;
  return 0;
}

//static inline int stack_arg_ix(lbm_stack_t *s, unsigned int ix, lbm_uint *res) {
//  if (ix > s->sp-1) return 0;
//  *res = s->data[s->sp-(ix+1)];
//  return 1;
//}

static inline int lbm_push_u32_2(lbm_stack_t *s, lbm_uint val0, lbm_uint val1) {
  int res = 1;
  res &= lbm_push_u32(s,val0);
  res &= lbm_push_u32(s,val1);
  return res;
}

static inline int lbm_push_u32_3(lbm_stack_t *s, lbm_uint val0, lbm_uint val1, lbm_uint val2) {
  int res = 1;
  res &= lbm_push_u32(s,val0);
  res &= lbm_push_u32(s,val1);
  res &= lbm_push_u32(s,val2);
  return res;
}

static inline int lbm_push_u32_4(lbm_stack_t *s, lbm_uint val0, lbm_uint val1, lbm_uint val2, lbm_uint val3) {
  int res = 1;
  res &= lbm_push_u32(s,val0);
  res &= lbm_push_u32(s,val1);
  res &= lbm_push_u32(s,val2);
  res &= lbm_push_u32(s,val3);
  return res;
}

static inline int lbm_push_u32_5(lbm_stack_t *s, lbm_uint val0, lbm_uint val1, lbm_uint val2, lbm_uint val3, lbm_uint val4) {
  int res = 1;
  res &= lbm_push_u32(s,val0);
  res &= lbm_push_u32(s,val1);
  res &= lbm_push_u32(s,val2);
  res &= lbm_push_u32(s,val3);
  res &= lbm_push_u32(s,val4);
  return res;
}

static inline int lbm_pop_u32_2(lbm_stack_t *s, lbm_uint *r0, lbm_uint *r1) {
  int res = 1;
  res &= lbm_pop_u32(s, r0);
  res &= lbm_pop_u32(s, r1);
  return res;
}

static inline int lbm_pop_u32_3(lbm_stack_t *s, lbm_uint *r0, lbm_uint *r1, lbm_uint *r2) {
  int res = 1;
  res &= lbm_pop_u32(s, r0);
  res &= lbm_pop_u32(s, r1);
  res &= lbm_pop_u32(s, r2);
  return res;
}

static inline int lbm_pop_u32_4(lbm_stack_t *s, lbm_uint *r0, lbm_uint *r1, lbm_uint *r2, lbm_uint *r3) {
  int res = 1;
  res &= lbm_pop_u32(s, r0);
  res &= lbm_pop_u32(s, r1);
  res &= lbm_pop_u32(s, r2);
  res &= lbm_pop_u32(s, r3);
  return res;
}

static inline int lbm_pop_u32_5(lbm_stack_t *s, lbm_uint *r0, lbm_uint *r1, lbm_uint *r2, lbm_uint *r3, lbm_uint *r4) {
  int res = 1;
  res &= lbm_pop_u32(s, r0);
  res &= lbm_pop_u32(s, r1);
  res &= lbm_pop_u32(s, r2);
  res &= lbm_pop_u32(s, r3);
  res &= lbm_pop_u32(s, r4);
  return res;
}


#endif
