/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

#ifndef RB_H_
#define RB_H_

#include <stdint.h>
#include <stdbool.h>
#include "vesc_c_if.h"

typedef struct {
	void *data;
	unsigned int head;
	unsigned int tail;
	unsigned int item_size;
	unsigned int item_count;
	lib_mutex mutex;
	bool full;
} rb_t;

void rb_init(rb_t *rb, void *buffer, int item_size, int item_count);
void rb_init_alloc(rb_t *rb, int item_size, int item_count);
void rb_free(rb_t *rb);
void rb_flush(rb_t *rb);
bool rb_insert(rb_t *rb, const void *data);
unsigned int rb_insert_multi(rb_t *rb, const void *data, unsigned int count);
bool rb_pop(rb_t *rb, void *data);
unsigned int rb_pop_multi(rb_t *rb, void *data, unsigned int count);
bool rb_is_full(rb_t *rb);
bool rb_is_empty(rb_t *rb);
unsigned int rb_get_item_count(rb_t *rb);
unsigned int rb_get_free_space(rb_t *rb);

#endif

