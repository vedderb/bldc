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

#include "rb.h"
#include <stdlib.h>
#include <string.h>

// Private functions (without locks)
static unsigned int get_item_count(rb_t *rb);
static unsigned int get_free_space(rb_t *rb);
static bool is_empty(rb_t *rb);
static bool pop(rb_t *rb, void *data);
static bool insert(rb_t *rb, const void *data);

void rb_init(rb_t *rb, void *buffer, int item_size, int item_count) {
	chMtxObjectInit(&(rb->mutex));
	rb->data = buffer;
	rb->item_size = item_size;
	rb->item_count = item_count;
	rb->head = 0;
	rb->tail = 0;
	rb->full = false;
}

void rb_init_alloc(rb_t *rb, int item_size, int item_count) {
	void *buffer = malloc(item_size * item_count);
	rb_init(rb, buffer, item_size, item_count);
}

void rb_free(rb_t *rb) {
	free(rb->data);
}

void rb_flush(rb_t *rb) {
	chMtxLock(&(rb->mutex));
	rb->head = 0;
	rb->tail = 0;
	rb->full = false;
	chMtxUnlock(&(rb->mutex));
}

bool rb_insert(rb_t *rb, const void *data) {
	chMtxLock(&(rb->mutex));
	bool res = insert(rb, data);
	chMtxUnlock(&(rb->mutex));
	return res;
}

unsigned int rb_insert_multi(rb_t *rb, const void *data, unsigned int count) {
	unsigned int cnt = 0;
	chMtxLock(&(rb->mutex));
	while (!rb->full && cnt < count) {
		insert(rb, (char*)data + rb->item_size * cnt);
		cnt++;
	}
	chMtxUnlock(&(rb->mutex));
	return cnt;
}

bool rb_pop(rb_t *rb, void *data) {
	chMtxLock(&(rb->mutex));
	bool res = pop(rb, data);
	chMtxUnlock(&(rb->mutex));
	return res;
}

unsigned int rb_pop_multi(rb_t *rb, void *data, unsigned int count) {
	unsigned int cnt = 0;
	chMtxLock(&(rb->mutex));
	while (!is_empty(rb) && cnt < count) {
		if (data) {
			pop(rb, (char*)data + rb->item_size * cnt);
		} else {
			pop(rb, 0);
		}
		cnt++;
	}
	chMtxUnlock(&(rb->mutex));
	return cnt;
}

bool rb_is_full(rb_t *rb) {
	chMtxLock(&(rb->mutex));
	bool res = rb->full;
	chMtxUnlock(&(rb->mutex));
	return res;
}

bool rb_is_empty(rb_t *rb) {
	chMtxLock(&(rb->mutex));
	bool res = is_empty(rb);
	chMtxUnlock(&(rb->mutex));
	return res;
}

unsigned int rb_get_item_count(rb_t *rb) {
	chMtxLock(&(rb->mutex));
	unsigned int res = get_item_count(rb);
	chMtxUnlock(&(rb->mutex));
	return res;
}

unsigned int rb_get_free_space(rb_t *rb) {
	chMtxLock(&(rb->mutex));
	unsigned int res = get_free_space(rb);
	chMtxUnlock(&(rb->mutex));
	return res;
}

// Private function implementations

static unsigned int get_item_count(rb_t *rb) {
	unsigned int res = rb->item_count;
	
	if (!rb->full) {
		if (rb->head >= rb->tail) {
			res = rb->head - rb->tail;
		} else {
			res = rb->item_count - rb->tail + rb->head;
		}
	}
	
	return res;
}

static unsigned int get_free_space(rb_t *rb) {
	return rb->item_count - get_item_count(rb);
}

static bool is_empty(rb_t *rb) {
	return rb->head == rb->tail && !rb->full;
}

static bool pop(rb_t *rb, void *data) {
	if (is_empty(rb)) {
		return false;
	}

	// Null will just advance the tail and discard the data
	if (data) {
		memcpy(data, (char*)(rb->data) + rb->tail * rb->item_size, rb->item_size);
	}

	rb->tail = (rb->tail + 1) % rb->item_count;
	rb->full = false;

	return true;
}

static bool insert(rb_t *rb, const void *data) {
	if (rb->full) {
		return false;
	}

	memcpy((char*)(rb->data) + rb->head * rb->item_size, data, rb->item_size);
	rb->head = (rb->head + 1) % rb->item_count;
	rb->full = rb->head == rb->tail;

	return true;
}
