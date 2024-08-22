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

#include "vesc_c_if.h"

HEADER

typedef struct {
	int a;
	int b;
	lib_thread thread;
} data;

static void thd(void *arg) {
	data *d = (data*)arg;

	while (!VESC_IF->should_terminate()) {
		VESC_IF->printf("Hello Thd");
		d->b++;
		VESC_IF->sleep_ms(1000);
	}
}

// Called when code is stopped
static void stop(void *arg) {
	data *d = (data*)arg;
	VESC_IF->printf("a: %d, b: %d", d->a, d->b);
	VESC_IF->request_terminate(d->thread);
	VESC_IF->printf("Terminated");
	VESC_IF->free(d);
}

INIT_FUN(lib_info *info) {
	INIT_START

	data *d = VESC_IF->malloc(sizeof(data));
	d->a = 5;
	d->b = 6;
	d->thread = VESC_IF->spawn(thd, 1024, "LibThd", d);
	
	info->stop_fun = stop;
	info->arg = d;
	
	VESC_IF->printf("Hello Example!");
	
	return true;
}

