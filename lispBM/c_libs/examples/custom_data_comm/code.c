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
#include "buffer.h"

HEADER

typedef struct {
	int msg_cnt;
	float msg_val;
	lib_thread thread;
} my_data;

// This thread sends the message counter and the last received data
// back every second. The Commands-signal onCustomAppDataReceived
// is received every time  VESC_IF->send_app_data is used.
static void thd(void *arg) {
	volatile my_data *d = (my_data*)arg;

	while (!VESC_IF->should_terminate()) {
		int32_t ind = 0;
		uint8_t buffer[10];
		buffer_append_int32(buffer, d->msg_cnt, &ind);
		buffer_append_float32_auto(buffer, d->msg_val, &ind);
		VESC_IF->send_app_data(buffer, ind);
		VESC_IF->sleep_ms(1000);
	}
}

// This callback is called every time mCommands.sendCustomAppData is used
// with the data it sends. The VESC buffer functions are compatible with
// the JavaScript DataView, so we can even communicate floats as in this
// example. Here we decode and save the data and increase a message
// counter.
static void data_rx_cb(unsigned char *data, unsigned int len) {
	volatile my_data *d = (my_data*)ARG;
	int32_t ind = 0;
	d->msg_val = buffer_get_float32_auto(data, &ind);
	d->msg_cnt++;
	VESC_IF->printf("Received %d bytes, number %.2f", len, (double)d->msg_val);
}

// Called when code is stopped
static void stop(void *arg) {
	my_data *d = (my_data*)arg;
	VESC_IF->request_terminate(d->thread);
	VESC_IF->set_app_data_handler(0); // Unregisted callback-function
	VESC_IF->printf("Terminated");
	VESC_IF->free(d);
}

INIT_FUN(lib_info *info) {
	INIT_START

	my_data *d = VESC_IF->malloc(sizeof(my_data));
	d->msg_val = 0.0;
	d->msg_cnt = 0.0;
	d->thread = VESC_IF->spawn(thd, 2048, "LibThd", d);
	
	info->stop_fun = stop;
	info->arg = d;
	
	// Register callback function that is used when app data is received
	VESC_IF->set_app_data_handler(data_rx_cb);
	
	return true;
}

