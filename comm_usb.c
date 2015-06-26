/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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

/*
 * comm.c
 *
 *  Created on: 22 nov 2012
 *      Author: benjamin
 */

#include "ch.h"
#include "hal.h"
#include "comm_usb.h"
#include "packet.h"
#include "myUSB.h"
#include "commands.h"

// Settings
#define PACKET_HANDLER				0

// Private variables
#define SERIAL_RX_BUFFER_SIZE		2048
static uint8_t serial_rx_buffer[SERIAL_RX_BUFFER_SIZE];
static int serial_rx_read_pos = 0;
static int serial_rx_write_pos = 0;
static WORKING_AREA(serial_read_thread_wa, 512);
static WORKING_AREA(serial_process_thread_wa, 4096);
static Mutex send_mutex;
static Thread *process_tp;

// Private functions
static void process_packet(unsigned char *data, unsigned int len);
static void send_packet(unsigned char *buffer, unsigned int len);
static void send_packet_wrapper(unsigned char *data, unsigned int len);

static msg_t serial_read_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("USB-Serial read");

	uint8_t buffer[128];
	int i;
	int len;
	int had_data = 0;

	for(;;) {
		len = chSequentialStreamRead(&SDU1, (uint8_t*) buffer, 1);

		for (i = 0;i < len;i++) {
			serial_rx_buffer[serial_rx_write_pos++] = buffer[i];

			if (serial_rx_write_pos == SERIAL_RX_BUFFER_SIZE) {
				serial_rx_write_pos = 0;
			}

			had_data = 1;
		}

		if (had_data) {
			chEvtSignal(process_tp, (eventmask_t) 1);
			had_data = 0;
		}
	}

	return 0;
}

static msg_t serial_process_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("USB-Serial process");

	process_tp = chThdSelf();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		while (serial_rx_read_pos != serial_rx_write_pos) {
			packet_process_byte(serial_rx_buffer[serial_rx_read_pos++], PACKET_HANDLER);

			if (serial_rx_read_pos == SERIAL_RX_BUFFER_SIZE) {
				serial_rx_read_pos = 0;
			}
		}
	}

	return 0;
}

static void process_packet(unsigned char *data, unsigned int len) {
	commands_set_send_func(send_packet_wrapper);
	commands_process_packet(data, len);
}

static void send_packet_wrapper(unsigned char *data, unsigned int len) {
	chMtxLock(&send_mutex);
	packet_send_packet(data, len, PACKET_HANDLER);
	chMtxUnlock();
}

static void send_packet(unsigned char *buffer, unsigned int len) {
	chSequentialStreamWrite(&SDU1, buffer, len);
}

void comm_usb_init(void) {
	myUSBinit();
	packet_init(send_packet, process_packet, PACKET_HANDLER);

	chMtxInit(&send_mutex);

	// Threads
	chThdCreateStatic(serial_read_thread_wa, sizeof(serial_read_thread_wa), NORMALPRIO, serial_read_thread, NULL);
	chThdCreateStatic(serial_process_thread_wa, sizeof(serial_process_thread_wa), NORMALPRIO, serial_process_thread, NULL);
}
