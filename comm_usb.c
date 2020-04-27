/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#include "ch.h"
#include "hal.h"
#include "comm_usb.h"
#include "packet.h"
#include "comm_usb_serial.h"
#include "commands.h"

// Settings
#define PACKET_HANDLER				0

// Private variables
#define SERIAL_RX_BUFFER_SIZE		2048
static uint8_t serial_rx_buffer[SERIAL_RX_BUFFER_SIZE];
static int serial_rx_read_pos = 0;
static int serial_rx_write_pos = 0;
static THD_WORKING_AREA(serial_read_thread_wa, 256);
static THD_WORKING_AREA(serial_process_thread_wa, 4096);
static mutex_t send_mutex;
static thread_t *process_tp;
static volatile unsigned int write_timeout_cnt = 0;
static volatile bool was_timeout = false;

// Private functions
static void process_packet(unsigned char *data, unsigned int len);
static void send_packet_raw(unsigned char *buffer, unsigned int len);

static THD_FUNCTION(serial_read_thread, arg) {
	(void)arg;

	chRegSetThreadName("USB read");

	uint8_t buffer[2];
	int had_data = 0;

	for(;;) {
		int len = chSequentialStreamRead(&SDU1, (uint8_t*) buffer, 1);

		for (int i = 0;i < len;i++) {
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
}

static THD_FUNCTION(serial_process_thread, arg) {
	(void)arg;

	chRegSetThreadName("USB process");

	process_tp = chThdGetSelfX();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		while (serial_rx_read_pos != serial_rx_write_pos) {
			packet_process_byte(serial_rx_buffer[serial_rx_read_pos++], PACKET_HANDLER);

			if (serial_rx_read_pos == SERIAL_RX_BUFFER_SIZE) {
				serial_rx_read_pos = 0;
			}
		}
	}
}

static void process_packet(unsigned char *data, unsigned int len) {
	commands_process_packet(data, len, comm_usb_send_packet);
}

static void send_packet_raw(unsigned char *buffer, unsigned int len) {
	/*
	 * Only write to USB if the cable has been connected at least once. If a timeout occurs
	 * make sure that this call does not stall on the next call, as the timeout probably occured
	 * because noone is listening on the USB.
	 */
	if (comm_usb_serial_configured_cnt() > 0) {
		unsigned int written = 0;
		if (was_timeout) {
			written = SDU1.vmt->writet(&SDU1, buffer, len, TIME_IMMEDIATE);
		} else {
			written = SDU1.vmt->writet(&SDU1, buffer, len, MS2ST(100));
		}

		was_timeout = written != len;
		if (was_timeout) {
			write_timeout_cnt++;
		}
	}
}

void comm_usb_init(void) {
	comm_usb_serial_init();
	packet_init(send_packet_raw, process_packet, PACKET_HANDLER);

	chMtxObjectInit(&send_mutex);

	// Threads
	chThdCreateStatic(serial_read_thread_wa, sizeof(serial_read_thread_wa), NORMALPRIO, serial_read_thread, NULL);
	chThdCreateStatic(serial_process_thread_wa, sizeof(serial_process_thread_wa), NORMALPRIO, serial_process_thread, NULL);
}

void comm_usb_send_packet(unsigned char *data, unsigned int len) {
	chMtxLock(&send_mutex);
	packet_send_packet(data, len, PACKET_HANDLER);
	chMtxUnlock(&send_mutex);
}

unsigned int comm_usb_get_write_timeout_cnt(void) {
	return write_timeout_cnt;
}
