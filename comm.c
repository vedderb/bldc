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
#include "comm.h"
#include "main.h"
#include "stm32f4xx_conf.h"
#include "mcpwm.h"
#include "servo.h"
#include "buffer.h"
#include "packet.h"
#include "myUSB.h"
#include "terminal.h"

#include <math.h>
#include <string.h>

// Settings
#define PACKET_BUFFER_LEN	30
#define PRINT_BUFFER_LEN	10
#define PRINT_MAXLEN		240

// Private variables
#define SERIAL_RX_BUFFER_SIZE		4096
static uint8_t serial_rx_buffer[SERIAL_RX_BUFFER_SIZE];
static int serial_rx_read_pos = 0;
static int serial_rx_write_pos = 0;
static WORKING_AREA(serial_read_thread_wa, 1024);
static WORKING_AREA(serial_process_thread_wa, 2048);
static WORKING_AREA(timer_thread_wa, 128);
static Mutex send_mutex;
static Thread *process_tp;

// Private functions
static void send_ping(void);
static void handle_res_packet(unsigned char *data, unsigned char len);
static void handle_nores_packet(unsigned char *data, unsigned char len);
static void process_packet(unsigned char *buffer, unsigned char len);
static void send_packet(unsigned char *buffer, unsigned char len);

static msg_t serial_read_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Serial read");

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

	chRegSetThreadName("Serial process");

	process_tp = chThdSelf();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		while (serial_rx_read_pos != serial_rx_write_pos) {
			packet_process_byte(serial_rx_buffer[serial_rx_read_pos++]);

			if (serial_rx_read_pos == SERIAL_RX_BUFFER_SIZE) {
				serial_rx_read_pos = 0;
			}
		}
	}

	return 0;
}

static msg_t timer_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Serial timer");

	for(;;) {
		packet_timerfunc();
		chThdSleepMilliseconds(1);
	}

	return 0;
}

static void process_packet(unsigned char *buffer, unsigned char len) {
	if (!len) {
		return;
	}

	switch (buffer[0]) {
	case 0:
		// Send the received data to the main loop
		main_process_packet(buffer + 1, len - 1);
		break;

	case 1:
		/*
		 * Packet that expects response
		 */
		handle_res_packet(buffer + 1, len - 1);
		break;

	case 2:
		/*
		 * Packet that expects no response
		 */
		handle_nores_packet(buffer + 1, len - 1);
		break;

	default:
		break;
	}
}

static void send_packet(unsigned char *buffer, unsigned char len) {
	chMtxLock(&send_mutex);
	chSequentialStreamWrite(&SDU1, buffer, len);
	chMtxUnlock();
}

void comm_init(void) {
	myUSBinit();
	packet_init(send_packet, process_packet);

	chMtxInit(&send_mutex);

	// Threads
	chThdCreateStatic(serial_read_thread_wa, sizeof(serial_read_thread_wa), NORMALPRIO, serial_read_thread, NULL);
	chThdCreateStatic(serial_process_thread_wa, sizeof(serial_process_thread_wa), NORMALPRIO, serial_process_thread, NULL);
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
}

static void send_ping(void) {
	unsigned char byte = CAR_PACKET_PING;
	packet_send_packet(&byte, 1);
}

static void handle_res_packet(unsigned char *data, unsigned char len) {
	CAR_RES_PACKET_ID car_res_packet;
	uint8_t buffer2[256];
	int32_t index;

	(void)len;

	car_res_packet = data[0];
	data++;
	len--;

	switch (car_res_packet) {
	case CAR_PACKET_READ_VALUES:
		index = 0;
		buffer2[index++] = CAR_PACKET_READ_VALUES;
		buffer_append_int16(buffer2, (int16_t)(37.0 * 10.0), &index);
		buffer_append_int16(buffer2, (int16_t)(38.0 * 10.0), &index);
		buffer_append_int16(buffer2, (int16_t)(39.0 * 10.0), &index);
		buffer_append_int16(buffer2, (int16_t)(40.0 * 10.0), &index);
		buffer_append_int16(buffer2, (int16_t)(41.0 * 10.0), &index);
		buffer_append_int16(buffer2, (int16_t)(42.0 * 10.0), &index);
		buffer_append_int16(buffer2, (int16_t)(43.0 * 10.0), &index);
		buffer_append_int32(buffer2, (int32_t)(mcpwm_read_reset_avg_motor_current() * 100.0), &index);
		buffer_append_int32(buffer2, (int32_t)(mcpwm_read_reset_avg_input_current() * 100.0), &index);
		buffer_append_int16(buffer2, (int16_t)(mcpwm_get_dutycycle_now() * 1000.0), &index);
		buffer_append_int32(buffer2, (int32_t)mcpwm_get_rpm(), &index);
		buffer_append_int16(buffer2, (int16_t)(GET_INPUT_VOLTAGE * 10.0), &index);
		packet_send_packet(buffer2, index);
		break;

	case CAR_PACKET_READ_POS:
		/*
		 * Read dead reckoning position.
		 */

		break;

	case CAR_PACKET_READ_SENS_ULTRA:

		break;

	case CAR_PACKET_READ_SENS_IR:

		break;

	case CAR_PACKET_READ_TRAVEL_COUNTER:

		break;

	case CAR_PACKET_PING:
		send_ping();
		break;

	default:
		break;
	}
}

static void handle_nores_packet(unsigned char *data, unsigned char len) {
	CAR_NORES_PACKET_ID car_nores_packet;

	(void)len;

	car_nores_packet = data[0];
	data++;
	len--;

	switch (car_nores_packet) {
	case CAR_PACKET_SET_POWER_SERVO:

		break;

	case CAR_PACKET_WRITE_POS:

		break;

	case CAR_PACKET_ADD_POINT:

		break;

	case CAR_PACKET_AP_RUN:

		break;

	case CAR_PACKET_AP_CLEAR:

		break;

	case CAR_PACKET_RESET_TRAVEL_CNT:

		break;

	case CAR_PACKET_SET_LIMITED:

		break;

	case CAR_PACKET_FULL_BRAKE:
		mcpwm_full_brake();
		break;

	case CAR_PACKET_SERVO_OFFSET:
		servos[0].offset = data[0];
		break;

	case CAR_PACKET_CAN_TEST:

		break;

	case CAR_PACKET_TERMINAL_CMD:
		data[len] = '\0';
		terminal_process_string((char*)data);
		break;

	default:
		break;
	}
}

void comm_print(char* str) {
	static char print_buffer[255];
	int i;
	print_buffer[0] = CAR_PACKET_PRINT;

	for (i = 0;i < 255;i++) {
		if (str[i] == '\0') {
			break;
		}
		print_buffer[i + 1] = str[i];
	}

	packet_send_packet((unsigned char*)print_buffer, i + 1);
	return;
}

void comm_send_samples(uint8_t *data, int len) {
	uint8_t buffer[len + 1];
	int index = 0;

	buffer[index++] = CAR_PACKET_SEND_SAMPLES;

	for (int i = 0;i < len;i++) {
		buffer[index++] = data[i];
	}

	packet_send_packet(buffer, index);
}
