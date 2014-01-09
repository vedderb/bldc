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
 * packet.c
 *
 *  Created on: 21 mar 2013
 *      Author: benjamin
 */

#include <string.h>
#include "packet.h"
#include "crc.h"

// Settings
#define RX_TIMEOUT				2

// Packet variables
static volatile unsigned char rx_state = 0;
static volatile unsigned char rx_timeout = 0;

// Function pointers
static void(*send_func)(unsigned char *data, unsigned char len) = 0;
static void(*process_func)(unsigned char *data, unsigned char len) = 0;

void packet_init(void (*s_func)(unsigned char *data, unsigned char len),
		void (*p_func)(unsigned char *data, unsigned char len)) {
	send_func = s_func;
	process_func = p_func;
}

void packet_send_packet(unsigned char *data, unsigned char len) {
	static uint8_t data_buffer[261];
	int b_ind = 0;

	data_buffer[b_ind++] = 2;
	data_buffer[b_ind++] = len;

	for(int i = 0;i < len;i++) {
		data_buffer[b_ind++] = data[i];
	}

	unsigned short crc = crc16(data, len);
	data_buffer[b_ind++] = (uint8_t)(crc >> 8);
	data_buffer[b_ind++] = (uint8_t)(crc & 0xFF);
	data_buffer[b_ind++] = 3;

	if (send_func) {
		send_func(data_buffer, len + 5);
	}
}

void packet_timerfunc(void) {
	if (rx_timeout) {
		rx_timeout--;
	} else {
		rx_state = 0;
	}
}

void packet_process_byte(uint8_t rx_data) {
	static unsigned char payload_length = 0;
	static unsigned char rx_buffer[256];
	static unsigned char rx_data_ptr = 0;
	static unsigned char crc_low = 0;
	static unsigned char crc_high = 0;

	switch (rx_state) {
	case 0:
		if (rx_data == 2) {
			rx_state++;
			rx_timeout = RX_TIMEOUT
					;
			rx_data_ptr = 0;
		} else {
			rx_state = 0;
		}
		break;

	case 1:
		payload_length = rx_data;
		rx_state++;
		rx_timeout = RX_TIMEOUT
				;
		break;

	case 2:
		rx_buffer[rx_data_ptr++] = rx_data;
		if (rx_data_ptr == payload_length) {
			rx_state++;
		}
		rx_timeout = RX_TIMEOUT
				;
		break;

	case 3:
		crc_high = rx_data;
		rx_state++;
		rx_timeout = RX_TIMEOUT
				;
		break;

	case 4:
		crc_low = rx_data;
		rx_state++;
		rx_timeout = RX_TIMEOUT
				;
		break;

	case 5:
		if (rx_data == 3) {
			if (crc16(rx_buffer, payload_length)
					== ((unsigned short) crc_high << 8
							| (unsigned short) crc_low)) {
				// Packet received!
				if (process_func) {
					process_func(rx_buffer, payload_length);
				}
			}
		}

		rx_state = 0;
		break;

	default:
		rx_state = 0;
		break;
	}
}
