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
#define PACKET_HANDLERS			2

typedef struct {
	volatile unsigned char rx_state;
	volatile unsigned char rx_timeout;
	void(*send_func)(unsigned char *data, unsigned char len);
	void(*process_func)(unsigned char *data, unsigned char len);
	unsigned char payload_length;
	unsigned char rx_buffer[256];
	unsigned char rx_data_ptr;
	unsigned char crc_low;
	unsigned char crc_high;
} PACKET_STATE_t;

static PACKET_STATE_t handler_states[PACKET_HANDLERS];

void packet_init(void (*s_func)(unsigned char *data, unsigned char len),
		void (*p_func)(unsigned char *data, unsigned char len), int handler_num) {
	handler_states[handler_num].send_func = s_func;
	handler_states[handler_num].process_func = p_func;
}

void packet_send_packet(unsigned char *data, unsigned char len, int handler_num) {
	uint8_t data_buffer[256];
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

	if (handler_states[handler_num].send_func) {
		handler_states[handler_num].send_func(data_buffer, len + 5);
	}
}

/**
 * Call this function every millisecond.
 */
void packet_timerfunc(void) {
	for (int i = 0;i < PACKET_HANDLERS;i++) {
		if (handler_states[i].rx_timeout) {
			handler_states[i].rx_timeout--;
		} else {
			handler_states[i].rx_state = 0;
		}
	}
}

void packet_process_byte(uint8_t rx_data, int handler_num) {
	switch (handler_states[handler_num].rx_state) {
	case 0:
		if (rx_data == 2) {
			handler_states[handler_num].rx_state++;
			handler_states[handler_num].rx_timeout = RX_TIMEOUT;
			handler_states[handler_num].rx_data_ptr = 0;
		} else {
			handler_states[handler_num].rx_state = 0;
		}
		break;

	case 1:
		handler_states[handler_num].payload_length = rx_data;
		handler_states[handler_num].rx_state++;
		handler_states[handler_num].rx_timeout = RX_TIMEOUT;
		break;

	case 2:
		handler_states[handler_num].rx_buffer[handler_states[handler_num].rx_data_ptr++] = rx_data;
		if (handler_states[handler_num].rx_data_ptr == handler_states[handler_num].payload_length) {
			handler_states[handler_num].rx_state++;
		}
		handler_states[handler_num].rx_timeout = RX_TIMEOUT;
		break;

	case 3:
		handler_states[handler_num].crc_high = rx_data;
		handler_states[handler_num].rx_state++;
		handler_states[handler_num].rx_timeout = RX_TIMEOUT;
		break;

	case 4:
		handler_states[handler_num].crc_low = rx_data;
		handler_states[handler_num].rx_state++;
		handler_states[handler_num].rx_timeout = RX_TIMEOUT;
		break;

	case 5:
		if (rx_data == 3) {
			if (crc16(handler_states[handler_num].rx_buffer, handler_states[handler_num].payload_length)
					== ((unsigned short)handler_states[handler_num].crc_high << 8
							| (unsigned short)handler_states[handler_num].crc_low)) {
				// Packet received!
				if (handler_states[handler_num].process_func) {
					handler_states[handler_num].process_func(handler_states[handler_num].rx_buffer,
							handler_states[handler_num].payload_length);
				}
			}
		}
		handler_states[handler_num].rx_state = 0;
		break;

	default:
		handler_states[handler_num].rx_state = 0;
		break;
	}
}
