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

#ifndef PACKET_H_
#define PACKET_H_

#include <stdint.h>

// Settings
#define PACKET_RX_TIMEOUT		1000
#define PACKET_HANDLERS			2
#define PACKET_MAX_PL_LEN		1024

// Functions
void packet_init(void (*s_func)(unsigned char *data, unsigned int len),
		void (*p_func)(unsigned char *data, unsigned int len), int handler_num);
void packet_process_byte(uint8_t rx_data, int handler_num);
void packet_timerfunc(void);
void packet_send_packet(unsigned char *data, unsigned int len, int handler_num);

#endif /* PACKET_H_ */
