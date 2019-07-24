/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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
#include <stdbool.h>

// Settings
#ifndef PACKET_RX_TIMEOUT
#define PACKET_RX_TIMEOUT		1000
#endif

#ifndef PACKET_HANDLERS
#define PACKET_HANDLERS			3
#endif

#ifndef PACKET_MAX_PL_LEN
#define PACKET_MAX_PL_LEN		512
#endif

// Functions
void packet_init(void (*s_func)(unsigned char *data, unsigned int len),
		void (*p_func)(unsigned char *data, unsigned int len), int handler_num);
void packet_reset(int handler_num);
void packet_process_byte(uint8_t rx_data, int handler_num);
void packet_timerfunc(void);
void packet_send_packet(unsigned char *data, unsigned int len, int handler_num);

#endif /* PACKET_H_ */
