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
 * comm.h
 *
 *  Created on: 22 nov 2012
 *      Author: benjamin
 */

#ifndef COMM_H_
#define COMM_H_

#include <stdint.h>

// Packets that expect response
typedef enum {
	COMM_READ_VALUES = 0,
	COMM_PRINT,
	COMM_SEND_SAMPLES,
	COMM_ROTOR_POSITION
} COMM_RES_PACKET_ID;

// Packets that don't expect any response
typedef enum {
	COMM_FULL_BRAKE = 0,
	COMM_SERVO_OFFSET,
	COMM_CAN_TEST,
	COMM_TERMINAL_CMD
} COMM_NORES_PACKET_ID;

// Functions
void comm_init(void);
void comm_print(char* str);
void comm_send_samples(uint8_t *data, int len);
void comm_send_rotor_pos(float rotor_pos);

#endif /* COMM_INTERFACE_H_ */
