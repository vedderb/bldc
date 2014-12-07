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
 * comm_can.h
 *
 *  Created on: 7 dec 2014
 *      Author: benjamin
 */

#ifndef COMM_CAN_H_
#define COMM_CAN_H_

#include "conf_general.h"

// Functions
void comm_can_init(void);
void comm_can_transmit(uint32_t id, uint8_t *data, uint8_t len);
void comm_can_set_duty(uint8_t controller_id, float duty);
void comm_can_set_current(uint8_t controller_id, float current);
void comm_can_set_current_brake(uint8_t controller_id, float current);
void comm_can_set_rpm(uint8_t controller_id, float rpm);

#endif /* COMM_CAN_H_ */
