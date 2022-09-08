/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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

#ifndef LIBCANARD_CANARD_DRIVER_H_
#define LIBCANARD_CANARD_DRIVER_H_

#include "ch.h"
#include "hal.h"

typedef struct {
	float age;
	float value;
} uavcan_cmd_info;

void canard_driver_init(void);
uavcan_cmd_info canard_driver_last_rawcmd(int can_if);
uavcan_cmd_info canard_driver_last_rpmcmd(int can_if);

#endif /* LIBCANARD_CANARD_DRIVER_H_ */
