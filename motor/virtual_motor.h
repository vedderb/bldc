/*
	Copyright 2019 Maximiliano Cordoba	mcordoba@powerdesigns.ca

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


#ifndef VIRTUAL_MOTOR_H_
#define VIRTUAL_MOTOR_H_

#include "datatypes.h"

void virtual_motor_init(volatile mc_configuration *conf);
void virtual_motor_set_configuration(volatile mc_configuration *conf);
void virtual_motor_int_handler(float v_alpha, float v_beta);
bool virtual_motor_is_connected(void);
float virtual_motor_get_angle_deg(void);
#endif /* VIRTUAL_MOTOR_H_ */
