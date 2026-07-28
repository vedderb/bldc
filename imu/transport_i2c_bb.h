/*
	Copyright 2026 Lukas Hrazky

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

#ifndef IMU_TRANSPORT_I2C_BB_H_
#define IMU_TRANSPORT_I2C_BB_H_

#include "transport.h"

// Initialise t as a bit-bang I2C transport on the given pins. bus_hz is the
// desired SCL clock, mapped to the nearest bit-bang rate bucket (0 = 400 kHz
// default).
void transport_i2c_bb_init(transport_t *t, stm32_gpio_t *sda_gpio, uint8_t sda_pin,
		stm32_gpio_t *scl_gpio, uint8_t scl_pin, uint32_t bus_hz);

#endif /* IMU_TRANSPORT_I2C_BB_H_ */
