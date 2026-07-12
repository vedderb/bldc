/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se
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

#ifndef IMU_BMI160_WRAPPER_H_
#define IMU_BMI160_WRAPPER_H_

#include "device.h"
#include "bmi160.h"

// interface is BMI160_I2C_INTF or BMI160_SPI_INTF.
imu_device_t bmi160_device(transport_t *transport, uint8_t interface);

#endif /* IMU_BMI160_WRAPPER_H_ */
