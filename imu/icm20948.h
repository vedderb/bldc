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

#ifndef IMU_ICM20948_H_
#define IMU_ICM20948_H_

#include "device.h"

imu_device_t icm20948_device(transport_t *transport);

// All banks
#define ICM20948_BANK_SEL						0x7F

// Bank 0 registers
#define ICM20948_PWR_MGMT_1						0x06
#define ICM20948_PIN_CFG						0x0F
#define ICM20948_ACCEL_XOUT_H					0x2D

// Bank 2 registers
#define ICM20948_ACCEL_CONFIG					0x14
#define ICM20948_GYRO_CONFIG_1					0x01

#endif /* IMU_ICM20948_H_ */
