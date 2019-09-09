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

#ifndef IMU_ICM20948_H_
#define IMU_ICM20948_H_

#include "ch.h"
#include "hal.h"

#include <stdint.h>
#include <stdbool.h>

#include "i2c_bb.h"

typedef struct {
	i2c_bb_state *i2cs;
	uint8_t i2c_address;
	void(*read_callback)(float *accel, float *gyro, float *mag);
	volatile bool is_running;
	volatile bool should_stop;
	int rate_hz;
} ICM20948_STATE;

void icm20948_init(ICM20948_STATE *s, i2c_bb_state *i2c_state, int ad0_val,
		stkalign_t *work_area, size_t work_area_size);
void icm20948_set_read_callback(ICM20948_STATE *s, void(*func)(float *accel, float *gyro, float *mag));
void icm20948_stop(ICM20948_STATE *s);

// All banks
#define ICM20948_BANK_SEL						0x7F

// Bank 0 registers
#define ICM20948_PWR_MGMT_1						0x06
#define ICM20948_PIN_CFG						0x0F
#define ICM20948_ACCEL_XOUT_H					0x2D

// Bank 2 registers
#define ICM20948_ACCEL_CONFIG					0x14
#define ICM20948_GYRO_CONFIG_1					0x01

#endif /* IMU_ICM20948_H_ */
