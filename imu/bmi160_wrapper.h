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

#ifndef IMU_BMI160_WRAPPER_H_
#define IMU_BMI160_WRAPPER_H_

#include "ch.h"
#include "hal.h"

#include <stdint.h>
#include <stdbool.h>

#include "i2c_bb.h"
#include "spi_bb.h"
#include "bmi160.h"

typedef struct {
	void(*read_callback)(float *accel, float *gyro, float *mag);
	struct bmi160_dev sensor;
	volatile bool is_running;
	volatile bool should_stop;
	int rate_hz;
} BMI_STATE;

void bmi160_wrapper_init(BMI_STATE *s, stkalign_t *work_area, size_t work_area_size);
void bmi160_wrapper_set_read_callback(BMI_STATE *s, void(*func)(float *accel, float *gyro, float *mag));
void bmi160_wrapper_stop(BMI_STATE *s);

#endif /* IMU_BMI160_WRAPPER_H_ */
