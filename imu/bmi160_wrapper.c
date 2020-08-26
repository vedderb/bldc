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

#include "bmi160_wrapper.h"

#include <stdio.h>
#include <string.h>

// Threads
static THD_FUNCTION(bmi_thread, arg);

// Private functions
static bool reset_init_bmi(BMI_STATE *s);
void user_delay_ms(uint32_t ms);

void bmi160_wrapper_init(BMI_STATE *s, stkalign_t *work_area, size_t work_area_size) {
	s->read_callback = 0;

	if (reset_init_bmi(s)) {
		s->should_stop = false;
		chThdCreateStatic(work_area, work_area_size, NORMALPRIO, bmi_thread, s);
	}
}

void bmi160_wrapper_set_read_callback(BMI_STATE *s, void(*func)(float *accel, float *gyro, float *mag)) {
	s->read_callback = func;
}

void bmi160_wrapper_stop(BMI_STATE *s) {
	s->should_stop = true;
	while(s->is_running) {
		chThdSleep(1);
	}
}

static bool reset_init_bmi(BMI_STATE *s) {
	s->sensor.delay_ms = user_delay_ms;

	bmi160_init(&(s->sensor));

	s->sensor.accel_cfg.range = BMI160_ACCEL_RANGE_16G;
	s->sensor.accel_cfg.bw = BMI160_ACCEL_BW_NORMAL_AVG4;
	s->sensor.accel_cfg.power = BMI160_ACCEL_NORMAL_MODE;

	s->sensor.gyro_cfg.range = BMI160_GYRO_RANGE_2000_DPS;
	s->sensor.gyro_cfg.bw = BMI160_GYRO_BW_NORMAL_MODE;
	s->sensor.gyro_cfg.power = BMI160_GYRO_NORMAL_MODE;

	if(s->rate_hz <= 25){
		s->sensor.accel_cfg.odr = BMI160_ACCEL_ODR_25HZ;
		s->sensor.gyro_cfg.odr = BMI160_GYRO_ODR_25HZ;
	}else if(s->rate_hz <= 50){
		s->sensor.accel_cfg.odr = BMI160_ACCEL_ODR_50HZ;
		s->sensor.gyro_cfg.odr = BMI160_GYRO_ODR_50HZ;
	}else if(s->rate_hz <= 100){
		s->sensor.accel_cfg.odr = BMI160_ACCEL_ODR_100HZ;
		s->sensor.gyro_cfg.odr = BMI160_GYRO_ODR_100HZ;
	}else if(s->rate_hz <= 200){
		s->sensor.accel_cfg.odr = BMI160_ACCEL_ODR_200HZ;
		s->sensor.gyro_cfg.odr = BMI160_GYRO_ODR_200HZ;
	}else if(s->rate_hz <= 400){
		s->sensor.accel_cfg.odr = BMI160_ACCEL_ODR_400HZ;
		s->sensor.gyro_cfg.odr = BMI160_GYRO_ODR_400HZ;
	}else if(s->rate_hz <= 800){
		s->sensor.accel_cfg.odr = BMI160_ACCEL_ODR_800HZ;
		s->sensor.gyro_cfg.odr = BMI160_GYRO_ODR_800HZ;
	}else{
		s->sensor.accel_cfg.odr = BMI160_ACCEL_ODR_1600HZ;
		s->sensor.gyro_cfg.odr = BMI160_GYRO_ODR_1600HZ;
	}

	int8_t res = bmi160_set_sens_conf(&(s->sensor));

	return res == BMI160_OK;
}

void user_delay_ms(uint32_t ms) {
	chThdSleepMilliseconds(ms);
}

static THD_FUNCTION(bmi_thread, arg) {
	BMI_STATE *s = (BMI_STATE*)arg;

	chRegSetThreadName("BMI Sampling");

	s->is_running = true;

	for(;;) {
		struct bmi160_sensor_data accel;
		struct bmi160_sensor_data gyro;

		int8_t res = bmi160_get_sensor_data((BMI160_ACCEL_SEL | BMI160_GYRO_SEL),
				&accel, &gyro, &(s->sensor));

		if (res != BMI160_OK) {
			chThdSleepMilliseconds(5);
			continue;
		}

		float tmp_accel[3], tmp_gyro[3], tmp_mag[3];

		tmp_accel[0] = (float)accel.x * 16.0 / 32768.0;
		tmp_accel[1] = (float)accel.y * 16.0 / 32768.0;
		tmp_accel[2] = (float)accel.z * 16.0 / 32768.0;

		tmp_gyro[0] = (float)gyro.x * 2000.0 / 32768.0;
		tmp_gyro[1] = (float)gyro.y * 2000.0 / 32768.0;
		tmp_gyro[2] = (float)gyro.z * 2000.0 / 32768.0;

		memset(tmp_mag, 0, sizeof(tmp_mag));

		if (s->read_callback) {
			s->read_callback(tmp_accel, tmp_gyro, tmp_mag);
		}

		if (s->should_stop) {
			s->is_running = false;
			return;
		}

		chThdSleepMicroseconds(1000000 / s->rate_hz);
	}
}
