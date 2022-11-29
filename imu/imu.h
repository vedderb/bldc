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

#ifndef IMU_IMU_H_
#define IMU_IMU_H_

#include "ch.h"
#include "hal.h"
#include "i2c_bb.h"
#include "spi_bb.h"

void imu_init(imu_config *set);
void imu_reset_orientation(void);
i2c_bb_state *imu_get_i2c(void);
void imu_init_mpu9x50(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin);
void imu_init_icm20948(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin, int ad0_val);
void imu_init_bmi160_i2c(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin);
void imu_init_lsm6ds3(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin);
void imu_init_bmi160_spi(
        stm32_gpio_t *nss_gpio, int nss_pin,
        stm32_gpio_t *sck_gpio, int sck_pin,
        stm32_gpio_t *mosi_gpio, int mosi_pin,
        stm32_gpio_t *miso_gpio, int miso_pin);
void imu_stop(void);
bool imu_startup_done(void);
float imu_get_roll(void);
float imu_get_pitch(void);
float imu_get_yaw(void);
void imu_get_rpy(float *rpy);
void imu_get_accel(float *accel);
void imu_get_gyro(float *gyro);
void imu_get_mag(float *mag);
void imu_derotate(float *input, float *output);
void imu_get_accel_derotated(float *accel);
void imu_get_gyro_derotated(float *gyro);
void imu_get_quaternions(float *q);
void imu_get_calibration(float yaw, float * imu_cal);
void imu_set_yaw(float yaw_deg);
void imu_set_read_callback(void (*func)(float *acc, float *gyro, float *mag, float dt));

#endif /* IMU_IMU_H_ */
