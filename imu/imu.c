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

#include "imu.h"
#include "hw.h"
#include "mpu9150.h"
#include "ahrs.h"
#include "timer.h"
#include "terminal.h"
#include "commands.h"
#include "icm20948.h"

#include <math.h>
#include <string.h>

// Private variables
static ATTITUDE_INFO m_att;
static float m_accel[3], m_gyro[3], m_mag[3];
static stkalign_t m_thd_work_area[THD_WORKING_AREA_SIZE(2048) / sizeof(stkalign_t)];
static i2c_bb_state m_i2c_bb;
static ICM20948_STATE m_icm20948_state;

// Private functions
static void imu_read_callback(float *accel, float *gyro, float *mag);
static void terminal_rpy(int argc, const char **argv);

void imu_init(void) {
	ahrs_init_attitude_info(&m_att);

#ifdef MPU9X50_SDA_GPIO
	imu_init_mpu9x50(MPU9X50_SDA_GPIO, MPU9X50_SDA_PIN,
			MPU9X50_SCL_GPIO, MPU9X50_SCL_PIN);
#endif

#ifdef ICM20948_SDA_GPIO
	imu_init_icm20948(ICM20948_SDA_GPIO, ICM20948_SDA_PIN,
			ICM20948_SCL_GPIO, ICM20948_SCL_PIN, ICM20948_AD0_VAL);
#endif

	terminal_register_command_callback(
			"imu_rpy",
			"Print 100 roll/pitch/yaw samples at 10 Hz",
			0,
			terminal_rpy);
}

i2c_bb_state *imu_get_i2c(void) {
	return &m_i2c_bb;
}

void imu_init_mpu9x50(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin) {

	mpu9150_init(sda_gpio, sda_pin,
			scl_gpio, scl_pin,
			m_thd_work_area, sizeof(m_thd_work_area));
	mpu9150_set_read_callback(imu_read_callback);
}

void imu_init_icm20948(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin, int ad0_val) {

	m_i2c_bb.sda_gpio = sda_gpio;
	m_i2c_bb.sda_pin = sda_pin;
	m_i2c_bb.scl_gpio = scl_gpio;
	m_i2c_bb.scl_pin = scl_pin;
	i2c_bb_init(&m_i2c_bb);

	icm20948_init(&m_icm20948_state,
			&m_i2c_bb, ad0_val,
			m_thd_work_area, sizeof(m_thd_work_area));
	icm20948_set_read_callback(&m_icm20948_state, imu_read_callback);
}

float imu_get_roll(void) {
	return ahrs_get_roll(&m_att);
}

float imu_get_pitch(void) {
	return ahrs_get_pitch(&m_att);
}

float imu_get_yaw(void) {
	return ahrs_get_yaw(&m_att);
}

void imu_get_rpy(float *rpy) {
	ahrs_get_roll_pitch_yaw(rpy, &m_att);
}

void imu_get_accel(float *accel) {
	memcpy(accel, m_accel, sizeof(m_accel));
}

void imu_get_gyro(float *gyro) {
	memcpy(gyro, m_gyro, sizeof(m_gyro));
}

void imu_get_mag(float *mag) {
	memcpy(mag, m_mag, sizeof(m_mag));
}

void imu_get_accel_derotated(float *accel) {
	float rpy[3];
	imu_get_rpy(rpy);

	const float ax = m_accel[0];
	const float ay = m_accel[1];
	const float az = m_accel[2];

	const float sr = sinf(rpy[0]);
	const float cr = -cosf(rpy[0]);
	const float sp = sinf(rpy[1]);
	const float cp = -cosf(rpy[1]);
	const float sy = sinf(rpy[2]);
	const float cy = cosf(rpy[2]);

	float c_ax = ax * cp + ay * sp * sr + az * sp * cr;
	float c_ay = ay * cr - az * sr;
	float c_az = -ax * sp + ay * cp * sr + az * cp * cr;
	float c_ax2 = cy * c_ax + sy * c_ay;
	float c_ay2 = sy * c_ax - cy * c_ay;

	accel[0] = c_ax2;
	accel[1] = c_ay2;
	accel[2] = c_az;
}

void imu_get_quaternions(float *q) {
	q[0] = m_att.q0;
	q[1] = m_att.q1;
	q[2] = m_att.q2;
	q[3] = m_att.q3;
}

static void imu_read_callback(float *accel, float *gyro, float *mag) {
	static uint32_t last_time = 0;
	float dt = timer_seconds_elapsed_since(last_time);
	last_time = timer_time_now();

#ifdef IMU_FLIP
	m_accel[0] = -accel[0];
	m_accel[1] = accel[1];
	m_accel[2] = -accel[2];

	m_gyro[0] = -gyro[0];
	m_gyro[1] = gyro[1];
	m_gyro[2] = -gyro[2];

	m_mag[0] = -mag[0];
	m_mag[1] = mag[1];
	m_mag[2] = -mag[2];
#else
	m_accel[0] = accel[0];
	m_accel[1] = accel[1];
	m_accel[2] = accel[2];

	m_gyro[0] = gyro[0];
	m_gyro[1] = gyro[1];
	m_gyro[2] = gyro[2];

	m_mag[0] = mag[0];
	m_mag[1] = mag[1];
	m_mag[2] = mag[2];
#endif

	float gyro_rad[3];
	gyro_rad[0] = m_gyro[0] * M_PI / 180.0;
	gyro_rad[1] = m_gyro[1] * M_PI / 180.0;
	gyro_rad[2] = m_gyro[2] * M_PI / 180.0;

	ahrs_update_madgwick_imu(gyro_rad, m_accel, dt, (ATTITUDE_INFO*)&m_att);
}

static void terminal_rpy(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	for (int i = 0;i < 100;i++) {
		commands_printf("R: %.2f P: %.2f Y: %.2f",
				(double)(imu_get_roll() * 180.0 / M_PI),
				(double)(imu_get_pitch() * 180.0 / M_PI),
				(double)(imu_get_yaw() * 180.0 / M_PI));

		chThdSleepMilliseconds(100);
	}

	commands_printf(" ");
}
