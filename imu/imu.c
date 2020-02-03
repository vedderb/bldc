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
#include "bmi160_wrapper.h"
#include "utils.h"

#include <math.h>
#include <string.h>

// Private variables
static ATTITUDE_INFO m_att;
static float m_accel[3], m_gyro[3], m_mag[3];
static stkalign_t m_thd_work_area[THD_WORKING_AREA_SIZE(2048) / sizeof(stkalign_t)];
static i2c_bb_state m_i2c_bb;
static ICM20948_STATE m_icm20948_state;
static BMI_STATE m_bmi_state;
static imu_config m_settings;
static float m_gyro_offset[3] = {0.0};
static systime_t init_time;
static bool imu_ready;

// Private functions
static void imu_read_callback(float *accel, float *gyro, float *mag);
static void terminal_gyro_info(int argc, const char **argv);
int8_t user_i2c_read(uint8_t dev_addr, uint8_t reg_addr, uint8_t *data, uint16_t len);
int8_t user_i2c_write(uint8_t dev_addr, uint8_t reg_addr, uint8_t *data, uint16_t len);

void imu_init(imu_config *set) {
	m_settings = *set;
	memset(m_gyro_offset, 0, sizeof(m_gyro_offset));

	imu_stop();

	imu_ready = false;
	init_time = chVTGetSystemTimeX();
	ahrs_update_all_parameters(1.0, 10.0, 0.0, 2.0);

	ahrs_init_attitude_info(&m_att);

	mpu9150_set_rate_hz(set->sample_rate_hz);
	m_icm20948_state.rate_hz = set->sample_rate_hz;
	m_bmi_state.rate_hz = set->sample_rate_hz;

	if (set->type == IMU_TYPE_INTERNAL) {
#ifdef MPU9X50_SDA_GPIO
		imu_init_mpu9x50(MPU9X50_SDA_GPIO, MPU9X50_SDA_PIN,
				MPU9X50_SCL_GPIO, MPU9X50_SCL_PIN);
#endif

#ifdef ICM20948_SDA_GPIO
		imu_init_icm20948(ICM20948_SDA_GPIO, ICM20948_SDA_PIN,
				ICM20948_SCL_GPIO, ICM20948_SCL_PIN, ICM20948_AD0_VAL);
#endif

#ifdef BMI160_SDA_GPIO
		imu_init_bmi160(BMI160_SDA_GPIO, BMI160_SDA_PIN,
				BMI160_SCL_GPIO, BMI160_SCL_PIN);
#endif
	} else if (set->type == IMU_TYPE_EXTERNAL_MPU9X50) {
		imu_init_mpu9x50(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
	} else if (set->type == IMU_TYPE_EXTERNAL_ICM20948) {
		imu_init_icm20948(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN, 0);
	} else if (set->type == IMU_TYPE_EXTERNAL_BMI160) {
		imu_init_bmi160(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
	}

	terminal_register_command_callback(
			"imu_gyro_info",
			"Print gyro offsets",
			0,
			terminal_gyro_info);
}

i2c_bb_state *imu_get_i2c(void) {
	return &m_i2c_bb;
}

void imu_init_mpu9x50(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin) {
	imu_stop();

	mpu9150_init(sda_gpio, sda_pin,
			scl_gpio, scl_pin,
			m_thd_work_area, sizeof(m_thd_work_area));
	mpu9150_set_read_callback(imu_read_callback);
}

void imu_init_icm20948(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin, int ad0_val) {
	imu_stop();

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

void imu_init_bmi160(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin) {
	imu_stop();

	m_i2c_bb.sda_gpio = sda_gpio;
	m_i2c_bb.sda_pin = sda_pin;
	m_i2c_bb.scl_gpio = scl_gpio;
	m_i2c_bb.scl_pin = scl_pin;
	i2c_bb_init(&m_i2c_bb);

	m_bmi_state.sensor.id = BMI160_I2C_ADDR;
	m_bmi_state.sensor.interface = BMI160_I2C_INTF;
	m_bmi_state.sensor.read = user_i2c_read;
	m_bmi_state.sensor.write = user_i2c_write;

	bmi160_wrapper_init(&m_bmi_state, m_thd_work_area, sizeof(m_thd_work_area));
	bmi160_wrapper_set_read_callback(&m_bmi_state, imu_read_callback);
}

void imu_stop(void) {
	mpu9150_stop();
	icm20948_stop(&m_icm20948_state);
	bmi160_wrapper_stop(&m_bmi_state);
}

bool imu_startup_done(void) {
	return imu_ready;
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

	if(!imu_ready && ST2MS(chVTGetSystemTimeX() - init_time) > 1000){
		ahrs_update_all_parameters(
				m_settings.accel_confidence_decay,
				m_settings.mahony_kp,
				m_settings.mahony_ki,
				m_settings.madgwick_beta);
		imu_ready = true;
	}

#ifdef IMU_FLIP
	accel[0] *= -1.0;
	accel[2] *= -1.0;
	gyro[0] *= -1.0;
	gyro[2] *= -1.0;
	mag[0] *= -1.0;
	mag[2] *= -1.0;
#endif

	// Rotate axes (ZYX)

	float s1 = sinf(m_settings.rot_yaw * M_PI / 180.0);
	float c1 = cosf(m_settings.rot_yaw * M_PI / 180.0);
	float s2 = sinf(m_settings.rot_pitch * M_PI / 180.0);
	float c2 = cosf(m_settings.rot_pitch * M_PI / 180.0);
	float s3 = sinf(m_settings.rot_roll * M_PI / 180.0);
	float c3 = cosf(m_settings.rot_roll * M_PI / 180.0);

	float m11 = c1 * c2;	float m12 = c1 * s2 * s3 - c3 * s1;	float m13 = s1 * s3 + c1 * c3 * s2;
	float m21 = c2 * s1;	float m22 = c1 * c3 + s1 * s2 * s3;	float m23 = c3 * s1 * s2 - c1 * s3;
	float m31 = -s2; 		float m32 = c2 * s3;				float m33 = c2 * c3;

	m_accel[0] = accel[0] * m11 + accel[1] * m12 + accel[2] * m13;
	m_accel[1] = accel[0] * m21 + accel[1] * m22 + accel[2] * m23;
	m_accel[2] = accel[0] * m31 + accel[1] * m32 + accel[2] * m33;

	m_gyro[0] = gyro[0] * m11 + gyro[1] * m12 + gyro[2] * m13;
	m_gyro[1] = gyro[0] * m21 + gyro[1] * m22 + gyro[2] * m23;
	m_gyro[2] = gyro[0] * m31 + gyro[1] * m32 + gyro[2] * m33;

	m_mag[0] = mag[0] * m11 + mag[1] * m12 + mag[2] * m13;
	m_mag[1] = mag[0] * m21 + mag[1] * m22 + mag[2] * m23;
	m_mag[2] = mag[0] * m31 + mag[1] * m32 + mag[2] * m33;

	// Accelerometer and Gyro offset compensation and estimation
	for (int i = 0;i < 3;i++) {
		m_accel[i] -= m_settings.accel_offsets[i];
		m_gyro[i] -= m_settings.gyro_offsets[i];

		if (m_settings.gyro_offset_comp_fact[i] > 0.0) {
			utils_step_towards(&m_gyro_offset[i], m_gyro[i], m_settings.gyro_offset_comp_fact[i] * dt);
			utils_truncate_number_abs(&m_gyro_offset[i], m_settings.gyro_offset_comp_clamp);
		} else {
			m_gyro_offset[i] = 0.0;
		}

		m_gyro[i] -= m_gyro_offset[i];
	}

	float gyro_rad[3];
	gyro_rad[0] = m_gyro[0] * M_PI / 180.0;
	gyro_rad[1] = m_gyro[1] * M_PI / 180.0;
	gyro_rad[2] = m_gyro[2] * M_PI / 180.0;

	switch (m_settings.mode){
		case (AHRS_MODE_MADGWICK):
			ahrs_update_madgwick_imu(gyro_rad, m_accel, dt, (ATTITUDE_INFO*)&m_att);
			break;
		case (AHRS_MODE_MAHONY):
			ahrs_update_mahony_imu(gyro_rad, m_accel, dt, (ATTITUDE_INFO*)&m_att);
			break;
	}
}

static void terminal_gyro_info(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	commands_printf("Gyro offsets: [%.3f %.3f %.3f]\n",
			(double)(m_settings.gyro_offsets[0] + m_gyro_offset[0]),
			(double)(m_settings.gyro_offsets[1] + m_gyro_offset[1]),
			(double)(m_settings.gyro_offsets[2] + m_gyro_offset[2]));
}

int8_t user_i2c_read(uint8_t dev_addr, uint8_t reg_addr, uint8_t *data, uint16_t len) {
	m_i2c_bb.has_error = 0;

	uint8_t txbuf[1];
	txbuf[0] = reg_addr;
	return i2c_bb_tx_rx(&m_i2c_bb, dev_addr, txbuf, 1, data, len) ? BMI160_OK : BMI160_E_COM_FAIL;
}

int8_t user_i2c_write(uint8_t dev_addr, uint8_t reg_addr, uint8_t *data, uint16_t len) {
	m_i2c_bb.has_error = 0;

	uint8_t txbuf[len + 1];
	txbuf[0] = reg_addr;
	memcpy(txbuf + 1, data, len);
	return i2c_bb_tx_rx(&m_i2c_bb, dev_addr, txbuf, len + 1, 0, 0) ? BMI160_OK : BMI160_E_COM_FAIL;
}
