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
#include "lsm6ds3.h"
#include "transport_i2c_bb.h"
#include "transport_spi_bb.h"
#include "transport_spi_hw.h"
#include "imu_thread.h"
#include "utils_math.h"
#include "Fusion.h"
#include "digital_filter.h"

#include <math.h>
#include <string.h>

// Private variables
static ATTITUDE_INFO m_att;
static FusionAhrs m_fusionAhrs;
static float m_accel[3], m_gyro[3], m_mag[3];
static transport_t m_transport;
static imu_device_t m_dev;
static imu_config m_settings;
static systime_t init_time;
static bool imu_ready;
static Biquad acc_x_biquad, acc_y_biquad, acc_z_biquad, gyro_x_biquad, gyro_y_biquad, gyro_z_biquad;
static char *m_imu_type_internal = "Unknown";

// Private functions
static void imu_read_callback(float *accel, float *gyro, float *mag);
static void terminal_imu_type_internal(int argc, const char **argv);

// Function pointers
static void (*m_read_callback)(float *acc, float *gyro, float *mag, float dt) = NULL;

void imu_init(imu_config *set) {
	bool imu_changed = set->sample_rate_hz != m_settings.sample_rate_hz ||
			set->type != m_settings.type || set->filter != m_settings.filter;

	m_settings = *set;

	//Biquad filters
	float fc;
	if(m_settings.accel_lowpass_filter_x > 0){
		fc = m_settings.accel_lowpass_filter_x / m_settings.sample_rate_hz;
		biquad_config(&acc_x_biquad, BQ_LOWPASS, fc);
	}
	if(m_settings.accel_lowpass_filter_y > 0){
		fc = m_settings.accel_lowpass_filter_y / m_settings.sample_rate_hz;
		biquad_config(&acc_y_biquad, BQ_LOWPASS, fc);
	}
	if(m_settings.accel_lowpass_filter_z > 0){
		fc = m_settings.accel_lowpass_filter_z / m_settings.sample_rate_hz;
		biquad_config(&acc_z_biquad, BQ_LOWPASS, fc);
	}
	if(m_settings.gyro_lowpass_filter > 0){
		fc = m_settings.gyro_lowpass_filter / m_settings.sample_rate_hz;
		biquad_config(&gyro_x_biquad, BQ_LOWPASS, fc);
		biquad_config(&gyro_y_biquad, BQ_LOWPASS, fc);
		biquad_config(&gyro_z_biquad, BQ_LOWPASS, fc);
	}

	if (!imu_changed) {
		return;
	}

	imu_stop();
	imu_reset_orientation();

	if (set->type == IMU_TYPE_INTERNAL) {
#ifdef MPU9X50_SDA_GPIO
		imu_init_mpu9x50(MPU9X50_SDA_GPIO, MPU9X50_SDA_PIN,
				MPU9X50_SCL_GPIO, MPU9X50_SCL_PIN);
		m_imu_type_internal = "MPU9X50";
#endif

#ifdef ICM20948_SDA_GPIO
		imu_init_icm20948(ICM20948_SDA_GPIO, ICM20948_SDA_PIN,
				ICM20948_SCL_GPIO, ICM20948_SCL_PIN);
		m_imu_type_internal = "ICM20948";
#endif

#ifdef BMI160_SDA_GPIO
		imu_init_bmi160_i2c(BMI160_SDA_GPIO, BMI160_SDA_PIN,
				BMI160_SCL_GPIO, BMI160_SCL_PIN);
		m_imu_type_internal = "BMI160";
#endif

#if defined(LSM6DS3_SDA_GPIO) && !defined(LSM6DS3_USE_SPI)
		imu_init_lsm6ds3(LSM6DS3_SDA_GPIO, LSM6DS3_SDA_PIN,
				LSM6DS3_SCL_GPIO, LSM6DS3_SCL_PIN);
		m_imu_type_internal = "LSM6DS3_I2C";
#endif

#ifdef LSM6DS3_USE_SPI
#ifdef LSM6DS3_NSS_GPIO
		if (imu_init_lsm6ds3_spi(
				LSM6DS3_NSS_GPIO, LSM6DS3_NSS_PIN,
				LSM6DS3_SCK_GPIO, LSM6DS3_SCK_PIN,
				LSM6DS3_MOSI_GPIO, LSM6DS3_MOSI_PIN,
				LSM6DS3_MISO_GPIO, LSM6DS3_MISO_PIN)) {
#ifdef LSM6DS3_HWSPI_DEV
			m_imu_type_internal = "LSM6DS3_SPI_HW";
#else
			m_imu_type_internal = "LSM6DS3_SPI";
#endif
		} else {
			// I2C fallback
#if defined(LSM6DS3_SDA_GPIO)
			imu_init_lsm6ds3(LSM6DS3_SDA_GPIO, LSM6DS3_SDA_PIN,
					LSM6DS3_SCL_GPIO, LSM6DS3_SCL_PIN);
			m_imu_type_internal = "LSM6DS3_I2C";
#endif
		}
#endif
#else
#ifdef LSM6DS3_NSS_GPIO
		palSetPadMode(LSM6DS3_NSS_GPIO, LSM6DS3_NSS_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palSetPad(LSM6DS3_NSS_GPIO, LSM6DS3_NSS_PIN);
		palSetPadMode(LSM6DS3_MISO_GPIO, LSM6DS3_MISO_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palClearPad(LSM6DS3_MISO_GPIO, LSM6DS3_MISO_PIN);
		imu_init_lsm6ds3(LSM6DS3_MOSI_GPIO, LSM6DS3_MOSI_PIN,
				LSM6DS3_SCK_GPIO, LSM6DS3_SCK_PIN);
		m_imu_type_internal = "LSM6DS3";
#endif
#endif

#ifdef BMI160_SPI_PORT_NSS
		imu_init_bmi160_spi(
				BMI160_SPI_PORT_NSS, BMI160_SPI_PIN_NSS,
				BMI160_SPI_PORT_SCK, BMI160_SPI_PIN_SCK,
				BMI160_SPI_PORT_MOSI, BMI160_SPI_PIN_MOSI,
				BMI160_SPI_PORT_MISO, BMI160_SPI_PIN_MISO);
		m_imu_type_internal = "BMI160_SPI";
#endif
	} else if (set->type == IMU_TYPE_EXTERNAL_MPU9X50) {
		imu_init_mpu9x50(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
	} else if (set->type == IMU_TYPE_EXTERNAL_ICM20948) {
		imu_init_icm20948(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
	} else if (set->type == IMU_TYPE_EXTERNAL_BMI160) {
		imu_init_bmi160_i2c(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
	} else if(set->type == IMU_TYPE_EXTERNAL_LSM6DS3) {
		imu_init_lsm6ds3(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
	} else if (set->type == IMU_TYPE_EXTERNAL_BMI160) {
		imu_init_bmi160_i2c(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
	}

	terminal_register_command_callback(
			"imu_type_internal",
			"Print internal IMU type",
			0,
			terminal_imu_type_internal);
}

void imu_reset_orientation(void) {
	imu_ready = false;
	init_time = chVTGetSystemTimeX();
	ahrs_init_attitude_info(&m_att);
	FusionAhrsInitialise(&m_fusionAhrs, 10.0, 1.0);
	ahrs_update_all_parameters(&m_att, 1.0, 10.0, 0.0, 2.0);
}

i2c_bb_state *imu_get_i2c(void) {
	return &m_transport.bus.i2c_bb;
}

void imu_init_mpu9x50(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin) {
	imu_stop();

	transport_i2c_bb_init(&m_transport, sda_gpio, sda_pin, scl_gpio, scl_pin, 0);
	m_dev = mpu9150_device(&m_transport);
	imu_thread_set_device(&m_dev, MIN(m_settings.sample_rate_hz, transport_max_sample_rate(&m_transport)));
	if (m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer)) {
		imu_thread_start(imu_read_callback);
	}
}

void imu_init_icm20948(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin) {
	imu_stop();

	transport_i2c_bb_init(&m_transport, sda_gpio, sda_pin, scl_gpio, scl_pin, 0);
	m_dev = icm20948_device(&m_transport);
	imu_thread_set_device(&m_dev, MIN(m_settings.sample_rate_hz, transport_max_sample_rate(&m_transport)));
	if (m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer)) {
		imu_thread_start(imu_read_callback);
	}
}

void imu_init_bmi160_i2c(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin) {
	imu_stop();

	transport_i2c_bb_init(&m_transport, sda_gpio, sda_pin, scl_gpio, scl_pin, 0);

	m_dev = bmi160_device(&m_transport, BMI160_I2C_INTF);
	int rate_hz = MIN(m_settings.sample_rate_hz, transport_max_sample_rate(&m_transport));
	imu_thread_set_device(&m_dev, rate_hz);
	if (m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer)) {
		imu_thread_start(imu_read_callback);
	}
}

void imu_init_bmi160_spi(stm32_gpio_t *nss_gpio, int nss_pin,
		stm32_gpio_t *sck_gpio, int sck_pin, stm32_gpio_t *mosi_gpio, int mosi_pin,
		stm32_gpio_t *miso_gpio, int miso_pin) {
	imu_stop();

	transport_spi_bb_init(&m_transport, nss_gpio, nss_pin, sck_gpio, sck_pin,
			mosi_gpio, mosi_pin, miso_gpio, miso_pin);

	m_dev = bmi160_device(&m_transport, BMI160_SPI_INTF);
	int rate_hz = MIN(m_settings.sample_rate_hz, transport_max_sample_rate(&m_transport));
	imu_thread_set_device(&m_dev, rate_hz);
	if (m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer)) {
		imu_thread_start(imu_read_callback);
	}
}

void imu_init_lsm6ds3(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin) {

#ifdef LSM6DS3_SPEED_700KHZ
	uint32_t bus_hz = 700000;
	commands_printf("LSM6DS3 speed: 700 kHz");
#else
	uint32_t bus_hz = 400000;
	commands_printf("LSM6DS3 speed: 400 kHz");
#endif

	transport_i2c_bb_init(&m_transport, sda_gpio, sda_pin, scl_gpio, scl_pin, bus_hz);
	m_dev = lsm6ds3_device(&m_transport);
	imu_thread_set_device(&m_dev, m_settings.sample_rate_hz);
	if (m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer)) {
		imu_thread_start(imu_read_callback);
	}
}

bool imu_init_lsm6ds3_spi(stm32_gpio_t *nss_gpio, int nss_pin,
		stm32_gpio_t *sck_gpio, int sck_pin, stm32_gpio_t *mosi_gpio, int mosi_pin,
		stm32_gpio_t *miso_gpio, int miso_pin) {
	imu_stop();

#ifdef LSM6DS3_HWSPI_DEV
	transport_spi_hw_init(&m_transport, &LSM6DS3_HWSPI_DEV, LSM6DS3_HWSPI_AF,
			nss_gpio, nss_pin, sck_gpio, sck_pin, mosi_gpio, mosi_pin, miso_gpio, miso_pin, 0);
#else
	transport_spi_bb_init(&m_transport, nss_gpio, nss_pin, sck_gpio, sck_pin,
			mosi_gpio, mosi_pin, miso_gpio, miso_pin);
#endif

	m_dev = lsm6ds3_device(&m_transport);
	imu_thread_set_device(&m_dev, m_settings.sample_rate_hz);
	if (m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer)) {
		imu_thread_start(imu_read_callback);
		return true;
	}

	return false;
}

void imu_stop(void) {
	imu_thread_stop();

#ifdef LSM6DS3_HWSPI_DEV
	spiStop(&LSM6DS3_HWSPI_DEV);
#endif
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

void imu_derotate(const float *input, float *output) {
	float rpy[3];
	imu_get_rpy(rpy);

	const float ax = input[0];
	const float ay = input[1];
	const float az = input[2];

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

	output[0] = c_ax2;
	output[1] = c_ay2;
	output[2] = c_az;
}

void imu_get_accel_derotated(float *accel) {
	imu_derotate(m_accel, accel);
}

void imu_get_gyro_derotated(float *gyro) {
	imu_derotate(m_gyro, gyro);
}

void imu_get_quaternions(float *q) {
	q[0] = m_att.q0;
	q[1] = m_att.q1;
	q[2] = m_att.q2;
	q[3] = m_att.q3;
}

void imu_get_calibration(float yaw, float *imu_cal) {
	// Backup current settings
	float backup_sample_rate = m_settings.sample_rate_hz;
	AHRS_MODE backup_ahrs_mode = m_settings.mode;
	float backup_roll = m_settings.rot_roll;
	float backup_pitch = m_settings.rot_pitch;
	float backup_yaw = m_settings.rot_yaw;
	float backup_accel_offset_x = m_settings.accel_offsets[0];
	float backup_accel_offset_y = m_settings.accel_offsets[1];
	float backup_accel_offset_z = m_settings.accel_offsets[2];
	float backup_gyro_offset_x = m_settings.gyro_offsets[0];
	float backup_gyro_offset_y = m_settings.gyro_offsets[1];
	float backup_gyro_offset_z = m_settings.gyro_offsets[2];

	// Override settings
	m_settings.sample_rate_hz = 1000;
	m_settings.mode = AHRS_MODE_MADGWICK;
	ahrs_update_all_parameters(&m_att, 1.0, 10.0, 0.0, 2.0);
	m_settings.rot_roll = 0;
	m_settings.rot_pitch = 0;
	m_settings.rot_yaw = 0;
	m_settings.accel_offsets[0] = 0;
	m_settings.accel_offsets[1] = 0;
	m_settings.accel_offsets[2] = 0;
	m_settings.gyro_offsets[0] = 0;
	m_settings.gyro_offsets[1] = 0;
	m_settings.gyro_offsets[2] = 0;

	// Sample gyro for offsets
	float original_gyro_offsets[3] = {0, 0, 0};
	for (int i = 0; i < 1000; i++) {
		original_gyro_offsets[0] += m_gyro[0];
		original_gyro_offsets[1] += m_gyro[1];
		original_gyro_offsets[2] += m_gyro[2];
		chThdSleepMilliseconds(1);
	}
	original_gyro_offsets[0] /= 1000;
	original_gyro_offsets[1] /= 1000;
	original_gyro_offsets[2] /= 1000;

	// Set gyro offsets
	m_settings.gyro_offsets[0] = original_gyro_offsets[0];
	m_settings.gyro_offsets[1] = original_gyro_offsets[1];
	m_settings.gyro_offsets[2] = original_gyro_offsets[2];

	// Reset AHRS and wait 1.5 seconds (for AHRS to settle now that gyro is calibrated)
	ahrs_init_attitude_info(&m_att);
	chThdSleepMilliseconds(1500);

	// Sample roll
	float roll_sample = 0;
	for (int i = 0; i < 250; i++) {
		roll_sample += imu_get_roll();
		chThdSleepMilliseconds(1);
	}
	roll_sample = roll_sample / 250;

	// Set roll rotations to level out roll axis
	m_settings.rot_roll = -RAD2DEG_f(roll_sample);

	// Rotate gyro offsets to match new IMU orientation
	float rotation1[3] = {DEG2RAD_f(m_settings.rot_roll), DEG2RAD_f(m_settings.rot_pitch), DEG2RAD_f(m_settings.rot_yaw)};
	utils_rotate_vector3(original_gyro_offsets, rotation1, m_settings.gyro_offsets, false);

	// Reset AHRS and wait 1.5 seconds (for AHRS to settle now that pitch is calibrated)
	ahrs_init_attitude_info(&m_att);
	chThdSleepMilliseconds(1500);

	// Sample pitch
	float pitch_sample = 0;
	for (int i = 0; i < 250; i++) {
		pitch_sample += imu_get_pitch();
		chThdSleepMilliseconds(1);
	}
	pitch_sample = pitch_sample / 250;

	// Set pitch rotation to level out pitch axis
	m_settings.rot_pitch = RAD2DEG_f(pitch_sample);

	// Rotate imu offsets to match
	float rotation2[3] = {DEG2RAD_f(m_settings.rot_roll), DEG2RAD_f(m_settings.rot_pitch), DEG2RAD_f(m_settings.rot_yaw)};
	utils_rotate_vector3(original_gyro_offsets, rotation2, m_settings.gyro_offsets, false);

	// Set yaw rotations to match user input
	m_settings.rot_yaw = yaw;

	// Rotate gyro offsets to match new IMU orientation
	float rotation3[3] = {DEG2RAD_f(m_settings.rot_roll), DEG2RAD_f(m_settings.rot_pitch), DEG2RAD_f(m_settings.rot_yaw)};
	utils_rotate_vector3(original_gyro_offsets, rotation3, m_settings.gyro_offsets, false);

	// Note to future person interested in calibration:
	// This is where accel calibration should go, because at this point the values should be 0,0,1
	// All the IMU units I've tested haven't needed significant accel correction, so I've skipped it.
	// I'm worried that blindly setting them to 0,0,1 may do more harm that good (need more testing).

	// Return calibration
	imu_cal[0] = m_settings.rot_roll;
	imu_cal[1] = m_settings.rot_pitch;
	imu_cal[2] = m_settings.rot_yaw;
	imu_cal[3] = m_settings.accel_offsets[0];
	imu_cal[4] = m_settings.accel_offsets[1];
	imu_cal[5] = m_settings.accel_offsets[2];
	imu_cal[6] = m_settings.gyro_offsets[0];
	imu_cal[7] = m_settings.gyro_offsets[1];
	imu_cal[8] = m_settings.gyro_offsets[2];

	// Restore settings
	m_settings.sample_rate_hz = backup_sample_rate;
	m_settings.mode = backup_ahrs_mode;
	ahrs_update_all_parameters(
			&m_att,
			m_settings.accel_confidence_decay,
			m_settings.mahony_kp,
			m_settings.mahony_ki,
			m_settings.madgwick_beta);
	m_settings.rot_roll = backup_roll;
	m_settings.rot_pitch = backup_pitch;
	m_settings.rot_yaw = backup_yaw;
	m_settings.accel_offsets[0] = backup_accel_offset_x;
	m_settings.accel_offsets[1] = backup_accel_offset_y;
	m_settings.accel_offsets[2] = backup_accel_offset_z;
	m_settings.gyro_offsets[0] = backup_gyro_offset_x;
	m_settings.gyro_offsets[1] = backup_gyro_offset_y;
	m_settings.gyro_offsets[2] = backup_gyro_offset_z;

	ahrs_init_attitude_info(&m_att);
	FusionAhrsReinitialise(&m_fusionAhrs);
}

/*
 * Set the yaw-component of the IMU state. Currently only works for the fusion filter.
 */
void imu_set_yaw(float yaw_deg) {
	FusionAhrsSetYaw(&m_fusionAhrs, yaw_deg);
}

void imu_set_read_callback(void (*func)(float *acc, float *gyro, float *mag, float dt)) {
	m_read_callback = func;
}

static void imu_read_callback(float *accel, float *gyro, float *mag) {
	static uint32_t last_time = 0;

	chSysLock();
	float dt = timer_seconds_elapsed_since(last_time);
	last_time = timer_time_now();
	chSysUnlock();

	if (!imu_ready && ST2MS(chVTGetSystemTimeX() - init_time) > 1000) {
		ahrs_update_all_parameters(
				&m_att,
				m_settings.accel_confidence_decay,
				m_settings.mahony_kp,
				m_settings.mahony_ki,
				m_settings.madgwick_beta);

		FusionAhrsSetGain(&m_fusionAhrs, m_settings.madgwick_beta);
		FusionAhrsSetAccConfDecay(&m_fusionAhrs, m_settings.accel_confidence_decay);

		imu_ready = true;
	}

#ifdef IMU_CUSTOM_FUNC
	IMU_CUSTOM_FUNC;
#endif // IMU_CUSTOM_FUNC

#ifdef IMU_ROT_Y_270
	float a2_old = accel[2];
	float g2_old = gyro[2];
	float m2_old = mag[2];
	accel[2] = accel[0];
	accel[0] = -a2_old;
	gyro[2] = gyro[0];
	gyro[0] = -g2_old;
	mag[2] = mag[0];
	mag[0] = -m2_old;
#endif

#ifdef IMU_FLIP
	accel[0] *= -1.0;
	accel[2] *= -1.0;
	gyro[0] *= -1.0;
	gyro[2] *= -1.0;
	mag[0] *= -1.0;
	mag[2] *= -1.0;
#endif

#ifdef IMU_ROT_180
	accel[0] *= -1.0;
	accel[1] *= -1.0;
	gyro[0] *= -1.0;
	gyro[1] *= -1.0;
	mag[0] *= -1.0;
	mag[1] *= -1.0;
#endif

#ifdef IMU_ROT_90
	float a0_old = accel[0];
	float g0_old = gyro[0];
	float m0_old = mag[0];
	accel[0] = accel[1];
	accel[1] = -a0_old;
	gyro[0] = gyro[1];
	gyro[1] = -g0_old;
	mag[0] = mag[1];
	mag[1] = -m0_old;
#endif

#ifdef IMU_ROT_270
	float a0_old = accel[0];
	float g0_old = gyro[0];
	float m0_old = mag[0];
	accel[0] = -accel[1];
	accel[1] = a0_old;
	gyro[0] = -gyro[1];
	gyro[1] = g0_old;
	mag[0] = -mag[1];
	mag[1] = m0_old;
#endif

	// Rotate axes (ZYX)

	float s1 = sinf(DEG2RAD_f(m_settings.rot_yaw));
	float c1 = cosf(DEG2RAD_f(m_settings.rot_yaw));
	float s2 = sinf(DEG2RAD_f(m_settings.rot_pitch));
	float c2 = cosf(DEG2RAD_f(m_settings.rot_pitch));
	float s3 = sinf(DEG2RAD_f(m_settings.rot_roll));
	float c3 = cosf(DEG2RAD_f(m_settings.rot_roll));

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
	for (int i = 0; i < 3; i++) {
		m_accel[i] -= m_settings.accel_offsets[i];
		m_gyro[i] -= m_settings.gyro_offsets[i];
	}

	// Apply filters
	if(m_settings.accel_lowpass_filter_x > 0){
		m_accel[0] = biquad_process(&acc_x_biquad, m_accel[0]);
	}
	if(m_settings.accel_lowpass_filter_y > 0){
		m_accel[1] = biquad_process(&acc_y_biquad, m_accel[1]);
	}
	if(m_settings.accel_lowpass_filter_z > 0){
		m_accel[2] = biquad_process(&acc_z_biquad, m_accel[2]);
	}
	if(m_settings.gyro_lowpass_filter > 0){
		m_gyro[0] = biquad_process(&gyro_x_biquad, m_gyro[0]);
		m_gyro[1] = biquad_process(&gyro_y_biquad, m_gyro[1]);
		m_gyro[2] = biquad_process(&gyro_z_biquad, m_gyro[2]);
	}

	float gyro_rad[3];
	gyro_rad[0] = DEG2RAD_f(m_gyro[0]);
	gyro_rad[1] = DEG2RAD_f(m_gyro[1]);
	gyro_rad[2] = DEG2RAD_f(m_gyro[2]);

	switch (m_settings.mode) {
		case AHRS_MODE_MADGWICK:
			ahrs_update_madgwick_imu(gyro_rad, m_accel, dt, (ATTITUDE_INFO *)&m_att);
			break;
		case AHRS_MODE_MAHONY:
			ahrs_update_mahony_imu(gyro_rad, m_accel, dt, (ATTITUDE_INFO *)&m_att);
			break;
		case AHRS_MODE_MADGWICK_FUSION: {
			FusionVector3 calibratedGyroscope = {
					.axis.x = m_gyro[0],
					.axis.y = m_gyro[1],
					.axis.z = m_gyro[2],
			};
			FusionVector3 calibratedAccelerometer = {
					.axis.x = m_accel[0],
					.axis.y = m_accel[1],
					.axis.z = m_accel[2],
			};
			FusionAhrsUpdateWithoutMagnetometer(&m_fusionAhrs, calibratedGyroscope, calibratedAccelerometer, dt);
			m_att.q0 = m_fusionAhrs.quaternion.element.w;
			m_att.q1 = m_fusionAhrs.quaternion.element.x;
			m_att.q2 = m_fusionAhrs.quaternion.element.y;
			m_att.q3 = m_fusionAhrs.quaternion.element.z;
		} break;
	}

	if (m_read_callback) {
		m_read_callback(m_accel, gyro_rad, m_mag, dt);
	}
}

static void terminal_imu_type_internal(int argc, const char **argv) {
	(void)argc;(void)argv;
	commands_printf(m_imu_type_internal);
}
