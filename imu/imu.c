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
#include "imu_config.h"
#include "mpu9150.h"
#include "ahrs.h"
#include "timer.h"
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

// Private functions
static void imu_read_callback(float *accel, float *gyro, float *mag);

// Function pointers
static void (*m_read_callback)(float *acc, float *gyro, float *mag, float dt) = NULL;

static imu_device_t imu_device_create(uint8_t dev, uint8_t com, transport_t *transport) {
	switch (dev) {
	case IMU_DEV_MPU9X50:
		return mpu9150_device(transport);
	case IMU_DEV_ICM20948:
		return icm20948_device(transport);
	case IMU_DEV_BMI160:
		return bmi160_device(transport, (com == IMU_COM_SPI_BB || com == IMU_COM_SPI_HW)
				? BMI160_SPI_INTF : BMI160_I2C_INTF);
	case IMU_DEV_LSM6DS3:
		return lsm6ds3_device(transport);
	default:
		return (imu_device_t){0};
	}
}

static uint8_t imu_dev_for_external(IMU_TYPE type) {
	switch (type) {
	case IMU_TYPE_EXTERNAL_MPU9X50:
		return IMU_DEV_MPU9X50;
	case IMU_TYPE_EXTERNAL_ICM20948:
		return IMU_DEV_ICM20948;
	case IMU_TYPE_EXTERNAL_BMI160:
		return IMU_DEV_BMI160;
	case IMU_TYPE_EXTERNAL_LSM6DS3:
		return IMU_DEV_LSM6DS3;
	case IMU_TYPE_OFF:
	case IMU_TYPE_INTERNAL:
		break;
	}
	return IMU_DEV_NONE;
}

#ifdef IMU_FALLBACK_COM
// Bind m_transport to the board's fallback bus. A board declares IMU_FALLBACK_COM
// plus the matching IMU_FALLBACK_* pins when it wires the IMU to a different bus
// on some hardware revisions (see imu/imu_config.h).
static void imu_fallback_transport_init(void) {
#if IMU_FALLBACK_COM == IMU_COM_I2C_BB
	transport_i2c_bb_init(&m_transport, IMU_FALLBACK_I2C_SDA_GPIO, IMU_FALLBACK_I2C_SDA_PIN,
			IMU_FALLBACK_I2C_SCL_GPIO, IMU_FALLBACK_I2C_SCL_PIN, IMU_FALLBACK_BUS_SPEED_HZ);
#else
#error "IMU_FALLBACK_COM currently supports only IMU_COM_I2C_BB"
#endif
}
#endif

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

	// Resolve the active (device, transport) pair: the internal IMU from
	// compile-time board macros, an external IMU from the runtime type.
	uint8_t dev = IMU_DEV_NONE;
	uint8_t com = IMU_COM_NONE;

	if (set->type == IMU_TYPE_INTERNAL) {
#if IMU_DEV != IMU_DEV_NONE
		dev = IMU_DEV;
		com = IMU_COM;

#if IMU_COM == IMU_COM_I2C_BB
		transport_i2c_bb_init(&m_transport, IMU_I2C_SDA_GPIO, IMU_I2C_SDA_PIN,
				IMU_I2C_SCL_GPIO, IMU_I2C_SCL_PIN, IMU_BUS_SPEED_HZ);
#elif IMU_COM == IMU_COM_SPI_BB
		transport_spi_bb_init(&m_transport, IMU_SPI_NSS_GPIO, IMU_SPI_NSS_PIN,
				IMU_SPI_SCK_GPIO, IMU_SPI_SCK_PIN, IMU_SPI_MOSI_GPIO, IMU_SPI_MOSI_PIN,
				IMU_SPI_MISO_GPIO, IMU_SPI_MISO_PIN);
#elif IMU_COM == IMU_COM_SPI_HW
		transport_spi_hw_init(&m_transport, &IMU_SPI_DEV, IMU_SPI_AF,
				IMU_SPI_NSS_GPIO, IMU_SPI_NSS_PIN, IMU_SPI_SCK_GPIO, IMU_SPI_SCK_PIN,
				IMU_SPI_MOSI_GPIO, IMU_SPI_MOSI_PIN, IMU_SPI_MISO_GPIO, IMU_SPI_MISO_PIN,
				IMU_BUS_SPEED_HZ);
#elif IMU_COM == IMU_COM_I2C_BB_SPI
		// LSM6DS3 wired to the SPI pins but driven as bit-bang I2C: park NSS high and MISO low, then
		// run I2C with SDA on the MOSI pin and SCL on the SCK pin.
		palSetPadMode(IMU_SPI_NSS_GPIO, IMU_SPI_NSS_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palSetPad(IMU_SPI_NSS_GPIO, IMU_SPI_NSS_PIN);
		palSetPadMode(IMU_SPI_MISO_GPIO, IMU_SPI_MISO_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palClearPad(IMU_SPI_MISO_GPIO, IMU_SPI_MISO_PIN);
		transport_i2c_bb_init(&m_transport, IMU_SPI_MOSI_GPIO, IMU_SPI_MOSI_PIN,
				IMU_SPI_SCK_GPIO, IMU_SPI_SCK_PIN, IMU_BUS_SPEED_HZ);
#endif
#endif // IMU_DEV != IMU_DEV_NONE
	} else {
		dev = imu_dev_for_external(set->type);
		if (dev != IMU_DEV_NONE) {
			com = IMU_COM_I2C_BB;
			transport_i2c_bb_init(&m_transport, HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
					HW_I2C_SCL_PORT, HW_I2C_SCL_PIN, 0);
		}
	}

	if (dev != IMU_DEV_NONE) {
		m_dev = imu_device_create(dev, com, &m_transport);
		uint16_t rate_hz = MIN(m_settings.sample_rate_hz, transport_max_sample_rate(&m_transport));
		imu_thread_set_device(&m_dev, rate_hz);
		bool configured = m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer);

#ifdef IMU_FALLBACK_COM
		if (!configured) {
			// No response on the primary bus. Some hardware revisions wire the
			// same IMU to a different bus, so re-bind to the fallback transport
			// and try once more.
			commands_printf("IMU: no response on primary bus, trying fallback");
			imu_fallback_transport_init();
			com = IMU_FALLBACK_COM;
			m_dev = imu_device_create(dev, com, &m_transport);
			rate_hz = MIN(m_settings.sample_rate_hz, transport_max_sample_rate(&m_transport));
			imu_thread_set_device(&m_dev, rate_hz);
			configured = m_dev.interface->configure(&m_dev, m_settings.filter, m_settings.use_magnetometer);
		}
#endif

		if (configured) {
			imu_thread_start(imu_read_callback);
		}
	}
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

void imu_stop(void) {
	imu_thread_stop();

#if IMU_COM == IMU_COM_SPI_HW
	spiStop(&IMU_SPI_DEV);
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
