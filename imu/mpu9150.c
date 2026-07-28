/*
	Copyright 2013 - 2019 Benjamin Vedder	benjamin@vedder.se
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

/*
 * Note: This driver also works for the MPU9250
 */

#include "mpu9150.h"
#include "ch.h"

#include <string.h>

// Settings
#define MAG_DIV					10
#define FAIL_DELAY_US			1000
#define MAX_IDENTICAL_READS		5
#define MPU_ADDR1				0x68
#define MPU_ADDR2				0x69
#define MPU_MAG_ADDR			0x0C // AK8975 magnetometer, reached over the i2c bypass

typedef struct {
	int16_t prev_raw[6];   // previous accel+gyro sample, to detect a stuck sensor
	uint32_t identical_reads;
	uint8_t mag_cnt;       // decimation counter for the magnetometer
	int16_t mag_raw[3];    // last magnetometer reading, reused between refreshes
	bool use_magnetometer;
} mpu_state_t;

static mpu_state_t m_state;

static bool read_reg(imu_device_t *dev, uint8_t addr, uint8_t reg, uint8_t *rx, size_t len) {
	return transport_read_reg(dev->transport, addr, reg, rx, len);
}

static bool write_reg(imu_device_t *dev, uint8_t addr, uint8_t reg, uint8_t value) {
	return transport_write_reg(dev->transport, addr, reg, &value, 1);
}

static bool get_raw_accel_gyro(imu_device_t *dev, int16_t *accel_gyro) {
	uint8_t rxb[14];

	if (!read_reg(dev, dev->dev_addr, MPU9150_ACCEL_XOUT_H, rxb, 14)) {
		return false;
	}

	// Acceleration
	for (int i = 0;i < 3; i++) {
		accel_gyro[i] = ((int16_t) ((uint16_t) rxb[2 * i] << 8) + rxb[2 * i + 1]);
	}

	// Angular rate (the temperature register at index 3 is skipped)
	for (int i = 4;i < 7; i++) {
		accel_gyro[i - 1] = ((int16_t) ((uint16_t) rxb[2 * i] << 8) + rxb[2 * i + 1]);
	}

	return true;
}

static bool get_raw_mag(imu_device_t *dev, int16_t *mag) {
	uint8_t rxb[6];

	if (!read_reg(dev, MPU_MAG_ADDR, MPU9150_HXL, rxb, 6)) {
		return false;
	}

	for (int i = 0; i < 3; i++) {
		mag[i] = ((int16_t) ((uint16_t) rxb[2 * i + 1] << 8) + rxb[2 * i]);
	}

	// Start the measurement for the next iteration
	return write_reg(dev, MPU_MAG_ADDR, MPU9150_CNTL, 0x01);
}

static void reset_init_mpu(imu_device_t *dev) {
	mpu_state_t *st = dev->priv;

	transport_recover(dev->transport);

	// Set clock source to gyro x; if that address doesn't answer, try the other one.
	if (!write_reg(dev, dev->dev_addr, MPU9150_PWR_MGMT_1, 0x01)) {
		dev->dev_addr = (dev->dev_addr == MPU_ADDR1) ? MPU_ADDR2 : MPU_ADDR1;
		if (!write_reg(dev, dev->dev_addr, MPU9150_PWR_MGMT_1, 0x01)) {
			return;
		}
	}

	bool ok =
		// Accelerometer full-scale range to +/- 16g
		write_reg(dev, dev->dev_addr, MPU9150_ACCEL_CONFIG, MPU9150_ACCEL_FS_16 << MPU9150_ACONFIG_AFS_SEL_BIT) &&
		// Gyroscope full-scale range to +/- 2000 deg/s
		write_reg(dev, dev->dev_addr, MPU9150_GYRO_CONFIG, MPU9150_GYRO_FS_2000 << MPU9150_GCONFIG_FS_SEL_BIT) &&
		// Low pass filter to 256Hz (1ms delay)
		write_reg(dev, dev->dev_addr, MPU9150_CONFIG, MPU9150_DLPF_BW_256);

	// Enable the i2c bypass so the magnetometer can be accessed directly
	if (ok && st->use_magnetometer) {
		write_reg(dev, dev->dev_addr, MPU9150_INT_PIN_CFG, 0x02);
	}
}

static bool configure(imu_device_t *dev, IMU_FILTER filter, bool use_mag) {
	(void)filter;

	mpu_state_t *st = dev->priv;
	memset(st, 0, sizeof(*st));
	st->use_magnetometer = use_mag;
	st->mag_cnt = MAG_DIV; // read the magnetometer on the first sample

	dev->dev_addr = MPU_ADDR1;
	reset_init_mpu(dev);

	uint8_t who = 0;
	read_reg(dev, dev->dev_addr, MPU9150_WHO_AM_I, &who, 1);
	switch (who) {
	case 0x68: dev->variant = "9150"; return true; // shared with the MPU-6050 die
	case 0x71: dev->variant = "9250"; return true;
	case 0x73: dev->variant = "9255"; return true;
	case 0x69: return true; // accepted historically; not a documented WHO_AM_I, leave variant unset
	default:   return false;
	}
}

static bool read_sample(imu_device_t *dev, float accel[3], float gyro[3], float mag[3]) {
	mpu_state_t *st = dev->priv;
	int16_t raw[6];

	if (!get_raw_accel_gyro(dev, raw)) {
		return false;
	}

	// A stuck sensor keeps returning the same bytes; treat a run of them as a failure.
	bool identical = true;
	for (int i = 0;i < 6;i++) {
		if (raw[i] != st->prev_raw[i]) {
			identical = false;
			break;
		}
	}
	st->identical_reads = identical ? st->identical_reads + 1 : 0;
	if (st->identical_reads >= MAX_IDENTICAL_READS) {
		return false;
	}

	memcpy(st->prev_raw, raw, sizeof(st->prev_raw));

	accel[0] = (float)raw[0] * 16.0 / 32768.0;
	accel[1] = (float)raw[1] * 16.0 / 32768.0;
	accel[2] = (float)raw[2] * 16.0 / 32768.0;

	gyro[0] = (float)raw[3] * 2000.0 / 32768.0;
	gyro[1] = (float)raw[4] * 2000.0 / 32768.0;
	gyro[2] = (float)raw[5] * 2000.0 / 32768.0;

	if (st->use_magnetometer) {
		// The magnetometer runs at ODR/MAG_DIV; the last reading is reused in between
		// (so the delivered mag lags the accel/gyro, as in the original driver).
		mag[0] = (float)st->mag_raw[0] * 1200.0 / 4096.0;
		mag[1] = (float)st->mag_raw[1] * 1200.0 / 4096.0;
		mag[2] = (float)st->mag_raw[2] * 1200.0 / 4096.0;

		if (++st->mag_cnt >= MAG_DIV) {
			st->mag_cnt = 0;
			if (!get_raw_mag(dev, st->mag_raw)) {
				reset_init_mpu(dev);
			}
		}
	} else {
		mag[0] = 0.0;
		mag[1] = 0.0;
		mag[2] = 0.0;
	}

	return true;
}

static void on_read_fail(imu_device_t *dev) {
	chThdSleepMicroseconds(FAIL_DELAY_US);
	reset_init_mpu(dev);
}

static const imu_device_interface_t mpu9150_interface = {
	.name = "MPU9X50",
	.configure = configure,
	.read_sample = read_sample,
	.on_read_fail = on_read_fail,
};

imu_device_t mpu9150_device(transport_t *transport) {
	return (imu_device_t){ .interface = &mpu9150_interface, .transport = transport, .priv = &m_state };
}
