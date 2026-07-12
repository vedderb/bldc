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

#include "icm20948.h"
#include "ch.h"

#define ICM20948_I2C_ADDR1	0x68

static bool read_reg(imu_device_t *dev, uint8_t reg, uint8_t *rx, size_t len) {
	return transport_read_reg(dev->transport, dev->dev_addr, reg, rx, len);
}

static bool write_reg(imu_device_t *dev, uint8_t reg, uint8_t value) {
	return transport_write_reg(dev->transport, dev->dev_addr, reg, &value, 1);
}

static bool reset_init_icm(imu_device_t *dev) {
	transport_recover(dev->transport);

	chThdSleep(1);

	// TODO: Check for errors

	// Set clock source to auto
	write_reg(dev, ICM20948_BANK_SEL, 0 << 4);
	write_reg(dev, ICM20948_PWR_MGMT_1, 1);

	// Set accelerometer to +-16 G and disable lp filter
	write_reg(dev, ICM20948_BANK_SEL, 2 << 4);
	write_reg(dev, ICM20948_ACCEL_CONFIG, 0b00000110);

	// Set gyro to +-2000 dps and disable lp filter
	write_reg(dev, ICM20948_BANK_SEL, 2 << 4);
	write_reg(dev, ICM20948_GYRO_CONFIG_1, 0b00000110);

	// I2C bypass to access magnetometer directly
//	write_reg(dev, ICM20948_BANK_SEL, 0);
//	write_reg(dev, ICM20948_PIN_CFG, 2);

	// Select bank0 so that data can be polled.
	write_reg(dev, ICM20948_BANK_SEL, 0 << 4);

	return true;
}

static bool configure(imu_device_t *dev, IMU_FILTER filter, bool use_mag) {
	(void)filter;
	(void)use_mag;

	dev->dev_addr = ICM20948_I2C_ADDR1;
	return reset_init_icm(dev);
}

static bool read_sample(imu_device_t *dev, float accel[3], float gyro[3], float mag[3]) {
	uint8_t rxb[12];

	if (!read_reg(dev, ICM20948_ACCEL_XOUT_H, rxb, 12)) {
		return false;
	}

	accel[0] = (float)((int16_t)((int16_t)rxb[0] << 8 | (int16_t)rxb[1])) * 16.0 / 32768.0;
	accel[1] = (float)((int16_t)((int16_t)rxb[2] << 8 | (int16_t)rxb[3])) * 16.0 / 32768.0;
	accel[2] = (float)((int16_t)((int16_t)rxb[4] << 8 | (int16_t)rxb[5])) * 16.0 / 32768.0;

	gyro[0] = (float)((int16_t)((int16_t)rxb[6] << 8 | (int16_t)rxb[7])) * 2000.0 / 32768.0;
	gyro[1] = (float)((int16_t)((int16_t)rxb[8] << 8 | (int16_t)rxb[9])) * 2000.0 / 32768.0;
	gyro[2] = (float)((int16_t)((int16_t)rxb[10] << 8 | (int16_t)rxb[11])) * 2000.0 / 32768.0;

	// TODO: Read magnetometer as well
	mag[0] = 0.0;
	mag[1] = 0.0;
	mag[2] = 0.0;

	return true;
}

static void on_read_fail(imu_device_t *dev) {
	reset_init_icm(dev);
	chThdSleepMilliseconds(10);
}

static const imu_device_interface_t icm20948_interface = {
	.name = "ICM20948",
	.configure = configure,
	.read_sample = read_sample,
	.on_read_fail = on_read_fail,
};

imu_device_t icm20948_device(transport_t *transport) {
	return (imu_device_t){ .interface = &icm20948_interface, .transport = transport };
}
