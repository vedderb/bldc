/*
	Copyright 2020 Mitch Lustig
	Copyright 2026 Benjamin Vedder benjamin@vedder.se
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

#include "lsm6ds3.h"
#include "commands.h"

static bool read_reg(imu_device_t *dev, uint8_t reg, uint8_t *res) {
	return transport_read_reg(dev->transport, dev->dev_addr, reg, res, 1);
}

static bool write_reg(imu_device_t *dev, uint8_t reg, uint8_t value) {
	return transport_write_reg(dev->transport, dev->dev_addr, reg, &value, 1);
}

static bool read_gyro_accel(imu_device_t *dev, uint8_t *res) {
	return transport_read_reg(dev->transport, dev->dev_addr, LSM6DS3_ACC_GYRO_OUTX_L_G, res, 12);
}

static bool configure(imu_device_t *dev, IMU_FILTER filter, bool use_mag) {
	(void)use_mag;

	uint8_t rxb[1];

	dev->dev_addr = LSM6DS3_ACC_GYRO_ADDR_A;
	bool res = read_reg(dev, LSM6DS3_ACC_GYRO_WHO_AM_I_REG, rxb);
	if (!res || (rxb[0] != 0x69 && rxb[0] != 0x6A && rxb[0] != 0x6C)) {
		commands_printf("LSM6DS3 Address A failed, trying B (rx: %d)", rxb[0]);
		dev->dev_addr = LSM6DS3_ACC_GYRO_ADDR_B;
		res = read_reg(dev, LSM6DS3_ACC_GYRO_WHO_AM_I_REG, rxb);
		if (!res || (rxb[0] != 0x69 && rxb[0] != 0x6A && rxb[0] != 0x6C)) {
			commands_printf("LSM6DS3 Address B failed (rx: %d)", rxb[0]);
			return false;
		}
	}

	bool is_trc = false;
	if (rxb[0] == 0x6A){
		is_trc = true;
		dev->variant = "TR-C";
	}

	// Accelerometer resolution and data rate
	uint8_t regv = LSM6DS3_ACC_GYRO_FS_XL_16g;
	regv |= LSM6DS3_ACC_GYRO_ODR_XL_6660Hz;

	// Accelerometer filtering
	#define LSM6DS3TRC_BW0_XL 0x1
	#define LSM6DS3TRC_LPF1_BW_SEL 0x2
	if (is_trc) {
		// Always use accelerometer analog low-pass at 400Hz
		regv |= LSM6DS3TRC_BW0_XL;
	} else if (dev->sample_rate_hz >= 208 && filter >= IMU_FILTER_MEDIUM) {
		// Filter at ODR/4 for MEDIUM and ODR/8 for HIGH
		// This filter also needs to be enabled in CTRL4_C
		uint16_t scaled_rate = filter == IMU_FILTER_HIGH ? dev->sample_rate_hz / 2 : dev->sample_rate_hz;
		if (scaled_rate <= 208) {
			regv |= LSM6DS3_ACC_GYRO_BW_XL_50Hz;
		} else if (scaled_rate <= 416) {
			regv |= LSM6DS3_ACC_GYRO_BW_XL_100Hz;
		} else if (scaled_rate <= 833) {
			regv |= LSM6DS3_ACC_GYRO_BW_XL_200Hz;
		}
	}

	res = write_reg(dev, LSM6DS3_ACC_GYRO_CTRL1_XL, regv);
	if (!res){
		commands_printf("LSM6DS3 Accel Config FAILED");
		return false;
	}

	// Extra accelerometer filtering for TRC variant
	if (is_trc) {
		#define LSM6DS3TRC_LPF2_XL_EN 0x80
		#define LSM6DS3TRC_HPCF_XL_ODR9 0x40
		#define LSM6DS3TRC_HPCF_XL_ODR50 0x00
		#define LSM6DS3TRC_HPCF_XL_ODR100 0x20
		regv = 0;
		if (filter == IMU_FILTER_MEDIUM) {
			regv |= LSM6DS3TRC_LPF2_XL_EN | LSM6DS3TRC_HPCF_XL_ODR50;
		} else if (filter == IMU_FILTER_HIGH) {
			regv |= LSM6DS3TRC_LPF2_XL_EN | LSM6DS3TRC_HPCF_XL_ODR100;
		}

		res = write_reg(dev, LSM6DS3_ACC_GYRO_CTRL8_XL, regv);
		if (!res) {
			commands_printf("LSM6DS3 Accel Filter Config FAILED");
			return false;
		}
	}

	// Gyro resolution and data rate
	regv = LSM6DS3_ACC_GYRO_FS_G_2000dps;

	if (is_trc) {
		regv |= LSM6DS3TRC_ACC_GYRO_ODR_G_6660Hz;
	} else {
		// On non-TRC there is no dedicated configurable gyro filter, the filtering
		// seems to depend on the actual ODR, so we can't oversample it.
		if (dev->sample_rate_hz <= 13) {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_13Hz;
		} else if (dev->sample_rate_hz <= 26) {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_26Hz;
		} else if (dev->sample_rate_hz <= 52) {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_52Hz;
		} else if (dev->sample_rate_hz <= 104) {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_104Hz;
		} else if (dev->sample_rate_hz <= 208) {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_208Hz;
		} else if (dev->sample_rate_hz <= 416) {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_416Hz;
		} else if (dev->sample_rate_hz <= 833) {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_833Hz;
		} else {
			regv |= LSM6DS3_ACC_GYRO_ODR_G_1660Hz;
		}
	}
	res = write_reg(dev, LSM6DS3_ACC_GYRO_CTRL2_G, regv);
	if (!res){
		commands_printf("LSM6DS3 Gyro Config FAILED");
		return false;
	}

	// Extra gyro filtering for TRC variant
	if (is_trc) {
		#define LSM6DS3TRC_FTYPE_L 0x00
		#define LSM6DS3TRC_FTYPE_M 0x01
		#define LSM6DS3TRC_FTYPE_H 0x10
		regv = 0;
		if (filter == IMU_FILTER_LOW) {
			regv |= LSM6DS3TRC_FTYPE_L;
		} else if (filter == IMU_FILTER_MEDIUM) {
			regv |= LSM6DS3TRC_FTYPE_M;
		} else if (filter == IMU_FILTER_HIGH) {
			regv |= LSM6DS3TRC_FTYPE_H;
		}

		res = write_reg(dev, LSM6DS3_ACC_GYRO_CTRL6_G, regv);
		if (!res){
			commands_printf("LSM6DS3 Gyro Filter FAILED");
			return false;
		}
	}

	// Miscellaneous filtering configuration in CTRL4_C
	// TRC Variant CTRL4 register is very different from other variants
	regv = 0;
	if (is_trc) {
		// Enable gyroscope digital low-pass filter LPF1
		regv = LSM6DS3_ACC_GYRO_LPF1_SEL_G_ENABLED;
	} else if (dev->sample_rate_hz >= 208 && filter >= IMU_FILTER_MEDIUM) {
		// Standard LSM6DS3 only: Set XL anti-aliasing filter to be manually configured
		regv = LSM6DS3_ACC_GYRO_BW_SCAL_ODR_ENABLED;
	}
	res = write_reg(dev, LSM6DS3_ACC_GYRO_CTRL4_C, regv);
	if (!res) {
		commands_printf("LSM6DS3 Misc Filter Config FAILED");
		return false;
	}

	// Configure block update and register auto-increment
	regv = LSM6DS3_ACC_GYRO_BDU_BLOCK_UPDATE | LSM6DS3_ACC_GYRO_IF_INC_ENABLED;
	res = write_reg(dev, LSM6DS3_ACC_GYRO_CTRL3_C, regv);
	if (!res) {
		commands_printf("LSM6DS3 BDU Config FAILED");
		return false;
	}

	return true;
}

static bool read_sample(imu_device_t *dev, float accel[3], float gyro[3], float mag[3]) {
	uint8_t rxb[12];
	if (!read_gyro_accel(dev, rxb)) {
		return false;
	}

	gyro[0] = (float)((int16_t)((uint16_t)rxb[1] << 8) + rxb[0]) * 4.375 * (2000 / 125) / 1000;
	gyro[1] = (float)((int16_t)((uint16_t)rxb[3] << 8) + rxb[2]) * 4.375 * (2000 / 125) / 1000;
	gyro[2] = (float)((int16_t)((uint16_t)rxb[5] << 8) + rxb[4]) * 4.375 * (2000 / 125) / 1000;
	accel[0] = (float)((int16_t)((uint16_t)rxb[7] << 8) + rxb[6]) * 0.061 * (16 >> 1) / 1000;
	accel[1] = (float)((int16_t)((uint16_t)rxb[9] << 8) + rxb[8]) * 0.061 * (16 >> 1) / 1000;
	accel[2] = (float)((int16_t)((uint16_t)rxb[11] << 8) + rxb[10]) * 0.061 * (16 >> 1) / 1000;
	mag[0] = 1; mag[1] = 2; mag[2] = 3;

	return true;
}

static const imu_device_interface_t lsm6ds3_interface = {
	.name = "LSM6DS3",
	.configure = configure,
	.read_sample = read_sample,
	.on_read_fail = NULL,
};

imu_device_t lsm6ds3_device(transport_t *transport) {
	return (imu_device_t){ .interface = &lsm6ds3_interface, .transport = transport };
}
