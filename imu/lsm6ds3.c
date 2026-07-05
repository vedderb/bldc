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

// Output sensitivities for the configured full scales (CTRL1_XL = ±16 g, CTRL2_G = ±2000 dps).
#define GYRO_DPS_PER_LSB	(70.0f / 1000.0f)   // 70 mdps/LSB
#define ACCEL_G_PER_LSB		(0.488f / 1000.0f)  // 0.488 mg/LSB

// The accelerometer and gyroscope share the CTRL1_XL / CTRL2_G ODR encoding (12.5..6660 Hz).
static const struct { uint16_t hz; uint8_t code; } odr_ladder[] = {
	{13,   LSM6DS3_ACC_GYRO_ODR_XL_13Hz},
	{26,   LSM6DS3_ACC_GYRO_ODR_XL_26Hz},
	{52,   LSM6DS3_ACC_GYRO_ODR_XL_52Hz},
	{104,  LSM6DS3_ACC_GYRO_ODR_XL_104Hz},
	{208,  LSM6DS3_ACC_GYRO_ODR_XL_208Hz},
	{416,  LSM6DS3_ACC_GYRO_ODR_XL_416Hz},
	{833,  LSM6DS3_ACC_GYRO_ODR_XL_833Hz},
	{1660, LSM6DS3_ACC_GYRO_ODR_XL_1660Hz},
	{3330, LSM6DS3_ACC_GYRO_ODR_XL_3330Hz},
	{6660, LSM6DS3_ACC_GYRO_ODR_XL_6660Hz},
};
#define ODR_LADDER_N (sizeof(odr_ladder) / sizeof(odr_ladder[0]))

static bool configure(imu_device_t *dev, IMU_FILTER filter, bool use_mag) {
	(void)use_mag;

	uint8_t rxb[1];

	dev->dev_addr = LSM6DS3_ACC_GYRO_ADDR_A;
	bool ok = read_reg(dev, LSM6DS3_ACC_GYRO_WHO_AM_I_REG, rxb);
	if (!ok || (rxb[0] != 0x69 && rxb[0] != 0x6A && rxb[0] != 0x6C)) {
		commands_printf("LSM6DS3 Address A failed, trying B (rx: %d)", rxb[0]);
		dev->dev_addr = LSM6DS3_ACC_GYRO_ADDR_B;
		ok = read_reg(dev, LSM6DS3_ACC_GYRO_WHO_AM_I_REG, rxb);
		if (!ok || (rxb[0] != 0x69 && rxb[0] != 0x6A && rxb[0] != 0x6C)) {
			commands_printf("LSM6DS3 Address B failed (rx: %d)", rxb[0]);
			return false;
		}
	}

	bool is_trc = false;
	if (rxb[0] == 0x6A) {
		is_trc = true;
		dev->variant = "TR-C";
	}

	// ODR selection. The accelerometer runs at its max ODR so the poll always reads the
	// freshest sample. The TR-C gyro has a separate filter and does the same; the non-TR-C
	// gyro has no separate filter (its bandwidth follows the ODR) and tops out at 1660 Hz, so
	// it tracks the sample rate instead of oversampling.
	uint8_t accel_odr = odr_ladder[ODR_LADDER_N - 1].code;
	uint8_t gyro_odr;
	if (is_trc) {
		gyro_odr = odr_ladder[ODR_LADDER_N - 1].code;
	} else {
		// non-TR-C gyro tops out at 1660 Hz
		int gyro_rate = dev->sample_rate_hz < 1660 ? dev->sample_rate_hz : 1660;
		uint8_t i = 0;
		while (i < ODR_LADDER_N - 1 && odr_ladder[i].hz < gyro_rate) {
			i++;
		}
		gyro_odr = odr_ladder[i].code;
	}

	// Accelerometer resolution and data rate
	uint8_t regv = LSM6DS3_ACC_GYRO_FS_XL_16g | accel_odr;

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

	ok = ok && write_reg(dev, LSM6DS3_ACC_GYRO_CTRL1_XL, regv);

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

		ok = ok && write_reg(dev, LSM6DS3_ACC_GYRO_CTRL8_XL, regv);
	}

	// Gyro resolution and data rate
	regv = LSM6DS3_ACC_GYRO_FS_G_2000dps | gyro_odr;
	ok = ok && write_reg(dev, LSM6DS3_ACC_GYRO_CTRL2_G, regv);

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

		ok = ok && write_reg(dev, LSM6DS3_ACC_GYRO_CTRL6_G, regv);
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
	ok = ok && write_reg(dev, LSM6DS3_ACC_GYRO_CTRL4_C, regv);

	// Configure block update and register auto-increment
	regv = LSM6DS3_ACC_GYRO_BDU_BLOCK_UPDATE | LSM6DS3_ACC_GYRO_IF_INC_ENABLED;
	ok = ok && write_reg(dev, LSM6DS3_ACC_GYRO_CTRL3_C, regv);

	if (!ok) {
		commands_printf("LSM6DS3 config failed");
		return false;
	}
	return true;
}

static bool read_sample(imu_device_t *dev, float accel[3], float gyro[3], float mag[3]) {
	uint8_t rxb[12];
	if (!read_gyro_accel(dev, rxb)) {
		return false;
	}

	gyro[0] = (int16_t)(rxb[1] << 8 | rxb[0]) * GYRO_DPS_PER_LSB;
	gyro[1] = (int16_t)(rxb[3] << 8 | rxb[2]) * GYRO_DPS_PER_LSB;
	gyro[2] = (int16_t)(rxb[5] << 8 | rxb[4]) * GYRO_DPS_PER_LSB;
	accel[0] = (int16_t)(rxb[7] << 8 | rxb[6]) * ACCEL_G_PER_LSB;
	accel[1] = (int16_t)(rxb[9] << 8 | rxb[8]) * ACCEL_G_PER_LSB;
	accel[2] = (int16_t)(rxb[11] << 8 | rxb[10]) * ACCEL_G_PER_LSB;
	mag[0] = 0; mag[1] = 0; mag[2] = 0;

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
