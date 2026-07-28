/*
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

#include "lsm6dsv32x.h"
#include "commands.h"

/*
 * Anti-alias low-pass cutoffs at different IMU_FILTER values:
 * fs = output rate, based on the configured Sample Rate (the poll frequency in poll mode, or the
 * data-ready rate in DRDY mode)
 *
 * poll:
 *   ODR: both 7.68 kHz
 *
 *   filter   accel    gyro
 *   LOW      fs/2     537 Hz (LPF1 bypassed)
 *   MEDIUM   fs/4     281 Hz (LPF1 000)
 *   HIGH     fs/8     156 Hz (LPF1 010)
 *
 * DRDY:
 *   ODR: both fs
 *
 *   filter   accel    gyro (scales with ODR)
 *   LOW      fs/2     <=537 Hz (LPF1 bypassed)
 *   MEDIUM   fs/4     <=281 Hz (LPF1 000)
 *   HIGH     fs/8     <=156 Hz (LPF1 010)
 */

#define LSM6DSV32X_ADDR_A		0x6A // SA0 = 0
#define LSM6DSV32X_ADDR_B		0x6B // SA0 = 1

#define REG_INT1_CTRL			0x0D
#define REG_WHO_AM_I			0x0F
#define REG_CTRL1				0x10 // accel operating mode + ODR
#define REG_CTRL2				0x11 // gyro operating mode + ODR
#define REG_CTRL3				0x12 // BOOT, BDU, IF_INC, SW_RESET
#define REG_CTRL4				0x13 // DRDY_MASK, DRDY_PULSED, ...
#define REG_CTRL6				0x15 // gyro LPF1 bandwidth + gyro full-scale
#define REG_CTRL7				0x16 // gyro LPF1 enable
#define REG_CTRL8				0x17 // accel LPF2 bandwidth + accel full-scale
#define REG_CTRL9				0x18 // accel LPF2 enable
#define REG_OUTX_L_G			0x22 // gyro X/Y/Z then accel X/Y/Z (12 bytes)

#define WHO_AM_I_VAL			0x70

#define CTRL3_BDU				(1 << 6)
#define CTRL3_IF_INC			(1 << 2)
#define CTRL4_DRDY_MASK			(1 << 3) // hold DRDY off until the filters settle
#define CTRL7_LPF1_G_EN			(1 << 0)
#define CTRL8_MUST_SET			(1 << 2) // CTRL8 bit 2 must be written as 1
#define CTRL9_LPF2_XL_EN		(1 << 3) // low-pass path (HP_SLOPE_XL_EN left at 0)
#define INT1_DRDY_G				(1 << 1) // gyro data-ready on INT1 (accel is synchronized)

// Full-scale ranges: CTRL6 FS_G[3:0] / CTRL8 FS_XL[1:0] register codes and the matching
// output sensitivities (datasheet table 3).
#define FS_G_4000DPS		0x0C
#define FS_G_2000DPS		0x04
#define FS_XL_32G			0x03
#define FS_XL_16G			0x02
#define GYRO_LSB_4000DPS	(140.0f / 1000.0f) // 140 mdps/LSB
#define GYRO_LSB_2000DPS	(70.0f / 1000.0f)  // 70 mdps/LSB
#define ACCEL_LSB_32G		(0.976f / 1000.0f) // 0.976 mg/LSB
#define ACCEL_LSB_16G		(0.488f / 1000.0f) // 0.488 mg/LSB

// Active full scale — change these four defines to switch between the ranges above.
#define FS_G_REG			FS_G_4000DPS
#define FS_XL_REG			FS_XL_32G
#define GYRO_DPS_PER_LSB	GYRO_LSB_4000DPS
#define ACCEL_G_PER_LSB		ACCEL_LSB_32G

// Accel and gyro share the ODR ladder 7.5 Hz * 2^n -> register nibble 0x2..0xC (tables 52
// and 55). The 8 Hz entry stands in for 7.5 Hz (the requested rate is an integer).
static const struct { uint16_t hz; uint8_t code; } odr_ladder[] = {
	{8, 0x2}, {15, 0x3}, {30, 0x4}, {60, 0x5}, {120, 0x6}, {240, 0x7},
	{480, 0x8}, {960, 0x9}, {1920, 0xA}, {3840, 0xB}, {7680, 0xC},
};
#define ODR_LADDER_N (sizeof(odr_ladder) / sizeof(odr_ladder[0]))

static bool read_reg(imu_device_t *dev, uint8_t reg, uint8_t *res) {
	return transport_read_reg(dev->transport, dev->dev_addr, reg, res, 1);
}

static bool write_reg(imu_device_t *dev, uint8_t reg, uint8_t value) {
	return transport_write_reg(dev->transport, dev->dev_addr, reg, &value, 1);
}

static bool configure(imu_device_t *dev, IMU_FILTER filter, bool use_mag) {
	(void)use_mag; // 6-axis part, no magnetometer

	uint8_t id = 0;
	dev->dev_addr = LSM6DSV32X_ADDR_A;
	bool ok = read_reg(dev, REG_WHO_AM_I, &id);
	if (!ok || id != WHO_AM_I_VAL) {
		commands_printf("LSM6DSV32X address A failed, trying B (rx: %d)", id);
		dev->dev_addr = LSM6DSV32X_ADDR_B;
		ok = read_reg(dev, REG_WHO_AM_I, &id);
		if (!ok || id != WHO_AM_I_VAL) {
			commands_printf("LSM6DSV32X address B failed (rx: %d)", id);
			return false;
		}
	}

	// Poll: run at the max ODR so the asynchronous poll always reads the freshest sample
	// (low, near-constant jitter). DRDY: quantize to the lowest ODR >= the requested rate so
	// each data-ready edge drives exactly one read; that ODR is then the real output rate.
	uint8_t odr_idx = ODR_LADDER_N - 1;
	if (dev->use_drdy) {
		odr_idx = 0;
		while (odr_idx < ODR_LADDER_N - 1 && odr_ladder[odr_idx].hz < dev->sample_rate_hz) {
			odr_idx++;
		}
		dev->sample_rate_hz = odr_ladder[odr_idx].hz; // report the real output rate
	}
	uint16_t odr_hz = odr_ladder[odr_idx].hz;
	uint8_t odr = odr_ladder[odr_idx].code;

	// Accel LPF2 low-pass anti-alias, choosing the widest cutoff <= target:
	// ODR/2 needs LPF2 disabled, ODR/{4,10,20,45,100,200,400,800} need it
	// enabled (datasheet table 69). The target is a fraction of the output rate
	// (sample_rate_hz): the requested rate in poll mode (the poll decimates the
	// max-ODR stream), the ODR itself in DRDY mode.
	//
	// The filter level sets the fraction: LOW ~ /2, MEDIUM ~ /4, HIGH ~ /8.
	uint8_t div = (filter == IMU_FILTER_HIGH) ? 8 : (filter == IMU_FILTER_MEDIUM) ? 4 : 2;
	uint16_t cutoff = dev->sample_rate_hz / div;

	bool lpf2_en = odr_hz / 2 > cutoff; // ODR/2 (LPF2 disabled) is the widest cutoff
	uint8_t lpf2_bw = 0;
	if (lpf2_en) {
		static const uint16_t lpf2_n[8] = {4, 10, 20, 45, 100, 200, 400, 800};
		lpf2_bw = 7; // narrowest (ODR/800) if nothing wider fits
		for (uint8_t code = 0; code < 8; code++) {
			if (odr_hz / lpf2_n[code] <= cutoff) {
				lpf2_bw = code;
				break;
			}
		}
	}

	// Block data update + register auto-increment.
	ok = ok && write_reg(dev, REG_CTRL3, CTRL3_BDU | CTRL3_IF_INC);

	// Accelerometer: Full scale + LPF2 low-pass anti-alias filter.
	ok = ok && write_reg(dev, REG_CTRL8, (lpf2_bw << 5) | CTRL8_MUST_SET | FS_XL_REG);
	ok = ok && write_reg(dev, REG_CTRL9, lpf2_en ? CTRL9_LPF2_XL_EN : 0);

	// Gyroscope: Full scale, LPF1 is bypassed at LOW (widest, ~530 Hz at the max ODR) and
	// enabled for MEDIUM (BW code 000, ~280 Hz) and HIGH (BW code 010, ~150 Hz); The
	// cutoffs scale down with the ODR, table 64 in datasheet.
	bool lpf1_en = filter != IMU_FILTER_LOW;
	uint8_t lpf1_bw = (filter == IMU_FILTER_HIGH) ? 0x2 : 0x0;
	ok = ok && write_reg(dev, REG_CTRL6, (lpf1_bw << 4) | FS_G_REG);
	ok = ok && write_reg(dev, REG_CTRL7, lpf1_en ? CTRL7_LPF1_G_EN : 0);

	// In DRDY mode mask the data-ready signal until the filters have settled.
	ok = ok && write_reg(dev, REG_CTRL4, dev->use_drdy ? CTRL4_DRDY_MASK : 0);

	// Enable the sensors last (writing the ODR powers them up in high-performance mode,
	// OP_MODE_XL/OP_MODE_G = 0).
	ok = ok && write_reg(dev, REG_CTRL1, odr);
	ok = ok && write_reg(dev, REG_CTRL2, odr);

	if (!ok) {
		commands_printf("LSM6DSV32X config failed");
		return false;
	}
	return true;
}

static bool read_sample(imu_device_t *dev, float accel[3], float gyro[3], float mag[3]) {
	uint8_t rxb[12];
	if (!transport_read_reg(dev->transport, dev->dev_addr, REG_OUTX_L_G, rxb, 12)) {
		return false;
	}

	gyro[0] = (int16_t)(rxb[1] << 8 | rxb[0]) * GYRO_DPS_PER_LSB;
	gyro[1] = (int16_t)(rxb[3] << 8 | rxb[2]) * GYRO_DPS_PER_LSB;
	gyro[2] = (int16_t)(rxb[5] << 8 | rxb[4]) * GYRO_DPS_PER_LSB;
	accel[0] = (int16_t)(rxb[7] << 8 | rxb[6]) * ACCEL_G_PER_LSB;
	accel[1] = (int16_t)(rxb[9] << 8 | rxb[8]) * ACCEL_G_PER_LSB;
	accel[2] = (int16_t)(rxb[11] << 8 | rxb[10]) * ACCEL_G_PER_LSB;

	// 6-axis part: no magnetometer. The AHRS ignores mag unless use_magnetometer is set,
	// which must not be enabled for this device.
	mag[0] = 0; mag[1] = 0; mag[2] = 0;

	return true;
}

static void enable_drdy_output(imu_device_t *dev, bool enable) {
	write_reg(dev, REG_INT1_CTRL, enable ? INT1_DRDY_G : 0);
}

static const imu_device_interface_t lsm6dsv32x_interface = {
	.name = "LSM6DSV32X",
	.configure = configure,
	.read_sample = read_sample,
	.on_read_fail = NULL,
	.enable_drdy_output = enable_drdy_output,
};

imu_device_t lsm6dsv32x_device(transport_t *transport) {
	return (imu_device_t){ .interface = &lsm6dsv32x_interface, .transport = transport };
}
