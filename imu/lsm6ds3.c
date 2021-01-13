/*
	Copyright 2020 Mitch Lustig

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
#include "terminal.h"
#include "i2c_bb.h"
#include "commands.h"
#include "utils.h"

#include <stdio.h>


static thread_t *lsm6ds3_thread_ref = NULL;
static i2c_bb_state m_i2c_bb;
static volatile uint16_t lsm6ds3_addr;
static int rate_hz = 1000;

static void terminal_read_reg(int argc, const char **argv);
static uint8_t read_single_reg(uint8_t reg);
static THD_FUNCTION(lsm6ds3_thread, arg);

// Function pointers
static void(*read_callback)(float *accel, float *gyro, float *mag) = 0;


void lsm6ds3_set_rate_hz(int hz) {
	rate_hz = hz;
}

void lsm6ds3_init(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin,
		stkalign_t *work_area, size_t work_area_size) {

	read_callback = 0;

	m_i2c_bb.sda_gpio = sda_gpio;
	m_i2c_bb.sda_pin = sda_pin;
	m_i2c_bb.scl_gpio = scl_gpio;
	m_i2c_bb.scl_pin = scl_pin;
	i2c_bb_init(&m_i2c_bb);

	uint8_t txb[2];
	uint8_t rxb[2];

	txb[0] = LSM6DS3_ACC_GYRO_WHO_AM_I_REG;
	lsm6ds3_addr = LSM6DS3_ACC_GYRO_ADDR_A;
	bool res = i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 1, rxb, 1);
	if (!res || rxb[0] != 0x69) {
		commands_printf("LSM6DS3 Address A failed, trying B");
		lsm6ds3_addr = LSM6DS3_ACC_GYRO_ADDR_B;
		res = i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 1, rxb, 1);
		if (!res || rxb[0] != 0x69) {
			commands_printf("LSM6DS3 Address B failed");
			return;
		}

	}

	// Configure imu
	// Set all accel speeds to MAX
	txb[0] = LSM6DS3_ACC_GYRO_CTRL1_XL;
	txb[1] = LSM6DS3_ACC_GYRO_BW_XL_400Hz | LSM6DS3_ACC_GYRO_FS_XL_16g | LSM6DS3_ACC_GYRO_ODR_XL_6660Hz;
	res = i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 2, rxb, 1);
	if(!res){
		commands_printf("LSM6DS3 Accel Config FAILED");
		return;
	}

	// Set all gyro speeds to MAX
	txb[0] = LSM6DS3_ACC_GYRO_CTRL2_G;
	txb[1] = LSM6DS3_ACC_GYRO_FS_G_2000dps | LSM6DS3_ACC_GYRO_ODR_G_1660Hz;
	res = i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 2, rxb, 1);
	if(!res){
		commands_printf("LSM6DS3 Gyro Config FAILED");
		return;
	}

	// Set ODR???
	txb[0] = LSM6DS3_ACC_GYRO_CTRL4_C;
	txb[1] = LSM6DS3_ACC_GYRO_BW_SCAL_ODR_ENABLED;
	res = i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 1, rxb, 1);
	if(!res){
		commands_printf("LSM6DS3 ODR Config FAILED");
		return;
	}

	terminal_register_command_callback(
				"lsm_read_reg",
				"Read register of the LSM6DS3",
				"[reg]",
				terminal_read_reg);

	lsm6ds3_thread_ref = chThdCreateStatic(work_area, work_area_size, NORMALPRIO, lsm6ds3_thread, NULL);
}

void lsm6ds3_stop(void) {
	if(lsm6ds3_thread_ref != NULL){
		chThdTerminate(lsm6ds3_thread_ref);
		chThdWait(lsm6ds3_thread_ref);
	}
	lsm6ds3_thread_ref = NULL;
	terminal_unregister_callback(terminal_read_reg);
}

void lsm6ds3_set_read_callback(void(*func)(float *accel, float *gyro, float *mag)) {
	read_callback = func;
}

static uint8_t read_single_reg(uint8_t reg) {
	uint8_t txb[2];
	uint8_t rxb[2];

	txb[0] = reg;
	bool res = i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 1, rxb, 2);

	if (res) {
		return rxb[0];
	} else {
		return 0;
	}
}

static void terminal_read_reg(int argc, const char **argv) {
	if (argc == 2) {
		int reg = -1;
		sscanf(argv[1], "%d", &reg);

		if (reg >= 0) {
			unsigned int res = read_single_reg(reg);

			char bl[9];
			utils_byte_to_binary(res & 0xFF, bl);

			commands_printf("Reg 0x%02x: %s (0x%02x)\n", reg, bl, res);
		} else {
			commands_printf("Invalid argument(s).\n");
		}
	} else {
		commands_printf("This command requires one argument.\n");
	}
}

static THD_FUNCTION(lsm6ds3_thread, arg) {
	(void)arg;
	chRegSetThreadName("LSM6SD3 Sampling");

	while (!chThdShouldTerminateX()) {

		uint8_t txb[2];
		uint8_t rxb[12];

		// Disable IMU writing to output registers
		txb[0] = LSM6DS3_ACC_GYRO_CTRL3_C;
		txb[1] = LSM6DS3_ACC_GYRO_BDU_BLOCK_UPDATE | LSM6DS3_ACC_GYRO_IF_INC_ENABLED;
		i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 2, rxb, 1);

		// Read IMU output registers
		txb[0] = LSM6DS3_ACC_GYRO_OUTX_L_G;
		bool res = i2c_bb_tx_rx(&m_i2c_bb, lsm6ds3_addr, txb, 1, rxb, 12);

		// Parse 6 axis values
		float gx = (float)((int16_t)((uint16_t)rxb[1] << 8) + rxb[0]) * 4.375 * (2000 / 125) / 1000;
		float gy = (float)((int16_t)((uint16_t)rxb[3] << 8) + rxb[2]) * 4.375 * (2000 / 125) / 1000;
		float gz = (float)((int16_t)((uint16_t)rxb[5] << 8) + rxb[4]) * 4.375 * (2000 / 125) / 1000;
		float ax = (float)((int16_t)((uint16_t)rxb[7] << 8) + rxb[6]) * 0.061 * (16 >> 1) / 1000;
		float ay = (float)((int16_t)((uint16_t)rxb[9] << 8) + rxb[8]) * 0.061 * (16 >> 1) / 1000;
		float az = (float)((int16_t)((uint16_t)rxb[11] << 8) + rxb[10]) * 0.061 * (16 >> 1) / 1000;

		if (res && read_callback) {
			float tmp_accel[3] = {ax,ay,az}, tmp_gyro[3] = {gx,gy,gz}, tmp_mag[3] = {1,2,3};
			read_callback(tmp_accel, tmp_gyro, tmp_mag);
		}

		// Delay between loops
		chThdSleepMilliseconds((int)((1000.0 / rate_hz)));
	}
}

