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

#include "icm20948.h"
#include "terminal.h"
#include "commands.h"
#include "utils_math.h"

#include <stdio.h>
#include <string.h>

// Threads
static THD_FUNCTION(icm_thread, arg);

// Private functions
static bool reset_init_icm(ICM20948_STATE *s);
static void terminal_read_reg(int argc, const char **argv);
static uint8_t read_single_reg(ICM20948_STATE *s, uint8_t reg);
static bool write_single_reg(ICM20948_STATE *s, uint8_t reg, uint8_t value);

// Private variables
static ICM20948_STATE *m_terminal_state = 0;

void icm20948_init(ICM20948_STATE *s, i2c_bb_state *i2c_state, int ad0_val,
		stkalign_t *work_area, size_t work_area_size) {

	s->i2cs = i2c_state;
	s->i2c_address = ad0_val ? 0x69 : 0x68;
	s->read_callback = 0;

	if (reset_init_icm(s)) {
		s->should_stop = false;
		chThdCreateStatic(work_area, work_area_size, NORMALPRIO, icm_thread, s);
	}

	// Only register terminal command for the first instance of this driver.
	if (m_terminal_state == 0) {
		m_terminal_state = s;
		terminal_register_command_callback(
				"icm_read_reg",
				"Read register of the ICM-20948",
				"[bank] [reg]",
				terminal_read_reg);
	}
}

void icm20948_set_read_callback(ICM20948_STATE *s, void(*func)(float *accel, float *gyro, float *mag)) {
	s->read_callback = func;
}

void icm20948_stop(ICM20948_STATE *s) {
	s->should_stop = true;
	while(s->is_running) {
		chThdSleep(1);
	}

	if (s == m_terminal_state) {
		terminal_unregister_callback(terminal_read_reg);
		m_terminal_state = 0;
	}
}

static void terminal_read_reg(int argc, const char **argv) {
	if (argc == 3) {
		int bank = -1;
		int reg = -1;
		sscanf(argv[1], "%d", &bank);
		sscanf(argv[2], "%d", &reg);

		if (reg >= 0 && (bank == 0 || bank == 1 || bank == 2)) {
			write_single_reg(m_terminal_state, ICM20948_BANK_SEL, bank << 4);
			unsigned int res = read_single_reg(m_terminal_state, reg);
			char bl[9];

			write_single_reg(m_terminal_state, ICM20948_BANK_SEL, 0 << 4);

			utils_byte_to_binary(res & 0xFF, bl);

			commands_printf("Reg 0x%02x: %s (0x%02x)\n", reg, bl, res);
		} else {
			commands_printf("Invalid argument(s).\n");
		}
	} else {
		commands_printf("This command requires one argument.\n");
	}
}

static bool write_single_reg(ICM20948_STATE *s, uint8_t reg, uint8_t value) {
	uint8_t txb[2];

	txb[0] = reg;
	txb[1] = value;

	bool res = i2c_bb_tx_rx(s->i2cs, s->i2c_address, txb, 2, 0, 0);
	return res;
}

static uint8_t read_single_reg(ICM20948_STATE *s, uint8_t reg) {
	uint8_t rxb[1];
	uint8_t txb[1];

	txb[0] = reg;
	bool res = i2c_bb_tx_rx(s->i2cs, s->i2c_address, txb, 1, rxb, 1);

	if (res) {
		return rxb[0];
	} else {
		return 0;
	}
}

static bool reset_init_icm(ICM20948_STATE *s) {
	i2c_bb_restore_bus(s->i2cs);

	chThdSleep(1);

	// TODO: Check for errors

	// Set clock source to auto
	write_single_reg(s, ICM20948_BANK_SEL, 0 << 4);
	write_single_reg(s, ICM20948_PWR_MGMT_1, 1);

	// Set accelerometer to +-16 G and disable lp filter
	write_single_reg(s, ICM20948_BANK_SEL, 2 << 4);
	write_single_reg(s, ICM20948_ACCEL_CONFIG, 0b00000110);

	// Set gyro to +-2000 dps and disable lp filter
	write_single_reg(s, ICM20948_BANK_SEL, 2 << 4);
	write_single_reg(s, ICM20948_GYRO_CONFIG_1, 0b00000110);

	// I2C bypass to access magnetometer directly
//	write_single_reg(s, ICM20948_BANK_SEL, 0);
//	write_single_reg(s, ICM20948_PIN_CFG, 2);

	// Select bank0 so that data can be polled.
	write_single_reg(s, ICM20948_BANK_SEL, 0 << 4);

	return true;
}

static THD_FUNCTION(icm_thread, arg) {
	ICM20948_STATE *s = (ICM20948_STATE*)arg;

	chRegSetThreadName("ICM Sampling");

	s->is_running = true;

	for(;;) {
		uint8_t txb[1];
		uint8_t rxb[12];
		txb[0] = ICM20948_ACCEL_XOUT_H;

		bool res = i2c_bb_tx_rx(s->i2cs, s->i2c_address, txb, 1, rxb, 12);

		if (res) {
			float accel[3], gyro[3], mag[3];

			accel[0] = (float)((int16_t)((int16_t)rxb[0] << 8 | (int16_t)rxb[1])) * 16.0 / 32768.0;
			accel[1] = (float)((int16_t)((int16_t)rxb[2] << 8 | (int16_t)rxb[3])) * 16.0 / 32768.0;
			accel[2] = (float)((int16_t)((int16_t)rxb[4] << 8 | (int16_t)rxb[5])) * 16.0 / 32768.0;

			gyro[0] = (float)((int16_t)((int16_t)rxb[6] << 8 | (int16_t)rxb[7])) * 2000.0 / 32768.0 ;
			gyro[1] = (float)((int16_t)((int16_t)rxb[8] << 8 | (int16_t)rxb[9])) * 2000.0 / 32768.0;
			gyro[2] = (float)((int16_t)((int16_t)rxb[10] << 8 | (int16_t)rxb[11])) * 2000.0 / 32768.0;

			// TODO: Read magnetometer as well
			memset(mag, 0, sizeof(mag));

			if (s->read_callback) {
				s->read_callback(accel, gyro, mag);
			}
		} else {
			reset_init_icm(s);
			chThdSleepMilliseconds(10);
		}

		if (s->should_stop) {
			s->is_running = false;
			return;
		}

		chThdSleepMicroseconds(1000000 / s->rate_hz);
	}
}
