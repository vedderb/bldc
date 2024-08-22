/*
	Copyright 2013 - 2019 Benjamin Vedder	benjamin@vedder.se

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

#include "conf_general.h"
#include "mpu9150.h"
#include "utils_math.h"
#include "stm32f4xx_conf.h"
#include "i2c_bb.h"
#include "terminal.h"
#include "commands.h"

#include <string.h>
#include <math.h>
#include <stdio.h>

// Settings
#define MPU_I2C_TIMEOUT			10
#define MAG_DIV 				10
#define FAIL_DELAY_US			1000
#define MIN_ITERATION_DELAY_US	500
#define MAX_IDENTICAL_READS		5
#define MPU_ADDR1				0x68
#define MPU_ADDR2				0x69

// Private variables
static unsigned char rx_buf[100];
static unsigned char tx_buf[100];
static volatile int16_t raw_accel_gyro_mag[9];
static volatile int16_t raw_accel_gyro_mag_no_offset[9];
static volatile int failed_reads;
static volatile int failed_mag_reads;
static volatile systime_t last_update_time;
static volatile systime_t update_time_diff;
static volatile int mag_updated;
static volatile uint16_t mpu_addr;
static volatile bool is_mpu9250;
static i2c_bb_state i2cs;
static volatile int16_t mpu9150_gyro_offsets[3];
static volatile bool mpu_found;
static volatile bool is_running;
static volatile bool should_stop;
static volatile int rate_hz = 200;
static volatile bool use_magnetometer = true;

// Private functions
static int reset_init_mpu(void);
static int get_raw_accel_gyro(int16_t* accel_gyro);
static uint8_t read_single_reg(uint8_t reg);
static int get_raw_mag(int16_t* mag);
static THD_FUNCTION(mpu_thread, arg);
static void terminal_status(int argc, const char **argv);
static void terminal_read_reg(int argc, const char **argv);
static thread_t *mpu_tp = 0;

// Function pointers
static void(*read_callback)(float *accel, float *gyro, float *mag) = 0;

void mpu9150_init(stm32_gpio_t *sda_gpio, int sda_pin,
		stm32_gpio_t *scl_gpio, int scl_pin,
		stkalign_t *work_area, size_t work_area_size) {

	failed_reads = 0;
	failed_mag_reads = 0;
	read_callback = 0;
	last_update_time = 0;
	update_time_diff = 0;
	mag_updated = 0;
	mpu_addr = MPU_ADDR1;
	is_mpu9250 = 0;
	is_running = false;
	should_stop = false;

	memset((void*)mpu9150_gyro_offsets, 0, sizeof(mpu9150_gyro_offsets));

	i2cs.sda_gpio = sda_gpio;
	i2cs.sda_pin = sda_pin;
	i2cs.scl_gpio = scl_gpio;
	i2cs.scl_pin = scl_pin;
	i2cs.rate = I2C_BB_RATE_400K;
	i2c_bb_init(&i2cs);

	reset_init_mpu();

	terminal_register_command_callback(
			"mpu_status",
			"Print status of the MPU 9150/9250",
			0,
			terminal_status);

	terminal_register_command_callback(
			"mpu_read_reg",
			"Read register of the MPU 9150/9250",
			"[reg]",
			terminal_read_reg);

	uint8_t res = read_single_reg(MPU9150_WHO_AM_I);
	if (res == 0x68 || res == 0x69 || res == 0x71 || res == 0x73) {
		mpu_found = true;
		if (!mpu_tp) {
			should_stop = false;
			chThdCreateStatic(work_area, work_area_size, NORMALPRIO, mpu_thread, NULL);
		}
	} else {
		mpu_found = false;
	}
}

static void terminal_status(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	if (mpu_found) {
		commands_printf(
				"Type       : %s\n"
				"Errors     : %i\n"
				"Errors Mag : %i\n",
				mpu9150_is_mpu9250() ? "MPU9250" : "MPU9150",
						mpu9150_get_failed_reads(),
						mpu9150_get_failed_mag_reads());
	} else {
		commands_printf("MPU9x50 not found\n");
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

void mpu9150_stop(void) {
	should_stop = true;
	while (is_running) {
		chThdSleep(1);
	}

	terminal_unregister_callback(terminal_status);
	terminal_unregister_callback(terminal_read_reg);
}

/**
 * Determine wether this is a MPU9150 or a MPU9250.
 *
 * @return
 * false: This is a MPU9150
 * true: This is a MPU9250
 */
bool mpu9150_is_mpu9250(void) {
	return is_mpu9250;
}

void mpu9150_set_read_callback(void(*func)(float *accel, float *gyro, float *mag)) {
	read_callback = func;
}

/**
 * Get the amount of milliseconds that have passed since
 * IMU values were received the last time.
 */
uint32_t mpu9150_get_time_since_update(void) {
	return (systime_t)((float)chVTTimeElapsedSinceX(last_update_time) /
			((float)CH_CFG_ST_FREQUENCY / 1000.0));
}

/*
 * Get the amount of milliseconds it took to read one sample of every axis.
 */
float mpu9150_get_last_sample_duration(void) {
	return (float)update_time_diff / (float)CH_CFG_ST_FREQUENCY * 1000.0;
}

int mpu9150_get_failed_reads(void) {
	return failed_reads;
}

int mpu9150_get_failed_mag_reads(void) {
	return failed_mag_reads;
}

/*
 * Returns 1 if the magnetometer was updated in the latest iteration, 0 otherwise.
 */
int mpu9150_mag_updated(void) {
	return mag_updated;
}

void mpu9150_sample_gyro_offsets(uint32_t iteratons) {
	int32_t offsets[3];
	memset(offsets, 0, sizeof(offsets));

	for(uint32_t i = 0;i < iteratons;i++) {
		offsets[0] += raw_accel_gyro_mag_no_offset[3];
		offsets[1] += raw_accel_gyro_mag_no_offset[4];
		offsets[2] += raw_accel_gyro_mag_no_offset[5];
		chThdSleepMilliseconds(10);
	}

	offsets[0] /= (int32_t)iteratons;
	offsets[1] /= (int32_t)iteratons;
	offsets[2] /= (int32_t)iteratons;

	mpu9150_gyro_offsets[0] = offsets[0];
	mpu9150_gyro_offsets[1] = offsets[1];
	mpu9150_gyro_offsets[2] = offsets[2];
}

void mpu9150_get_raw_accel_gyro_mag(int16_t *accel_gyro_mag) {
	memcpy(accel_gyro_mag, (int16_t*)raw_accel_gyro_mag, sizeof(raw_accel_gyro_mag));
}

void mpu9150_get_accel(float *accel) {
	accel[0] = (float)raw_accel_gyro_mag[0] * 16.0 / 32768.0;
	accel[1] = (float)raw_accel_gyro_mag[1] * 16.0 / 32768.0;
	accel[2] = (float)raw_accel_gyro_mag[2] * 16.0 / 32768.0;
}

void mpu9150_get_gyro(float *gyro) {
	gyro[0] = (float)raw_accel_gyro_mag[3] * 2000.0 / 32768.0;
	gyro[1] = (float)raw_accel_gyro_mag[4] * 2000.0 / 32768.0;
	gyro[2] = (float)raw_accel_gyro_mag[5] * 2000.0 / 32768.0;
}

void mpu9150_get_mag(float *mag) {
	if(use_magnetometer){
		mag[0] = (float)raw_accel_gyro_mag[6] * 1200.0 / 4096.0;
		mag[1] = (float)raw_accel_gyro_mag[7] * 1200.0 / 4096.0;
		mag[2] = (float)raw_accel_gyro_mag[8] * 1200.0 / 4096.0;
	} else {
		mag[0] = 0.0;
		mag[1] = 0.0;
		mag[2] = 0.0;
	}
}

void mpu9150_get_accel_gyro_mag(float *accel, float *gyro, float *mag) {
	mpu9150_get_accel(accel);
	mpu9150_get_gyro(gyro);
	mpu9150_get_mag(mag);
}

void mpu9150_set_rate_hz(int hz) {
	rate_hz = hz;
}

void mpu9150_set_mag_enabled(bool enabled) {
	use_magnetometer = enabled;
}

static THD_FUNCTION(mpu_thread, arg) {
	(void)arg;
	chRegSetThreadName("MPU Sampling");

	is_running = true;
	mpu_tp = chThdGetSelfX();

	static int16_t raw_accel_gyro_mag_tmp[9];
	static int mag_cnt = MAG_DIV;
	static systime_t iteration_timer = 0;
	static int identical_reads = 0;

	iteration_timer = chVTGetSystemTimeX();

	for(;;) {
		if (should_stop) {
			is_running = false;
			mpu_tp = 0;
			return;
		}

		if (get_raw_accel_gyro(raw_accel_gyro_mag_tmp)) {
			int is_identical = 1;
			for (int i = 0;i < 6;i++) {
				if (raw_accel_gyro_mag_tmp[i] != raw_accel_gyro_mag_no_offset[i]) {
					is_identical = 0;
					break;
				}
			}

			if (is_identical) {
				identical_reads++;
			} else {
				identical_reads = 0;
			}

			if (identical_reads >= MAX_IDENTICAL_READS) {
				failed_reads++;
				chThdSleepMicroseconds(FAIL_DELAY_US);
				reset_init_mpu();
				iteration_timer = chVTGetSystemTimeX();
			} else {
				memcpy((uint16_t*)raw_accel_gyro_mag_no_offset, raw_accel_gyro_mag_tmp, sizeof(raw_accel_gyro_mag));
				raw_accel_gyro_mag_tmp[3] -= mpu9150_gyro_offsets[0];
				raw_accel_gyro_mag_tmp[4] -= mpu9150_gyro_offsets[1];
				raw_accel_gyro_mag_tmp[5] -= mpu9150_gyro_offsets[2];
				memcpy((uint16_t*)raw_accel_gyro_mag, raw_accel_gyro_mag_tmp, sizeof(raw_accel_gyro_mag));

				update_time_diff = chVTGetSystemTimeX() - last_update_time;
				last_update_time = chVTGetSystemTimeX();

				if (read_callback) {
					float tmp_accel[3], tmp_gyro[3], tmp_mag[3];
					mpu9150_get_accel_gyro_mag(tmp_accel, tmp_gyro, tmp_mag);
					read_callback(tmp_accel, tmp_gyro, tmp_mag);
				}

				if(use_magnetometer){
					mag_cnt++;
					if (mag_cnt >= MAG_DIV) {
						mag_cnt = 0;
						mag_updated = 1;

						int16_t raw_mag_tmp[3];

						if (get_raw_mag(raw_mag_tmp)) {
							memcpy((uint16_t*)raw_accel_gyro_mag_tmp + 6, raw_mag_tmp, sizeof(raw_mag_tmp));
						} else {
							failed_mag_reads++;
							chThdSleepMicroseconds(FAIL_DELAY_US);
							reset_init_mpu();
							iteration_timer = chVTGetSystemTimeX();
						}
					} else {
						mag_updated = 0;
					}
				}
			}
		} else {
			failed_reads++;
			chThdSleepMicroseconds(FAIL_DELAY_US);
			reset_init_mpu();
			iteration_timer = chVTGetSystemTimeX();
		}

		iteration_timer += US2ST(1000000 / rate_hz);
		systime_t time_start = chVTGetSystemTimeX();
		if (iteration_timer > time_start) {
			chThdSleep(iteration_timer - time_start);
		} else {
			chThdSleepMicroseconds(MIN_ITERATION_DELAY_US);
			iteration_timer = chVTGetSystemTimeX();
		}
	}
}

static int reset_init_mpu(void) {
	i2c_bb_restore_bus(&i2cs);

	// Set clock source to gyro x
	tx_buf[0] = MPU9150_PWR_MGMT_1;
	tx_buf[1] = 0x01;
	bool res = i2c_bb_tx_rx(&i2cs, mpu_addr, tx_buf, 2, rx_buf, 0);

	// Try the other address
	if (!res) {
		if (mpu_addr == MPU_ADDR1) {
			mpu_addr = MPU_ADDR2;
		} else {
			mpu_addr = MPU_ADDR1;
		}

		// Set clock source to gyro x
		tx_buf[0] = MPU9150_PWR_MGMT_1;
		tx_buf[1] = 0x01;
		res = i2c_bb_tx_rx(&i2cs, mpu_addr, tx_buf, 2, rx_buf, 0);

		if (!res) {
			return 0;
		}
	}

	// Set accelerometer full-scale range to +/- 16g
	tx_buf[0] = MPU9150_ACCEL_CONFIG;
	tx_buf[1] = MPU9150_ACCEL_FS_16 << MPU9150_ACONFIG_AFS_SEL_BIT;
	res = i2c_bb_tx_rx(&i2cs, mpu_addr, tx_buf, 2, rx_buf, 0);

	if (!res) {
		return 0;
	}

	// Set gyroscope full-scale range to +/- 2000 deg/s
	tx_buf[0] = MPU9150_GYRO_CONFIG;
	tx_buf[1] = MPU9150_GYRO_FS_2000 << MPU9150_GCONFIG_FS_SEL_BIT;
	res = i2c_bb_tx_rx(&i2cs, mpu_addr, tx_buf, 2, rx_buf, 0);

	if (!res) {
		return 0;
	}

	// Set low pass filter to 256Hz (1ms delay)
	tx_buf[0] = MPU9150_CONFIG;
	tx_buf[1] = MPU9150_DLPF_BW_256;
	res = i2c_bb_tx_rx(&i2cs, mpu_addr, tx_buf, 2, rx_buf, 0);

	if (!res) {
		return 0;
	}

	if(use_magnetometer){
		// Set the i2c bypass enable pin to true to access the magnetometer
		tx_buf[0] = MPU9150_INT_PIN_CFG;
		tx_buf[1] = 0x02;
		res = i2c_bb_tx_rx(&i2cs, mpu_addr, tx_buf, 2, rx_buf, 0);

		if (!res) {
			return 0;
		}
	}

	is_mpu9250 = read_single_reg(MPU9150_WHO_AM_I) == 0x71;

	return 1;
}

static int get_raw_accel_gyro(int16_t* accel_gyro) {
	tx_buf[0] = MPU9150_ACCEL_XOUT_H;
	bool res = i2c_bb_tx_rx(&i2cs, mpu_addr, tx_buf, 1, rx_buf, 14);

	if (!res) {
		return 0;
	}

	// Acceleration
	for (int i = 0;i < 3; i++) {
		accel_gyro[i] = ((int16_t) ((uint16_t) rx_buf[2 * i] << 8)
				+ rx_buf[2 * i + 1]);
	}

	// Angular rate
	for (int i = 4;i < 7; i++) {
		accel_gyro[i - 1] = ((int16_t) ((uint16_t) rx_buf[2 * i] << 8)
				+ rx_buf[2 * i + 1]);
	}

	return 1;
}

static uint8_t read_single_reg(uint8_t reg) {
	uint8_t rxb[2];
	uint8_t txb[2];

	txb[0] = reg;
	bool res = i2c_bb_tx_rx(&i2cs, mpu_addr, txb, 1, rxb, 1);

	if (res) {
		return rxb[0];
	} else {
		return 0;
	}
}

static int get_raw_mag(int16_t* mag) {
	tx_buf[0] = MPU9150_HXL;
	bool res = i2c_bb_tx_rx(&i2cs, 0x0C, tx_buf, 1, rx_buf, 6);

	if (!res) {
		return 0;
	}

	for (int i = 0; i < 3; i++) {
		mag[i] = ((int16_t) ((uint16_t) rx_buf[2 * i + 1] << 8) + rx_buf[2 * i]);
	}

	// Start the measurement for the next iteration
	tx_buf[0] = MPU9150_CNTL;
	tx_buf[1] = 0x01;
	res = i2c_bb_tx_rx(&i2cs, 0x0C, tx_buf, 2, rx_buf, 0);

	if (!res) {
		return 0;
	}

	return 1;
}
