/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Jakub Tomczak

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

#include "enc_as504x.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils_math.h"
#include "spi_bb.h"
#include "timer.h"

#include <string.h>
#include <math.h>

#define AS504x_SPI_READ_BIT 								0x4000
#define AS504x_SPI_WRITE_BIT 								0x0000

#define AS504x_SPI_DIAG_OCF_BIT_POS							8
#define AS504x_SPI_DIAG_COF_BIT_POS							9
#define AS504x_SPI_DIAG_COMP_LOW_BIT_POS					10
#define AS504x_SPI_DIAG_COMP_HIGH_BIT_POS					11

#define AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK			0x3FFF

#define AS504x_SPI_DIAG_ADR									0x3FFD
#define AS504x_SPI_MAGN_ADR									0x3FFE
#define AS504x_SPI_CLEAR_ERROR_ADR							0x0001

#define AS504x_SPI_READ_DIAG_MSG							(AS504x_SPI_DIAG_ADR | AS504x_SPI_READ_BIT)
#define AS504x_SPI_READ_MAGN_MSG							(AS504x_SPI_MAGN_ADR | AS504x_SPI_READ_BIT)
#define AS504x_SPI_READ_CLEAR_ERROR_MSG						(AS504x_SPI_CLEAR_ERROR_ADR | AS504x_SPI_READ_BIT)

#define AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD		5

#define AS504x_DATA_INVALID_THRESHOLD						20000
#define AS504x_REFRESH_DIAG_AFTER_NSAMPLES					100

// Private functions
static void long_delay(void);
static uint8_t AS504x_fetch_diag(AS504x_config_t *cfg);
static uint8_t AS504x_verify_serial(AS504x_config_t *cfg);
static void AS504x_deserialize_diag(AS504x_config_t *cfg);
static void AS504x_fetch_clear_err_diag(AS504x_config_t *cfg);
static uint8_t AS504x_spi_transfer_err_check(spi_bb_state *sw_spi,
		uint16_t *in_buf, const uint16_t *out_buf, int length);
static void AS504x_determinate_if_connected(AS504x_config_t *cfg, bool was_last_valid);

bool enc_as504x_init(AS504x_config_t *cfg) {
	memset(&cfg->state, 0, sizeof(AS504x_state));
	spi_bb_init(&(cfg->sw_spi));
	return true;
}

void enc_as504x_deinit(AS504x_config_t *cfg) {
	spi_bb_deinit(&(cfg->sw_spi));
	cfg->state.last_enc_angle = 0.0;
	cfg->state.spi_error_rate = 0.0;
}

void enc_as504x_routine(AS504x_config_t *cfg) {
	uint16_t pos;

	float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
	if (timestep > 1.0) {
		timestep = 1.0;
	}
	cfg->state.last_update_time = timer_time_now();

	// if MOSI is defined, use diagnostics
	if (cfg->sw_spi.mosi_gpio != 0) {
		spi_bb_begin(&(cfg->sw_spi));
		spi_bb_transfer_16(&(cfg->sw_spi), 0, 0, 1);
		spi_bb_end(&(cfg->sw_spi));

		long_delay();

		spi_bb_begin(&(cfg->sw_spi));
		cfg->state.spi_data_err_raised = AS504x_spi_transfer_err_check(&cfg->sw_spi, &pos, 0, 1);
		spi_bb_end(&(cfg->sw_spi));
		cfg->state.spi_val = pos;

		// get diagnostic every AS504x_REFRESH_DIAG_AFTER_NSAMPLES
		cfg->state.diag_fetch_now_count++;
		if (cfg->state.diag_fetch_now_count >= AS504x_REFRESH_DIAG_AFTER_NSAMPLES ||
				cfg->state.spi_data_err_raised) {
			// clear error flags before getting new diagnostics data
			AS504x_fetch_clear_err_diag(cfg);

			if (!AS504x_fetch_diag(cfg)) {
				if (!AS504x_verify_serial(cfg)) {
					AS504x_deserialize_diag(cfg);
					AS504x_determinate_if_connected(cfg, true);
				} else {
					AS504x_determinate_if_connected(cfg, false);
				}
			} else {
				AS504x_determinate_if_connected(cfg, false);
			}
			cfg->state.diag_fetch_now_count = 0;
		}
	} else {
		spi_bb_begin(&(cfg->sw_spi));
		spi_bb_transfer_16(&(cfg->sw_spi), &pos, 0, 1);
		spi_bb_end(&(cfg->sw_spi));
		cfg->state.spi_val = pos;

		if(0x0000 == pos || 0xFFFF == pos) {
			cfg->state.data_last_invalid_counter++;
		} else {
			cfg->state.data_last_invalid_counter = 0;
			AS504x_determinate_if_connected(cfg, true);
		}

		if (cfg->state.data_last_invalid_counter >= AS504x_DATA_INVALID_THRESHOLD) {
			AS504x_determinate_if_connected(cfg, false);
			cfg->state.data_last_invalid_counter = AS504x_DATA_INVALID_THRESHOLD;
		}
	}

	if (spi_bb_check_parity(pos) && !cfg->state.spi_data_err_raised) {
		pos &= 0x3FFF;
		cfg->state.last_enc_angle = ((float) pos * 360.0) / 16384.0;
		UTILS_LP_FAST(cfg->state.spi_error_rate, 0.0, timestep);
	} else {
		++cfg->state.spi_error_cnt;
		UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, timestep);
	}
}

float enc_as504x_read_angle(AS504x_config_t *cfg) {
	enc_as504x_routine(cfg);
	return cfg->state.last_enc_angle;
}

static void long_delay(void) {
	for (volatile int i = 0; i < 40; i++) {
		__NOP();
	}
}

static uint8_t AS504x_verify_serial(AS504x_config_t *cfg) {
	uint16_t serial_diag_flgs, serial_magnitude, test_magnitude;
	uint8_t test_AGC_value, test_is_Comp_high, test_is_Comp_low;

	serial_magnitude = cfg->state.sensor_diag.serial_magnitude;
	serial_diag_flgs = cfg->state.sensor_diag.serial_diag_flgs;

	test_magnitude = serial_magnitude
			& AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
	test_AGC_value = serial_diag_flgs;
	test_is_Comp_low = (serial_diag_flgs >> AS504x_SPI_DIAG_COMP_LOW_BIT_POS)
			& 1;
	test_is_Comp_high = (serial_diag_flgs >> AS504x_SPI_DIAG_COMP_HIGH_BIT_POS)
			& 1;

	if (test_is_Comp_high && test_is_Comp_low) {
		return 1;
	}
	if ((uint32_t) test_magnitude + (uint32_t) test_AGC_value == 0) {
		return 1;
	}

	return 0;
}

static uint8_t AS504x_fetch_diag(AS504x_config_t *cfg) {
	uint16_t recf[2], senf[2] = { AS504x_SPI_READ_DIAG_MSG,
			AS504x_SPI_READ_MAGN_MSG };
	uint8_t ret = 0;

	spi_bb_begin(&(cfg->sw_spi));
	spi_bb_transfer_16(&(cfg->sw_spi), 0, senf, 1);
	spi_bb_end(&(cfg->sw_spi));

	long_delay();

	spi_bb_begin(&(cfg->sw_spi));
	ret |= AS504x_spi_transfer_err_check(&(cfg->sw_spi), recf, senf + 1, 1);
	spi_bb_end(&(cfg->sw_spi));

	long_delay();

	spi_bb_begin(&(cfg->sw_spi));
	ret |= AS504x_spi_transfer_err_check(&(cfg->sw_spi), recf + 1, 0, 1);
	spi_bb_end(&(cfg->sw_spi));

	if (!ret) {
		if (spi_bb_check_parity(recf[0]) && spi_bb_check_parity(recf[1])) {
			cfg->state.sensor_diag.serial_diag_flgs = recf[0];
			cfg->state.sensor_diag.serial_magnitude = recf[1];
		}
	}

	return ret;
}

static void AS504x_deserialize_diag(AS504x_config_t *cfg) {
	cfg->state.sensor_diag.AGC_value = cfg->state.sensor_diag.serial_diag_flgs;
	cfg->state.sensor_diag.is_OCF = (cfg->state.sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_OCF_BIT_POS) & 1;
	cfg->state.sensor_diag.is_COF = (cfg->state.sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COF_BIT_POS) & 1;
	cfg->state.sensor_diag.is_Comp_low = (cfg->state.sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COMP_LOW_BIT_POS) & 1;
	cfg->state.sensor_diag.is_Comp_high = (cfg->state.sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COMP_HIGH_BIT_POS) & 1;
	cfg->state.sensor_diag.magnitude = cfg->state.sensor_diag.serial_magnitude
			& AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
}

static void AS504x_fetch_clear_err_diag(AS504x_config_t *cfg) {
	uint16_t recf, senf = AS504x_SPI_READ_CLEAR_ERROR_MSG;

	spi_bb_begin(&(cfg->sw_spi));
	spi_bb_transfer_16(&(cfg->sw_spi), 0, &senf, 1);
	spi_bb_end(&(cfg->sw_spi));

	long_delay();

	spi_bb_begin(&(cfg->sw_spi));
	spi_bb_transfer_16(&(cfg->sw_spi), &recf, 0, 1);
	spi_bb_end(&(cfg->sw_spi));

	cfg->state.sensor_diag.serial_error_flags = recf;
}

static uint8_t AS504x_spi_transfer_err_check(spi_bb_state *sw_spi,
		uint16_t *in_buf, const uint16_t *out_buf, int length) {
	spi_bb_transfer_16(sw_spi, in_buf, out_buf, length);

	for (int len_count = 0; len_count < length; len_count++) {
		if (((in_buf[len_count]) >> 14) & 0b01) {
			return 1;
		}
	}

	return 0;
}

static void AS504x_determinate_if_connected(AS504x_config_t *cfg, bool was_last_valid) {
	if (!was_last_valid) {
		cfg->state.spi_communication_error_count++;

		if (cfg->state.spi_communication_error_count
				>= AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD) {
			cfg->state.spi_communication_error_count =
					AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD;
			cfg->state.sensor_diag.is_connected = 0;
		}
	} else {
		if (cfg->state.spi_communication_error_count) {
			cfg->state.spi_communication_error_count--;
		} else {
			cfg->state.sensor_diag.is_connected = 1;
		}
	}
}
