/*
	Copyright 2026 Benjamin Vedder	benjamin@vedder.se

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

#include "enc_amt22.h"

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

// Private functions
static bool verify_checksum(uint16_t message);

bool enc_amt22_init(AMT22_config_t *cfg) {
	memset(&cfg->state, 0, sizeof(AS504x_state));
	spi_bb_init(&(cfg->sw_spi));
	return true;
}

void enc_amt22_deinit(AMT22_config_t *cfg) {
	spi_bb_deinit(&(cfg->sw_spi));
	cfg->state.last_enc_angle = 0.0;
	cfg->state.spi_error_rate = 0.0;
}

void enc_amt22_routine(AMT22_config_t *cfg) {
	uint16_t pos;

	float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
	if (timestep > 1.0) {
		timestep = 1.0;
	}
	cfg->state.last_update_time = timer_time_now();

	spi_bb_begin(&(cfg->sw_spi));
	// Use two 8-bit transfers to respect TB
	uint16_t b0 = spi_bb_exchange_8(&(cfg->sw_spi), 0x00);
	spi_bb_delay();
	spi_bb_delay();
	uint16_t b1 = spi_bb_exchange_8(&(cfg->sw_spi), 0x00);
	spi_bb_end(&(cfg->sw_spi));

	pos = b0 << 8 | b1;
	cfg->state.spi_val = pos;

	if (verify_checksum(pos)) {
		pos &= 0x3FFF;
		cfg->state.last_enc_angle = ((float) pos * 360.0) / 16384.0;
		UTILS_LP_FAST(cfg->state.spi_error_rate, 0.0, timestep);
	} else {
		++cfg->state.spi_error_cnt;
		UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, timestep);
	}
}

float enc_amt22_read_angle(AMT22_config_t *cfg) {
	enc_amt22_routine(cfg);
	return cfg->state.last_enc_angle;
}

static bool verify_checksum(uint16_t message) {
	uint16_t checksum = 0x3;

	for (int i = 0; i < 14; i += 2) {
		checksum ^= (message >> i) & 0x3;
	}

	return checksum == (message >> 14);
}

