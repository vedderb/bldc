/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Marcos Chaparro	mchaparro@powerdesigns.ca
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

#include "enc_mt6816.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils_math.h"
#include "spi_bb.h"
#include "timer.h"

#include <math.h>
#include <string.h>

#define MT6816_NO_MAGNET_ERROR_MASK		0x0002

bool enc_mt6816_init(MT6816_config_t *cfg) {
	if (cfg->spi_dev == NULL) {
		return false;
	}

	memset(&cfg->state, 0, sizeof(MT6816_state));

	palSetPadMode(cfg->sck_gpio, cfg->sck_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->miso_gpio, cfg->miso_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->nss_gpio, cfg->nss_pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);

	spiStart(cfg->spi_dev, &(cfg->hw_spi_cfg));

	cfg->state.spi_error_rate = 0.0;
	cfg->state.encoder_no_magnet_error_rate = 0.0;

	return true;
}

void enc_mt6816_deinit(MT6816_config_t *cfg) {
	if (cfg->spi_dev == NULL) {
		return;
	}

	palSetPadMode(cfg->miso_gpio, cfg->miso_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(cfg->sck_gpio, cfg->sck_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(cfg->nss_gpio, cfg->nss_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin, PAL_MODE_INPUT_PULLUP);

	spiStop(cfg->spi_dev);

	cfg->state.last_enc_angle = 0.0;
	cfg->state.spi_error_rate = 0.0;
}

void enc_mt6816_routine(MT6816_config_t *cfg) {
	float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
	if (timestep > 1.0) {
		timestep = 1.0;
	}
	cfg->state.last_update_time = timer_time_now();

	uint16_t pos;
	uint16_t reg_data_03;
	uint16_t reg_data_04;
	uint16_t reg_addr_03 = 0x8300;
	uint16_t reg_addr_04 = 0x8400;

#define SPI_BEGIN()		spi_bb_delay(); palClearPad(cfg->nss_gpio, cfg->nss_pin); spi_bb_delay();
#define SPI_END()		spi_bb_delay(); palSetPad(cfg->nss_gpio, cfg->nss_pin); spi_bb_delay();

	// TODO: The fact that the polled version is used means that it is
	// more or less pointless to use the hardware SPI as the CPU sits
	// and wastes cycles waiting for the hardware to finish.
	//
	// A better approach would be to use spiStartExchangeI and use a callback
	// for when the operation finishes and process the data from there.
	SPI_BEGIN();
	reg_data_03 = spiPolledExchange(cfg->spi_dev, reg_addr_03);
	SPI_END();
	spi_bb_delay();
	SPI_BEGIN();
	reg_data_04 = spiPolledExchange(cfg->spi_dev, reg_addr_04);
	SPI_END();

	pos = (reg_data_03 << 8) | reg_data_04;
	cfg->state.spi_val = pos;

	if (spi_bb_check_parity(pos)) {
		if (pos & MT6816_NO_MAGNET_ERROR_MASK) {
			++cfg->state.encoder_no_magnet_error_cnt;
			UTILS_LP_FAST(cfg->state.encoder_no_magnet_error_rate, 1.0, timestep);
		} else {
			pos = pos >> 2;
			cfg->state.last_enc_angle = ((float) pos * 360.0) / 16384.0;
			UTILS_LP_FAST(cfg->state.spi_error_rate, 0.0, timestep);
			UTILS_LP_FAST(cfg->state.encoder_no_magnet_error_rate, 0.0, timestep);
		}
	} else {
		++cfg->state.spi_error_cnt;
		UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, timestep);
	}
}
