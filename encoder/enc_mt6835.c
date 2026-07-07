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

#include "enc_mt6835.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "utils_math.h"
#include "timer.h"

#include <math.h>
#include <string.h>

#define MT6835_BURST_CMD		0xA0
#define MT6835_BURST_ADDR		0x03
#define MT6835_BURST_LEN		6

#define MT6835_ANGLE_RES		2097152.0

bool enc_mt6835_init(MT6835_config_t *cfg) {
	if (cfg->spi_dev == NULL) {
		return false;
	}

	memset(&cfg->state, 0, sizeof(MT6835_state));

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

	return true;
}

void enc_mt6835_deinit(MT6835_config_t *cfg) {
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

static uint8_t mt6835_crc8(uint8_t *data, int len) {
	uint8_t crc = 0x00;
	for (int i = 0; i < len; i++) {
		crc ^= data[i];
		for (int j = 0; j < 8; j++) {
			if (crc & 0x80) {
				crc = (crc << 1) ^ 0x07;
			} else {
				crc <<= 1;
			}
		}
	}
	return crc;
}

void enc_mt6835_routine(MT6835_config_t *cfg) {
	float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
	if (timestep > 1.0) {
		timestep = 1.0;
	}
	cfg->state.last_update_time = timer_time_now();

	uint8_t tx_buf[MT6835_BURST_LEN];
	uint8_t rx_buf[MT6835_BURST_LEN];

	tx_buf[0] = MT6835_BURST_CMD;
	tx_buf[1] = MT6835_BURST_ADDR;
	tx_buf[2] = 0xFF;
	tx_buf[3] = 0xFF;
	tx_buf[4] = 0xFF;
	tx_buf[5] = 0xFF;

	spiSelect(cfg->spi_dev);
	spiExchange(cfg->spi_dev, MT6835_BURST_LEN, tx_buf, rx_buf);
	spiUnselect(cfg->spi_dev);

	uint8_t status = rx_buf[4] & 0x07;
	uint8_t crc_rx = rx_buf[5];
	uint8_t crc_calc = mt6835_crc8(&rx_buf[2], 3);

	uint32_t angle_raw = ((uint32_t)rx_buf[2] << 13) |
						 ((uint32_t)rx_buf[3] << 5) |
						 ((uint32_t)rx_buf[4] >> 3);

	if (crc_rx != crc_calc || status != 0) {
		cfg->state.spi_error_cnt++;
		UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, timestep);
	} else {
		cfg->state.spi_val = angle_raw;
		cfg->state.last_enc_angle = ((float)angle_raw * 360.0) / MT6835_ANGLE_RES;
		UTILS_LP_FAST(cfg->state.spi_error_rate, 0.0, timestep);
	}
}
