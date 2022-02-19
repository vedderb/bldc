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
#include "utils.h"
#include "spi_bb.h"
#include <math.h>

#define MT6816_NO_MAGNET_ERROR_MASK		0x0002

static MT6816_config_t MT6816_config_now = { 0 };

static float spi_error_rate = 0.0;
static float encoder_no_magnet_error_rate = 0.0;
static float encoder_no_magnet_error_cnt = 0.0;
static float last_enc_angle = 0.0;
static uint32_t spi_error_cnt = 0;
static uint32_t spi_val = 0;

encoder_ret_t enc_mt6816_init(MT6816_config_t *mt6816_config) {
	if (mt6816_config->spi_dev == NULL) {
		return ENCODER_ERROR;
	}

	MT6816_config_now = *mt6816_config;

	palSetPadMode(MT6816_config_now.sck_gpio, MT6816_config_now.sck_pin,
			PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(MT6816_config_now.miso_gpio, MT6816_config_now.miso_pin,
			PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);

	palSetPadMode(MT6816_config_now.nss_gpio, MT6816_config_now.nss_pin,
					PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	palSetPadMode(MT6816_config_now.mosi_gpio, MT6816_config_now.mosi_pin,
			PAL_MODE_ALTERNATE(6) | PAL_STM32_OSPEED_HIGHEST);

	spiStart(MT6816_config_now.spi_dev, &(MT6816_config_now.hw_spi_cfg));

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);
	spi_error_rate = 0.0;
	encoder_no_magnet_error_rate = 0.0;

	return ENCODER_OK;
}

void enc_mt6816_deinit(void) {
	if (MT6816_config_now.spi_dev == NULL) {
		return;
	}

	palSetPadMode(MT6816_config_now.miso_gpio,
			MT6816_config_now.miso_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(MT6816_config_now.sck_gpio,
			MT6816_config_now.sck_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(MT6816_config_now.nss_gpio,
			MT6816_config_now.nss_pin, PAL_MODE_INPUT_PULLUP);

	palSetPadMode(MT6816_config_now.mosi_gpio, MT6816_config_now.mosi_pin, PAL_MODE_INPUT_PULLUP);

	spiStop(MT6816_config_now.spi_dev);

	last_enc_angle = 0.0;
	spi_error_rate = 0.0;
}

float enc_mt6816_read_deg(void) {
	return last_enc_angle;
}

void enc_mt6816_routine(float rate) {
	uint16_t pos;
	uint16_t reg_data_03;
	uint16_t reg_data_04;
	uint16_t reg_addr_03 = 0x8300;
	uint16_t reg_addr_04 = 0x8400;

#define SPI_BEGIN()		spi_bb_delay(); palClearPad(MT6816_config_now.nss_gpio, MT6816_config_now.nss_pin); spi_bb_delay();
#define SPI_END()		spi_bb_delay(); palSetPad(MT6816_config_now.nss_gpio, MT6816_config_now.nss_pin); spi_bb_delay();

	SPI_BEGIN();
	reg_data_03 = spiPolledExchange(MT6816_config_now.spi_dev, reg_addr_03);
	SPI_END();
	spi_bb_delay();
	SPI_BEGIN();
	reg_data_04 = spiPolledExchange(MT6816_config_now.spi_dev, reg_addr_04);
	SPI_END();

	pos = (reg_data_03 << 8) | reg_data_04;
	spi_val = pos;

	if (spi_bb_check_parity(pos)) {
		if (pos & MT6816_NO_MAGNET_ERROR_MASK) {
			++encoder_no_magnet_error_cnt;
			UTILS_LP_FAST(encoder_no_magnet_error_rate, 1.0, 1.0 / rate);
		} else {
			pos = pos >> 2;
			last_enc_angle = ((float) pos * 360.0) / 16384.0;
			UTILS_LP_FAST(spi_error_rate, 0.0, 1.0 / rate);
			UTILS_LP_FAST(encoder_no_magnet_error_rate, 0.0, 1.0 / rate);
		}
	} else {
		++spi_error_cnt;
		UTILS_LP_FAST(spi_error_rate, 1.0, 1.0 / rate);
	}
}

uint32_t enc_mt6816_spi_get_val(void) {
	return spi_val;
}

uint32_t enc_mt6816_spi_get_error_cnt(void) {
	return spi_error_cnt;
}

uint32_t enc_mt6816_get_no_magnet_error_cnt(void) {
	return encoder_no_magnet_error_cnt;
}

uint32_t enc_mt6816_get_no_magnet_error_rate(void) {
	return encoder_no_magnet_error_rate;
}
