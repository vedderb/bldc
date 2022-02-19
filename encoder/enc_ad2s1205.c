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

#include "enc_ad2s1205.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>
#include "spi_bb.h"

static AD2S1205_config_t AD2S1205_config_now = { 0 };

static uint16_t spi_val = 0;
static float resolver_loss_of_tracking_error_rate = 0.0;
static float resolver_degradation_of_signal_error_rate = 0.0;
static float resolver_loss_of_signal_error_rate = 0.0;
static uint32_t resolver_loss_of_tracking_error_cnt = 0;
static uint32_t resolver_degradation_of_signal_error_cnt = 0;
static uint32_t resolver_loss_of_signal_error_cnt = 0;
static uint32_t spi_error_cnt = 0;
static float spi_error_rate = 0.0;
static float last_enc_angle = 0.0;

void enc_ad2s1205_deinit(void) {
	spi_bb_deinit(&(AD2S1205_config_now.sw_spi));

#ifdef HW_SPI_DEV
	spiStop(&HW_SPI_DEV);
#endif

	// TODO: (TO BE TESTED!!) DEINITIALIZE ALSO SAMPLE AND RDVEL
#if defined(AD2S1205_SAMPLE_GPIO)
	palSetPadMode(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN, PAL_MODE_INPUT_PULLUP);	// Prepare for a falling edge SAMPLE assertion
#endif
#if defined(AD2S1205_RDVEL_GPIO)
	palSetPadMode(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN, PAL_MODE_INPUT_PULLUP);	// Will always read position
#endif
}

encoder_ret_t enc_ad2s1205_init(AD2S1205_config_t *AD2S1205_config) {
	AD2S1205_config_now = *AD2S1205_config;

	spi_bb_init(&(AD2S1205_config_now.sw_spi));

	resolver_loss_of_tracking_error_rate = 0.0;
	resolver_degradation_of_signal_error_rate = 0.0;
	resolver_loss_of_signal_error_rate = 0.0;
	resolver_loss_of_tracking_error_cnt = 0;
	resolver_loss_of_signal_error_cnt = 0;

	// TODO: Choose pins on comm port when these are not defined
#if defined(AD2S1205_SAMPLE_GPIO)
	palSetPadMode(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN);	// Prepare for a falling edge SAMPLE assertion
#endif

#if defined(AD2S1205_RDVEL_GPIO)
	palSetPadMode(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);		// Will always read position
#endif

	AD2S1205_config_now = *AD2S1205_config;

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	return ENCODER_OK;
}

float enc_ad2s1205_read_deg(void) {
	return last_enc_angle;
}

void enc_ad2s1205_routine(float rate) {
	uint16_t pos;
	// SAMPLE signal should have been be asserted in sync with ADC sampling
#ifdef AD2S1205_RDVEL_GPIO
	palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);	// Always read position
#endif

	palSetPad(AD2S1205_config_now.sw_spi.sck_gpio, AD2S1205_config_now.sw_spi.sck_pin);

	spi_bb_delay();
	spi_bb_begin(&(AD2S1205_config_now.sw_spi)); // CS uses the same mcu pin as AS5047
	spi_bb_delay();

	spi_bb_transfer_16(&(AD2S1205_config_now.sw_spi), &pos, 0, 1);
	spi_bb_end(&(AD2S1205_config_now.sw_spi));

	spi_val = pos;

	uint16_t RDVEL = pos & 0x0008; // 1 means a position read

	if ((RDVEL != 0)) {

		bool DOS = ((pos & 0x04) == 0);
		bool LOT = ((pos & 0x02) == 0);
		bool LOS = DOS && LOT;
		bool parity_error = spi_bb_check_parity(pos);//16 bit frame has odd parity
		bool angle_is_correct = true;

		if (LOS) {
			LOT = DOS = 0;
		}

		if (!parity_error) {
			UTILS_LP_FAST(spi_error_rate, 0.0, 1.0 / rate);
		} else {
			angle_is_correct = false;
			++spi_error_cnt;
			UTILS_LP_FAST(spi_error_rate, 1.0, 1.0 / rate);
		}

		pos &= 0xFFF0;
		pos = pos >> 4;
		pos &= 0x0FFF;

		if (LOT) {
			angle_is_correct = false;
			++resolver_loss_of_tracking_error_cnt;
			UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 1.0, 1.0 / rate);
		} else {
			UTILS_LP_FAST(resolver_loss_of_tracking_error_rate, 0.0, 1.0 / rate);
		}

		if (DOS) {
			angle_is_correct = false;
			++resolver_degradation_of_signal_error_cnt;
			UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 1.0, 1.0 / rate);
		} else {
			UTILS_LP_FAST(resolver_degradation_of_signal_error_rate, 0.0, 1.0 / rate);
		}

		if (LOS) {
			angle_is_correct = false;
			++resolver_loss_of_signal_error_cnt;
			UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 1.0, 1.0 / rate);
		} else {
			UTILS_LP_FAST(resolver_loss_of_signal_error_rate, 0.0, 1.0 / rate);
		}

		if (angle_is_correct) {
			last_enc_angle = ((float) pos * 360.0) / 4096.0;
		}
	}
}

float enc_ad2s1205_resolver_loss_of_tracking_error_rate(void) {
	return resolver_loss_of_tracking_error_rate;
}

float enc_ad2s1205_resolver_degradation_of_signal_error_rate(void) {
	return resolver_degradation_of_signal_error_rate;
}

float enc_ad2s1205_resolver_loss_of_signal_error_rate(void) {
	return resolver_loss_of_signal_error_rate;
}

uint32_t enc_ad2s1205_resolver_loss_of_tracking_error_cnt(void) {
	return resolver_loss_of_tracking_error_cnt;
}

uint32_t enc_ad2s1205_resolver_degradation_of_signal_error_cnt(void) {
	return resolver_degradation_of_signal_error_cnt;
}

uint32_t enc_ad2s1205_resolver_loss_of_signal_error_cnt(void) {
	return resolver_loss_of_signal_error_cnt;
}

uint32_t AD2S1205_spi_get_error_cnt(void) {
	return spi_error_cnt;
}
