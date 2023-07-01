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
#include "utils_math.h"
#include "spi_bb.h"
#include "timer.h"

#include <string.h>
#include <math.h>

bool enc_ad2s1205_init(AD2S1205_config_t *cfg) {
	spi_bb_init(&(cfg->sw_spi));

	memset(&cfg->state, 0, sizeof(AD2S1205_state));

	// TODO: Choose pins on comm port when these are not defined
#if defined(AD2S1205_SAMPLE_GPIO)
	palSetPadMode(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN);	// Prepare for a falling edge SAMPLE assertion
#endif

#if defined(AD2S1205_RDVEL_GPIO)
	palSetPadMode(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);		// Will always read position
#endif

	return true;
}

void enc_ad2s1205_deinit(AD2S1205_config_t *cfg) {
	spi_bb_deinit(&(cfg->sw_spi));

#if defined(AD2S1205_SAMPLE_GPIO)
	palSetPadMode(AD2S1205_SAMPLE_GPIO, AD2S1205_SAMPLE_PIN, PAL_MODE_INPUT_PULLUP);	// Prepare for a falling edge SAMPLE assertion
#endif
#if defined(AD2S1205_RDVEL_GPIO)
	palSetPadMode(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN, PAL_MODE_INPUT_PULLUP);	// Will always read position
#endif
}

void enc_ad2s1205_routine(AD2S1205_config_t *cfg) {
	uint16_t pos;

	float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
	if (timestep > 1.0) {
		timestep = 1.0;
	}
	cfg->state.last_update_time = timer_time_now();

	// SAMPLE signal should have been be asserted in sync with ADC sampling
#ifdef AD2S1205_RDVEL_GPIO
	palSetPad(AD2S1205_RDVEL_GPIO, AD2S1205_RDVEL_PIN);	// Always read position
#endif

	palSetPad(cfg->sw_spi.sck_gpio, cfg->sw_spi.sck_pin);

	spi_bb_delay();
	spi_bb_begin(&(cfg->sw_spi));
	spi_bb_delay();

	spi_bb_transfer_16(&(cfg->sw_spi), &pos, 0, 1);
	spi_bb_end(&(cfg->sw_spi));

	cfg->state.spi_val = pos;

	uint16_t RDVEL = pos & 0x0008;

	if(cfg->state.spi_val == 0){ // an empty SPI packet means that the resolver IC is not responding
		++cfg->state.resolver_void_packet_cnt;
		UTILS_LP_FAST(cfg->state.resolver_void_packet_error_rate, 1.0, timestep);
		if(cfg->state.resolver_void_packet_error_rate > cfg->state.resolver_VOIDspi_peak_error_rate){
				cfg->state.resolver_VOIDspi_peak_error_rate = cfg->state.resolver_void_packet_error_rate;
			}
	}else{
		UTILS_LP_FAST(cfg->state.resolver_void_packet_error_rate, 0.0, timestep);
		if(RDVEL == 0){
            ++cfg->state.resolver_vel_packet_cnt;
            UTILS_LP_FAST(cfg->state.resolver_vel_packet_error_rate, 1.0, timestep);
			if(cfg->state.resolver_vel_packet_error_rate > cfg->state.resolver_VELread_peak_error_rate ) {
				cfg->state.resolver_VELread_peak_error_rate = cfg->state.resolver_vel_packet_error_rate;
			}
		}else{
			UTILS_LP_FAST(cfg->state.resolver_vel_packet_error_rate, 0.0, timestep);
		}
	}

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
			UTILS_LP_FAST(cfg->state.spi_error_rate, 0.0, timestep);
		} else {
			angle_is_correct = false;
			++cfg->state.spi_error_cnt;
			UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, timestep);
			if(cfg->state.spi_error_rate > cfg->state.resolver_SPI_peak_error_rate){
				cfg->state.resolver_SPI_peak_error_rate = cfg->state.spi_error_rate;
			}
		}

		pos &= 0xFFF0;
		pos = pos >> 4;
		pos &= 0x0FFF;

		if (LOT) {
			angle_is_correct = false;
			++cfg->state.resolver_loss_of_tracking_error_cnt;
			UTILS_LP_FAST(cfg->state.resolver_loss_of_tracking_error_rate, 1.0, timestep);
			if(cfg->state.resolver_loss_of_tracking_error_rate > cfg->state.resolver_LOT_peak_error_rate) {
				cfg->state.resolver_LOT_peak_error_rate = cfg->state.resolver_loss_of_tracking_error_rate;
			}
		} else {
			UTILS_LP_FAST(cfg->state.resolver_loss_of_tracking_error_rate, 0.0, timestep);
		}

		if (DOS) {
			angle_is_correct = false;
			++cfg->state.resolver_degradation_of_signal_error_cnt;
			UTILS_LP_FAST(cfg->state.resolver_degradation_of_signal_error_rate, 1.0, timestep);
			if(cfg->state.resolver_degradation_of_signal_error_rate > cfg->state.resolver_DOS_peak_error_rate ) {
				cfg->state.resolver_DOS_peak_error_rate = cfg->state.resolver_degradation_of_signal_error_rate;
			}
		} else {
			UTILS_LP_FAST(cfg->state.resolver_degradation_of_signal_error_rate, 0.0, timestep);
		}

		if (LOS) {
			angle_is_correct = false;
			++cfg->state.resolver_loss_of_signal_error_cnt;
			UTILS_LP_FAST(cfg->state.resolver_loss_of_signal_error_rate, 1.0, timestep);
			if(cfg->state.resolver_loss_of_signal_error_rate > cfg->state.resolver_LOS_peak_error_rate ) {
				cfg->state.resolver_LOS_peak_error_rate = cfg->state.resolver_loss_of_signal_error_rate;
			}
		} else {
			UTILS_LP_FAST(cfg->state.resolver_loss_of_signal_error_rate, 0.0, timestep);
		}

		if (angle_is_correct) {
			cfg->state.last_enc_angle = ((float) pos * 360.0) / 4096.0;
		}
	}
}

void enc_ad2s1205_reset_errors(AD2S1205_config_t *cfg){
	cfg->state.spi_error_cnt = 0;
	cfg->state.spi_error_rate = 0.0;
	cfg->state.resolver_loss_of_tracking_error_rate = 0.0;
	cfg->state.resolver_degradation_of_signal_error_rate = 0.0;
	cfg->state.resolver_loss_of_signal_error_rate = 0.0;
	cfg->state.resolver_loss_of_tracking_error_cnt = 0;
	cfg->state.resolver_degradation_of_signal_error_cnt = 0;
	cfg->state.resolver_loss_of_signal_error_cnt = 0;
	cfg->state.resolver_void_packet_cnt = 0;
	cfg->state.resolver_void_packet_error_rate = 0.0;
	cfg->state.resolver_vel_packet_cnt = 0;
	cfg->state.resolver_vel_packet_error_rate = 0.0;
	cfg->state.resolver_LOT_peak_error_rate = 0.0;
	cfg->state.resolver_LOS_peak_error_rate = 0.0;
	cfg->state.resolver_DOS_peak_error_rate = 0.0;
	cfg->state.resolver_SPI_peak_error_rate = 0.0;
	cfg->state.resolver_VELread_peak_error_rate = 0.0;
	cfg->state.resolver_VOIDspi_peak_error_rate = 0.0;
}