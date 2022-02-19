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

#include "enc_sincos.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>
#include "hw.h"

#define SINCOS_SAMPLE_RATE_HZ       20000
#define SINCOS_MIN_AMPLITUDE        1.0         // sqrt(sin^2 + cos^2) has to be larger than this
#define SINCOS_MAX_AMPLITUDE        1.65        // sqrt(sin^2 + cos^2) has to be smaller than this

ENCSINCOS_config_t enc_sincos_config_now = { 0 };

static uint32_t sincos_signal_below_min_error_cnt = 0;
static uint32_t sincos_signal_above_max_error_cnt = 0;
static float sincos_signal_low_error_rate = 0.0;
static float sincos_signal_above_max_error_rate = 0.0;

static float last_enc_angle = 0.0;

void enc_sincos_deinit(void) {
	last_enc_angle = 0.0;
	sincos_signal_low_error_rate = 0.0;
	sincos_signal_above_max_error_rate = 0.0;
}

encoder_ret_t enc_sincos_init(ENCSINCOS_config_t *enc_sincos_config) {
	enc_sincos_config_now = *enc_sincos_config;

	sincos_signal_below_min_error_cnt = 0;
	sincos_signal_above_max_error_cnt = 0;
	sincos_signal_low_error_rate = 0.0;
	sincos_signal_above_max_error_rate = 0.0;
	last_enc_angle = 0.0;
	enc_sincos_config_now = *enc_sincos_config;
	return ENCODER_OK;
}

float enc_sincos_read_deg(void) {
	float angle = 0.0;
	float sin = ENCODER_SIN_VOLTS * enc_sincos_config_now.s_gain - enc_sincos_config_now.s_offset;
	float cos = ENCODER_COS_VOLTS * enc_sincos_config_now.c_gain - enc_sincos_config_now.c_offset;

	float module = SQ(sin) + SQ(cos);

	if (module > SQ(SINCOS_MAX_AMPLITUDE) )	{
		// signals vector outside of the valid area. Increase error count and discard measurement
		++sincos_signal_above_max_error_cnt;
		UTILS_LP_FAST(sincos_signal_above_max_error_rate, 1.0, 1./SINCOS_SAMPLE_RATE_HZ);
		angle = last_enc_angle;
	} else {
		if (module < SQ(SINCOS_MIN_AMPLITUDE)) {
			++sincos_signal_below_min_error_cnt;
			UTILS_LP_FAST(sincos_signal_low_error_rate, 1.0, 1./SINCOS_SAMPLE_RATE_HZ);
			angle = last_enc_angle;
		} else {
			UTILS_LP_FAST(sincos_signal_above_max_error_rate, 0.0, 1./SINCOS_SAMPLE_RATE_HZ);
			UTILS_LP_FAST(sincos_signal_low_error_rate, 0.0, 1./SINCOS_SAMPLE_RATE_HZ);

			float angle_tmp = RAD2DEG_f(utils_fast_atan2(sin, cos));
			UTILS_LP_FAST(angle, angle_tmp, enc_sincos_config_now.filter_constant);
			last_enc_angle = angle;
		}
	}

	return last_enc_angle;
}

uint32_t enc_sincos_get_signal_below_min_error_cnt(void) {
	return sincos_signal_below_min_error_cnt;
}

uint32_t enc_sincos_get_signal_above_max_error_cnt(void) {
	return sincos_signal_above_max_error_cnt;
}

float enc_sincos_get_signal_below_min_error_rate(void) {
	return sincos_signal_low_error_rate;
}

float enc_sincos_get_signal_above_max_error_rate(void) {
	return sincos_signal_above_max_error_rate;
}
