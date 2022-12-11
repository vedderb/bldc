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
#include "utils_math.h"
#include "hw.h"
#include "timer.h"

#include <math.h>
#include <string.h>

#define SINCOS_MIN_AMPLITUDE        0.7         // sqrt(sin^2 + cos^2) has to be larger than this
#define SINCOS_MAX_AMPLITUDE        1.3         // sqrt(sin^2 + cos^2) has to be smaller than this

bool enc_sincos_init(ENCSINCOS_config_t *cfg) {
	memset(&cfg->state, 0, sizeof(ENCSINCOS_state));
	return true;
}

void enc_sincos_deinit(ENCSINCOS_config_t *cfg) {
	memset(&cfg->state, 0, sizeof(ENCSINCOS_state));
}

float enc_sincos_read_deg(ENCSINCOS_config_t *cfg) {
	float sin = (ENCODER_SIN_VOLTS - cfg->s_offset) * cfg->s_gain;
	float cos = (ENCODER_COS_VOLTS - cfg->c_offset) * cfg->c_gain;

	UTILS_LP_FAST(cfg->state.sin_filter, sin, cfg->filter_constant);
	UTILS_LP_FAST(cfg->state.cos_filter, cos, cfg->filter_constant);
	sin = cfg->state.sin_filter;
	cos = cfg->state.cos_filter;

	// phase error compensation
	cos = (cos + sin * cfg->sph) / cfg->cph;

	float module = SQ(sin) + SQ(cos);

	float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
	if (timestep > 1.0) {
		timestep = 1.0;
	}
	cfg->state.last_update_time = timer_time_now();

	if (module > SQ(SINCOS_MAX_AMPLITUDE) )	{
		// signals vector outside of the valid area. Increase error count and discard measurement
		++cfg->state.signal_above_max_error_cnt;
		UTILS_LP_FAST(cfg->state.signal_above_max_error_rate, 1.0, timestep);
	} else if (module < SQ(SINCOS_MIN_AMPLITUDE)) {
		++cfg->state.signal_below_min_error_cnt;
		UTILS_LP_FAST(cfg->state.signal_low_error_rate, 1.0, timestep);
	} else {
		UTILS_LP_FAST(cfg->state.signal_above_max_error_rate, 0.0, timestep);
		UTILS_LP_FAST(cfg->state.signal_low_error_rate, 0.0, timestep);
		cfg->state.last_enc_angle = RAD2DEG_f(utils_fast_atan2(sin, cos)) + 180.0;
	}

	return cfg->state.last_enc_angle;
}
