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

#ifndef ENC_TLE5012_H_
#define ENC_TLE5012_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

bool enc_tle5012_init_sw_ssc(TLE5012_config_t *cfg);
void enc_tle5012_deinit(TLE5012_config_t *cfg);
void enc_tle5012_routine(TLE5012_config_t *cfg);
tle5012_errortypes enc_tle5012_get_temperature(TLE5012_config_t *cfg, double *temperature);
tle5012_errortypes enc_tle5012_get_magnet_magnitude(TLE5012_config_t *cfg, uint16_t *magnitude);

// Macros
#define TLE5012_LAST_ANGLE(cfg)		((cfg)->state.last_enc_angle)

#endif /* ENC_TLE5012_H_ */
