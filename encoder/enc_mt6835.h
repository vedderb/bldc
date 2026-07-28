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

#ifndef ENC_MT6835_H_
#define ENC_MT6835_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

bool enc_mt6835_init(MT6835_config_t *cfg);
void enc_mt6835_deinit(MT6835_config_t *cfg);
void enc_mt6835_routine(MT6835_config_t *cfg);

#define MT6835_LAST_ANGLE(cfg)		((cfg)->state.last_enc_angle)

#endif /* ENC_MT6835_H_ */
