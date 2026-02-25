/*
	Copyright 2016 - 2026 Benjamin Vedder	benjamin@vedder.se
	Copyright 2026 Igor Gorniak gorniak.igor@gmail.com

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

#ifndef ENC_MA782_H_
#define ENC_MA782_H_

#include "encoder/encoder_datatype.h"

bool enc_ma782_init(ma782_config_t *cfg);
void enc_ma782_deinit(ma782_config_t *cfg);
void enc_ma782_routine(ma782_config_t *cfg);
void enc_ma782_print_status(ma782_config_t *cfg);

extern ma782_config_t encoder_cfg_ma782;

// Macros
#define MA782_LAST_ANGLE(cfg)		((cfg)->state.last_enc_angle)

#endif /* ENC_MA782_H_ */
