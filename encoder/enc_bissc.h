/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Manoukian Vincent	    manoukianv@gmail.com

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

#ifndef ENC_BISSC_H_
#define ENC_BISSC_H_

#include "encoder/encoder_datatype.h"

bool enc_bissc_init(BISSC_config_t *cfg);
void enc_bissc_deinit(BISSC_config_t *cfg);
void enc_bissc_routine(BISSC_config_t *cfg);

extern BISSC_config_t encoder_cfg_bissc;

// Macros
#define BISSC_LAST_ANGLE(cfg)		((cfg)->state.last_enc_angle)

#endif /* ENC_BISSC_H_ */
