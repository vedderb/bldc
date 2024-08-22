/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
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

#ifndef ENC_AS504X_H_
#define ENC_AS504X_H_

#include "encoder/encoder_datatype.h"

// Functions
bool enc_as504x_init(AS504x_config_t *AS504x_config);
void enc_as504x_deinit(AS504x_config_t *cfg);
void enc_as504x_routine(AS504x_config_t *cfg);
float enc_as504x_read_angle(AS504x_config_t *cfg);

// Macros
#define AS504x_LAST_ANGLE(cfg)		((cfg)->state.last_enc_angle)
#define AS504x_IS_CONNECTED(cfg)	((cfg)->state.sensor_diag.is_connected)
#define AS504x_IS_COMP_HIGH(cfg)	((cfg)->state.sensor_diag.is_Comp_high)
#define AS504x_IS_COMP_LOW(cfg)		((cfg)->state.sensor_diag.is_Comp_low)


#endif /* ENC_AS504X_H_ */
