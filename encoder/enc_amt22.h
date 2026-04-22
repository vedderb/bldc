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

#ifndef ENC_AMT22_H_
#define ENC_AMT22_H_

#include "encoder/encoder_datatype.h"

// Functions
bool enc_amt22_init(AMT22_config_t *AS504x_config);
void enc_amt22_deinit(AMT22_config_t *cfg);
void enc_amt22_routine(AMT22_config_t *cfg);
float enc_amt22_read_angle(AMT22_config_t *cfg);

// Macros
#define AMT22_LAST_ANGLE(cfg)		((cfg)->state.last_enc_angle)

#endif /* ENC_AMT22_H_ */
