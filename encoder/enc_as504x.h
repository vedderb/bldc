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

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void enc_as504x_deinit(void);
encoder_ret_t enc_as504x_init(AS504x_config_t *AS504x_config);

float enc_as504x_read_deg(void);
void enc_as504x_routine(float rate);

AS504x_diag enc_as504x_get_diag(void);
float enc_as504x_spi_get_error_rate(void);
uint32_t enc_as504x_spi_get_val(void);
uint32_t enc_as504x_spi_get_error_cnt(void);

#endif /* ENC_AS504X_H_ */
