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

#ifndef ENCODER_AS504X_H_
#define ENCODER_AS504X_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

void AS504x_deinit(void);
encoder_ret_t AS504x_init(AS504x_config_t *AS504x_config);

float AS504x_read_deg(void);
void AS504x_routine(void);

AS504x_diag AS504x_get_diag(void);
float AS504x_spi_get_error_rate(void);
uint32_t AS504x_spi_get_val(void);
uint32_t AS504x_spi_get_error_cnt(void);
#endif /* ENCODER_AS504X_H_ */
