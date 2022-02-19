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

#ifndef ENC_MT6816_H_
#define ENC_MT6816_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

encoder_ret_t enc_mt6816_init(MT6816_config_t *mt6816_config);
void enc_mt6816_deinit(void);

float enc_mt6816_read_deg(void);
void enc_mt6816_routine(float rate);

uint32_t enc_mt6816_spi_get_val(void);
uint32_t enc_mt6816_spi_get_error_cnt(void);
uint32_t enc_mt6816_get_no_magnet_error_cnt(void);
uint32_t enc_mt6816_get_no_magnet_error_rate(void);

#endif /* ENC_MT6816_H_ */
