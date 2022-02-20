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

#ifndef ENC_ABI_H_
#define ENC_ABI_H_

#include "datatypes.h"
#include "encoder/encoder_datatype.h"

// Functions
bool enc_abi_init(ABI_config_t *cfg);
void enc_abi_deinit(ABI_config_t *cfg);
float enc_abi_read_deg(ABI_config_t *cfg);

// Call this functions on index pin change interrupts
void enc_abi_pin_isr(ABI_config_t *cfg);

#endif /* ENC_ABI_H_ */
