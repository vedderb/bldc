/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

/*
 * encoder.h
 *
 *  Created on: 7 mar 2015
 *      Author: benjamin
 */

#ifndef ENCODER_H_
#define ENCODER_H_

#include "conf_general.h"

#ifdef __cplusplus
extern "C" {
#endif

// Functions
void encoder_deinit(void);
void encoder_init_abi(uint32_t counts);
void encoder_init_as5047p_spi(void);
bool encoder_is_configured(void);
float encoder_read_deg(void);
void encoder_reset(void);
void encoder_tim_isr(void);
void encoder_set_counts(uint32_t counts);
bool encoder_index_found(void);

#ifdef __cplusplus
}
#endif

#endif /* ENCODER_H_ */
