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

#ifndef ENCODER_ENCODER_H_
#define ENCODER_ENCODER_H_

#include "hal.h"
#include "encoder_datatype.h"

#include "enc_abi.h"
#include "enc_ad2s1205.h"
#include "enc_as5x47u.h"
#include "enc_as504x.h"
#include "enc_bissc.h"
#include "enc_mt6816.h"
#include "enc_sincos.h"
#include "enc_tle5012.h"
#include "enc_ts5700n8501.h"

// Functions
bool encoder_init(volatile mc_configuration *conf);
void encoder_update_config(volatile mc_configuration *conf);
void encoder_deinit(void);

void encoder_set_custom_callbacks (
		float (*read_deg)(void),
		bool (*has_fault)(void),
		char* (*print_info)(void));

float encoder_read_deg(void);
float encoder_read_deg_multiturn(void);
void encoder_set_deg(float deg);
encoder_type_t encoder_is_configured(void);
bool encoder_index_found(void);
void encoder_reset_multiturn(void);
void encoder_reset_errors(void);
float encoder_get_error_rate(void);

void encoder_check_faults(volatile mc_configuration *m_conf, bool is_second_motor);

// Interrupt handlers
void encoder_pin_isr(void);
void encoder_tim_isr(void);

#endif /* ENCODER_ENCODER_H_ */
