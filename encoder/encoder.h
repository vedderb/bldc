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

#include "encoder_datatype.h"
#include "hal.h"

// GENERIC GLOBAL
void encoder_deinit(void);
encoder_ret_t encoder_init(encoder_type_t encoder_config);

float encoder_read_deg(void);
float encoder_read_deg_multiturn(void);
encoder_type_t encoder_is_configured(void);
bool encoder_index_found(void);
void encoder_reset_multiturn(void);

// SPECIFIC GLOBAL
//AD2S1205
float encoder_resolver_loss_of_tracking_error_rate(void);
float encoder_resolver_degradation_of_signal_error_rate(void);
float encoder_resolver_loss_of_signal_error_rate(void);
uint32_t encoder_resolver_loss_of_tracking_error_cnt(void);
uint32_t encoder_resolver_degradation_of_signal_error_cnt(void);
uint32_t encoder_resolver_loss_of_signal_error_cnt(void);

//ABI
void encoder_set_counts(uint32_t counts);

//MT6816
float encoder_get_no_magnet_error_rate(void);
uint32_t encoder_get_no_magnet_error_cnt(void);

//AS504x
AS504x_diag encoder_get_diag(void);

//SINCOS
uint32_t encoder_get_signal_below_min_error_cnt(void);
uint32_t encoder_get_signal_above_max_error_cnt(void);
float encoder_get_signal_below_min_error_rate(void);
float encoder_get_signal_above_max_error_rate(void);
void encoder_sincos_conf_set(float sin_gain, float sin_offset,
		float cos_gain, float cos_offset, float sincos_filter_constant);

//TS5700N8501
uint8_t* encoder_get_raw_status(void);
int16_t encoder_get_abm(void);
void encoder_reset_errors(void);

//SPI ENCODERS
uint32_t encoder_spi_get_val(void);
float encoder_spi_get_error_rate(void);
uint32_t encoder_spi_get_error_cnt(void);

void encoder_reset(void);
void encoder_tim_isr(void);

#endif /* ENCODER_ENCODER_H_ */
