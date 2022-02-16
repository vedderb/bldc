/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#ifndef ENCODER_H_
#define ENCODER_H_

#include "conf_general.h"

// Functions
void encoder_deinit(void);
void encoder_init_abi(uint32_t counts);
void encoder_init_as5047p_spi(void);
void encoder_init_mt6816_spi(void);
void encoder_init_ad2s1205_spi(void);
void encoder_init_sincos(float sin_gain, float sin_offset,
						 float cos_gain, float cos_offset, float sincos_filter_constant);
void encoder_init_ts5700n8501(void);
bool encoder_is_configured(void);
float encoder_read_deg(void);
float encoder_read_deg_multiturn(void);
void encoder_reset(void);
void encoder_tim_isr(void);
void encoder_set_counts(uint32_t counts);
bool encoder_index_found(void);

uint32_t encoder_spi_get_val(void);
uint32_t encoder_spi_get_error_cnt(void);
float encoder_spi_get_error_rate(void);
uint32_t encoder_get_no_magnet_error_cnt(void);
float encoder_get_no_magnet_error_rate(void);
uint32_t encoder_resolver_loss_of_tracking_error_cnt(void);
uint32_t encoder_resolver_degradation_of_signal_error_cnt(void);
uint32_t encoder_resolver_loss_of_signal_error_cnt(void);
float encoder_resolver_loss_of_tracking_error_rate(void);
float encoder_resolver_degradation_of_signal_error_rate(void);
float encoder_resolver_loss_of_signal_error_rate(void);
uint32_t encoder_sincos_get_signal_below_min_error_cnt(void);
uint32_t encoder_sincos_get_signal_above_max_error_cnt(void);
float encoder_sincos_get_signal_below_min_error_rate(void);
float encoder_sincos_get_signal_above_max_error_rate(void);
uint8_t* encoder_ts5700n8501_get_raw_status(void);
int16_t encoder_ts57n8501_get_abm(void);
void encoder_ts57n8501_reset_errors(void);
void encoder_ts57n8501_reset_multiturn(void);
AS504x_diag encoder_AS504x_get_diag(void);

#define AS504x_SPI_READ_BIT 								0x4000
#define AS504x_SPI_WRITE_BIT 								0x0000

#define AS504x_SPI_DIAG_OCF_BIT_POS							8
#define AS504x_SPI_DIAG_COF_BIT_POS							9
#define AS504x_SPI_DIAG_COMP_LOW_BIT_POS					10
#define AS504x_SPI_DIAG_COMP_HIGH_BIT_POS					11


#define AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK			0x3FFF


#define AS504x_SPI_DIAG_ADR									0x3FFD
#define AS504x_SPI_MAGN_ADR									0x3FFE
#define AS504x_SPI_CLEAR_ERROR_ADR							0x0001

#define AS504x_SPI_READ_DIAG_MSG							(AS504x_SPI_DIAG_ADR | AS504x_SPI_READ_BIT)
#define AS504x_SPI_READ_MAGN_MSG							(AS504x_SPI_MAGN_ADR | AS504x_SPI_READ_BIT)
#define AS504x_SPI_READ_CLEAR_ERROR_MSG						(AS504x_SPI_CLEAR_ERROR_ADR | AS504x_SPI_READ_BIT)

#define AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD		5

#define AS504x_DATA_INVALID_THRESHOLD						20000
#define AS504x_REFRESH_DIAG_AFTER_NSAMPLES					100

#endif /* ENCODER_H_ */
