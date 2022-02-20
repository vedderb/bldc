/*
	Copyright 2022 Jakub Tomczak
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#ifndef ENCODER_ENCODER_DATATYPE_H_
#define ENCODER_ENCODER_DATATYPE_H_

#include <stdint.h>
#include <stdbool.h>
#include "ch.h"
#include "hal.h"
#include "spi_bb.h"

typedef enum {
	ENCODER_OK = 0, ENCODER_NONE, ENCODER_ERROR
} encoder_ret_t;

typedef enum {
	ENCODER_TYPE_NONE = 0,
	ENCODER_TYPE_AS504x,
	ENCODER_TYPE_MT6816,
	ENCODER_TYPE_AD2S1205_SPI,
	ENCODER_TYPE_SINCOS,
	ENCODER_TYPE_TS5700N8501,
	ENCODER_TYPE_ABI
} encoder_type_t;

typedef struct {
	spi_bb_state sw_spi;
} AD2S1205_config_t;

typedef struct {
	float spi_error_rate;
	float encoder_no_magnet_error_rate;
	uint32_t encoder_no_magnet_error_cnt;
	float last_enc_angle;
	uint32_t spi_error_cnt;
	uint32_t spi_val;
} MT6816_state;

typedef struct {
	SPIDriver *spi_dev;
	SPIConfig hw_spi_cfg;
	stm32_gpio_t *nss_gpio;
	int nss_pin;
	stm32_gpio_t *sck_gpio;
	int sck_pin;
	stm32_gpio_t *mosi_gpio;
	int mosi_pin;
	stm32_gpio_t *miso_gpio;
	int miso_pin;

	MT6816_state state;
} MT6816_config_t;

typedef struct {
	uint32_t counts;
	stm32_gpio_t *A_gpio;
	uint8_t A_pin;
	stm32_gpio_t *B_gpio;
	uint8_t B_pin;
	stm32_gpio_t *I_gpio;
	uint8_t I_pin;
} ABI_config_t;

typedef struct {
	uint32_t signal_below_min_error_cnt;
	uint32_t signal_above_max_error_cnt;
	float signal_low_error_rate;
	float signal_above_max_error_rate;
	float last_enc_angle;
	uint32_t last_update_time;
} ENCSINCOS_state;

typedef struct {
	uint32_t refresh_rate_hz;
	float s_gain;
	float s_offset;
	float c_gain;
	float c_offset;
	float filter_constant;

	ENCSINCOS_state state;
} ENCSINCOS_config_t;

typedef struct {
	SerialDriver *sd;
	stm32_gpio_t *TX_gpio;
	uint8_t TX_pin;
	stm32_gpio_t *RX_gpio;
	uint8_t RX_pin;
	stm32_gpio_t *EXT_gpio;
	uint8_t EXT_pin;
	SerialConfig uart_param;
} TS5700N8501_config_t;

typedef struct {
	uint8_t is_connected;
	uint8_t AGC_value;
	uint16_t magnitude;
	uint8_t is_OCF;
	uint8_t is_COF;
	uint8_t is_Comp_low;
	uint8_t is_Comp_high;
	uint16_t serial_diag_flgs;
	uint16_t serial_magnitude;
	uint16_t serial_error_flags;
} AS504x_diag;

typedef struct {
	uint16_t diag_fetch_now_count;
	uint32_t data_last_invalid_counter;
	uint32_t spi_communication_error_count;
	uint8_t spi_data_err_raised;
	AS504x_diag sensor_diag;
	uint16_t spi_val;
	float last_enc_angle;
	uint32_t spi_error_cnt;
	float spi_error_rate;
} AS504x_state;

typedef struct {
	spi_bb_state sw_spi;
	AS504x_state state;
} AS504x_config_t;

#endif /* ENCODER_ENCODER_DATATYPE_H_ */
