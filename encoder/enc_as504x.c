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

#include "enc_as504x.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include "spi_bb.h"
#include <math.h>

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

static AS504x_config_t AS504x_config_now = { 0 };

//Private variables
#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
static uint16_t AS504x_diag_fetch_now_count = 0;
#endif
#if !(AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS)
static uint32_t AS504x_data_last_invalid_counter = 0;
#endif
static uint32_t AS504x_spi_communication_error_count = 0;
static uint8_t spi_data_err_raised = 0;
static AS504x_diag AS504x_sensor_diag = { 0 };
static uint16_t spi_val = 0;
static float last_enc_angle = 0.0;
static uint32_t spi_error_cnt = 0;
static float spi_error_rate = 0.0;

// Private functions
#if (AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS)
static void long_delay(void);
static uint8_t AS504x_fetch_diag(void);
static uint8_t AS504x_verify_serial(void);
static void AS504x_deserialize_diag(void);
static void AS504x_fetch_clear_err_diag(void);
static uint8_t AS504x_spi_transfer_err_check(uint16_t *in_buf,
		const uint16_t *out_buf, int length);
#endif
static void AS504x_determinate_if_connected(bool was_last_valid);

void enc_as504x_routine(float rate) {
	uint16_t pos;
// if MOSI is defined, use diagnostics
#if AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS
	spi_bb_begin(&(AS504x_config_now.sw_spi));
	spi_bb_transfer_16(&(AS504x_config_now.sw_spi), 0, 0, 1);
	spi_bb_end(&(AS504x_config_now.sw_spi));

	long_delay();

	spi_bb_begin(&(AS504x_config_now.sw_spi));
	spi_data_err_raised = AS504x_spi_transfer_err_check(&pos, 0, 1);
	spi_bb_end(&(AS504x_config_now.sw_spi));
	spi_val = pos;

	// get diagnostic every AS504x_REFRESH_DIAG_AFTER_NSAMPLES
	AS504x_diag_fetch_now_count++;
	if (AS504x_diag_fetch_now_count >= AS504x_REFRESH_DIAG_AFTER_NSAMPLES
			|| spi_data_err_raised) {
		// clear error flags before getting new diagnostics data
		AS504x_fetch_clear_err_diag();

		if (!AS504x_fetch_diag()) {
			if (!AS504x_verify_serial()) {
				AS504x_deserialize_diag();
				AS504x_determinate_if_connected(true);
			} else {
				AS504x_determinate_if_connected(false);
			}
		} else {
			AS504x_determinate_if_connected(false);
		}
		AS504x_diag_fetch_now_count = 0;
	}
#else
        spi_bb_begin(&(AS504x_config_now.sw_spi));
        spi_bb_transfer_16(&(AS504x_config_now.sw_spi), &pos, 0, 1);
        spi_bb_end(&(AS504x_config_now.sw_spi));
        spi_val = pos;

        if(0x0000 == pos || 0xFFFF == pos) {
            AS504x_data_last_invalid_counter++;
        } else {
            AS504x_data_last_invalid_counter = 0;
            AS504x_determinate_if_connected(true);
        }

        if (AS504x_data_last_invalid_counter >= AS504x_DATA_INVALID_THRESHOLD) {
            AS504x_determinate_if_connected(false);
            AS504x_data_last_invalid_counter = AS504x_DATA_INVALID_THRESHOLD;
        }
#endif

	if (spi_bb_check_parity(pos) && !spi_data_err_raised) {
		pos &= 0x3FFF;
		last_enc_angle = ((float) pos * 360.0) / 16384.0;
		UTILS_LP_FAST(spi_error_rate, 0.0, 1.0 / rate);
	} else {
		++spi_error_cnt;
		UTILS_LP_FAST(spi_error_rate, 1.0, 1. / rate);
	}
}

void enc_as504x_deinit(void) {
	nvicDisableVector(HW_ENC_EXTI_CH);
	nvicDisableVector(HW_ENC_TIM_ISR_CH);

	TIM_DeInit(HW_ENC_TIM);

	spi_bb_deinit(&(AS504x_config_now.sw_spi));

#ifdef HW_SPI_DEV
	spiStop(&HW_SPI_DEV);
#endif

	AS504x_config_now.is_init = 0;
	last_enc_angle = 0.0;
	spi_error_rate = 0.0;
}

encoder_ret_t enc_as504x_init(AS504x_config_t *AS504x_config) {
	AS504x_config_now = *AS504x_config;
	spi_bb_init(&(AS504x_config_now.sw_spi));

	AS504x_config->is_init = 1;
	AS504x_config_now = *AS504x_config;

	nvicEnableVector(HW_ENC_TIM_ISR_CH, 6);

	spi_error_rate = 0.0;
	return ENCODER_OK;
}

#if (AS504x_USE_SW_MOSI_PIN || AS5047_USE_HW_SPI_PINS)
static void long_delay(void) {
	for (volatile int i = 0; i < 40; i++) {
		__NOP();
	}
}

static uint8_t AS504x_verify_serial() {
	uint16_t serial_diag_flgs, serial_magnitude, test_magnitude;
	uint8_t test_AGC_value, test_is_Comp_high, test_is_Comp_low;

	serial_magnitude = enc_as504x_get_diag().serial_magnitude;
	serial_diag_flgs = enc_as504x_get_diag().serial_diag_flgs;

	test_magnitude = serial_magnitude
			& AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
	test_AGC_value = serial_diag_flgs;
	test_is_Comp_low = (serial_diag_flgs >> AS504x_SPI_DIAG_COMP_LOW_BIT_POS)
			& 1;
	test_is_Comp_high = (serial_diag_flgs >> AS504x_SPI_DIAG_COMP_HIGH_BIT_POS)
			& 1;

	if (test_is_Comp_high && test_is_Comp_low) {
		return 1;
	}
	if ((uint32_t) test_magnitude + (uint32_t) test_AGC_value == 0) {
		return 1;
	}

	return 0;
}

static uint8_t AS504x_fetch_diag(void) {
	uint16_t recf[2], senf[2] = { AS504x_SPI_READ_DIAG_MSG,
			AS504x_SPI_READ_MAGN_MSG };
	uint8_t ret = 0;

	spi_bb_begin(&(AS504x_config_now.sw_spi));
	spi_bb_transfer_16(&(AS504x_config_now.sw_spi), 0, senf, 1);
	spi_bb_end(&(AS504x_config_now.sw_spi));

	long_delay();

	spi_bb_begin(&(AS504x_config_now.sw_spi));
	ret |= AS504x_spi_transfer_err_check(recf, senf + 1, 1);
	spi_bb_end(&(AS504x_config_now.sw_spi));

	long_delay();

	spi_bb_begin(&(AS504x_config_now.sw_spi));
	ret |= AS504x_spi_transfer_err_check(recf + 1, 0, 1);
	spi_bb_end(&(AS504x_config_now.sw_spi));

	if (!ret) {
		if (spi_bb_check_parity(recf[0]) && spi_bb_check_parity(recf[1])) {
			AS504x_sensor_diag.serial_diag_flgs = recf[0];
			AS504x_sensor_diag.serial_magnitude = recf[1];
		}
	}

	return ret;
}

static void AS504x_deserialize_diag() {
	AS504x_sensor_diag.AGC_value = AS504x_sensor_diag.serial_diag_flgs;
	AS504x_sensor_diag.is_OCF = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_OCF_BIT_POS) & 1;
	AS504x_sensor_diag.is_COF = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COF_BIT_POS) & 1;
	AS504x_sensor_diag.is_Comp_low = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COMP_LOW_BIT_POS) & 1;
	AS504x_sensor_diag.is_Comp_high = (AS504x_sensor_diag.serial_diag_flgs
			>> AS504x_SPI_DIAG_COMP_HIGH_BIT_POS) & 1;
	AS504x_sensor_diag.magnitude = AS504x_sensor_diag.serial_magnitude
			& AS504x_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
}

static void AS504x_fetch_clear_err_diag() {
	uint16_t recf, senf = AS504x_SPI_READ_CLEAR_ERROR_MSG;

	spi_bb_begin(&(AS504x_config_now.sw_spi));
	spi_bb_transfer_16(&(AS504x_config_now.sw_spi), 0, &senf, 1);
	spi_bb_end(&(AS504x_config_now.sw_spi));

	long_delay();

	spi_bb_begin(&(AS504x_config_now.sw_spi));
	spi_bb_transfer_16(&(AS504x_config_now.sw_spi), &recf, 0, 1);
	spi_bb_end(&(AS504x_config_now.sw_spi));

	AS504x_sensor_diag.serial_error_flags = recf;
}

static uint8_t AS504x_spi_transfer_err_check(uint16_t *in_buf,
		const uint16_t *out_buf, int length) {
	spi_bb_transfer_16(&(AS504x_config_now.sw_spi) ,in_buf, out_buf, length);

	for (int len_count = 0; len_count < length; len_count++) {
		if (((in_buf[len_count]) >> 14) & 0b01) {
			return 1;
		}
	}

	return 0;
}
#endif

static void AS504x_determinate_if_connected(bool was_last_valid) {
	if (!was_last_valid) {
		AS504x_spi_communication_error_count++;

		if (AS504x_spi_communication_error_count
				>= AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD) {
			AS504x_spi_communication_error_count =
					AS504x_CONNECTION_DETERMINATOR_ERROR_THRESHOLD;
			AS504x_sensor_diag.is_connected = 0;
		}
	} else {
		if (AS504x_spi_communication_error_count) {
			AS504x_spi_communication_error_count--;
		} else {
			AS504x_sensor_diag.is_connected = 1;
		}
	}
}

AS504x_diag enc_as504x_get_diag(void) {
	return AS504x_sensor_diag;
}

float enc_as504x_read_deg(void) {
	return last_enc_angle;
}

uint32_t enc_as504x_spi_get_val(void) {
	return spi_val;
}

uint32_t enc_as504x_spi_get_error_cnt(void) {
	return spi_error_cnt;
}

float enc_as504x_spi_get_error_rate(void) {
	return spi_error_rate;
}
