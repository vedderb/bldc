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

#include "enc_ts5700n8501.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

static THD_FUNCTION(ts5700n8501_thread, arg);
static THD_WORKING_AREA(ts5700n8501_thread_wa, 512);

static volatile bool ts5700n8501_stop_now = true;
static volatile bool ts5700n8501_is_running = false;
static volatile uint8_t ts5700n8501_raw_status[8] = { 0 };
static volatile bool ts5700n8501_reset_errors = false;
static volatile bool ts5700n8501_reset_multiturn = false;

static TS5700N8501_config_t ts5700n8501_config_now = { 0 };

static float spi_error_rate = 0.0;
static uint32_t spi_error_cnt = 0;
static uint32_t spi_val = 0;

static float last_enc_angle = 0.0;

void enc_ts5700n8501_deinit(void) {

	ts5700n8501_stop_now = true;
	while (ts5700n8501_is_running) {
		chThdSleepMilliseconds(1);
	}

	palSetPadMode(ts5700n8501_config_now.TX_gpio,
			ts5700n8501_config_now.TX_pin,
			PAL_MODE_INPUT_PULLUP);
	palSetPadMode(ts5700n8501_config_now.RX_gpio,
			ts5700n8501_config_now.RX_pin,
			PAL_MODE_INPUT_PULLUP);
#ifdef HW_ADC_EXT_GPIO
	palSetPadMode(ts5700n8501_config_now.EXT_gpio, ts5700n8501_config_now.EXT_pin, PAL_MODE_INPUT_ANALOG);
#endif

	ts5700n8501_config_now.is_init = 0;

	last_enc_angle = 0.0;
	spi_error_rate = 0.0;
}

encoder_ret_t enc_ts5700n8501_init(TS5700N8501_config_t *ts5700n8501_config) {
#ifdef HW_UART_DEV
	spi_error_rate = 0.0;
	spi_error_cnt = 0;
	ts5700n8501_is_running = true;
	ts5700n8501_stop_now = false;

	ts5700n8501_config_now = *ts5700n8501_config;

	chThdCreateStatic(ts5700n8501_thread_wa, sizeof(ts5700n8501_thread_wa),
	NORMALPRIO - 10, ts5700n8501_thread, NULL);

	ts5700n8501_config_now.is_init = 1;
	ts5700n8501_config->is_init = 1;
	return ENCODER_OK;
#else
	ts5700n8501_config->is_init = 0;
	return ENCODER_ERROR;
#endif
}

float enc_ts5700n8501_read_deg(void) {
	return last_enc_angle;
}

uint8_t* enc_ts5700n8501_get_raw_status(void) {
	return (uint8_t*) ts5700n8501_raw_status;
}

int16_t enc_ts5700n8501_get_abm(void) {
	return (uint16_t) ts5700n8501_raw_status[4]
			| ((uint16_t) ts5700n8501_raw_status[5] << 8);
}

void enc_ts5700n8501_reset_errors(void) {
	ts5700n8501_reset_errors = true;
}

void enc_ts5700n8501_reset_multiturn(void) {
	ts5700n8501_reset_multiturn = true;
}

static void TS5700N8501_delay_uart(void) {
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
}

/*
 * It is important to switch to receive mode immediately after sending the readout command,
 * as the TS5700N8501 starts sending the reply after 3 microseconds. Therefore use software
 * UART on TX so that the enable signal can be controlled manually. This function runs while
 * the system is locked, but it should finish fast enough to not cause problems for other
 * things due to the high baud rate.
 */
static void TS5700N8501_send_byte(uint8_t b) {
	utils_sys_lock_cnt();
#ifdef HW_ADC_EXT_GPIO
	palSetPad(ts5700n8501_config_now.EXT_gpio, ts5700n8501_config_now.EXT_pin);
#endif
	TS5700N8501_delay_uart();
	palWritePad(ts5700n8501_config_now.TX_gpio,
			ts5700n8501_config_now.TX_pin, 0);
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	for (int i = 0; i < 8; i++) {
		palWritePad(ts5700n8501_config_now.TX_gpio,
				ts5700n8501_config_now.TX_pin,
				(b & (0x80 >> i)) ? PAL_HIGH : PAL_LOW);
		TS5700N8501_delay_uart();
	}
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	palWritePad(ts5700n8501_config_now.TX_gpio,
			ts5700n8501_config_now.TX_pin, 1);
	TS5700N8501_delay_uart();
#ifdef HW_ADC_EXT_GPIO
	palClearPad(ts5700n8501_config_now.EXT_gpio, ts5700n8501_config_now.EXT_pin);
#endif
	utils_sys_unlock_cnt();
}

static THD_FUNCTION(ts5700n8501_thread, arg) {
	(void) arg;

	chRegSetThreadName("TS5700N8501");

	SerialConfig sd_init = ts5700n8501_config_now.uart_param;

	sdStart(&HW_UART_DEV, &sd_init);
	palSetPadMode(ts5700n8501_config_now.TX_gpio,
			ts5700n8501_config_now.TX_pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
	palSetPadMode(ts5700n8501_config_now.RX_gpio,
			ts5700n8501_config_now.RX_pin,
			PAL_MODE_ALTERNATE(HW_UART_GPIO_AF) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
#ifdef HW_ADC_EXT_GPIO
	palSetPadMode(ts5700n8501_config_now.EXT_gpio, ts5700n8501_config_now.EXT_pin, PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
#endif

	for (;;) {
		// Check if it is time to stop.
		if (ts5700n8501_stop_now) {
			ts5700n8501_is_running = false;
			return;
		}

		if (ts5700n8501_reset_errors) {
			for (int i = 0; i < 20; i++) {
				TS5700N8501_send_byte(0b01011101);
				chThdSleep(2);
			}

			ts5700n8501_reset_errors = false;
		}

		if (ts5700n8501_reset_multiturn) {
			for (int i = 0; i < 20; i++) {
				TS5700N8501_send_byte(0b01000110);
				chThdSleep(2);
			}

			ts5700n8501_reset_multiturn = false;
		}

		TS5700N8501_send_byte(0b01011000);

#define LOOP_RATE	((float)(CH_CFG_ST_FREQUENCY / 2))
		chThdSleep(2);

		uint8_t reply[11];
		int reply_ind = 0;

		msg_t res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
		while (res != MSG_TIMEOUT ) {
			if (reply_ind < (int) sizeof(reply)) {
				reply[reply_ind++] = res;
			}
			res = sdGetTimeout(&HW_UART_DEV, TIME_IMMEDIATE);
		}

		uint8_t crc = 0;
		for (int i = 0; i < (reply_ind - 1); i++) {
			crc = (reply[i] ^ crc);
		}

		if (reply_ind == 11 && crc == reply[reply_ind - 1]) {
			uint32_t pos = (uint32_t) reply[2] + ((uint32_t) reply[3] << 8)
					+ ((uint32_t) reply[4] << 16);
			spi_val = pos;
			last_enc_angle = (float) pos / 131072.0 * 360.0;
			UTILS_LP_FAST(spi_error_rate, 0.0, 1.0 / LOOP_RATE);

			ts5700n8501_raw_status[0] = reply[1]; // SF
			ts5700n8501_raw_status[1] = reply[2]; // ABS0
			ts5700n8501_raw_status[2] = reply[3]; // ABS1
			ts5700n8501_raw_status[3] = reply[4]; // ABS2
			ts5700n8501_raw_status[4] = reply[6]; // ABM0
			ts5700n8501_raw_status[5] = reply[7]; // ABM1
			ts5700n8501_raw_status[6] = reply[8]; // ABM2
			ts5700n8501_raw_status[7] = reply[9]; // ALMC
		} else {
			++spi_error_cnt;
			UTILS_LP_FAST(spi_error_rate, 1.0, 1.0 / LOOP_RATE);
		}
	}
}
