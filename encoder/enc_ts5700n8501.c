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
#include "utils_math.h"
#include "utils_sys.h"

#include <string.h>
#include <math.h>

// Private functions
static THD_FUNCTION(ts5700n8501_thread, arg);

bool enc_ts5700n8501_init(TS5700N8501_config_t *cfg) {
	if (cfg->sd == NULL) {
		return false;
	}

	memset(&cfg->state, 0, sizeof(TS5700N8501_state));
	cfg->state.stop_now = false;
	cfg->state.is_running = true;

	chThdCreateStatic(cfg->thread_wa, cfg->thread_wa_size,
			NORMALPRIO - 10, ts5700n8501_thread, cfg);

	return true;
}

void enc_ts5700n8501_deinit(TS5700N8501_config_t *cfg) {
	if (cfg->sd == NULL) {
		return;
	}

	cfg->state.stop_now = true;
	while (cfg->state.is_running) {
		chThdSleepMilliseconds(1);
	}

	palSetPadMode(cfg->TX_gpio, cfg->TX_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(cfg->RX_gpio, cfg->RX_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(cfg->EXT_gpio, cfg->EXT_pin, PAL_MODE_INPUT_ANALOG);

	cfg->state.last_enc_angle = 0.0;
	cfg->state.spi_error_rate = 0.0;
}

#pragma GCC push_options
#pragma GCC optimize ("O0")

static void TS5700N8501_delay_uart(void) {
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP();
}

/*
 * It is important to switch to receive mode immediately after sending the readout command,
 * as the TS5700N8501 starts sending the reply after 3 microseconds. Therefore use software
 * UART on TX so that the enable signal can be controlled manually. This function runs while
 * the system is locked, but it should finish fast enough to not cause problems for other
 * things due to the high baud rate.
 */
static void TS5700N8501_send_byte(TS5700N8501_config_t *cfg, uint8_t b) {
	stm32_gpio_t *tx_io = cfg->TX_gpio;
	uint8_t tx_pin = cfg->TX_pin;

	stm32_gpio_t *ext_io = cfg->EXT_gpio;
	uint8_t ext_pin = cfg->EXT_pin;

	utils_sys_lock_cnt();

	palSetPad(ext_io, ext_pin);
	TS5700N8501_delay_uart();
	palWritePad(tx_io, tx_pin, 0);

	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();

	for (int i = 0; i < 8; i++) {
		palWritePad(tx_io, tx_pin, (b & (0x80 >> i)) ? PAL_HIGH : PAL_LOW);
		TS5700N8501_delay_uart();
	}

	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();
	__NOP(); __NOP(); __NOP();

	palWritePad(tx_io, tx_pin, 1);
	TS5700N8501_delay_uart();
	palClearPad(ext_io, ext_pin);
	utils_sys_unlock_cnt();
}

#pragma GCC pop_options

static THD_FUNCTION(ts5700n8501_thread, arg) {
	TS5700N8501_config_t *cfg = (TS5700N8501_config_t*)arg;

	chRegSetThreadName("TS5700N8501");

	sdStart(cfg->sd, &cfg->uart_param);
	palSetPadMode(cfg->TX_gpio, cfg->TX_pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
	palSetPadMode(cfg->RX_gpio, cfg->RX_pin,
			PAL_MODE_ALTERNATE(cfg->sd_af) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);
	palSetPadMode(cfg->EXT_gpio, cfg->EXT_pin, PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_PULLUP);

	for (;;) {
		// Check if it is time to stop.
		if (cfg->state.stop_now) {
			cfg->state.is_running = false;
			return;
		}

		if (cfg->state.reset_errors) {
			for (int i = 0; i < 20; i++) {
				TS5700N8501_send_byte(cfg, 0b01011101);
				chThdSleep(2);
			}

			cfg->state.reset_errors = false;
		}

		if (cfg->state.reset_multiturn) {
			for (int i = 0; i < 20; i++) {
				TS5700N8501_send_byte(cfg, 0b01000110);
				chThdSleep(2);
			}

			cfg->state.reset_multiturn = false;
		}

		TS5700N8501_send_byte(cfg, 0b01011000);

#define LOOP_RATE	((float)(CH_CFG_ST_FREQUENCY / 2))
		chThdSleep(2);

		uint8_t reply[11];
		int reply_ind = 0;

		msg_t res = sdGetTimeout(cfg->sd, TIME_IMMEDIATE);
		while (res != MSG_TIMEOUT ) {
			if (reply_ind < (int) sizeof(reply)) {
				reply[reply_ind++] = res;
			}
			res = sdGetTimeout(cfg->sd, TIME_IMMEDIATE);
		}

		uint8_t crc = 0;
		for (int i = 0; i < (reply_ind - 1); i++) {
			crc = (reply[i] ^ crc);
		}

		if (reply_ind == 11 && crc == reply[reply_ind - 1]) {
			uint32_t pos = (uint32_t) reply[2] + ((uint32_t) reply[3] << 8)
					+ ((uint32_t) reply[4] << 16);
			cfg->state.spi_val = pos;
			cfg->state.last_enc_angle = (float) pos / 131072.0 * 360.0;
			UTILS_LP_FAST(cfg->state.spi_error_rate, 0.0, 1.0 / LOOP_RATE);

			cfg->state.raw_status[0] = reply[1]; // SF
			cfg->state.raw_status[1] = reply[2]; // ABS0
			cfg->state.raw_status[2] = reply[3]; // ABS1
			cfg->state.raw_status[3] = reply[4]; // ABS2
			cfg->state.raw_status[4] = reply[6]; // ABM0
			cfg->state.raw_status[5] = reply[7]; // ABM1
			cfg->state.raw_status[6] = reply[8]; // ABM2
			cfg->state.raw_status[7] = reply[9]; // ALMC
		} else {
			++cfg->state.spi_error_cnt;
			UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, 1.0 / LOOP_RATE);
		}
	}
}
