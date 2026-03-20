
/*
	Copyright 2026 Igor Gorniak gorniak.igor@gmail.com
	Copyright 2016 - 2026 Benjamin Vedder	benjamin@vedder.se

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

#include "enc_ma782.h"

#include <math.h>
#include <string.h>

#include "ch.h"
#include "hal.h"
#include "mc_interface.h"
#include "utils_math.h"
#include "spi_bb.h"
#include "timer.h"
#include "commands.h"
#include "hw.h"


#define BIT_MASK(l, h)  (((1U << ((h) + 1)) - 1) & ~((1U << (l)) - 1))

#define MA782_READ_REG_CMD 0x40
#define MA782_READ_REG_ADDR(x) ((x) & BIT_MASK(0, 4))
#define MA782_WRITE_REG_CMD 0x80
#define MA782_WRITE_REG_ADDR(x) ((x) & BIT_MASK(0, 4))

#define MA782_REG_FW 				0x0E

#define MA782_FILTER_WINDOW 		6
#define MA782_RESOLUTION_BITS 		9
#define MA782_MAX_RESOLUTION_BITS 	12

static void ma782_error(ma782_state_t *state, unsigned err) {
	state->error |= err;
	state->error_count++;
}

static uint16_t ma782_resolution_mask(ma782_state_t *state) {
	(void)(state);
	return BIT_MASK(4 + (MA782_MAX_RESOLUTION_BITS - MA782_RESOLUTION_BITS), 15);
}

static uint8_t ma782_read_reg(ma782_config_t *cfg, uint8_t reg_addr) {
	ma782_state_t *state = &cfg->state;

	if (state->substate != MA782_IDLE) {
		ma782_error(state, MA782_READ_NOT_IDLE);
		return false;
	}

	state->tx_data = (MA782_READ_REG_CMD | MA782_READ_REG_ADDR(reg_addr)) << 8;

	spiSelectI(cfg->spi_dev);
	__NOP();
	spiPolledExchange(cfg->spi_dev, state->tx_data);
	spiUnselectI(cfg->spi_dev);
	__NOP();


	spiSelectI(cfg->spi_dev);
	__NOP();
	state->rx_data = spiPolledExchange(cfg->spi_dev, 0);
	spiUnselectI(cfg->spi_dev);

	return state->rx_data & BIT_MASK(0, 7);
}

static bool ma782_write_reg(ma782_config_t *cfg, uint8_t reg_addr, uint8_t reg_val) {
	ma782_state_t *state = &cfg->state;

	if (state->substate != MA782_IDLE) {
		ma782_error(state, MA782_WRITE_NOT_IDLE);
		return false;
	}

	state->tx_data = ((MA782_WRITE_REG_CMD |\
					   MA782_WRITE_REG_ADDR(reg_addr)) << 8)
				     | reg_val;

	spiSelectI(cfg->spi_dev);
	spiPolledExchange(cfg->spi_dev, state->tx_data);
	spiUnselectI(cfg->spi_dev);

	__NOP();

	spiSelectI(cfg->spi_dev);
	state->rx_data = spiPolledExchange(cfg->spi_dev, 0);
	spiUnselectI(cfg->spi_dev);

	if ((state->rx_data & BIT_MASK(0, 7)) != reg_val) {
		ma782_error(state, MA782_WRITE_REG_FAIL);
		return false;
	}

	if (ma782_read_reg(cfg, reg_addr) != reg_val) {
		ma782_error(state, MA782_WRITE_READOUT_FAIL);
		return false;
	}

	return true;
}

static bool ma782_read_angle(ma782_config_t *cfg) {
	ma782_state_t *state = &cfg->state;

	if (state->substate != MA782_IDLE) {
		ma782_error(state, MA782_ANGLE_NOT_IDLE);
		return false;
	}

	state->tx_data = 0;

	// There is a weird corner case where the DMA may not read all of the Rx data. This
	// causes the RXNE flag to be set when an exchange starts, causing the first byte of
	// data received to be from the previous exchange. This is corrected by reading the
	// SPI data register, clearing the RXNE flag.
	volatile uint32_t test = cfg->spi_dev->spi->DR;
	(void)test; // get rid of unused warning

	state->substate = MA782_READ_ANGLE_REQ;

	spiSelectI(cfg->spi_dev);
	__NOP();
	spiStartExchangeI(cfg->spi_dev, 1, &state->tx_data, &state->rx_data);

	return true;
}

static bool ma782_read_angle_finish(ma782_config_t *cfg) {
	ma782_state_t *state = &cfg->state;

	uint16_t angle = (uint16_t)state->rx_data;
	angle &= ma782_resolution_mask(state);

	float ret = (float)angle * (360.0f / 65535.0f);
	state->last_enc_angle = ret;

	state->substate = MA782_IDLE;

	return true;
}

static void ma782_error_cb(SPIDriver *pspi) {
	if(pspi != NULL && pspi->app_arg != NULL) {
		ma782_config_t *cfg = (ma782_config_t*)pspi->app_arg;
		ma782_state_t *state = &cfg->state;

		ma782_error(state, MA782_SPI_ERROR);
	}
}

static void ma782_init_config(ma782_config_t *cfg) {
	memset(&cfg->state, 0, sizeof(ma782_state_t));
}

void enc_ma782_routine(ma782_config_t *cfg) {
	ma782_state_t *state = &cfg->state;

	if (cfg->spi_dev->state == SPI_READY) {
		if(state->start > 0) {
			ma782_read_angle(cfg);
		}
		UTILS_LP_FAST(state->spi_comm_error_rate, 0.0, 0.0001);
	} else {
		++state->spi_comm_error_cnt;
		ma782_error(state, MA782_SPI_NOT_READY);
		// compute rate with factor 0.0001 for 10000hz
		UTILS_LP_FAST(state->spi_comm_error_rate, 1.0, 0.0001);
	}
}

void compute_ma782_callback(SPIDriver *pspi) {
	if (pspi != NULL && pspi->app_arg != NULL) {

		ma782_config_t *cfg = (ma782_config_t*)pspi->app_arg;
		ma782_state_t *state = &cfg->state;

		spiUnselectI(cfg->spi_dev);

		state->spi_cnt++;

		switch (state->substate) {
			case MA782_IDLE:
				ma782_error(state, MA782_CALLBACK_IN_IDLE);
				break;
			case MA782_READ_ANGLE_REQ:
				ma782_read_angle_finish(cfg);
				break;
			default:
				ma782_error(state, MA782_UNKNOWN_STATE);
				break;
		}
	}
}

static uint8_t ma782_read_fw(ma782_config_t *cfg) {
	return ma782_read_reg(cfg, MA782_REG_FW) >> 4;
}

static bool ma782_write_fw(ma782_config_t *cfg, uint8_t value) {
	return ma782_write_reg(cfg, MA782_REG_FW, (value & 0x0F) << 4);
}

static bool ma782_set_filter_window(ma782_config_t *cfg, uint8_t value) {
	if (ma782_read_fw(cfg) != value) {
		return ma782_write_fw(cfg, value);
	}
	return true;
}

bool enc_ma782_init(ma782_config_t *cfg) {
	ma782_state_t *state = &cfg->state;

	if (cfg->spi_dev == NULL) {
		return false;
	}

	ma782_init_config(cfg);

	cfg->spi_dev->app_arg = (void*)cfg;
	cfg->spi_dev->err_cb = ma782_error_cb;

	palSetPadMode(cfg->sck_gpio, cfg->sck_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->miso_gpio, cfg->miso_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(cfg->nss_gpio, cfg->nss_pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->en_gpio, cfg->en_pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	chThdSleepMilliseconds(1);
	palSetPad(cfg->en_gpio, cfg->en_pin);
	chThdSleepMilliseconds(1);
	spiStart(cfg->spi_dev, &(cfg->hw_spi_cfg));
	chThdSleepMilliseconds(1);

	bool ret = true;
	if (!ma782_set_filter_window(cfg, MA782_FILTER_WINDOW)) {
		ret = false;
	}

	if (ret == true) {
		state->start = 1;
	}

	return ret;
}

void enc_ma782_deinit(ma782_config_t *cfg) {
	if (cfg->spi_dev != NULL) {

		palClearPad(cfg->en_gpio, cfg->en_pin);

		palSetPadMode(cfg->miso_gpio, cfg->miso_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->sck_gpio, cfg->sck_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->nss_gpio, cfg->nss_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->en_gpio, cfg->en_pin, PAL_MODE_INPUT_PULLDOWN);

		spiStop(cfg->spi_dev);

		cfg->state.last_enc_angle = 0.0f;
		cfg->state.spi_error_cnt = 0;
		cfg->state.spi_error_rate = 0.0f;
	}
}

void enc_ma782_print_status(ma782_config_t *cfg) {
	ma782_state_t *state = &cfg->state;
	commands_printf("MA782 STATUS:\n"
				"Last angle       : %.3f\n"
				"Error       : 0x%04x\n"
				"Error count : %u\n"
				"SPI cnt     : %u\n"
				"Last TX     : %04x\n"
				"Last RX     : %04x\n"
				"Substate    : %u\n"
				"Start       : %u\n"
				,
				(double)state->last_enc_angle,
				(unsigned)state->error,
				(unsigned)state->error_count,
				(unsigned)state->spi_cnt,
				(unsigned)state->tx_data,
				(unsigned)state->rx_data,
				(unsigned)state->substate,
				(unsigned)state->start
	);
}
