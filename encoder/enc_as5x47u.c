/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Zach O'Brien

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

#include "enc_as5x47u.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils_math.h"
#include "spi_bb.h"
#include "timer.h"

#include <string.h>
#include <math.h>

#define AS5x47U_SPI_READ_BIT 								0x4000
#define AS5x47U_SPI_WRITE_BIT 								0x0000

#define AS5x47U_SPI_DIAG_FUSA_ERROR_BIT_POS					10
#define AS5x47U_SPI_DIAG_COF_BIT_POS						2
#define AS5x47U_SPI_DIAG_COMP_LOW_BIT_POS					3
#define AS5x47U_SPI_DIAG_COMP_HIGH_BIT_POS					4

#define AS5x47U_SPI_ERRFL_WDTST_BIT_POS						7
#define AS5x47U_SPI_ERRFL_CRC_ERROR_BIT_POS					6
#define AS5x47U_SPI_ERRFL_MAG_HALF_BIT_POS					1

#define AS5x47U_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK		0x3FFF
#define AS5x47U_SPI_AGC_MASK								0xFF
#define AS5x47U_SPI_WARN_FLAG_MASK							0x8000
#define AS5x47U_SPI_ERROR_FLAG_MASK							0x4000

#define AS5x47U_SPI_ERRFL_ADR								0x0001
#define AS5x47U_SPI_DIAG_ADR								0x3FF5
#define AS5x47U_SPI_MAGN_ADR								0x3FFD
#define AS5x47U_SPI_AGC_ADR									0x3FF9
#define AS5x47U_SPI_POS_ADR									0x3FFF

#define AS5x47U_SPI_READ_ERRFL_MSG			(AS5x47U_SPI_ERRFL_ADR | AS5x47U_SPI_READ_BIT)
#define AS5x47U_SPI_READ_DIAG_MSG			(AS5x47U_SPI_DIAG_ADR  | AS5x47U_SPI_READ_BIT)
#define AS5x47U_SPI_READ_MAGN_MSG			(AS5x47U_SPI_MAGN_ADR  | AS5x47U_SPI_READ_BIT)
#define AS5x47U_SPI_READ_AGC_MSG			(AS5x47U_SPI_AGC_ADR   | AS5x47U_SPI_READ_BIT)
#define AS5x47U_SPI_READ_POS_MSG			(AS5x47U_SPI_POS_ADR   | AS5x47U_SPI_READ_BIT)

#define AS5x47U_SPI_READ_ERRFL_CRC							(0x06)
#define AS5x47U_SPI_READ_DIAG_CRC							(0x6F)
#define AS5x47U_SPI_READ_MAGN_CRC							(0x87)
#define AS5x47U_SPI_READ_AGC_CRC							(0xF3)
#define AS5x47U_SPI_READ_POS_CRC							(0xBD)

#define AS5x47U_CONNECTION_DETERMINATOR_ERROR_THRESHOLD		5

enum {
	SPI_SEQ_TX_MAG_RX_POS,
	SPI_SEQ_TX_POS_RX_MAG,
	SPI_SEQ_TX_AGC_RX_POS,
	SPI_SEQ_TX_POS_RX_AGC,
	SPI_SEQ_TX_DIAG_RX_POS,
	SPI_SEQ_TX_POS_RX_DIAG,
	SPI_SEQ_TX_ERRFL_RX_POS,
	SPI_SEQ_TX_POS_RX_ERRFL,
	SPI_SEQ_PREV_ERR,
};

// Private functions
static void AS5x47U_determinate_if_connected(AS5x47U_config_t *cfg, bool was_last_valid);
static void AS5x47U_start_spi_exchange_precalc_crc(AS5x47U_config_t *cfg,
		uint16_t tx_data, uint8_t tx_crc);
static void AS5x47U_process_pos(AS5x47U_config_t *cfg, uint16_t posData);

static uint8_t enc_as5x47u_crc8(const uint8_t *data, size_t len, const uint8_t inital) {
	// Lookup table generated using poly 0x1D
	static const uint8_t crc8_lookup[] = {
		0x00, 0x1D, 0x3A, 0x27, 0x74, 0x69, 0x4E, 0x53, 0xE8, 0xF5, 0xD2, 0xCF, 0x9C, 0x81, 0xA6, 0xBB,
		0xCD, 0xD0, 0xF7, 0xEA, 0xB9, 0xA4, 0x83, 0x9E, 0x25, 0x38, 0x1F, 0x02, 0x51, 0x4C, 0x6B, 0x76,
		0x87, 0x9A, 0xBD, 0xA0, 0xF3, 0xEE, 0xC9, 0xD4, 0x6F, 0x72, 0x55, 0x48, 0x1B, 0x06, 0x21, 0x3C,
		0x4A, 0x57, 0x70, 0x6D, 0x3E, 0x23, 0x04, 0x19, 0xA2, 0xBF, 0x98, 0x85, 0xD6, 0xCB, 0xEC, 0xF1,
		0x13, 0x0E, 0x29, 0x34, 0x67, 0x7A, 0x5D, 0x40, 0xFB, 0xE6, 0xC1, 0xDC, 0x8F, 0x92, 0xB5, 0xA8,
		0xDE, 0xC3, 0xE4, 0xF9, 0xAA, 0xB7, 0x90, 0x8D, 0x36, 0x2B, 0x0C, 0x11, 0x42, 0x5F, 0x78, 0x65,
		0x94, 0x89, 0xAE, 0xB3, 0xE0, 0xFD, 0xDA, 0xC7, 0x7C, 0x61, 0x46, 0x5B, 0x08, 0x15, 0x32, 0x2F,
		0x59, 0x44, 0x63, 0x7E, 0x2D, 0x30, 0x17, 0x0A, 0xB1, 0xAC, 0x8B, 0x96, 0xC5, 0xD8, 0xFF, 0xE2,
		0x26, 0x3B, 0x1C, 0x01, 0x52, 0x4F, 0x68, 0x75, 0xCE, 0xD3, 0xF4, 0xE9, 0xBA, 0xA7, 0x80, 0x9D,
		0xEB, 0xF6, 0xD1, 0xCC, 0x9F, 0x82, 0xA5, 0xB8, 0x03, 0x1E, 0x39, 0x24, 0x77, 0x6A, 0x4D, 0x50,
		0xA1, 0xBC, 0x9B, 0x86, 0xD5, 0xC8, 0xEF, 0xF2, 0x49, 0x54, 0x73, 0x6E, 0x3D, 0x20, 0x07, 0x1A,
		0x6C, 0x71, 0x56, 0x4B, 0x18, 0x05, 0x22, 0x3F, 0x84, 0x99, 0xBE, 0xA3, 0xF0, 0xED, 0xCA, 0xD7,
		0x35, 0x28, 0x0F, 0x12, 0x41, 0x5C, 0x7B, 0x66, 0xDD, 0xC0, 0xE7, 0xFA, 0xA9, 0xB4, 0x93, 0x8E,
		0xF8, 0xE5, 0xC2, 0xDF, 0x8C, 0x91, 0xB6, 0xAB, 0x10, 0x0D, 0x2A, 0x37, 0x64, 0x79, 0x5E, 0x43,
		0xB2, 0xAF, 0x88, 0x95, 0xC6, 0xDB, 0xFC, 0xE1, 0x5A, 0x47, 0x60, 0x7D, 0x2E, 0x33, 0x14, 0x09,
		0x7F, 0x62, 0x45, 0x58, 0x0B, 0x16, 0x31, 0x2C, 0x97, 0x8A, 0xAD, 0xB0, 0xE3, 0xFE, 0xD9, 0xC4,
	};
	uint8_t cksum = inital;
	size_t i;
	for (i = 0; i < len; i++) {
		cksum ^= data[i];
		cksum = crc8_lookup[cksum];
	}
	return cksum;
}

void enc_as5x47u_spi_callback(SPIDriver *pspi) {
	if (pspi != NULL && pspi->app_arg != NULL) {
		AS5x47U_config_t *cfg = (AS5x47U_config_t*)pspi->app_arg;
		spiUnselectI(cfg->spi_dev);
	
		// Determine time step for error rate calculation
		float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
		if (timestep > 1.0) {
			timestep = 1.0;
		}
		cfg->state.last_update_time = timer_time_now();

		uint8_t rx_crc = cfg->state.rx_buf[2];
		uint8_t calc_crc = enc_as5x47u_crc8(cfg->state.rx_buf, 2, 0xC4) ^ 0xFF;
		uint16_t rx_data = cfg->state.rx_buf[0] << 8 | cfg->state.rx_buf[1];
		if (calc_crc == rx_crc) {
			AS5x47U_determinate_if_connected(cfg, true);
			cfg->state.sensor_diag.is_error =
				(uint8_t)((rx_data & AS5x47U_SPI_ERROR_FLAG_MASK) != 0);

			if (!cfg->state.sensor_diag.is_error) {
				UTILS_LP_FAST(cfg->state.spi_error_rate, 0.0, timestep);

				switch(cfg->state.spi_seq) {
				case SPI_SEQ_TX_MAG_RX_POS:
					// Receive position, then request position while receiving magnitude
					AS5x47U_process_pos(cfg, rx_data);
					cfg->state.spi_seq = SPI_SEQ_TX_POS_RX_MAG;
					AS5x47U_start_spi_exchange_precalc_crc(
						cfg, AS5x47U_SPI_READ_POS_MSG, AS5x47U_SPI_READ_POS_CRC);
					break;

				case SPI_SEQ_TX_POS_RX_MAG:
					// Receive magnitude
					cfg->state.sensor_diag.serial_magnitude = rx_data;
					cfg->state.sensor_diag.magnitude = rx_data
						& AS5x47U_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
					cfg->state.spi_seq = SPI_SEQ_TX_AGC_RX_POS;
					break;

				case SPI_SEQ_TX_AGC_RX_POS:
					// Receive position, then request position while receiving AGC
					AS5x47U_process_pos(cfg, rx_data);
					cfg->state.spi_seq = SPI_SEQ_TX_POS_RX_AGC;
					AS5x47U_start_spi_exchange_precalc_crc(
						cfg, AS5x47U_SPI_READ_POS_MSG, AS5x47U_SPI_READ_POS_CRC);
					break;

				case SPI_SEQ_TX_POS_RX_AGC:
					// Receive AGC
					cfg->state.sensor_diag.serial_AGC_value = rx_data;
					cfg->state.sensor_diag.AGC_value = (uint8_t)rx_data;
					cfg->state.spi_seq = SPI_SEQ_TX_DIAG_RX_POS;
					break;
				case SPI_SEQ_TX_DIAG_RX_POS:
					// Receive position, then request position while requesting diagnostic flags
					AS5x47U_process_pos(cfg, rx_data);
					cfg->state.spi_seq = SPI_SEQ_TX_POS_RX_DIAG;
					AS5x47U_start_spi_exchange_precalc_crc(
							cfg, AS5x47U_SPI_READ_POS_MSG, AS5x47U_SPI_READ_POS_CRC);
					break;

				case SPI_SEQ_TX_POS_RX_DIAG:
					// Receive diagnostic flags
					cfg->state.sensor_diag.serial_diag_flgs = rx_data;
					cfg->state.sensor_diag.is_broken_hall =
							(rx_data >> AS5x47U_SPI_DIAG_FUSA_ERROR_BIT_POS) & 1;
					cfg->state.sensor_diag.is_COF =
							(rx_data >> AS5x47U_SPI_DIAG_COF_BIT_POS) & 1;
					cfg->state.sensor_diag.is_Comp_low =
							(rx_data >> AS5x47U_SPI_DIAG_COMP_LOW_BIT_POS) & 1;
					cfg->state.sensor_diag.is_Comp_high =
							(rx_data >> AS5x47U_SPI_DIAG_COMP_HIGH_BIT_POS) & 1;
					cfg->state.spi_seq = SPI_SEQ_TX_ERRFL_RX_POS;
					break;

				case SPI_SEQ_TX_ERRFL_RX_POS:
					// Receive position, then request pos while receiving error flags
					AS5x47U_process_pos(cfg, rx_data);
					cfg->state.spi_seq = SPI_SEQ_TX_POS_RX_ERRFL;
					AS5x47U_start_spi_exchange_precalc_crc(
							cfg, AS5x47U_SPI_READ_POS_MSG, AS5x47U_SPI_READ_POS_CRC);
					break;
				case SPI_SEQ_TX_POS_RX_ERRFL:
					// Receive error flags
					cfg->state.spi_seq = 0;
					cfg->state.sensor_diag.serial_error_flgs = rx_data;
					cfg->state.sensor_diag.is_wdtst =
							(rx_data >> AS5x47U_SPI_ERRFL_WDTST_BIT_POS) & 1;
					cfg->state.sensor_diag.is_crc_error =
							(rx_data >> AS5x47U_SPI_ERRFL_CRC_ERROR_BIT_POS) & 1;
					cfg->state.sensor_diag.is_mag_half =
							(rx_data >> AS5x47U_SPI_ERRFL_MAG_HALF_BIT_POS) & 1;
					cfg->state.spi_seq = SPI_SEQ_TX_MAG_RX_POS;
					break;
				case SPI_SEQ_PREV_ERR:
					// Not sure what was just received, but just requested ERRFL, so ignore rx
					// data and prepare to receive ERRFL next exchange
					cfg->state.spi_seq = SPI_SEQ_TX_POS_RX_ERRFL;
					break;
				default:
					// Something went wrong, just start the sequence over.
					cfg->state.spi_seq = SPI_SEQ_TX_MAG_RX_POS;
					break;
				}
			} else {
				// Error flag is set
				AS5x47U_determinate_if_connected(cfg, true); // Encoder is connected...
				UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, timestep); // But there is an error
				++cfg->state.spi_error_cnt;
				// Error flag is set so need to read error register next
				cfg->state.spi_seq = SPI_SEQ_PREV_ERR;
			}
		} else {
			// CRC error
			AS5x47U_determinate_if_connected(cfg, false);
			UTILS_LP_FAST(cfg->state.spi_error_rate, 1.0, timestep);
			++cfg->state.spi_error_cnt;
			cfg->state.spi_seq = SPI_SEQ_PREV_ERR;
		}
	}
}

static void as5x47u_spi_err_callback(SPIDriver *pspi) {
	if(pspi != NULL && pspi->app_arg != NULL) {
		AS5x47U_config_t *cfg = (AS5x47U_config_t*)pspi->app_arg;
		// Make sure we won't process the data
		memset(cfg->state.rx_buf, 0, sizeof(cfg->state.rx_buf));
	}
}

bool enc_as5x47u_init(AS5x47U_config_t *cfg) {
	if (cfg->spi_dev == NULL) {
		return false;
	}

	memset(&cfg->state, 0, sizeof(AS5x47U_state));

	palSetPadMode(cfg->sck_gpio, cfg->sck_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->miso_gpio, cfg->miso_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->nss_gpio, cfg->nss_pin,
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin,
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);

	cfg->spi_dev->app_arg = (void*)cfg;
	cfg->spi_dev->err_cb = as5x47u_spi_err_callback;

	// Set in encoder_cfg.c, kept here for visability
	// cfg->spi_dev->config->end_cb = enc_as5x47u_spi_callback;

	spiStart(cfg->spi_dev, &(cfg->hw_spi_cfg));

	cfg->state.spi_error_rate = 0.0;

	return true;
}

void enc_as5x47u_deinit(AS5x47U_config_t *cfg) {
	if (cfg->spi_dev != NULL) {
		palSetPadMode(cfg->miso_gpio, cfg->miso_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->sck_gpio, cfg->sck_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->nss_gpio, cfg->nss_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin, PAL_MODE_INPUT_PULLUP);

		spiStop(cfg->spi_dev);

		cfg->state.last_enc_angle = 0.0;
		cfg->state.spi_error_rate = 0.0;
	}
}

void enc_as5x47u_routine(AS5x47U_config_t *cfg) {
	switch(cfg->state.spi_seq) {
	case SPI_SEQ_TX_MAG_RX_POS:
		// Request magnitude
		AS5x47U_start_spi_exchange_precalc_crc(
			cfg, AS5x47U_SPI_READ_MAGN_MSG, AS5x47U_SPI_READ_MAGN_CRC);
		break;

	case SPI_SEQ_TX_AGC_RX_POS:
		// Request AGC
		AS5x47U_start_spi_exchange_precalc_crc( cfg, AS5x47U_SPI_READ_AGC_MSG,
				AS5x47U_SPI_READ_AGC_CRC);
		break;

	case SPI_SEQ_TX_DIAG_RX_POS:
		// Request diagnostic flags
		AS5x47U_start_spi_exchange_precalc_crc( cfg, AS5x47U_SPI_READ_DIAG_MSG,
				AS5x47U_SPI_READ_DIAG_CRC);
		break;

	case SPI_SEQ_TX_ERRFL_RX_POS:
		// Request error flags
		AS5x47U_start_spi_exchange_precalc_crc(cfg, AS5x47U_SPI_READ_ERRFL_MSG,
				AS5x47U_SPI_READ_ERRFL_CRC);
		break;

	default:
	case SPI_SEQ_PREV_ERR:
		// There was a problem of some sort, request ERRFL
		AS5x47U_start_spi_exchange_precalc_crc(cfg, AS5x47U_SPI_READ_ERRFL_MSG,
				AS5x47U_SPI_READ_ERRFL_CRC);
		break;
	}
}

static void AS5x47U_process_pos(AS5x47U_config_t *cfg, uint16_t posData) {
	cfg->state.spi_val = posData;
	posData &= AS5x47U_SPI_EXCLUDE_PARITY_AND_ERROR_BITMASK;
	cfg->state.last_enc_angle = (float)(posData * 360) / (float)(1 << 14);
}

static void AS5x47U_determinate_if_connected(AS5x47U_config_t *cfg, bool was_last_valid) {
	if (!was_last_valid) {
		cfg->state.spi_communication_error_count++;

		if (cfg->state.spi_communication_error_count >=
				AS5x47U_CONNECTION_DETERMINATOR_ERROR_THRESHOLD) {
			cfg->state.spi_communication_error_count =
				AS5x47U_CONNECTION_DETERMINATOR_ERROR_THRESHOLD;
			cfg->state.sensor_diag.is_connected = 0;
		}
	} else {
		if (cfg->state.spi_communication_error_count) {
			cfg->state.spi_communication_error_count--;
		} else {
			cfg->state.sensor_diag.is_connected = 1;
		}
	}
}

/* TODO: uncomment if needed
static void AS5x47U_start_spi_exchange(AS5x47U_config_t *cfg, uint16_t tx_data) {
	uint8_t tx_to_crc[] = {(tx_data >> 8) & 0xFF, (tx_data & 0xFF)};
	uint8_t tx_crc = enc_as5x47u_crc8(tx_to_crc, 2, 0xC4) ^ 0xFF;
	AS5x47U_start_spi_exchange_precalc_crc(cfg, tx_data, tx_crc);
}
*/

static void AS5x47U_start_spi_exchange_precalc_crc(AS5x47U_config_t *cfg,
		uint16_t tx_data, uint8_t tx_crc) {
	cfg->state.tx_buf[0] = (tx_data >> 8) & 0xFF;
	cfg->state.tx_buf[1] = tx_data & 0xFF;
	cfg->state.tx_buf[2] = tx_crc;
	memset(cfg->state.rx_buf, 0, sizeof(cfg->state.rx_buf));

	// There is a weird corner case where the DMA may not read all of the Rx data. This
	// causes the RXNE flag to be set when an exchange starts, causing the first byte of
	// data received to be from the previous exchange. This is corrected by reading the
	// SPI data register, clearing the RXNE flag.
	volatile uint32_t test = cfg->spi_dev->spi->DR;
	(void)test; // get rid of unused warning

	spiSelectI(cfg->spi_dev);
	spiStartExchangeI(cfg->spi_dev, 3, cfg->state.tx_buf, cfg->state.rx_buf);
}
