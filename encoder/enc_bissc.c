/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Marcos Chaparro	mchaparro@powerdesigns.ca
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

#include "enc_bissc.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils_math.h"
#include "spi_bb.h"
#include "timer.h"

#include <math.h>
#include <string.h>

bool enc_bissc_init(BISSC_config_t *cfg) {
	if (cfg->spi_dev == NULL) {
		return false;
	}

	memset(&cfg->state, 0, sizeof(BISSC_state));

	palSetPadMode(cfg->sck_gpio, cfg->sck_pin, 
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->miso_gpio, cfg->miso_pin, 
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->nss_gpio, cfg->nss_pin, 
			PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin, 
			PAL_MODE_ALTERNATE(cfg->spi_af) | PAL_STM32_OSPEED_HIGHEST);

	cfg->spi_dev->app_arg = (void*)cfg;

	//Start driver with BissC SPI settings
	spiStart(cfg->spi_dev, &(cfg->hw_spi_cfg));

	cfg->state.spi_data_error_cnt = 0;
	cfg->state.spi_data_error_rate = 0.0;
	cfg->state.spi_comm_error_cnt = 0;
	cfg->state.spi_comm_error_rate = 0.0;

	//Init CRC table
	uint8_t POLY = 0x43;
	for(int i = 0; i < 64; i++){
		int crc = i;

		for (int j = 0; j < 6; j++){
			if (crc & 0x20){
				crc <<= 1;
				crc ^= POLY;
			} else {
				crc <<= 1;
			}
		}
		cfg->tableCRC6n[i] = crc;
	}

	return true;
}

void enc_bissc_deinit(BISSC_config_t *cfg) {
	if (cfg->spi_dev != NULL) {
		palSetPadMode(cfg->miso_gpio, cfg->miso_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->sck_gpio, cfg->sck_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->nss_gpio, cfg->nss_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(cfg->mosi_gpio, cfg->mosi_pin, PAL_MODE_INPUT_PULLUP);

		spiStop(cfg->spi_dev);

		cfg->state.last_enc_angle = 0.0;
		cfg->state.spi_data_error_cnt = 0;
		cfg->state.spi_data_error_rate = 0.0;
		cfg->state.spi_comm_error_cnt = 0;
		cfg->state.spi_comm_error_rate = 0.0;
	}
}

void enc_bissc_routine(BISSC_config_t *cfg) {
	if (cfg->spi_dev->state == SPI_READY) {
		spiSelectI(cfg->spi_dev);
		spiStartReceiveI(cfg->spi_dev, 8, (void *)cfg->state.decod_buf);
		UTILS_LP_FAST(encoder_cfg_bissc.state.spi_comm_error_rate, 0.0, 0.0001);
	} else {
		++encoder_cfg_bissc.state.spi_comm_error_cnt;
		// compute rate with factor 0.0001 for 10000hz
		UTILS_LP_FAST(encoder_cfg_bissc.state.spi_comm_error_rate, 1.0, 0.0001);
	}
}

void compute_bissc_callback(SPIDriver *pspi) {
	if (pspi != NULL && pspi->app_arg != NULL) {
		BISSC_config_t *cfg = (BISSC_config_t*)pspi->app_arg;
		spiUnselectI(cfg->spi_dev);

		float timestep = timer_seconds_elapsed_since(cfg->state.last_update_time);
		if (timestep > 1.0) {
			timestep = 1.0;
		}
		cfg->state.last_update_time = timer_time_now();

		int lenghtDataBit = cfg->enc_res;

		uint64_t rxData64;
		rxData64 = (uint64_t)cfg->state.decod_buf[0] << 56;
		rxData64 |= (uint64_t)cfg->state.decod_buf[1] << 48;
		rxData64 |= (uint64_t)cfg->state.decod_buf[2] << 40;
		rxData64 |= (uint64_t)cfg->state.decod_buf[3] << 32;
		rxData64 |= (uint64_t)cfg->state.decod_buf[4] << 24;
		rxData64 |= (uint64_t)cfg->state.decod_buf[5] << 16;
		rxData64 |= (uint64_t)cfg->state.decod_buf[6] << 8;
		rxData64 |= (uint64_t)cfg->state.decod_buf[7];

		// sample of rxData64
		// like this 1100000000000000100001100111010000000101110111100000000000000000
		rxData64 <<= __builtin_clzll(rxData64);		// slice rxData to have a value starting with 1
		rxData64 &= 0x3FFFFFFFFFFFFFFF; 			// remove the 2 first bit

		// remove the first 1, count how many digit stay in buffer after removing the 0, if there is more than 32 digits,
		// keep only 32st (on the left)
		// 32 because the format is : (1+1+lenghtDataBit+1+1+6) - Align bitstream to left (Startbit, CDS, 22-bit Position, Error, Warning, CRC)
		// int nbBit = log2(rxData64)+1;
		int nbBit =  64-__builtin_clzll(rxData64);  // Much faster than log2
		if ( nbBit >= ( lenghtDataBit + 10 ) ) {
			rxData64 >>= nbBit-( lenghtDataBit + 10 );
		}

		uint8_t crcRx = rxData64 & 0x3F; 									 //extract last 6-bit digits to get CRC
		uint32_t dataRx = (rxData64 >> 6) & ((1<<(lenghtDataBit + 2)) - 1);  //Shift out CRC, AND with 24-bit mask to get raw data (position, error, warning)
		cfg->state.spi_val = (dataRx >> 2) & ((1<<lenghtDataBit) - 1); 			     //Shift out error and warning, AND with 22-bit mask to get position

		uint8_t crc = 0;  //CRC seed is 0b000000
		crc = ((dataRx >> 30) & 0x03);
		crc = cfg->tableCRC6n[((dataRx >> 24) & 0x3F) ^ crc];
		crc = cfg->tableCRC6n[((dataRx >> 18) & 0x3F) ^ crc];
		crc = cfg->tableCRC6n[((dataRx >> 12) & 0x3F) ^ crc];
		crc = cfg->tableCRC6n[((dataRx >> 6) & 0x3F) ^ crc];
		crc = cfg->tableCRC6n[((dataRx >> 0) & 0x3F) ^ crc];
		crc = 0x3F & ~crc; //CRC is output inverted

		if(crc != crcRx)
		{
			++cfg->state.spi_data_error_cnt;
			UTILS_LP_FAST(cfg->state.spi_data_error_rate, 1.0, timestep);
		} else {
			UTILS_LP_FAST(cfg->state.spi_data_error_rate, 0.0, timestep);
			cfg->state.last_enc_angle = ((float)cfg->state.spi_val * 360.0) / ((1<<lenghtDataBit) - 1);
		}

	}

}