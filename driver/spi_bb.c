/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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

#include "spi_bb.h"
#include "timer.h"

// Software SPI

void spi_bb_init(spi_bb_state *s) {
	chMtxObjectInit(&s->mutex);

	palSetPadMode(s->miso_gpio, s->miso_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(s->sck_gpio, s->sck_pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(s->nss_gpio, s->nss_pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	if (s->mosi_gpio) {
		palSetPadMode(s->mosi_gpio, s->mosi_pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
		palSetPad(s->mosi_gpio, s->mosi_pin);
		palSetPad(s->nss_gpio, s->nss_pin);
	}
}

void spi_bb_deinit(spi_bb_state *s) {
	palSetPadMode(s->miso_gpio, s->miso_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(s->sck_gpio, s->sck_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(s->nss_gpio, s->nss_pin, PAL_MODE_INPUT_PULLUP);

	if (s->mosi_gpio) {
		palSetPadMode(s->mosi_gpio, s->mosi_pin, PAL_MODE_INPUT_PULLUP);
	}
}

void ssc_bb_init(spi_bb_state *s) {
	chMtxObjectInit(&s->mutex);

	if (s->mosi_gpio && s->nss_gpio && s->sck_gpio) { // TODO: test
		palSetPadMode(s->mosi_gpio, s->mosi_pin, PAL_MODE_INPUT_PULLUP);
		palSetPadMode(s->sck_gpio, s->sck_pin,
				PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
		palSetPadMode(s->nss_gpio, s->nss_pin,
				PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	}

	palClearPad(s->sck_gpio, s->sck_pin);
	palSetPad(s->nss_gpio, s->nss_pin);
}

void ssc_bb_deinit(spi_bb_state *s) {
	chMtxObjectInit(&s->mutex);

	palSetPadMode(s->mosi_gpio, s->miso_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(s->sck_gpio, s->sck_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(s->nss_gpio, s->nss_pin, PAL_MODE_INPUT_PULLUP);
}

uint8_t spi_bb_exchange_8(spi_bb_state *s, uint8_t x) {
	uint8_t rx;
	spi_bb_transfer_8(s, &rx, &x, 1);
	return rx;
}


void spi_bb_transfer_8(
		spi_bb_state *s, 
		uint8_t *in_buf, 
		const uint8_t *out_buf,
		int length
		) {
	for (int i = 0; i < length; i++) {
		uint8_t send = out_buf ? out_buf[i] : 0xFF;
		uint8_t receive = 0;

		for (int bit = 0; bit < 8; bit++) {
			if(s->mosi_gpio) {
				palWritePad(s->mosi_gpio, s->mosi_pin, send >> 7);
				send <<= 1;
			}

			palSetPad(s->sck_gpio, s->sck_pin);
			spi_bb_delay();

			int samples = 0;
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);

			palClearPad(s->sck_gpio, s->sck_pin);

			// does 5 samples of each pad read, to minimize noise
			receive <<= 1;
			if (samples > 2) {
				receive |= 1;
			}

			spi_bb_delay();
		}

		if (in_buf) {
			in_buf[i] = receive;
		}
	}
}

void spi_bb_transfer_16(
		spi_bb_state *s, 
		uint16_t *in_buf, 
		const uint16_t *out_buf, 
		int length
		) {
	for (int i = 0; i < length; i++) {
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
		uint16_t receive = 0;

		for (int bit = 0; bit < 16; bit++) {
			if(s->mosi_gpio) {
				palWritePad(s->mosi_gpio, s->mosi_pin, send >> 15);
				send <<= 1;
			}

			palSetPad(s->sck_gpio, s->sck_pin);
			spi_bb_delay_short();

			int samples = 0;
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);
			__NOP();
			samples += palReadPad(s->miso_gpio, s->miso_pin);

			receive <<= 1;
			if (samples > 2) {
				receive |= 1;
			}

			palClearPad(s->sck_gpio, s->sck_pin);
			spi_bb_delay_short();
		}

		if (in_buf) {
			in_buf[i] = receive;
		}
	}
}

/**
 * @brief 
 * Software data transfer using SSC protocol.
 * (SSC = SPI with mosi/miso combined into 1 bidirectional wire)
 * 
 * @param s  		pointer software spi state
 * @param in_buf 	pointer to empty rx array of uint16_t with same N as length
 * @param out_buf 	pointer to tx array of uint16_t with same N as length
 * @param length 	number of 16 bit transfers
 * @param write 	0 = read, 1 = write
 */
void ssc_bb_transfer_16(
		spi_bb_state *s, 
		uint16_t *in_buf, 
		const uint16_t *out_buf, 
		int length, 
		bool write
		) {

	for (int i = 0; i < length; i++) {
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
		uint16_t receive = 0;

		//ssc uses mosi for all data
		if(write && s->mosi_gpio){
			palWritePad(s->mosi_gpio, s->mosi_pin, send >> 15); 
			palSetPadMode(s->mosi_gpio, s->mosi_pin,
				PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
		} else {
			palSetPadMode(s->mosi_gpio, s->mosi_pin, PAL_MODE_INPUT_PULLUP);
			write = false;
		}

		for (int bit = 0; bit < 16; bit++) {
			// Data is put on the data line with the rising edge of SCK 
			// and read with the falling edge of SCK. (tle5012)
			palSetPad(s->sck_gpio, s->sck_pin); 
			
			if(write){
				palWritePad(s->mosi_gpio, s->mosi_pin, send >> 15); 
				send <<= 1;
			}

			spi_bb_delay_short();
			palClearPad(s->sck_gpio, s->sck_pin);
			spi_bb_delay_short();

			// read when clk low
			receive <<= 1;
			receive |= palReadPad(s->mosi_gpio, s->mosi_pin);
		}

		if (in_buf) {
			in_buf[i] = receive;
		}
	}
}

void spi_bb_begin(spi_bb_state *s) {
	spi_bb_delay();
	palClearPad(s->nss_gpio, s->nss_pin);
	spi_bb_delay();
}

void spi_bb_end(spi_bb_state *s) {
	spi_bb_delay();
	palSetPad(s->nss_gpio, s->nss_pin);
	spi_bb_delay();
}

void spi_bb_delay(void) {
	// ~1500 ns long
	for (volatile int i = 0; i < 6; i++) {
		__NOP();
	}
}

void spi_bb_delay_short(void) {
	__NOP(); __NOP();
	__NOP(); __NOP();
}

bool spi_bb_check_parity(uint16_t x) {
	x ^= x >> 8;
	x ^= x >> 4;
	x ^= x >> 2;
	x ^= x >> 1;
	return (~x) & 1;
}
