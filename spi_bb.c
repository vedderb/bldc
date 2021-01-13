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
	palSetPadMode(s->mosi_gpio, s->mosi_pin, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(s->mosi_gpio, s->mosi_pin);
	palSetPad(s->nss_gpio, s->nss_pin);

	s->has_started = false;
	s->has_error = false;
}


uint8_t spi_bb_exchange_8(spi_bb_state *s, uint8_t x) {
	uint8_t rx;
	spi_bb_transfer_8(s ,&rx, &x, 1);
	return rx;
}

void spi_bb_transfer_8(spi_bb_state *s, uint8_t *in_buf, const uint8_t *out_buf, int length) {
	for (int i = 0; i < length; i++) {
		uint8_t send = out_buf ? out_buf[i] : 0xFF;
		uint8_t receive = 0;

		for (int bit = 0; bit < 8; bit++) {
			palWritePad(s->mosi_gpio, s->mosi_pin, send >> 7);
			send <<= 1;

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

		if (in_buf)	{
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
	for (volatile int i = 0; i < 40; i++) {
		__NOP();
	}
}
