/*
	Copyright 2015 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

#include "spi_sw.h"
#include "ch.h"
#include "hal.h"
#include <stdbool.h>

// Private variables
static bool init_done = false;

// Private functions
static void spi_sw_delay(void);

void spi_sw_init(void) {
	if (!init_done) {
		palSetPadMode(NRF_PORT_MISO, NRF_PIN_MISO, PAL_MODE_INPUT);
		palSetPadMode(NRF_PORT_CSN, NRF_PIN_CSN, PAL_MODE_OUTPUT_PUSHPULL);
		palSetPadMode(NRF_PORT_SCK, NRF_PIN_SCK, PAL_MODE_OUTPUT_PUSHPULL);
		palSetPadMode(NRF_PORT_MOSI, NRF_PIN_MOSI, PAL_MODE_OUTPUT_PUSHPULL);

		palSetPad(NRF_PORT_CSN, NRF_PIN_CSN);
		palClearPad(NRF_PORT_SCK, NRF_PIN_SCK);
		init_done = true;
	}
}

void spi_sw_transfer(char *in_buf, const char *out_buf, int length) {
	palClearPad(NRF_PORT_SCK, NRF_PIN_SCK);
	spi_sw_delay();

	for (int i = 0;i < length;i++) {
		unsigned char send = out_buf ? out_buf[i] : 0;
		unsigned char recieve = 0;

		for (int bit=0;bit < 8;bit++) {
			palWritePad(NRF_PORT_MOSI, NRF_PIN_MOSI, send >> 7);
			send <<= 1;

			spi_sw_delay();

			recieve <<= 1;
			if (palReadPad(NRF_PORT_MISO, NRF_PIN_MISO)) {
				recieve |= 0x1;
			}

			palSetPad(NRF_PORT_SCK, NRF_PIN_SCK);
			spi_sw_delay();
			palClearPad(NRF_PORT_SCK, NRF_PIN_SCK);
		}

		if (in_buf) {
			in_buf[i] = recieve;
		}
	}
}

void spi_sw_begin(void) {
	palClearPad(NRF_PORT_CSN, NRF_PIN_CSN);
	spi_sw_delay();
}

void spi_sw_end(void) {
	spi_sw_delay();
	palSetPad(NRF_PORT_CSN, NRF_PIN_CSN);
}

static void spi_sw_delay(void) {
	for (volatile int i = 0;i < 5;i++) {
		__NOP();
	}
}
