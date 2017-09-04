/*
	Copyright 2016-2017 Benjamin Vedder	benjamin@vedder.se

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

#include "spi_sw.h"
#include "utils.h"
#include <stdbool.h>

// Private variables
static bool m_init_done = false;
static stm32_gpio_t *m_port_csn = NRF_PORT_CSN;
static int m_pin_csn = NRF_PIN_CSN;
static stm32_gpio_t *m_port_sck = NRF_PORT_SCK;
static int m_pin_sck = NRF_PIN_SCK;
static stm32_gpio_t *m_port_mosi = NRF_PORT_MOSI;
static int m_pin_mosi = NRF_PIN_MOSI;
static stm32_gpio_t *m_port_miso = NRF_PORT_MISO;
static int m_pin_miso = NRF_PIN_MISO;

// Private functions
static void spi_sw_delay(void);

void spi_sw_init(void) {
	if (!m_init_done) {
		palSetPadMode(m_port_miso, m_pin_miso, PAL_MODE_INPUT);
		palSetPadMode(m_port_csn, m_pin_csn, PAL_MODE_OUTPUT_PUSHPULL);
		palSetPadMode(m_port_sck, m_pin_sck, PAL_MODE_OUTPUT_PUSHPULL);
		palSetPadMode(m_port_mosi, m_pin_mosi, PAL_MODE_OUTPUT_PUSHPULL);

		palSetPad(m_port_csn, m_pin_csn);
		palClearPad(m_port_sck, m_pin_sck);
		m_init_done = true;
	}
}

void spi_sw_stop(void) {
	palSetPadMode(m_port_miso, m_pin_miso, PAL_MODE_INPUT);
	palSetPadMode(m_port_csn, m_pin_csn, PAL_MODE_INPUT);
	palSetPadMode(m_port_sck, m_pin_sck, PAL_MODE_INPUT);
	palSetPadMode(m_port_mosi, m_pin_mosi, PAL_MODE_INPUT);
	m_init_done = false;
}

void spi_sw_change_pins(
		stm32_gpio_t *port_csn, int pin_csn,
		stm32_gpio_t *port_sck, int pin_sck,
		stm32_gpio_t *port_mosi, int pin_mosi,
		stm32_gpio_t *port_miso, int pin_miso) {

	bool init_was_done = m_init_done;

	if (init_was_done) {
		spi_sw_stop();
	}

	m_port_csn = port_csn;
	m_pin_csn = pin_csn;
	m_port_sck = port_sck;
	m_pin_sck = pin_sck;
	m_port_mosi = port_mosi;
	m_pin_mosi = pin_mosi;
	m_port_miso = port_miso;
	m_pin_miso = pin_miso;

	if (init_was_done) {
		spi_sw_init();
	}
}

void spi_sw_transfer(char *in_buf, const char *out_buf, int length) {
	palClearPad(m_port_sck, m_pin_sck);
	spi_sw_delay();

	for (int i = 0;i < length;i++) {
		unsigned char send = out_buf ? out_buf[i] : 0;
		unsigned char recieve = 0;

		for (int bit=0;bit < 8;bit++) {
			palWritePad(m_port_mosi, m_pin_mosi, send >> 7);
			send <<= 1;

			spi_sw_delay();

			int r1, r2, r3;
			r1 = palReadPad(m_port_miso, m_pin_miso);
			__NOP();
			r2 = palReadPad(m_port_miso, m_pin_miso);
			__NOP();
			r3 = palReadPad(m_port_miso, m_pin_miso);

			recieve <<= 1;
			if (utils_middle_of_3_int(r1, r2, r3)) {
				recieve |= 1;
			}

			palSetPad(m_port_sck, m_pin_sck);
			spi_sw_delay();
			palClearPad(m_port_sck, m_pin_sck);
		}

		if (in_buf) {
			in_buf[i] = recieve;
		}
	}
}

void spi_sw_begin(void) {
	palClearPad(m_port_csn, m_pin_csn);
	spi_sw_delay();
}

void spi_sw_end(void) {
	spi_sw_delay();
	palSetPad(m_port_csn, m_pin_csn);
}

static void spi_sw_delay(void) {
	for (volatile int i = 0;i < 5;i++) {
		__NOP();
	}
}
