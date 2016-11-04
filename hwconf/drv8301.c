/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#include "hw.h"
#ifdef HW_HAS_DRV8301

#include "drv8301.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils.h"

// Private functions
static uint16_t spi_exchange(uint16_t x);
static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length);
static void spi_begin(void);
static void spi_end(void);
static void spi_delay(void);

void drv8301_init(void) {
	// DRV8301 SPI
	palSetPadMode(DRV8301_MISO_GPIO, DRV8301_MISO_PIN, PAL_MODE_INPUT);
	palSetPadMode(DRV8301_SCK_GPIO, DRV8301_SCK_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(DRV8301_CS_GPIO, DRV8301_CS_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(DRV8301_MOSI_GPIO, DRV8301_MOSI_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(DRV8301_MOSI_GPIO, DRV8301_MOSI_PIN);

	chThdSleepMilliseconds(100);

	// Disable OC
	drv8301_write_reg(2, 0x0430);
	drv8301_write_reg(2, 0x0430);
}

void drv8301_set_oc_adj(int val) {
	int reg = drv8301_read_reg(2);
	reg &= 0x003F;
	reg |= (val & 0x1F) << 6;
	drv8301_write_reg(2, reg);
}

unsigned int drv8301_read_reg(int reg) {
	uint16_t out = 0;
	out |= (1 << 15);
	out |= (reg & 0x0F) << 11;
	out |= 0x807F;

	spi_begin();
	spi_exchange(out);
	spi_end();

	spi_begin();
	uint16_t res = spi_exchange(0xFFFF);
	spi_end();

	return res;
}

void drv8301_write_reg(int reg, int data) {
	uint16_t out = 0;
	out |= (reg & 0x0F) << 11;
	out |= data & 0x7FF;

	spi_begin();
	spi_exchange(out);
	spi_end();
}

// Software SPI
static uint16_t spi_exchange(uint16_t x) {
	uint16_t rx;
	spi_transfer(&rx, &x, 1);
	return rx;
}

static void spi_transfer(uint16_t *in_buf, const uint16_t *out_buf, int length) {
	for (int i = 0;i < length;i++) {
		uint16_t send = out_buf ? out_buf[i] : 0xFFFF;
		uint16_t recieve = 0;

		for (int bit = 0;bit < 16;bit++) {
			palWritePad(DRV8301_MOSI_GPIO, DRV8301_MOSI_PIN, send >> 15);
			send <<= 1;

			palSetPad(DRV8301_SCK_GPIO, DRV8301_SCK_PIN);
			spi_delay();

			palClearPad(DRV8301_SCK_GPIO, DRV8301_SCK_PIN);

			int r1, r2, r3;
			r1 = palReadPad(DRV8301_MISO_GPIO, DRV8301_MISO_PIN);
			__NOP();
			r2 = palReadPad(DRV8301_MISO_GPIO, DRV8301_MISO_PIN);
			__NOP();
			r3 = palReadPad(DRV8301_MISO_GPIO, DRV8301_MISO_PIN);

			recieve <<= 1;
			if (utils_middle_of_3_int(r1, r2, r3)) {
				recieve |= 1;
			}

			spi_delay();
		}

		if (in_buf) {
			in_buf[i] = recieve;
		}
	}
}

static void spi_begin(void) {
	palClearPad(DRV8301_CS_GPIO, DRV8301_CS_PIN);
}

static void spi_end(void) {
	palSetPad(DRV8301_CS_GPIO, DRV8301_CS_PIN);
}

static void spi_delay(void) {
	for (volatile int i = 0;i < 10;i++) {
		__NOP();
	}
}

#endif
