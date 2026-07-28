/*
	Copyright 2026 Lukas Hrazky

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

#include "transport_spi_bb.h"

// SPI read sets bit 7 of the register address, write clears it. This is a SPI-bus
// concern, kept here so drivers pass a clean register number.
#define SPI_READ_BIT 0x80

static spi_bb_state *bus_of(transport_t *t) {
	return &t->bus.spi_bb;
}

static bool read_reg(transport_t *t, uint8_t dev_addr, uint8_t reg, uint8_t *rx, size_t len) {
	(void)dev_addr;
	spi_bb_state *s = bus_of(t);

	chMtxLock(&s->mutex);
	spi_bb_begin(s);
	spi_bb_exchange_8_mode_3(s, reg | SPI_READ_BIT);
	spi_bb_delay_short();
	for (size_t i = 0; i < len; i++) {
		rx[i] = spi_bb_exchange_8_mode_3(s, 0);
	}
	spi_bb_end(s);
	chMtxUnlock(&s->mutex);

	return true;
}

static bool write_reg(transport_t *t, uint8_t dev_addr, uint8_t reg, const uint8_t *tx, size_t len) {
	(void)dev_addr;
	spi_bb_state *s = bus_of(t);

	chMtxLock(&s->mutex);
	spi_bb_begin(s);
	spi_bb_exchange_8_mode_3(s, reg & ~SPI_READ_BIT);
	spi_bb_delay();
	for (size_t i = 0; i < len; i++) {
		spi_bb_exchange_8_mode_3(s, tx[i]);
	}
	spi_bb_end(s);
	chMtxUnlock(&s->mutex);

	return true;
}

static uint16_t max_sample_rate(transport_t *t) {
	(void)t;
	return 2500;
}

static const transport_interface_t spi_bb_interface = {
	.name = "spi-bb",
	.max_sample_rate = max_sample_rate,
	.read_reg = read_reg,
	.write_reg = write_reg,
	.recover = NULL,
	.deinit = NULL,
};

void transport_spi_bb_init(transport_t *t, stm32_gpio_t *nss_gpio, uint8_t nss_pin,
		stm32_gpio_t *sck_gpio, uint8_t sck_pin, stm32_gpio_t *mosi_gpio, uint8_t mosi_pin,
		stm32_gpio_t *miso_gpio, uint8_t miso_pin) {
	t->interface = &spi_bb_interface;

	spi_bb_state *s = &t->bus.spi_bb;
	s->nss_gpio = nss_gpio;
	s->nss_pin = nss_pin;
	s->sck_gpio = sck_gpio;
	s->sck_pin = sck_pin;
	s->mosi_gpio = mosi_gpio;
	s->mosi_pin = mosi_pin;
	s->miso_gpio = miso_gpio;
	s->miso_pin = miso_pin;
	spi_bb_init(s);
}
