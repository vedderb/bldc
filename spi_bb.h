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

#ifndef SPI_BB_H_
#define SPI_BB_H_

#include "ch.h"
#include "hal.h"
#include "stdint.h"
#include "stdbool.h"

typedef struct {
	stm32_gpio_t *nss_gpio;
	int nss_pin;
	stm32_gpio_t *sck_gpio;
	int sck_pin;
    stm32_gpio_t *mosi_gpio;
	int mosi_pin;
    stm32_gpio_t *miso_gpio;
	int miso_pin;
	bool has_started;
	bool has_error;
	mutex_t mutex;
} spi_bb_state;

void spi_bb_init(spi_bb_state *s);
uint8_t spi_bb_exchange_8(spi_bb_state *s, uint8_t x);
void spi_bb_transfer_8(spi_bb_state *s, uint8_t *in_buf, const uint8_t *out_buf, int length);
void spi_bb_begin(spi_bb_state *s);
void spi_bb_end(spi_bb_state *s);
void spi_bb_delay(void);

#endif /* SPI_BB_H_ */
