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
#ifndef SPI_SW_H_
#define SPI_SW_H_

#include "hw.h"
#include "ch.h"
#include "hal.h"

// Functions
void spi_sw_init(void);
void spi_sw_stop(void);
void spi_sw_change_pins(
		stm32_gpio_t *port_csn, int pin_csn,
		stm32_gpio_t *port_sck, int pin_sck,
		stm32_gpio_t *port_mosi, int pin_mosi,
		stm32_gpio_t *port_miso, int pin_miso);
void spi_sw_transfer(char *in_buf, const char *out_buf, int length);
void spi_sw_begin(void);
void spi_sw_end(void);

#endif /* SPI_SW_H_ */
