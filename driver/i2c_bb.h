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

#ifndef I2C_BB_H_
#define I2C_BB_H_

#include "ch.h"
#include "hal.h"
#include "stdint.h"
#include "stdbool.h"

typedef enum {
	I2C_BB_RATE_100K = 0,
	I2C_BB_RATE_200K,
	I2C_BB_RATE_400K,
	I2C_BB_RATE_700K
} I2C_BB_RATE;

typedef struct {
	stm32_gpio_t *sda_gpio; int sda_pin;
	stm32_gpio_t *scl_gpio; int scl_pin;
	I2C_BB_RATE rate;
	bool has_started;
	bool has_error;
	mutex_t mutex;
} i2c_bb_state;

void i2c_bb_init(i2c_bb_state *s);
void i2c_bb_restore_bus(i2c_bb_state *s);
bool i2c_bb_tx_rx(i2c_bb_state *s, uint16_t addr, uint8_t *txbuf, size_t txbytes, uint8_t *rxbuf, size_t rxbytes);

#endif /* I2C_BB_H_ */
