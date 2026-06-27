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

#include "transport_i2c_bb.h"
#include <string.h>

static i2c_bb_state *bus_of(transport_t *t) {
	return &t->bus.i2c_bb;
}

static bool read_reg(transport_t *t, uint8_t dev_addr, uint8_t reg, uint8_t *rx, size_t len) {
	return i2c_bb_tx_rx(bus_of(t), dev_addr, &reg, 1, rx, len);
}

static bool write_reg(transport_t *t, uint8_t dev_addr, uint8_t reg, const uint8_t *tx, size_t len) {
	if (len > IMU_MAX_BURST) {
		return false;
	}
	uint8_t buf[1 + IMU_MAX_BURST];
	buf[0] = reg;
	memcpy(buf + 1, tx, len);
	return i2c_bb_tx_rx(bus_of(t), dev_addr, buf, 1 + len, 0, 0);
}

static void recover(transport_t *t) {
	i2c_bb_restore_bus(bus_of(t));
}

// The 700 kHz bus allows a higher sample rate than 400 kHz. The lower bus
// speeds were never tested, they'll most likely require a lower limit.
static uint16_t max_sample_rate(transport_t *t) {
	return t->bus.i2c_bb.rate >= I2C_BB_RATE_700K ? 1200 : 900;
}

static const transport_interface_t i2c_bb_interface = {
	.name = "i2c-bb",
	.max_sample_rate = max_sample_rate,
	.read_reg = read_reg,
	.write_reg = write_reg,
	.recover = recover,
	.deinit = NULL,
};

// Map a desired SCL clock onto the fastest bit-bang I2C rate bucket not exceeding it.
static I2C_BB_RATE hz_to_rate(uint32_t bus_hz) {
	if (bus_hz == 0) {
		return I2C_BB_RATE_400K; // default
	}
	if (bus_hz >= 700000) {
		return I2C_BB_RATE_700K;
	}
	if (bus_hz >= 400000) {
		return I2C_BB_RATE_400K;
	}
	if (bus_hz >= 200000) {
		return I2C_BB_RATE_200K;
	}
	return I2C_BB_RATE_100K;
}

void transport_i2c_bb_init(transport_t *t, stm32_gpio_t *sda_gpio, uint8_t sda_pin,
		stm32_gpio_t *scl_gpio, uint8_t scl_pin, uint32_t bus_hz) {
	t->interface = &i2c_bb_interface;

	i2c_bb_state *s = &t->bus.i2c_bb;
	s->sda_gpio = sda_gpio;
	s->sda_pin = sda_pin;
	s->scl_gpio = scl_gpio;
	s->scl_pin = scl_pin;
	s->rate = hz_to_rate(bus_hz);
	i2c_bb_init(s);
}
