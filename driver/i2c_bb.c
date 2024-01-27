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

#include "i2c_bb.h"
#include "timer.h"

// This is based on https://en.wikipedia.org/wiki/I%C2%B2C

// Macros
#define SDA_LOW()				palClearPad(s->sda_gpio, s->sda_pin)
#define SDA_HIGH()				palSetPad(s->sda_gpio, s->sda_pin)
#define SCL_LOW()				palClearPad(s->scl_gpio, s->scl_pin)
#define SCL_HIGH()				palSetPad(s->scl_gpio, s->scl_pin)
#define READ_SDA()				palReadPad(s->sda_gpio, s->sda_pin)
#define READ_SCL()				palReadPad(s->scl_gpio, s->scl_pin)

// Private functions
static void i2c_start_cond(i2c_bb_state *s);
static void i2c_stop_cond(i2c_bb_state *s);
static void i2c_write_bit(i2c_bb_state *s, bool bit);
static bool i2c_read_bit(i2c_bb_state *s);
static bool i2c_write_byte(i2c_bb_state *s, bool send_start, bool send_stop, unsigned char byte);
static unsigned char i2c_read_byte(i2c_bb_state *s, bool nack, bool send_stop);
static bool clock_stretch_timeout(i2c_bb_state *s);
static void i2c_delay(float seconds);

static inline float rate2secs(i2c_bb_state *s) {
	switch (s->rate) {
	case I2C_BB_RATE_100K: return 3.5e-6;
	case I2C_BB_RATE_200K: return 1.0e-6;
	case I2C_BB_RATE_400K: return 2.5e-7;
	case I2C_BB_RATE_700K: return 0.0;
	}

	return 1.0e-6;
}

void i2c_bb_init(i2c_bb_state *s) {
	chMtxObjectInit(&s->mutex);
	palSetPadMode(s->sda_gpio, s->sda_pin, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLUP);
	palSetPadMode(s->scl_gpio, s->scl_pin, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_PUDR_PULLUP);
	s->has_started = false;
	s->has_error = false;
}

void i2c_bb_restore_bus(i2c_bb_state *s) {
	chMtxLock(&s->mutex);

	SCL_HIGH();
	SDA_HIGH();

	chThdSleep(1);

	for(int i = 0;i < 16;i++) {
		SCL_LOW();
		chThdSleep(1);
		SCL_HIGH();
		chThdSleep(1);
	}

	s->has_started = false;

	i2c_start_cond(s);
	i2c_stop_cond(s);

	s->has_error = false;

	chMtxUnlock(&s->mutex);
}

bool i2c_bb_tx_rx(i2c_bb_state *s, uint16_t addr, uint8_t *txbuf, size_t txbytes, uint8_t *rxbuf, size_t rxbytes) {
	chMtxLock(&s->mutex);

	if (txbytes > 0 && txbuf) {
		i2c_write_byte(s, true, false, addr << 1);

		if (s->has_error) {
			chMtxUnlock(&s->mutex);
			return false;
		}

		for (unsigned int i = 0;i < txbytes;i++) {
			i2c_write_byte(s, false, false, txbuf[i]);

			if (s->has_error) {
				chMtxUnlock(&s->mutex);
				return false;
			}
		}
	}

	if (rxbytes > 0) {
		i2c_write_byte(s, true, false, addr << 1 | 1);

		if (s->has_error) {
			chMtxUnlock(&s->mutex);
			return false;
		}

		for (unsigned int i = 0;i < rxbytes;i++) {
			rxbuf[i] = i2c_read_byte(s, i == (rxbytes - 1), false);
			if (s->has_error) {
				chMtxUnlock(&s->mutex);
				return false;
			}
		}
	}

	i2c_stop_cond(s);

	chMtxUnlock(&s->mutex);

	return !s->has_error;
}

static void i2c_start_cond(i2c_bb_state *s) {
	if (s->has_started) {
		// if started, do a restart condition
		SDA_HIGH();
		i2c_delay(rate2secs(s));
		SCL_HIGH();

		if (!clock_stretch_timeout(s)) {
			return;
		}

		// Repeated start setup time, minimum 4.7us
		i2c_delay(rate2secs(s));
	}

	if (READ_SDA() == 0) {
//		arbitration_lost();
		s->has_error = true;
	}

	// SCL is high, set SDA from 1 to 0.
	SDA_LOW();
	i2c_delay(rate2secs(s));
	SCL_LOW();
	s->has_started = true;
}

static void i2c_stop_cond(i2c_bb_state *s) {
	SDA_LOW();
	i2c_delay(rate2secs(s));

	SCL_HIGH();

	if (!clock_stretch_timeout(s)) {
		return;
	}

	// Stop bit setup time, minimum 4us
	i2c_delay(rate2secs(s));

	// SCL is high, set SDA from 0 to 1
	SDA_HIGH();

	i2c_delay(rate2secs(s));

	if (READ_SDA() == 0) {
//		arbitration_lost();
		s->has_error = true;
	}

	s->has_started = false;
}

static void i2c_write_bit(i2c_bb_state *s, bool bit) {
	if (bit) {
		SDA_HIGH();
	} else {
		SDA_LOW();
	}

	// SDA change propagation delay
	i2c_delay(rate2secs(s));

	// Set SCL high to indicate a new valid SDA value is available
	SCL_HIGH();

	// Wait for SDA value to be read by slave, minimum of 4us for standard mode
	i2c_delay(rate2secs(s));

	if (!clock_stretch_timeout(s)) {
		return;
	}

	// SCL is high, now data is valid

	// If SDA is high, check that nobody else is driving SDA
	if (bit && (READ_SDA() == 0)) {
//		arbitration_lost();
		s->has_error = true;
	}

	// Clear the SCL to low in preparation for next change
	SCL_LOW();
}

static bool i2c_read_bit(i2c_bb_state *s) {
	bool bit;

	// Let the slave drive data
	SDA_HIGH();

	// Wait for SDA value to be written by slave, minimum of 4us for standard mode
	i2c_delay(rate2secs(s));

	// Set SCL high to indicate a new valid SDA value is available
	SCL_HIGH();

	if (!clock_stretch_timeout(s)) {
		return false;
	}

	// Wait for SDA value to be written by slave, minimum of 4us for standard mode
	i2c_delay(rate2secs(s));

	// SCL is high, read out bit
	bit = READ_SDA();

	// Set SCL low in preparation for next operation
	SCL_LOW();

	return bit;
}

static bool i2c_write_byte(i2c_bb_state *s, bool send_start, bool send_stop, unsigned char byte) {
	unsigned bit;
	bool nack;

	if (send_start) {
		i2c_start_cond(s);
	}

	for (bit = 0;bit < 8;bit++) {
		i2c_write_bit(s, (byte & 0x80) != 0);
		byte <<= 1;
	}

	nack = i2c_read_bit(s);

	if (send_stop) {
		i2c_stop_cond(s);
	}

	return nack;
}

static unsigned char i2c_read_byte(i2c_bb_state *s, bool nack, bool send_stop) {
	unsigned char byte = 0;
	unsigned char bit;

	for (bit = 0;bit < 8;bit++) {
		byte = (byte << 1) | i2c_read_bit(s);
	}

	i2c_write_bit(s, nack);

	if (send_stop) {
		i2c_stop_cond(s);
	}

	return byte;
}

static bool clock_stretch_timeout(i2c_bb_state *s) {
	uint32_t time_start = timer_time_now();

	while(READ_SCL() == 0) {
		if (timer_seconds_elapsed_since(time_start) > 0.01) {
			s->has_error = true;
			return false;
		}
	}

	return true;
}

static void i2c_delay(float seconds) {
	timer_sleep(seconds);
}
