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

#include "general.h"

// Use variables for ports and pins, so that they can be changed
stm32_gpio_t *platform_swdio_port = SWDIO_PORT_DEFAULT;
int platform_swdio_pin = SWDIO_PIN_DEFAULT;
stm32_gpio_t *platform_swclk_port = SWCLK_PORT_DEFAULT;
int platform_swclk_pin = SWCLK_PIN_DEFAULT;

void platform_delay(uint32_t ms) {
	chThdSleepMilliseconds(ms);
}

uint32_t platform_time_ms(void) {
	return ST2MS(chVTGetSystemTimeX());
}

void platform_srst_set_val(bool assert) {
	(void)assert;
}

bool platform_srst_get_val(void) {
	return false;
}
