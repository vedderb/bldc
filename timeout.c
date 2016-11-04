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

#include "timeout.h"
#include "mc_interface.h"

// Private variables
static volatile systime_t timeout_msec;
static volatile systime_t last_update_time;
static volatile float timeout_brake_current;
static volatile bool has_timeout;

// Threads
static THD_WORKING_AREA(timeout_thread_wa, 512);
static THD_FUNCTION(timeout_thread, arg);

void timeout_init(void) {
	timeout_msec = 1000;
	last_update_time = 0;
	timeout_brake_current = 0.0;
	has_timeout = false;

	chThdCreateStatic(timeout_thread_wa, sizeof(timeout_thread_wa), NORMALPRIO, timeout_thread, NULL);
}

void timeout_configure(systime_t timeout, float brake_current) {
	timeout_msec = timeout;
	timeout_brake_current = brake_current;
}

void timeout_reset(void) {
	last_update_time = chVTGetSystemTime();
}

bool timeout_has_timeout(void) {
	return has_timeout;
}

systime_t timeout_get_timeout_msec(void) {
	return timeout_msec;
}

float timeout_get_brake_current(void) {
	return timeout_brake_current;
}

static THD_FUNCTION(timeout_thread, arg) {
	(void)arg;

	chRegSetThreadName("Timeout");

	for(;;) {
		if (timeout_msec != 0 && chVTTimeElapsedSinceX(last_update_time) > MS2ST(timeout_msec)) {
			mc_interface_unlock();
			mc_interface_set_brake_current(timeout_brake_current);
			has_timeout = true;
		} else {
			has_timeout = false;
		}

		chThdSleepMilliseconds(10);
	}
}
