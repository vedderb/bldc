/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

/*
 * servo_dec.c
 *
 *  Created on: 20 jan 2013
 *      Author: benjamin
 */

#include "servo_dec.h"
#include "stm32f4xx_conf.h"
#include "ch.h"
#include "hal.h"
#include "hw.h"

/*
 * Settings
 */
#define SERVO_NUM				1
#define TIMER_FREQ				1000000

// Private variables
static volatile systime_t last_update_time;
static volatile float servo_pos[SERVO_NUM];
static volatile float pulse_start = 1.0;
static volatile float pulse_width = 1.0;

// Function pointers
static void(*done_func)(void) = 0;

static void icuwidthcb(ICUDriver *icup) {
	float len = ((float)icuGetWidth(icup) / ((float)TIMER_FREQ / 1000.0)) - pulse_start;

	if (len > pulse_width) {
		if (len < pulse_width * 1.2) {
			len = pulse_width;
		} else {
			// Too long pulse. Most likely something is wrong.
			len = -1.0;
		}
	}

	if (len > 0.0) {
		servo_pos[0] = (len * 2.0 - pulse_width) / pulse_width;
		last_update_time = chTimeNow();

		if (done_func) {
			done_func();
		}
	}
}

static void icuperiodcb(ICUDriver *icup) {
	(void)icup;
}

static ICUConfig icucfg = {
		ICU_INPUT_ACTIVE_HIGH,
		TIMER_FREQ,
		icuwidthcb,
		icuperiodcb,
		NULL,
		HW_ICU_CHANNEL,
		0
};

/**
 * Initialize the serve decoding driver.
 *
 * @param d_func
 * A function that should be called every time the servo signals have been
 * decoded. Can be NULL.
 */
void servodec_init(void (*d_func)(void)) {
	icuStart(&ICUD3, &icucfg);
	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_ALTERNATE(HW_ICU_GPIO_AF));
	icuEnable(&ICUD3);

	// Set our function pointer
	done_func = d_func;
}

/**
 * Change the limits of how the servo pulses should be decoded.
 *
 * @param start
 * The amount of milliseconds the pulse starts at (default is 1.0)
 *
 * @param width
 * The width of the pulse in milliseconds (default is 1.0)
 */
void servodec_set_pulse_options(float start, float width) {
	pulse_start = start;
	pulse_width = width;
}

/**
 * Get a decoded servo value.
 *
 * @param servo_num
 * The servo index. If it is out of range, 0.0 will be returned.
 *
 * @return
 * The servo value in the range [-1.0 1.0].
 */
float servodec_get_servo(int servo_num) {
	if (servo_num < SERVO_NUM) {
		return servo_pos[servo_num];
	} else {
		return 0.0;
	}
}

/**
 * Get the amount of milliseconds that has passed since
 * the last time servo positions were received.
 *
 * @return
 * The amount of milliseconds that have passed since an update.
 */
uint32_t servodec_get_time_since_update(void) {
	return chTimeElapsedSince(last_update_time) / (CH_FREQUENCY / 1000);
}
