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

#include "servo_dec.h"
#include "stm32f4xx_conf.h"
#include "ch.h"
#include "hal.h"
#include "hw.h"
#include "utils.h"

/*
 * Settings
 */
#define SERVO_NUM				1
#define TIMER_FREQ				1000000

// Private variables
static volatile systime_t last_update_time;
static volatile float servo_pos[SERVO_NUM];
static volatile float pulse_start = 1.0;
static volatile float pulse_end = 2.0;
static volatile float last_len_received[SERVO_NUM];
static volatile bool use_median_filter = false;
static volatile bool is_running = false;

// Function pointers
static void(*done_func)(void) = 0;

static void icuwidthcb(ICUDriver *icup) {
	last_len_received[0] = ((float)icuGetWidthX(icup) / ((float)TIMER_FREQ / 1000.0));
	float len = last_len_received[0] - pulse_start;
	const float len_set = (pulse_end - pulse_start);

	if (len > len_set) {
		if (len < (len_set * 1.2)) {
			len = len_set;
		} else {
			// Too long pulse. Most likely something is wrong.
			len = -1.0;
		}
	} else if (len < 0.0) {
		if ((len + pulse_start) > (pulse_start * 0.8)) {
			len = 0.0;
		} else {
			// Too short pulse. Most likely something is wrong.
			len = -1.0;
		}
	}

	if (len >= 0.0) {
		if (use_median_filter) {
			float c = (len * 2.0 - len_set) / len_set;
			static float c1 = 0.5;
			static float c2 = 0.5;
			float med = utils_middle_of_3(c, c1, c2);

			c2 = c1;
			c1 = c;

			servo_pos[0] = med;
		} else {
			servo_pos[0] = (len * 2.0 - len_set) / len_set;
		}

		last_update_time = chVTGetSystemTime();

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
	icuStart(&HW_ICU_DEV, &icucfg);
	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_ALTERNATE(HW_ICU_GPIO_AF));
	icuStartCapture(&HW_ICU_DEV);
	icuEnableNotifications(&HW_ICU_DEV);

	for (int i = 0;i < SERVO_NUM;i++) {
		servo_pos[i] = 0.0;
		last_len_received[i] = 0.0;
	}

	// Set our function pointer
	done_func = d_func;

	is_running = true;
}

/**
 * Stop the servo decoding driver
 */
void servodec_stop(void) {
	if (is_running) {
		icuStop(&HW_ICU_DEV);
		palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_INPUT);
		pulse_start = 1.0;
		pulse_end = 2.0;
		use_median_filter = false;
		done_func = 0;
	}

	is_running = false;
}

/**
 * Change the limits of how the servo pulses should be decoded.
 *
 * @param start
 * The amount of milliseconds the pulse starts at (default is 1.0)
 *
 * @param end
 * he amount of milliseconds the pulse ends at (default is 2.0)
 */
void servodec_set_pulse_options(float start, float end, bool median_filter) {
	pulse_start = start;
	pulse_end = end;
	use_median_filter = median_filter;
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
	return chVTTimeElapsedSinceX(last_update_time) / (CH_CFG_ST_FREQUENCY / 1000);
}

/**
 * Get the length of the last received pulse.
 *
 * @param servo_num
 * The servo index. If it is out of range, 0.0 will be returned.
 *
 * @return
 * The length of the last received pulse.
 */
float servodec_get_last_pulse_len(int servo_num) {
	if (servo_num < SERVO_NUM) {
		return last_len_received[servo_num];
	} else {
		return 0.0;
	}
}
