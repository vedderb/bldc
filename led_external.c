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
 * led_external.c
 *
 *  Created on: 5 dec 2014
 *      Author: benjamin
 */

#include "led_external.h"
#include "ch.h"
#include "hal.h"
#include "ws2811.h"
#include "mcpwm.h"

// Macros
#define HAS_FAULT()		(mcpwm_get_fault() != FAULT_CODE_NONE)

// Private variables
static WORKING_AREA(led_thread_wa, 1024);
static volatile LED_EXT_STATE state;

// Private function prototypes
static msg_t led_thread(void *arg);
static uint32_t scale_color(uint32_t color, float scale);
static void wait_for_state_change(void);

void led_external_init(void) {
	chThdCreateStatic(led_thread_wa, sizeof(led_thread_wa), LOWPRIO, led_thread, NULL);
}

void led_external_set_state(LED_EXT_STATE new_state) {
	state = new_state;
}

static msg_t led_thread(void *arg) {
	(void) arg;
	chRegSetThreadName("LEDs External");

	for(;;) {
		mc_fault_code fault = mcpwm_get_fault();
		if (fault != FAULT_CODE_NONE) {
			ws2811_set_all(COLOR_BLACK);
			for (int i = 0;i < (int)fault;i++) {
				ws2811_set_led_color(i, COLOR_RED);
				chThdSleepMilliseconds(200);
			}

			chThdSleepMilliseconds(1000);

			float scale = 1.0;
			for (int i = 0;i < 50;i++) {
				scale -= 0.02;
				uint32_t color = scale_color(COLOR_RED, scale);
				for (int i = 0;i < (int)fault;i++) {
					ws2811_set_led_color(i, color);
				}
				chThdSleepMilliseconds(10);
			}
		} else {
			uint32_t red_weak = scale_color(COLOR_RED, 0.3);
			LED_EXT_STATE state_last = state;

			switch (state) {
			case LED_EXT_OFF:
				ws2811_set_all(COLOR_BLACK);
				wait_for_state_change();
				break;

			case LED_EXT_NORMAL:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					ws2811_set_led_color(i, red_weak);
					ws2811_set_led_color(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}
				wait_for_state_change();
				break;

			case LED_EXT_BRAKE:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					ws2811_set_led_color(i, COLOR_RED);
					ws2811_set_led_color(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}
				wait_for_state_change();
				break;

			case LED_EXT_TURN_LEFT:
			case LED_EXT_BRAKE_TURN_LEFT:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					if (state == LED_EXT_TURN_LEFT) {
						ws2811_set_led_color(i, red_weak);
					} else {
						ws2811_set_led_color(i, COLOR_RED);
					}
					ws2811_set_led_color(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}

				while (state == state_last && !HAS_FAULT()) {
					if ((chTimeNow() / (CH_FREQUENCY / 2)) % 2) {
						ws2811_set_led_color(WS2811_LED_NUM / 2 - 1, 0xFF9900);
						ws2811_set_led_color(WS2811_LED_NUM / 2, 0xFF9900);
					} else {
						ws2811_set_led_color(WS2811_LED_NUM / 2 - 1, COLOR_BLACK);
						ws2811_set_led_color(WS2811_LED_NUM / 2, COLOR_BLACK);
					}
					chThdSleepMilliseconds(10);
				}
				break;

			case LED_EXT_TURN_RIGHT:
			case LED_EXT_BRAKE_TURN_RIGHT:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					if (state == LED_EXT_TURN_RIGHT) {
						ws2811_set_led_color(i, red_weak);
					} else {
						ws2811_set_led_color(i, COLOR_RED);
					}
					ws2811_set_led_color(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}

				while (state == state_last && !HAS_FAULT()) {
					if ((chTimeNow() / (CH_FREQUENCY / 2)) % 2) {
						ws2811_set_led_color(0, 0xFF9900);
						ws2811_set_led_color(WS2811_LED_NUM - 1, 0xFF9900);
					} else {
						ws2811_set_led_color(0, COLOR_BLACK);
						ws2811_set_led_color(WS2811_LED_NUM - 1, COLOR_BLACK);
					}
					chThdSleepMilliseconds(10);
				}
				break;

			default:
				chThdSleepMilliseconds(10);
				break;
			}
		}

		chThdSleepMilliseconds(1);
	}

	return (msg_t) 0;
}

static uint32_t scale_color(uint32_t color, float scale) {
	uint32_t r = (color >> 16) & 0xFF;
	uint32_t g = (color >> 8) & 0xFF;
	uint32_t b = color & 0xFF;

	r *= scale;
	g *= scale;
	b *= scale;

	return (r << 16) | (g << 8) | b;
}

static void wait_for_state_change(void) {
	LED_EXT_STATE st = state;
	while (state == st && !HAS_FAULT()) {
		chThdSleepMilliseconds(10);
	}
}
