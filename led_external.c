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

#include "led_external.h"
#include "ch.h"
#include "hal.h"
#include "ws2811.h"
#include "mc_interface.h"
#include "utils.h"
#include "hw.h"

// Macros
#define HAS_FAULT()		(mc_interface_get_fault() != FAULT_CODE_NONE)

// Private variables
static THD_WORKING_AREA(led_thread_wa, 1024);
static volatile LED_EXT_STATE state;
static volatile bool reverse_leds;

// Private function prototypes
static THD_FUNCTION(led_thread, arg);
static uint32_t scale_color(uint32_t color, float scale);
static void wait_for_state_change(void);
static void set_led_wrapper(int led, uint32_t color);

void led_external_init(void) {
	reverse_leds = false;
	state = LED_EXT_OFF;
	chThdCreateStatic(led_thread_wa, sizeof(led_thread_wa), LOWPRIO, led_thread, NULL);
}

void led_external_set_state(LED_EXT_STATE new_state) {
	state = new_state;
}

void led_external_set_reversed(bool newstate) {
	reverse_leds = newstate;
}

static THD_FUNCTION(led_thread, arg) {
	(void) arg;
	chRegSetThreadName("LEDs External");

	float batt_level = 0.0;

	for(;;) {
		mc_fault_code fault = mc_interface_get_fault();
		if (fault != FAULT_CODE_NONE) {
			ws2811_set_all(COLOR_BLACK);
			for (int i = 0;i < (int)fault;i++) {
				set_led_wrapper(i, COLOR_RED);
				chThdSleepMilliseconds(200);
			}

			chThdSleepMilliseconds(1000);

			float scale = 1.0;
			for (int i = 0;i < 50;i++) {
				scale -= 0.02;
				uint32_t color = scale_color(COLOR_RED, scale);
				for (int i = 0;i < (int)fault;i++) {
					set_led_wrapper(i, color);
				}
				chThdSleepMilliseconds(10);
			}
		} else {
			uint32_t red_weak = scale_color(COLOR_RED, 0.5);
			LED_EXT_STATE state_last = state;
			bool rev_last = reverse_leds;

			switch (state) {
			case LED_EXT_OFF:
				ws2811_set_all(COLOR_BLACK);
				wait_for_state_change();
				break;

			case LED_EXT_NORMAL:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					set_led_wrapper(i, red_weak);
					set_led_wrapper(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}
				wait_for_state_change();
				break;

			case LED_EXT_BRAKE:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					set_led_wrapper(i, COLOR_RED);
					set_led_wrapper(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}
				wait_for_state_change();
				break;

			case LED_EXT_TURN_LEFT:
			case LED_EXT_BRAKE_TURN_LEFT:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					if (state == LED_EXT_TURN_LEFT) {
						set_led_wrapper(i, red_weak);
					} else {
						set_led_wrapper(i, COLOR_RED);
					}
					set_led_wrapper(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}

				while (state == state_last && rev_last == reverse_leds && !HAS_FAULT()) {
					if ((chVTGetSystemTime() / (CH_CFG_ST_FREQUENCY / 2)) % 2) {
						set_led_wrapper(WS2811_LED_NUM / 2 - 1, COLOR_ORANGE);
						set_led_wrapper(WS2811_LED_NUM / 2, COLOR_ORANGE);
					} else {
						set_led_wrapper(WS2811_LED_NUM / 2 - 1, COLOR_BLACK);
						set_led_wrapper(WS2811_LED_NUM / 2, COLOR_BLACK);
					}
					chThdSleepMilliseconds(10);
				}
				break;

			case LED_EXT_TURN_RIGHT:
			case LED_EXT_BRAKE_TURN_RIGHT:
				for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
					if (state == LED_EXT_TURN_RIGHT) {
						set_led_wrapper(i, red_weak);
					} else {
						set_led_wrapper(i, COLOR_RED);
					}
					set_led_wrapper(i + WS2811_LED_NUM / 2, COLOR_WHITE);
				}

				while (state == state_last && rev_last == reverse_leds && !HAS_FAULT()) {
					if ((chVTGetSystemTime() / (CH_CFG_ST_FREQUENCY / 2)) % 2) {
						set_led_wrapper(0, COLOR_ORANGE);
						set_led_wrapper(WS2811_LED_NUM - 1, COLOR_ORANGE);
					} else {
						set_led_wrapper(0, COLOR_BLACK);
						set_led_wrapper(WS2811_LED_NUM - 1, COLOR_BLACK);
					}
					chThdSleepMilliseconds(10);
				}
				break;

			case LED_EXT_BATT:
				while (state == state_last && rev_last == reverse_leds && !HAS_FAULT()) {
					batt_level = utils_map(GET_INPUT_VOLTAGE(), LED_EXT_BATT_LOW, LED_EXT_BATT_HIGH, 0.0, 1.0);
					for (int i = 0;i < WS2811_LED_NUM / 2;i++) {
						if (i < (WS2811_LED_NUM / 2) * batt_level) {
							if (i < (WS2811_LED_NUM / 2) / 3) {
								set_led_wrapper(i, COLOR_RED);
								set_led_wrapper(WS2811_LED_NUM - i - 1, COLOR_RED);
							} else if (i < (2 * (WS2811_LED_NUM / 2)) / 3) {
								set_led_wrapper(i, COLOR_YELLOW);
								set_led_wrapper(WS2811_LED_NUM - i - 1, COLOR_YELLOW);
							} else {
								set_led_wrapper(i, COLOR_GREEN);
								set_led_wrapper(WS2811_LED_NUM - i - 1, COLOR_GREEN);
							}
						} else {
							set_led_wrapper(i, COLOR_BLACK);
							set_led_wrapper(WS2811_LED_NUM - i - 1, COLOR_BLACK);
						}
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
	bool reversed = reverse_leds;
	while (state == st && reversed == reverse_leds && !HAS_FAULT()) {
		chThdSleepMilliseconds(10);
	}
}

static void set_led_wrapper(int led, uint32_t color) {
	if (reverse_leds) {
		if (led < WS2811_LED_NUM / 2) {
			ws2811_set_led_color(led + WS2811_LED_NUM / 2, color);
		} else {
			ws2811_set_led_color(led - WS2811_LED_NUM / 2, color);
		}
	} else {
		ws2811_set_led_color(led, color);
	}
}
