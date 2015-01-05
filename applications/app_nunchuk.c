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
 * app_nunchuk.c
 *
 *  Created on: 18 okt 2014
 *      Author: benjamin
 */

#include "app.h"
#include "ch.h"
#include "hal.h"
#include "hw.h"
#include "mcpwm.h"
#include "commands.h"
#include "utils.h"
#include "timeout.h"
#include <string.h>
#include <math.h>
#include "led_external.h"
#include "datatypes.h"

// Settings
#define OUTPUT_ITERATION_TIME_MS		1
#define MAX_CURR_DIFFERENCE				5.0

// Threads
static msg_t chuk_thread(void *arg);
static WORKING_AREA(chuk_thread_wa, 1024);
static msg_t output_thread(void *arg);
static WORKING_AREA(output_thread_wa, 1024);

// Private variables
static volatile chuk_control_type ctrl_type = CHUK_CTRL_TYPE_CURRENT_NOREV;
static volatile bool is_running = false;
static volatile float hysteres = 0.15;
static volatile float rpm_lim_start = 200000.0;
static volatile float rpm_lim_end = 250000.0;
static volatile chuck_data_t chuck_data;
static volatile int chuck_error = 0;
static volatile float ramp_time_pos = 0.5;
static volatile float ramp_time_neg = 0.25;

void app_nunchuk_configure(chuk_control_type ctrlt,
		float hyst, float lim_rpm_start, float lim_rpm_end,
		float r_time_pos, float r_time_neg) {
	ctrl_type = ctrlt;
	hysteres = hyst;
	rpm_lim_start = lim_rpm_start;
	rpm_lim_end = lim_rpm_end;
	ramp_time_pos = r_time_pos;
	ramp_time_neg = r_time_neg;
}

void app_nunchuk_start(void) {
	chuck_data.js_y = 128;

	chThdCreateStatic(chuk_thread_wa, sizeof(chuk_thread_wa), NORMALPRIO, chuk_thread, NULL);
	chThdCreateStatic(output_thread_wa, sizeof(output_thread_wa), NORMALPRIO, output_thread, NULL);
}

float app_nunchuk_get_decoded_chuk(void) {
	return ((float)chuck_data.js_y - 128.0) / 128.0;
}

static msg_t chuk_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP Nunchuk");
	is_running = true;

	uint8_t rxbuf[10];
	uint8_t txbuf[10];
	msg_t status = RDY_OK;
	systime_t tmo = MS2ST(5);
	i2caddr_t chuck_addr = 0x52;

	hw_start_i2c();
	chThdSleepMilliseconds(10);

	for(;;) {
		bool is_ok = true;

		txbuf[0] = 0xF0;
		txbuf[1] = 0x55;
		i2cAcquireBus(&HW_I2C_DEV);
		status = i2cMasterTransmitTimeout(&HW_I2C_DEV, chuck_addr, txbuf, 2, rxbuf, 0, tmo);
		i2cReleaseBus(&HW_I2C_DEV);
		is_ok = status == RDY_OK;

		if (is_ok) {
			txbuf[0] = 0xFB;
			txbuf[1] = 0x00;
			i2cAcquireBus(&HW_I2C_DEV);
			status = i2cMasterTransmitTimeout(&HW_I2C_DEV, chuck_addr, txbuf, 2, rxbuf, 0, tmo);
			i2cReleaseBus(&HW_I2C_DEV);
			is_ok = status == RDY_OK;
		}

		if (is_ok) {
			txbuf[0] = 0x00;
			i2cAcquireBus(&HW_I2C_DEV);
			status = i2cMasterTransmitTimeout(&HW_I2C_DEV, chuck_addr, txbuf, 1, rxbuf, 0, tmo);
			i2cReleaseBus(&HW_I2C_DEV);
			is_ok = status == RDY_OK;
		}

		if (is_ok) {
			chThdSleepMilliseconds(3);

			i2cAcquireBus(&HW_I2C_DEV);
			status = i2cMasterReceiveTimeout(&HW_I2C_DEV, chuck_addr, rxbuf, 6, tmo);
			i2cReleaseBus(&HW_I2C_DEV);
			is_ok = status == RDY_OK;
		}

		if (is_ok) {
			static uint8_t last_buffer[10];
			int same = 1;

			for (int i = 0;i < 6;i++) {
				if (last_buffer[i] != rxbuf[i]) {
					same = 0;
				}
			}

			memcpy(last_buffer, rxbuf, 6);

			if (!same) {
				chuck_error = 0;
				chuck_data.js_x = rxbuf[0];
				chuck_data.js_y = rxbuf[1];
				chuck_data.acc_x = (rxbuf[2] << 2) | ((rxbuf[5] >> 2) & 3);
				chuck_data.acc_y = (rxbuf[3] << 2) | ((rxbuf[5] >> 4) & 3);
				chuck_data.acc_z = (rxbuf[4] << 2) | ((rxbuf[5] >> 6) & 3);
				chuck_data.bt_z = !((rxbuf[5] >> 0) & 1);
				chuck_data.bt_c = !((rxbuf[5] >> 1) & 1);

				timeout_reset();
			}

			if (timeout_has_timeout()) {
				chuck_error = 1;
			}
		} else {
			chuck_error = 2;
			hw_try_restore_i2c();
			chThdSleepMilliseconds(100);
		}

		chThdSleepMilliseconds(10);
	}

	return 0;
}

static msg_t output_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Nunchuk output");

	for(;;) {
		chThdSleepMilliseconds(OUTPUT_ITERATION_TIME_MS);

		if (timeout_has_timeout() || chuck_error != 0 || ctrl_type == CHUK_CTRL_TYPE_NONE) {
			continue;
		}

		static bool is_reverse = false;
		static bool was_z = false;
		const float current_now = mcpwm_get_tot_current_directional_filtered();

		if (chuck_data.bt_z && !was_z && ctrl_type == CHUK_CTRL_TYPE_CURRENT &&
				fabsf(current_now) < MAX_CURR_DIFFERENCE) {
			if (is_reverse) {
				is_reverse = false;
			} else {
				is_reverse = true;
			}
		}

		was_z = chuck_data.bt_z;

		led_external_set_reversed(is_reverse);

		float out_val = app_nunchuk_get_decoded_chuk();
		utils_deadband(&out_val, hysteres, 1.0);

		// LEDs
		float x_axis = ((float)chuck_data.js_x - 128.0) / 128.0;
		if (out_val < -0.001) {
			if (x_axis < -0.4) {
				led_external_set_state(LED_EXT_BRAKE_TURN_LEFT);
			} else if (x_axis > 0.4) {
				led_external_set_state(LED_EXT_BRAKE_TURN_RIGHT);
			} else {
				led_external_set_state(LED_EXT_BRAKE);
			}
		} else {
			if (x_axis < -0.4) {
				led_external_set_state(LED_EXT_TURN_LEFT);
			} else if (x_axis > 0.4) {
				led_external_set_state(LED_EXT_TURN_RIGHT);
			} else {
				led_external_set_state(LED_EXT_NORMAL);
			}
		}

		// If c is pressed and no throttle is used, maintain the current speed with PID control
		static bool was_pid = false;

		if (chuck_data.bt_c && out_val == 0.0) {
			static float pid_rpm = 0.0;

			if (!was_pid) {
				was_pid = true;
				pid_rpm = mcpwm_get_rpm();
			}

			if ((is_reverse && pid_rpm < 0.0) || (!is_reverse && pid_rpm > 0.0)) {
				mcpwm_set_pid_speed(pid_rpm);
			}

			continue;
		}

		was_pid = false;

		float current = 0;
		const volatile mc_configuration *mcconf = mcpwm_get_configuration();

		if (out_val >= 0.0) {
			current = out_val * mcconf->l_current_max;
		} else {
			current = out_val * fabsf(mcconf->l_current_min);
		}

		// Apply ramping
		static float prev_current = 0.0;
		const float current_range = mcconf->l_current_max + fabsf(mcconf->l_current_min);
		const float ramp_time = fabsf(current) > fabsf(prev_current) ? ramp_time_pos : ramp_time_neg;

		if (ramp_time > 0.01) {
			const float ramp_step = ((float)OUTPUT_ITERATION_TIME_MS * current_range) / (ramp_time * 1000.0);

			float current_goal = prev_current;
			const float goal_tmp = current_goal;
			utils_step_towards(&current_goal, current, ramp_step);
			bool is_decreasing = current_goal < goal_tmp;

			// Make sure the desired current is close to the actual current to avoid surprises
			// when changing direction
			float goal_tmp2 = current_goal;
			if (is_reverse) {
				if (fabsf(current_goal + current_now) > MAX_CURR_DIFFERENCE) {
					utils_step_towards(&goal_tmp2, -current_now, 2.0 * ramp_step);
				}
			} else {
				if (fabsf(current_goal - current_now) > MAX_CURR_DIFFERENCE) {
					utils_step_towards(&goal_tmp2, current_now, 2.0 * ramp_step);
				}
			}

			// Always allow negative ramping
			bool is_decreasing2 = goal_tmp2 < current_goal;
			if (!is_decreasing || is_decreasing2) {
				current_goal = goal_tmp2;
			}

			current = current_goal;
		}

		prev_current = current;

		if (current < 0.0) {
			mcpwm_set_brake_current(current);
		} else {
			// Apply soft RPM limit
			float rpm = mcpwm_get_rpm();
			if (is_reverse) {
				rpm = -rpm;
			}

			if (rpm > rpm_lim_end && current > 0.0) {
				current = mcconf->cc_min_current;
			} else if (rpm > rpm_lim_start && current > 0.0) {
				current = utils_map(rpm, rpm_lim_start, rpm_lim_end, current, mcconf->cc_min_current);
			} else if (rpm < -rpm_lim_end && current < 0.0) {
				current = mcconf->cc_min_current;
			} else if (rpm < -rpm_lim_start && current < 0.0) {
				rpm = -rpm;
				current = -current;
				current = utils_map(rpm, rpm_lim_start, rpm_lim_end, current, mcconf->cc_min_current);
				current = -current;
			}

			if (is_reverse) {
				mcpwm_set_current(-current);
			} else {
				mcpwm_set_current(current);
			}
		}
	}

	return 0;
}
