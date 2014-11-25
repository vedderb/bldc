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

// Types
typedef struct {
	int js_x;
	int js_y;
	int acc_x;
	int acc_y;
	int acc_z;
	bool bt_c;
	bool bt_z;
} CHUCK_DATA;

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
static volatile CHUCK_DATA chuck_data;
static volatile int chuck_error = 0;

void app_nunchuk_configure(chuk_control_type ctrlt,
		float hyst, float lim_rpm_start, float lim_rpm_end) {
	ctrl_type = ctrlt;
	hysteres = hyst;
	rpm_lim_start = lim_rpm_start;
	rpm_lim_end = lim_rpm_end;
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
			chThdSleepMilliseconds(30);
		}

		chThdSleepMilliseconds(10);
	}

	return 0;
}

static msg_t output_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Nunchuk output");

	for(;;) {
		chThdSleepMilliseconds(1);

		if (timeout_has_timeout() || chuck_error != 0 || ctrl_type == CHUK_CTRL_TYPE_NONE) {
			continue;
		}

		float out_val = app_nunchuk_get_decoded_chuk();
		utils_deadband(&out_val, hysteres, 1.0);

		// If c is pressed and no throttle is used, maintain the current speed with PID control
		static bool was_pid = false;

		if (chuck_data.bt_c && out_val == 0.0) {
			static float pid_rpm = 0.0;

			if (!was_pid) {
				was_pid = true;
				pid_rpm = mcpwm_get_rpm();
			}

			if (ctrl_type == CHUK_CTRL_TYPE_CURRENT || pid_rpm > 0.0) {
				mcpwm_set_pid_speed(pid_rpm);
			}

			continue;
		}

		was_pid = false;

		float current = 0;
		bool current_mode_brake = false;
		const volatile mc_configuration *mcconf = mcpwm_get_configuration();

		if (out_val >= 0.0) {
			current = out_val * mcconf->l_current_max;
		} else {
			current = out_val * fabsf(mcconf->l_current_min);
			current_mode_brake = ctrl_type == CHUK_CTRL_TYPE_CURRENT_NOREV;
		}

		if (current_mode_brake) {
			mcpwm_set_brake_current(current);
		} else {

			// Apply soft RPM limit if z is not pressed.
			if (!chuck_data.bt_z) {
				float rpm = mcpwm_get_rpm();
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
			}

			mcpwm_set_current(current);
		}
	}

	return 0;
}
