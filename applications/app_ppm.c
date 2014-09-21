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
 * app_ppm.c
 *
 *  Created on: 18 apr 2014
 *      Author: benjamin
 */

#include "app.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "servo_dec.h"
#include "mcpwm.h"
#include "timeout.h"
#include "utils.h"
#include <math.h>

// Threads
static msg_t ppm_thread(void *arg);
static WORKING_AREA(ppm_thread_wa, 1024);
static Thread *ppm_tp;
VirtualTimer vt;

// Private functions
static void servodec_func(void);

// Private variables
static volatile ppm_control_type ctrl_type = PPM_CTRL_TYPE_CURRENT;
static volatile float pid_max_erpm = 15000.0;
static volatile bool is_running = false;
static volatile float hysteres = 0.15;
static volatile float pulse_s = 1.0;
static volatile float pulse_e = 1.0;
static volatile float rpm_lim_start = 200000.0;
static volatile float rpm_lim_end = 250000.0;

// Private functions
void update(void *p);

void app_ppm_configure(ppm_control_type ctrlt, float pme, float hyst,
		float pulse_start, float pulse_width, float lim_rpm_start, float lim_rpm_end) {
	ctrl_type = ctrlt;
	pid_max_erpm = pme;
	hysteres = hyst;
	pulse_s = pulse_start;
	pulse_e = pulse_width;
	rpm_lim_start = lim_rpm_start;
	rpm_lim_end = lim_rpm_end;

	if (is_running) {
		servodec_set_pulse_options(pulse_s, pulse_e);
	}
}

void app_ppm_start(void) {
	chThdCreateStatic(ppm_thread_wa, sizeof(ppm_thread_wa), NORMALPRIO, ppm_thread, NULL);

	chSysLock();
	chVTSetI(&vt, MS2ST(1), update, NULL);
	chSysUnlock();
}

static void servodec_func(void) {
	chSysLockFromIsr();
	timeout_reset();
	chEvtSignalI(ppm_tp, (eventmask_t) 1);
	chSysUnlockFromIsr();
}

void update(void *p) {
	chSysLockFromIsr();
	chVTSetI(&vt, MS2ST(1), update, p);
	chEvtSignalI(ppm_tp, (eventmask_t) 1);
	chSysUnlockFromIsr();
}

static msg_t ppm_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP_PPM");
	ppm_tp = chThdSelf();

	servodec_set_pulse_options(pulse_s, pulse_e);
	servodec_init(servodec_func);
	is_running = true;

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		if (timeout_has_timeout()) {
			continue;
		}

		float servo_val = servodec_get_servo(0);

		switch (ctrl_type) {
		case PPM_CTRL_TYPE_CURRENT_NOREV:
		case PPM_CTRL_TYPE_DUTY_NOREV:
		case PPM_CTRL_TYPE_PID_NOREV:
			servo_val += 1.0;
			servo_val /= 2.0;
			break;

		default:
			break;
		}

		servo_val /= (1.0 - hysteres);

		if (servo_val > hysteres) {
			servo_val -= hysteres;
		} else if (servo_val < -hysteres) {
			servo_val += hysteres;
		} else {
			servo_val = 0.0;
		}

		float current = 0;
		bool current_mode = false;
		bool current_mode_brake = false;
		const volatile mc_configuration *mcconf = mcpwm_get_configuration();

		switch (ctrl_type) {
		case PPM_CTRL_TYPE_CURRENT:
		case PPM_CTRL_TYPE_CURRENT_NOREV:
			current_mode = true;
			if (servo_val >= 0.0) {
				current = servo_val * mcconf->l_current_max;
			} else {
				current = servo_val * fabsf(mcconf->l_current_min);
			}
			break;

		case PPM_CTRL_TYPE_CURRENT_NOREV_BRAKE:
			current_mode = true;
			if (servo_val >= 0.0) {
				current = servo_val * mcconf->l_current_max;
			} else {
				current = fabsf(servo_val * mcconf->l_current_min);
				current_mode_brake = true;
			}
			break;

		case PPM_CTRL_TYPE_DUTY:
		case PPM_CTRL_TYPE_DUTY_NOREV:
			mcpwm_set_duty(servo_val);
			break;

		case PPM_CTRL_TYPE_PID:
		case PPM_CTRL_TYPE_PID_NOREV:
			mcpwm_set_pid_speed(servo_val * pid_max_erpm);
			break;

		default:
			break;
		}

		if (current_mode) {
			if (current_mode_brake) {
				mcpwm_set_brake_current(current);
			} else {
				// Apply soft RPM limit
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

				mcpwm_set_current(current);
			}
		}

	}

	return 0;
}
