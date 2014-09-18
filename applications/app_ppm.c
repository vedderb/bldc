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
#include <math.h>

// Threads
static msg_t ppm_thread(void *arg);
static WORKING_AREA(ppm_thread_wa, 1024);
static Thread *ppm_tp;
static VirtualTimer vt;

// Private functions
static void servodec_func(void);
static void trig_func(void *p);

// Private variables
static volatile ppm_control_type ctrl_type;
static volatile float pid_max_erpm;

void app_ppm_configure(ppm_control_type ctrlt, float pme) {
	ctrl_type = ctrlt;
	pid_max_erpm = pme;
}

void app_ppm_start(void) {
	chThdCreateStatic(ppm_thread_wa, sizeof(ppm_thread_wa), NORMALPRIO, ppm_thread, NULL);
}

static void trig_func(void *p) {
	(void)p;

	chSysLock();
	chVTSetI(&vt, MS2ST(10), trig_func, NULL);
	chSysUnlock();

	chEvtSignalI(ppm_tp, (eventmask_t) 1);
}

static void servodec_func(void) {
	chSysLockFromIsr();
	chEvtSignalI(ppm_tp, (eventmask_t) 1);
	chSysUnlockFromIsr();
}

static msg_t ppm_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP_PPM");
	ppm_tp = chThdSelf();

	servodec_init(servodec_func);

	chSysLock();
	chVTSetI(&vt, MS2ST(10), trig_func, NULL);
	chSysUnlock();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

#define HYST			0.15

		if (servodec_get_time_since_update() < 500) {
			float servo_val = servodec_get_servo_as_float(0);

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

			servo_val /= (1.0 - HYST);

			if (servo_val > HYST) {
				servo_val -= HYST;
			} else if (servo_val < -HYST) {
				servo_val += HYST;
			} else {
				servo_val = 0.0;
			}

			switch (ctrl_type) {
			case PPM_CTRL_TYPE_CURRENT:
			case PPM_CTRL_TYPE_CURRENT_NOREV:
				if (servo_val >= 0.0) {
					mcpwm_set_current(servo_val * mcpwm_get_configuration()->l_current_max);
				} else {
					mcpwm_set_current(servo_val * fabsf(mcpwm_get_configuration()->l_current_min));
				}
				break;

			case PPM_CTRL_TYPE_CURRENT_NOREV_BRAKE:
				if (servo_val >= 0.0) {
					mcpwm_set_current(servo_val * mcpwm_get_configuration()->l_current_max);
				} else {
					mcpwm_set_brake_current(fabsf(servo_val * mcpwm_get_configuration()->l_current_min));
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
		} else {
			mcpwm_set_current(0.0);
		}
	}

	return 0;
}
