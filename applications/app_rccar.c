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
 * app_rccar.c
 *
 *  Created on: 18 apr 2014
 *      Author: benjamin
 */

#include "app.h"
#ifdef USE_APP_RCCAR

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "servo_dec.h"
#include "mcpwm.h"
#include <math.h>

// Threads
static msg_t rccar_thread(void *arg);
static WORKING_AREA(rccar_thread_wa, 1024);
static Thread *rccar_tp;
static VirtualTimer vt;

// Private functions
static void servodec_func(void);
static void trig_func(void *p);

void app_rccar_init(void) {
	chThdCreateStatic(rccar_thread_wa, sizeof(rccar_thread_wa), NORMALPRIO, rccar_thread, NULL);
}

static void trig_func(void *p) {
	(void)p;

	chSysLock();
	chVTSetI(&vt, MS2ST(10), trig_func, NULL);
	chSysUnlock();

	chEvtSignalI(rccar_tp, (eventmask_t) 1);
}

static void servodec_func(void) {
	chSysLockFromIsr();
	chEvtSignalI(rccar_tp, (eventmask_t) 1);
	chSysUnlockFromIsr();
}

static msg_t rccar_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP_RCCAR");
	rccar_tp = chThdSelf();

	servodec_init(servodec_func);

	chSysLock();
	chVTSetI(&vt, MS2ST(10), trig_func, NULL);
	chSysUnlock();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

#define HYST			0.15
#define USE_PID			1
#define PID_MAX_RPM		15000

		if (servodec_get_time_since_update() < 500) {
			float servo_val = servodec_get_servo_as_float(0);
			servo_val /= (1.0 - HYST);

			if (servo_val > HYST) {
				servo_val -= HYST;
			} else if (servo_val < -HYST) {
				servo_val += HYST;
			} else {
				servo_val = 0.0;
			}

#if USE_PID
			mcpwm_set_pid_speed(servo_val * PID_MAX_RPM);
#else
			mcpwm_set_current(servo_val * MCPWM_CURRENT_MAX);
#endif
		} else {
			mcpwm_set_current(0.0);
		}
	}

	return 0;
}

#endif
