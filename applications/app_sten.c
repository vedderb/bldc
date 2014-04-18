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
 * app_sten.c
 *
 *  Created on: 18 apr 2014
 *      Author: benjamin
 */

#include "app.h"
#ifdef USE_APP_STEN

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "servo_dec.h"
#include "mcpwm.h"

// Threads
static msg_t sten_thread(void *arg);
static WORKING_AREA(sten_thread_wa, 1024);
static Thread *sten_tp;

// Private functions
static void servodec_func(void);

void app_sten_init(void) {
	chThdCreateStatic(sten_thread_wa, sizeof(sten_thread_wa), NORMALPRIO, sten_thread, NULL);
	servodec_init(servodec_func);
}

static void servodec_func(void) {
	chSysLockFromIsr();
	chEvtSignalI(sten_tp, (eventmask_t) 1);
	chSysUnlockFromIsr();
}

static msg_t sten_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP_STEN");
	sten_tp = chThdSelf();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

#define HYST		0.15

		if (servodec_get_time_since_update() < 500 && mcpwm_get_duty_cycle_now() > -0.01) {
			float servo_val = servodec_get_servo_as_float(0);
			servo_val /= (1.0 - HYST);

			if (servo_val > HYST) {
				servo_val -= HYST;
				mcpwm_set_current(servo_val * MCPWM_IN_CURRENT_MAX);
			} else if (servo_val < -HYST) {
				servo_val += HYST;
				mcpwm_set_current(servo_val * MCPWM_IN_CURRENT_MAX);
			} else {
				mcpwm_set_current(0.0);
			}
		} else {
			mcpwm_set_current(0.0);
		}
	}

	return 0;
}

#endif
