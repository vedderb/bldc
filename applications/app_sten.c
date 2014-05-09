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
#include "utils.h"
#include <math.h>

// Threads
static msg_t sten_thread(void *arg);
static msg_t log_thread(void *arg);
static WORKING_AREA(sten_thread_wa, 1024);
static WORKING_AREA(log_thread_wa, 1024);
static Thread *sten_tp;
static VirtualTimer vt;

// Private functions
static void servodec_func(void);
static void trig_func(void *p);

/*
 * This callback is invoked when a transmission buffer has been completely
 * read by the driver.
 */
static void txend1(UARTDriver *uartp) {
	(void)uartp;
}

/*
 * This callback is invoked when a transmission has physically completed.
 */
static void txend2(UARTDriver *uartp) {
	(void)uartp;
}

/*
 * This callback is invoked on a receive error, the errors mask is passed
 * as parameter.
 */
static void rxerr(UARTDriver *uartp, uartflags_t e) {
	(void)uartp;
	(void)e;
}

/*
 * This callback is invoked when a character is received but the application
 * was not ready to receive it, the character is passed as parameter.
 */
static void rxchar(UARTDriver *uartp, uint16_t c) {
	(void)uartp;
	(void)c;
}

/*
 * This callback is invoked when a receive buffer has been completely written.
 */
static void rxend(UARTDriver *uartp) {
	(void)uartp;
}

/*
 * UART driver configuration structure.
 */
static UARTConfig uart_cfg = {
		txend1,
		txend2,
		rxend,
		rxchar,
		rxerr,
		115200,
		0,
		USART_CR2_LINEN,
		0
};

void app_sten_init(void) {
	chThdCreateStatic(sten_thread_wa, sizeof(sten_thread_wa), NORMALPRIO, sten_thread, NULL);
	chThdCreateStatic(log_thread_wa, sizeof(log_thread_wa), NORMALPRIO - 1, log_thread, NULL);
	servodec_init(servodec_func);

	chSysLock();
	chVTSetI(&vt, MS2ST(10), trig_func, NULL);
	chSysUnlock();
}

static void trig_func(void *p) {
	(void)p;

	chSysLock();
	chVTSetI(&vt, MS2ST(10), trig_func, NULL);
	chSysUnlock();

	chEvtSignalI(sten_tp, (eventmask_t) 1);
}

static msg_t log_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("LOGGING");

	uartStart(&UARTD6, &uart_cfg);
	palSetPadMode(GPIOC, 6, PAL_MODE_ALTERNATE(GPIO_AF_USART6) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);
	palSetPadMode(GPIOC, 7, PAL_MODE_ALTERNATE(GPIO_AF_USART6) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_PULLUP);

	systime_t time = chTimeNow();

	for(;;) {
		time += MS2ST(40);

		// UART code...

		chThdSleepUntil(time);
	}

	return 0;
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

#define HYST		0.10

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

			mcpwm_set_current(servo_val * MCPWM_CURRENT_MAX);
		} else {
			mcpwm_set_current(0.0);
		}
	}

	return 0;
}

#endif
