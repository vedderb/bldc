/*
 * app_gurgalof.c
 *
 *  Created on: 18 apr 2014
 *      Author: benjamin
 */

#include "app.h"
#ifdef USE_APP_GURGALOF

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "servo_dec.h"
#include "mcpwm.h"
#include "hw.h"

// Threads
static msg_t gurgalof_thread(void *arg);
static WORKING_AREA(gurgalof_thread_wa, 1024);

void app_gurgalof_init(void) {
	chThdCreateStatic(gurgalof_thread_wa, sizeof(gurgalof_thread_wa), NORMALPRIO, gurgalof_thread, NULL);
}

static msg_t gurgalof_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP_GURGALOF");

	for(;;) {
#define MIN_PWR	0.2
		float pwr = (float)ADC_Value[ADC_IND_EXT];
		pwr /= 4095.0;
		pwr /= (1.0 - MIN_PWR);
		pwr -= MIN_PWR;

		if (pwr < 0.0) {
			mcpwm_set_duty(0.0);
		} else {
			mcpwm_set_duty(pwr);
//			mcpwm_set_current(pwr * 40.0);
		}

		chThdSleepMilliseconds(10);
	}

	return 0;
}

#endif
