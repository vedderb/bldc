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
 * servo_dec.c
 *
 *  Created on: 20 jan 2013
 *      Author: benjamin
 */

#include "servo_dec.h"
#include "stm32f4xx_conf.h"
#include "ch.h"
#include "hal.h"

/*
 * Settings
 */
#define USE_PROGRAMMING_CONN	0

#if USE_PROGRAMMING_CONN
#define SERVO_NUM				3
#else
#define SERVO_NUM				1
#endif

#define TIMER_FREQ				1000000
#define INTERRUPT_TRESHOLD		3

// Private variables
static volatile uint32_t interrupt_time = 0;
static volatile int8_t servo_pos[SERVO_NUM];
static volatile uint32_t time_since_update;
static VirtualTimer vt;

// Private functions
static void update_counters(void *p);

// Function pointers
static void(*done_func)(void) = 0;

/**
 * Initialize the serve decoding driver.
 *
 * @param d_func
 * A function that should be called every time the servo signals have been
 * decoded. Can be NULL.
 */
void servodec_init(void (*d_func)(void)) {
	// Initialize variables
	time_since_update = 0;
	interrupt_time = 0;

	NVIC_InitTypeDef   NVIC_InitStructure;
	EXTI_InitTypeDef   EXTI_InitStructure;
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	uint16_t PrescalerValue = 0;

	// ------------- EXTI -------------- //
	// Clocks
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);
#if USE_PROGRAMMING_CONN
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
#endif
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);

#if USE_PROGRAMMING_CONN
	palSetPadMode(GPIOA, 13, PAL_MODE_INPUT);
	palSetPadMode(GPIOA, 14, PAL_MODE_INPUT);
	palSetPadMode(GPIOB, 3, PAL_MODE_INPUT);

	SYSCFG_EXTILineConfig(EXTI_PortSourceGPIOA, EXTI_PinSource13);
	SYSCFG_EXTILineConfig(EXTI_PortSourceGPIOA, EXTI_PinSource14);
	SYSCFG_EXTILineConfig(EXTI_PortSourceGPIOB, EXTI_PinSource3);

	EXTI_InitStructure.EXTI_Line = EXTI_Line3 | EXTI_Line13 | EXTI_Line14;
#else
	palSetPadMode(GPIOB, 5, PAL_MODE_INPUT);

	SYSCFG_EXTILineConfig(EXTI_PortSourceGPIOB, EXTI_PinSource5);

	EXTI_InitStructure.EXTI_Line = EXTI_Line5;
#endif

	EXTI_InitStructure.EXTI_Mode = EXTI_Mode_Interrupt;
	EXTI_InitStructure.EXTI_Trigger = EXTI_Trigger_Rising_Falling;
	EXTI_InitStructure.EXTI_LineCmd = ENABLE;
	EXTI_Init(&EXTI_InitStructure);

#if USE_PROGRAMMING_CONN
	NVIC_InitStructure.NVIC_IRQChannel = EXTI3_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);

	NVIC_InitStructure.NVIC_IRQChannel = EXTI15_10_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
#else
	NVIC_InitStructure.NVIC_IRQChannel = EXTI9_5_IRQn;
	NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelSubPriority = 2;
	NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
	NVIC_Init(&NVIC_InitStructure);
#endif

	// ------------- Timer3 ------------- //
	/* Compute the prescaler value */
	/* TIM3 clock enable */
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE);

	PrescalerValue = (uint16_t) ((SYSTEM_CORE_CLOCK / 2) / TIMER_FREQ) - 1;

	/* Time base configuration */
	TIM_TimeBaseStructure.TIM_Period = 65535;
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;

	TIM_TimeBaseInit(TIM3, &TIM_TimeBaseStructure);

	/* Prescaler configuration */
	TIM_PrescalerConfig(TIM3, PrescalerValue, TIM_PSCReloadMode_Immediate);

	/* TIM3 enable counter */
	TIM_Cmd(TIM3, ENABLE);

	// Set up a virtual timer to update the counters
	chSysLock();
	chVTSetI(&vt, MS2ST(1), update_counters, NULL);
	chSysUnlock();

	// Set our function pointer
	done_func = d_func;
}

static void update_counters(void *p) {
	(void)p;

	chSysLockFromIsr();
	chVTSetI(&vt, MS2ST(1), update_counters, p);
	chSysUnlockFromIsr();

	interrupt_time++;
	time_since_update++;
}

void servodec_int_handler(void) {
	static int curr_index = 0;

	// Long time since last interrupt means that a new cycle has started
	if (interrupt_time >= INTERRUPT_TRESHOLD) {
		curr_index = 0;
		interrupt_time = 0;
		TIM3->CNT = 0;
		return;
	}

	if (curr_index < SERVO_NUM) {
		// Use floating point because we can :)
		float time_ms = (float)(TIM3->CNT);
		time_ms = (time_ms * 1000.0) / (float)TIMER_FREQ;

		if (time_ms < 0.4) {
			return;
		}

		TIM3->CNT = 0;

		// Check if pulse is within valid range
		if (time_ms > 0.8 && time_ms < 2.2) {

			// Truncate (just in case)
			if (time_ms > 2.0) {
				time_ms = 2.0;
			}

			if (time_ms < 1.0) {
				time_ms = 1.0;
			}

			// Update position
			servo_pos[curr_index] = (int8_t)((time_ms - 1.5)  * 255.0);
		}
	}

	curr_index++;

	if (curr_index == SERVO_NUM) {
		time_since_update = 0;

		// Call the function pointer if it is not NULL.
		if (done_func) {
			done_func();
		}
	}

	interrupt_time = 0;
}

/**
 * Get a decoded servo value as an integer.
 *
 * @param servo_num
 * The servo index. If it is out of range, 0 will be returned.
 *
 * @return
 * The servo value in the range [-128 127].
 */
int8_t servodec_get_servo(int servo_num) {
	if (servo_num < SERVO_NUM) {
		return servo_pos[servo_num];
	} else {
		return 0;
	}
}

/**
 * Get a decoded servo value as a float.
 *
 * @param servo_num
 * The servo index. If it is out of range, 0 will be returned.
 *
 * @return
 * The servo value in the range [-1.0 1.0].
 */
float servodec_get_servo_as_float(int servo_num) {
	return (float)servodec_get_servo(servo_num) / 128.0;
}

/**
 * Get the amount of milliseconds that has passed since
 * the last time servo positions were received.
 *
 * @return
 * The amount of milliseconds that have passed since an update.
 */
uint32_t servodec_get_time_since_update(void) {
	return time_since_update;
}
