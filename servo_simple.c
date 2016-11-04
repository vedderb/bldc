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

#include "servo_simple.h"
#include "ch.h"
#include "hal.h"
#include "hw.h"
#include "conf_general.h"
#include "stm32f4xx_conf.h"
#include "utils.h"

// Settings
#define TIM_CLOCK			1000000 // Hz

#if SERVO_OUT_ENABLE && SERVO_OUT_SIMPLE

void servo_simple_init(void) {
	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;

	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_ALTERNATE(HW_ICU_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE);

	TIM_TimeBaseStructure.TIM_Period = (uint16_t)((uint32_t)TIM_CLOCK / (uint32_t)SERVO_OUT_RATE_HZ);
	TIM_TimeBaseStructure.TIM_Prescaler = (uint16_t)((168000000 / 2) / TIM_CLOCK) - 1;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;

	TIM_TimeBaseInit(TIM3, &TIM_TimeBaseStructure);

	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM1;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_Pulse = 0;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;

	TIM_OC2Init(TIM3, &TIM_OCInitStructure);
	TIM_OC2PreloadConfig(TIM3, TIM_OCPreload_Enable);

	TIM_ARRPreloadConfig(TIM3, ENABLE);

	servo_simple_set_output(0.5);

	TIM_Cmd(TIM3, ENABLE);
}

void servo_simple_set_output(float out) {
	utils_truncate_number(&out, 0.0, 1.0);

	float us = (float)SERVO_OUT_PULSE_MIN_US + out * (float)(SERVO_OUT_PULSE_MAX_US - SERVO_OUT_PULSE_MIN_US);
	us *= (float)TIM_CLOCK / 1000000.0;
	TIM3->CCR2 = (uint32_t)us;
}

#endif
