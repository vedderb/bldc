/*
	Copyright 2024 Benjamin Vedder	benjamin@vedder.se

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

#include "pwm_servo.h"
#include "ch.h"
#include "hal.h"
#include "conf_general.h"
#include "utils.h"

#pragma GCC push_options
#pragma GCC optimize ("Os")

// Settings
#define TIM_CLOCK			2000000 // Hz

// Private variables
static volatile bool m_is_running = false;

uint32_t pwm_servo_init(uint32_t freq_hz, float duty) {
	// Ensure that there is no overflow and that the resolution is reasonable
	utils_truncate_number_uint32(&freq_hz, TIM_CLOCK / 65000, TIM_CLOCK / 100);

	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_ALTERNATE(HW_ICU_GPIO_AF) |
			PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	HW_ICU_TIM_CLK_EN();

	HW_ICU_TIMER->CR1 = 0;
	HW_ICU_TIMER->ARR = (uint16_t)((uint32_t)TIM_CLOCK / (uint32_t)freq_hz);
	HW_ICU_TIMER->PSC = (uint16_t)((168000000 / 2) / TIM_CLOCK) - 1;
	HW_ICU_TIMER->EGR = TIM_PSCReloadMode_Immediate;

	utils_truncate_number(&duty, 0.0, 1.0);
	uint32_t output = (uint32_t)((float)HW_ICU_TIMER->ARR * duty);

	if (HW_ICU_CHANNEL == ICU_CHANNEL_1) {
		HW_ICU_TIMER->CCER = TIM_OutputState_Enable;
		HW_ICU_TIMER->CCMR1 = TIM_OCMode_PWM1 | TIM_OCPreload_Enable;
		HW_ICU_TIMER->CCR1 = output;
	} else if (HW_ICU_CHANNEL == ICU_CHANNEL_2) {
		HW_ICU_TIMER->CCER = (TIM_OutputState_Enable << 4);
		HW_ICU_TIMER->CCMR1 = (TIM_OCMode_PWM1 << 8) | (TIM_OCPreload_Enable << 8);
		HW_ICU_TIMER->CCR2 = output;
	}

	HW_ICU_TIMER->CR1 |= TIM_CR1_ARPE;

	pwm_servo_set_servo_out(0.5);

	HW_ICU_TIMER->CR1 |= TIM_CR1_CEN;

	m_is_running = true;

	return freq_hz;
}

void pwm_servo_init_servo(void) {
	pwm_servo_init(SERVO_OUT_RATE_HZ, 0.0);
}

void pwm_servo_stop(void) {
	if (m_is_running) {
		palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_INPUT);
	}

	TIM_DeInit(HW_ICU_TIMER);
	m_is_running = false;
}

float pwm_servo_set_duty(float duty) {
	if (!m_is_running) {
		return -1.0;
	}

	utils_truncate_number(&duty, 0.0, 1.0);
	uint32_t output = (uint32_t)((float)HW_ICU_TIMER->ARR * duty);

	if (HW_ICU_CHANNEL == ICU_CHANNEL_1) {
		HW_ICU_TIMER->CCR1 = output;
	} else if (HW_ICU_CHANNEL == ICU_CHANNEL_2) {
		HW_ICU_TIMER->CCR2 = output;
	}

	return (float)output / (float)HW_ICU_TIMER->ARR;

}

void pwm_servo_set_servo_out(float output) {
	if (!m_is_running) {
		return;
	}

	utils_truncate_number(&output, 0.0, 1.0);

	float us = (float)SERVO_OUT_PULSE_MIN_US + output *
			(float)(SERVO_OUT_PULSE_MAX_US - SERVO_OUT_PULSE_MIN_US);
	us *= (float)TIM_CLOCK / 1000000.0;

	if (HW_ICU_CHANNEL == ICU_CHANNEL_1) {
		HW_ICU_TIMER->CCR1 = (uint32_t)us;
	} else if (HW_ICU_CHANNEL == ICU_CHANNEL_2) {
		HW_ICU_TIMER->CCR2 = (uint32_t)us;
	}
}

bool pwm_servo_is_running(void) {
	return m_is_running;
}

#pragma GCC pop_options
