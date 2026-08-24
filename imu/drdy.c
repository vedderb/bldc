/*
	Copyright 2026 Lukas Hrazky

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

#include "drdy.h"
#include "timer.h"
#include "stm32f4xx_conf.h"

// Concurrently armed instances the ISR dispatch scans. An instance that doesn't get a slot is
// never signalled and its waiter always times out into the timed fallback.
#define DRDY_SLOTS 2

static drdy_t *volatile m_armed[DRDY_SLOTS];
// OR of the armed instances' EXTI lines for a cheap early-out in the ISR dispatch.
static volatile uint32_t m_lines_mask;

void drdy_bind(drdy_t *drdy, stm32_gpio_t *gpio, uint32_t pin) {
	chBSemObjectInit(&drdy->sem, true); // start taken
	drdy->gpio = gpio;
	drdy->pin = pin;
	drdy->exti_line = 1 << pin;
	drdy->timestamp = 0;
	drdy->int_count = 0;
	drdy->timeout_count = 0;
}

void drdy_init(drdy_t *drdy) {
	// Register before unmasking the line so the dispatch can't miss the first edge
	chSysLock();
	for (int i = 0; i < DRDY_SLOTS; i++) {
		if (!m_armed[i]) {
			m_armed[i] = drdy;
			m_lines_mask |= drdy->exti_line;
			break;
		}
	}
	chSysUnlock();

	palSetPadMode(drdy->gpio, drdy->pin, PAL_MODE_INPUT_PULLDOWN);

	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);
	// GPIO ports are 0x400 apart starting from GPIOA, making the EXTI port source their index
	SYSCFG_EXTILineConfig(((uint32_t)drdy->gpio - GPIOA_BASE) / 0x400, drdy->pin);

	// Enable only the data-ready line, the shared EXTI vector is enabled at boot
	EXTI_InitTypeDef exti;
	exti.EXTI_Line = drdy->exti_line;
	exti.EXTI_Mode = EXTI_Mode_Interrupt;
	exti.EXTI_Trigger = EXTI_Trigger_Rising;
	exti.EXTI_LineCmd = ENABLE;
	EXTI_Init(&exti);
}

void drdy_deinit(drdy_t *drdy) {
	// Disable only the line, the shared EXTI vector stays enabled
	EXTI_InitTypeDef exti;
	exti.EXTI_Line = drdy->exti_line;
	exti.EXTI_Mode = EXTI_Mode_Interrupt;
	exti.EXTI_Trigger = EXTI_Trigger_Rising;
	exti.EXTI_LineCmd = DISABLE;
	EXTI_Init(&exti);

	chSysLock();
	for (int i = 0; i < DRDY_SLOTS; i++) {
		if (m_armed[i] == drdy) {
			m_armed[i] = NULL;
		}
	}
	m_lines_mask &= ~drdy->exti_line;
	chSysUnlock();
}

bool drdy_wait(drdy_t *drdy, systime_t timeout) {
	if (chBSemWaitTimeout(&drdy->sem, timeout) == MSG_TIMEOUT) {
		drdy->timeout_count++;
		return false;
	}
	return true;
}

void drdy_signal(drdy_t *drdy) {
	chBSemSignal(&drdy->sem);
}

void drdy_exti_dispatch(void) {
	if (!(EXTI->PR & m_lines_mask)) {
		return;
	}

	for (int i = 0; i < DRDY_SLOTS; i++) {
		drdy_t *drdy = m_armed[i];
		if (drdy && EXTI_GetITStatus(drdy->exti_line) != RESET) {
			EXTI_ClearITPendingBit(drdy->exti_line);
			drdy->timestamp = timer_time_now();
			drdy->int_count++;
			chSysLockFromISR();
			chBSemSignalI(&drdy->sem);
			chSysUnlockFromISR();
		}
	}
}

uint32_t drdy_timestamp(const drdy_t *drdy) {
	return drdy->timestamp;
}

uint32_t drdy_interrupt_count(const drdy_t *drdy) {
	return drdy->int_count;
}

uint32_t drdy_timeout_count(const drdy_t *drdy) {
	return drdy->timeout_count;
}
