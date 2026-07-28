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
#include "conf_general.h"

#ifdef IMU_DRDY_GPIO

#include "hal.h"
#include "stm32f4xx_conf.h"

static binary_semaphore_t m_sem;
static volatile bool m_sem_ready = false;
static volatile uint32_t m_int_count;
static volatile uint32_t m_timeout_count;

bool drdy_present(void) {
	return true;
}

void drdy_init(void) {
	chBSemObjectInit(&m_sem, true); // start taken
	m_int_count = 0;
	m_timeout_count = 0;
	m_sem_ready = true;

	palSetPadMode(IMU_DRDY_GPIO, IMU_DRDY_PIN, PAL_MODE_INPUT_PULLDOWN);

	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);
	SYSCFG_EXTILineConfig(IMU_DRDY_EXTI_PORTSRC, IMU_DRDY_EXTI_PINSRC);

	// Enable only the data-ready line, the shared EXTI vector is enabled at boot
	EXTI_InitTypeDef exti;
	exti.EXTI_Line = IMU_DRDY_EXTI_LINE;
	exti.EXTI_Mode = EXTI_Mode_Interrupt;
	exti.EXTI_Trigger = EXTI_Trigger_Rising;
	exti.EXTI_LineCmd = ENABLE;
	EXTI_Init(&exti);
}

void drdy_deinit(void) {
	m_sem_ready = false;

	// Disable only the line, the shared EXTI vector stays enabled
	EXTI_InitTypeDef exti;
	exti.EXTI_Line = IMU_DRDY_EXTI_LINE;
	exti.EXTI_Mode = EXTI_Mode_Interrupt;
	exti.EXTI_Trigger = EXTI_Trigger_Rising;
	exti.EXTI_LineCmd = DISABLE;
	EXTI_Init(&exti);
}

bool drdy_wait(systime_t timeout) {
	if (chBSemWaitTimeout(&m_sem, timeout) == MSG_TIMEOUT) {
		m_timeout_count++;
		return false;
	}
	return true;
}

void drdy_signal(void) {
	if (m_sem_ready) {
		chBSemSignal(&m_sem);
	}
}

void drdy_signal_isr(void) {
	m_int_count++;
	chSysLockFromISR();
	if (m_sem_ready) {
		chBSemSignalI(&m_sem);
	}
	chSysUnlockFromISR();
}

uint32_t drdy_interrupt_count(void) {
	return m_int_count;
}

uint32_t drdy_timeout_count(void) {
	return m_timeout_count;
}

#else // no DRDY pin wired on this board

bool drdy_present(void) {
	return false;
}

void drdy_init(void) {
}

void drdy_deinit(void) {
}

bool drdy_wait(systime_t timeout) {
	(void)timeout;
	return false;
}

void drdy_signal(void) {
}

void drdy_signal_isr(void) {
}

uint32_t drdy_interrupt_count(void) {
	return 0;
}

uint32_t drdy_timeout_count(void) {
	return 0;
}

#endif
