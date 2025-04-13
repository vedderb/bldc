/*
	Copyright 2025 Benjamin Vedder	benjamin@vedder.se

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

#include "enc_pwm.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "mc_interface.h"
#include "utils_math.h"
#include "hw.h"
#include "timer.h"
#include "servo_dec.h"
#include "pwm_servo.h"

#include <math.h>
#include <string.h>

static volatile uint32_t m_icu_last_width = 0;
static volatile uint32_t m_icu_last_period = 0;
static volatile uint32_t m_icu_update_cnt = 0;
static volatile float m_icu_angle = 0.0;
static volatile float m_icu_angle_last = 0.0;
static volatile bool m_update_abi = false;
static volatile uint32_t m_ts_last = 0;
static volatile float m_speed_now = 0.0;

static void icuwidthcb(ICUDriver *icup) {
	m_icu_last_width = icuGetWidthX(icup);
	m_icu_last_period = icuGetPeriodX(icup);
	m_icu_update_cnt++;

	m_icu_angle = fminf(m_icu_last_width, m_icu_last_period) / m_icu_last_period * 360.0;

	if (m_icu_update_cnt > 2) {
		float dt = timer_seconds_elapsed_since(m_ts_last);
		m_speed_now = utils_angle_difference(m_icu_angle, m_icu_angle_last) / dt;
	}

	m_ts_last = timer_time_now();
	m_icu_angle_last = m_icu_angle;

	if (m_update_abi) {
		HW_ENC_TIM->CNT = m_icu_angle / 360.0 * (float)(HW_ENC_TIM->ARR);
	}
}

static ICUConfig m_icucfg = {
		ICU_INPUT_ACTIVE_HIGH,
		1000000,
		icuwidthcb,
		NULL,
		NULL,
		HW_ICU_CHANNEL,
		0
};

bool enc_pwm_init(bool update_abi) {
	m_update_abi = update_abi;
	m_icu_update_cnt = 0;
	m_speed_now = 0.0;

	servodec_stop();
	pwm_servo_stop();

	if (HW_ICU_DEV.state == ICU_ACTIVE) {
		icuStopCapture(&HW_ICU_DEV);
		icuStop(&HW_ICU_DEV);
	}

	icuStart(&HW_ICU_DEV, &m_icucfg);
	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_ALTERNATE(HW_ICU_GPIO_AF));
	icuStartCapture(&HW_ICU_DEV);
	icuEnableNotifications(&HW_ICU_DEV);

	return true;
}

void enc_pwm_deinit(void) {
	m_icu_update_cnt = 0;

	if (HW_ICU_DEV.state == ICU_ACTIVE) {
		icuStopCapture(&HW_ICU_DEV);
		icuStop(&HW_ICU_DEV);
	}
}

float enc_pwm_read_deg(void) {
	float dt = timer_seconds_elapsed_since(m_ts_last);
	return m_icu_angle + m_speed_now * dt;
}

uint32_t enc_pwm_update_cnt(void) {
	return m_icu_update_cnt;
}
