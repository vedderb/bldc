/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se
	Copyright 2022 Jakub Tomczak

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

#include "encoder/enc_abi.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils_math.h"

#include <string.h>
#include <math.h>

bool enc_abi_init(ABI_config_t *cfg) {
	EXTI_InitTypeDef EXTI_InitStructure;

	memset(&cfg->state, 0, sizeof(ABI_state));

	palSetPadMode(cfg->A_gpio, cfg->A_pin, PAL_MODE_ALTERNATE(cfg->tim_af));
	palSetPadMode(cfg->B_gpio, cfg->B_pin, PAL_MODE_ALTERNATE(cfg->tim_af));
	palSetPadMode(cfg->I_gpio, cfg->I_pin, PAL_MODE_INPUT_PULLUP);

	// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Enable SYSCFG clock
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);

	TIM_EncoderInterfaceConfig(cfg->timer,
			TIM_EncoderMode_TI12, TIM_ICPolarity_Rising, TIM_ICPolarity_Rising);
	TIM_SetAutoreload(cfg->timer, cfg->counts - 1);

	// Filter
	cfg->timer->CCMR1 |= 6 << 12 | 6 << 4;
	cfg->timer->CCMR2 |= 6 << 4;

	TIM_Cmd(cfg->timer, ENABLE);

	// Interrupt on index pulse

	// Connect EXTI Line to pin
	SYSCFG_EXTILineConfig(cfg->exti_portsrc, cfg->exti_pinsrc);

	// Configure EXTI Line
	EXTI_InitStructure.EXTI_Line = cfg->exti_line;
	EXTI_InitStructure.EXTI_Mode = EXTI_Mode_Interrupt;
	EXTI_InitStructure.EXTI_Trigger = EXTI_Trigger_Rising;
	EXTI_InitStructure.EXTI_LineCmd = ENABLE;
	EXTI_Init(&EXTI_InitStructure);

	// Enable and set EXTI Line Interrupt to the highest priority
	nvicEnableVector(cfg->exti_ch, 0);

	return true;
}

void enc_abi_deinit(ABI_config_t *cfg) {
	nvicDisableVector(cfg->exti_ch);
	TIM_DeInit(cfg->timer);
	palSetPadMode(cfg->A_gpio, cfg->A_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(cfg->B_gpio, cfg->B_pin, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(cfg->I_gpio, cfg->I_pin, PAL_MODE_INPUT_PULLUP);
}

float enc_abi_read_deg(ABI_config_t *cfg) {
	return ((float)cfg->timer->CNT * 360.0) / (float)cfg->counts;
}

void enc_abi_pin_isr(ABI_config_t *cfg) {
	// Only reset if the pin is still high to avoid too short pulses, which
	// most likely are noise.
	__NOP();
	__NOP();
	__NOP();
	__NOP();
	if (palReadPad(cfg->I_gpio, cfg->I_pin)) {
		const unsigned int cnt = cfg->timer->CNT;
		const unsigned int lim = cfg->counts / 20;

		if (cfg->state.index_found) {
			// Some plausibility filtering.
			if (cnt > (cfg->counts - lim) || cnt < lim) {
				cfg->timer->CNT = 0;
				cfg->state.bad_pulses = 0;
			} else {
				cfg->state.bad_pulses++;

				if (cfg->state.bad_pulses > 5) {
					cfg->state.index_found = 0;
				}
			}
		} else {
			cfg->timer->CNT = 0;
			cfg->state.index_found = true;
			cfg->state.bad_pulses = 0;
		}
	}
}
