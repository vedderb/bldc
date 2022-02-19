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

#include "encoder/ABI.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include <math.h>

static float last_enc_angle = 0.0;
static ABI_config_t ABI_config_now = { 0 };

void ABI_deinit(void) {
	nvicDisableVector(HW_ENC_EXTI_CH);

	TIM_DeInit(HW_ENC_TIM);

	palSetPadMode(ABI_config_now.A_gpio,
			ABI_config_now.A_pin,
			PAL_MODE_INPUT_PULLUP);
	palSetPadMode(ABI_config_now.B_gpio,
			ABI_config_now.B_pin,
			PAL_MODE_INPUT_PULLUP);

	last_enc_angle = 0.0;

	ABI_config_now.is_init = 0;
}

encoder_ret_t ABI_init(ABI_config_t *abi_config) {

	EXTI_InitTypeDef EXTI_InitStructure;

	// Initialize variables
	ABI_config_now = *abi_config;

	palSetPadMode(ABI_config_now.A_gpio,
			ABI_config_now.A_pin,
			PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));
	palSetPadMode(ABI_config_now.B_gpio,
			ABI_config_now.B_pin,
			PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));
//	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_ALTERNATE(HW_ENC_TIM_AF));

// Enable timer clock
	HW_ENC_TIM_CLK_EN();

	// Enable SYSCFG clock
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_SYSCFG, ENABLE);

	TIM_EncoderInterfaceConfig(HW_ENC_TIM, TIM_EncoderMode_TI12,
	TIM_ICPolarity_Rising,
	TIM_ICPolarity_Rising);
	TIM_SetAutoreload(HW_ENC_TIM, ABI_config_now.counts - 1);

	// Filter
	HW_ENC_TIM->CCMR1 |= 6 << 12 | 6 << 4;
	HW_ENC_TIM->CCMR2 |= 6 << 4;

	TIM_Cmd(HW_ENC_TIM, ENABLE);

	// Interrupt on index pulse

	// Connect EXTI Line to pin
	SYSCFG_EXTILineConfig(HW_ENC_EXTI_PORTSRC, HW_ENC_EXTI_PINSRC);

	// Configure EXTI Line
	EXTI_InitStructure.EXTI_Line = HW_ENC_EXTI_LINE;
	EXTI_InitStructure.EXTI_Mode = EXTI_Mode_Interrupt;
	EXTI_InitStructure.EXTI_Trigger = EXTI_Trigger_Rising;
	EXTI_InitStructure.EXTI_LineCmd = ENABLE;
	EXTI_Init(&EXTI_InitStructure);

	abi_config->is_init = 1;
	ABI_config_now = *abi_config;

	// Enable and set EXTI Line Interrupt to the highest priority
	nvicEnableVector(HW_ENC_EXTI_CH, 0);

	return ENCODER_OK;
}

float ABI_read_deg(void) {
	last_enc_angle = ((float) HW_ENC_TIM->CNT * 360.0) / (float) ABI_config_now.counts;
	return last_enc_angle;
}

