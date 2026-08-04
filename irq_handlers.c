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

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "isr_vector_table.h"
#include "mc_interface.h"
#include "mcpwm_foc.h"
#include "hw.h"
#include "encoder/encoder.h"
#include "main.h"
#include "irq_handlers.h"
#include "imu/drdy.h"

CH_IRQ_HANDLER(ADC1_2_3_IRQHandler) {
	CH_IRQ_PROLOGUE();
	ADC_ClearITPendingBit(ADC1, ADC_IT_JEOC);
	mc_interface_adc_inj_int_handler();
	CH_IRQ_EPILOGUE();
}

void irq_handlers_init(void) {
	nvicEnableVector(EXTI9_5_IRQn, 6);
	nvicEnableVector(EXTI15_10_IRQn, 6);

	// Latch supply dips below the PVD threshold into crash_info: a sag that resets
	// nothing is otherwise invisible, and one latched right before a reset is
	// evidence of a supply-caused reset. 2.9V is the highest available threshold,
	// well below normal 3.3V operation and above the ~1.7V POR level.
	PWR->CR |= PWR_CR_PLS_LEV7 | PWR_CR_PVDE; // level 7 = 2.9V

	// The PVD output is wired to EXTI line 16; the rising edge is VDD crossing
	// below the threshold.
	EXTI_InitTypeDef exti;
	exti.EXTI_Line = EXTI_Line16;
	exti.EXTI_Mode = EXTI_Mode_Interrupt;
	exti.EXTI_Trigger = EXTI_Trigger_Rising;
	exti.EXTI_LineCmd = ENABLE;
	EXTI_Init(&exti);

	nvicEnableVector(PVD_IRQn, 6);
}

// The STM32 multiplexes EXTI lines 5-9 and 10-15 onto one NVIC vector each. Every GPIO EXTI
// source is checked here by line, so a source is serviced whichever group its pin falls in,
// and sources that share a group's vector coalesce into one handler. Add a source by checking
// its line below (and, for a source on EXTI lines 0-4, adding that line's handler).
static void exti_gpio_dispatch(void) {
	if (EXTI_GetITStatus(HW_ENC_EXTI_LINE) != RESET) {
		encoder_pin_isr();
		EXTI_ClearITPendingBit(HW_ENC_EXTI_LINE);
	}
#ifdef IMU_DRDY_GPIO
	if (EXTI_GetITStatus(IMU_DRDY_EXTI_LINE) != RESET) {
		EXTI_ClearITPendingBit(IMU_DRDY_EXTI_LINE);
		drdy_signal_isr();
	}
#endif
}

CH_IRQ_HANDLER(EXTI9_5_IRQHandler) {
	CH_IRQ_PROLOGUE();
	exti_gpio_dispatch();
	CH_IRQ_EPILOGUE();
}

CH_IRQ_HANDLER(EXTI15_10_IRQHandler) {
	CH_IRQ_PROLOGUE();
	exti_gpio_dispatch();
	CH_IRQ_EPILOGUE();
}

CH_IRQ_HANDLER(HW_ENC_TIM_ISR_VEC) {
	if (TIM_GetITStatus(HW_ENC_TIM, TIM_IT_Update) != RESET) {
		encoder_tim_isr();

		// Clear the IT pending bit
		TIM_ClearITPendingBit(HW_ENC_TIM, TIM_IT_Update);
	}
}

CH_IRQ_HANDLER(TIM2_IRQHandler) {
	if (TIM_GetITStatus(TIM2, TIM_IT_CC2) != RESET) {
		mcpwm_foc_tim_sample_int_handler();

		// Clear the IT pending bit
		TIM_ClearITPendingBit(TIM2, TIM_IT_CC2);
	}
	TIM_ClearITPendingBit(TIM2, TIM_IT_CC2);
}

CH_IRQ_HANDLER(PVD_IRQHandler) {
	if (EXTI_GetITStatus(EXTI_Line16) != RESET) {
		// Log the fault. Supply voltage dropped below 2.9V,
		// could corrupt an ongoing flash programming
		mc_interface_fault_stop(FAULT_CODE_MCU_UNDER_VOLTAGE, false, true);

		// Latch the dip for the crash diagnostics
		crash_info.pvd_dips++;
		crash_info.pvd_last_uptime = chVTGetSystemTimeX() / CH_CFG_ST_FREQUENCY;

		// Mask the line to bound a bouncing supply to one latched dip per
		// timeout thread iteration (which re-arms it)
		EXTI->IMR &= ~EXTI_Line16;

		// Clear the PVD pending bit
		EXTI_ClearITPendingBit(EXTI_Line16);
		EXTI_ClearFlag(EXTI_Line16);
	}
}

CH_IRQ_HANDLER(NMI_Handler) {
	main_fault_handler();
}

CH_IRQ_HANDLER(HardFault_Handler) {
	main_fault_handler();
}

CH_IRQ_HANDLER(MemManage_Handler) {
	main_fault_handler();
}

CH_IRQ_HANDLER(BusFault_Handler) {
	main_fault_handler();
}

CH_IRQ_HANDLER(UsageFault_Handler) {
	main_fault_handler();
}
