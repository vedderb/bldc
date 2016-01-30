/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

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

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "isr_vector_table.h"
#include "mc_interface.h"
#include "servo.h"
#include "hw.h"
#include "encoder.h"

#if SERVO_OUT_ENABLE && !SERVO_OUT_SIMPLE
CH_IRQ_HANDLER(TIM7_IRQHandler) {
	CH_IRQ_PROLOGUE();
	TIM_ClearITPendingBit(TIM7, TIM_IT_Update);
	servo_irq();
	CH_IRQ_EPILOGUE();
}
#endif

CH_IRQ_HANDLER(ADC1_2_3_IRQHandler) {
	CH_IRQ_PROLOGUE();
	ADC_ClearITPendingBit(ADC1, ADC_IT_JEOC);
	mc_interface_adc_inj_int_handler();
	CH_IRQ_EPILOGUE();
}

CH_IRQ_HANDLER(HW_ENC_EXTI_ISR_VEC) {
	if (EXTI_GetITStatus(HW_ENC_EXTI_LINE) != RESET) {
		encoder_reset();

		// Clear the EXTI line pending bit
		EXTI_ClearITPendingBit(HW_ENC_EXTI_LINE);
	}
}
