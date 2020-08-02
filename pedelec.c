/*
 * pedelec.c
 *
 *  Created on: Sep 3, 2019
 *      Author: Maximiliano Cordoba
 *      email: mcordoba@paltatech.com
 */

#include "pedelec.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "mc_interface.h"
#include "utils.h"
#include "math.h"
#include "commands.h"

static volatile float pedelec_frecuency = 0.0;
static volatile uint16_t pedelec_timer_C2_value = 0;
static volatile uint16_t pedelec_timer_C1_value = 0;
static volatile TIM_ICInitTypeDef  TIM_ICInitStructure;
static volatile bool pedelec_pulse_detected_flag = false;

static void TIM_PWMinput_config(void);

static void TIM_PWMinput_config(void){

	HW_PEDELEC_TIM_CLK_EN();

	palSetPadMode(HW_PEDELEC_GPIO, HW_PEDELEC_PIN, PAL_MODE_INPUT_PULLUP | PAL_MODE_ALTERNATE(HW_PEDELEC_GPIO_AF));

	nvicEnableVector(HW_PEDELEC_TIM_ISR_CH,5);

	  /* ---------------------------------------------------------------------------
	    TIM4 configuration: PWM Input mode

	    In this example TIM4 input clock (TIM4CLK) is set to 2 * APB1 clock (PCLK1),
	    since APB1 prescaler is different from 1.
	      TIM4CLK = 2 * PCLK1
	      PCLK1 = HCLK / 4
	      => TIM4CLK = HCLK / 2 = SystemCoreClock /2

	    External Signal Frequency = TIM4 counter clock / TIM4_CCR2 in Hz.

	    External Signal DutyCycle = (TIM4_CCR1*100)/(TIM4_CCR2) in %.

	  --------------------------------------------------------------------------- */
	TIM_ICInitStructure.TIM_Channel = HW_PEDELEC_TIM_CHANNEL;
	TIM_ICInitStructure.TIM_ICPolarity = TIM_ICPolarity_Rising;
	TIM_ICInitStructure.TIM_ICSelection = TIM_ICSelection_DirectTI;
	TIM_ICInitStructure.TIM_ICPrescaler = TIM_ICPSC_DIV1;
	TIM_ICInitStructure.TIM_ICFilter = 0x0;

	TIM_PWMIConfig(HW_PEDELEC_TIMER, &TIM_ICInitStructure);

	/* Select the TIM4 Input Trigger: TI2FP2 */
	TIM_SelectInputTrigger(HW_PEDELEC_TIMER, TIM_TS_TI2FP2);

//	TIM_SetIC1Prescaler(HW_PEDELEC_TIMER,TIM_ICPSC_DIV8);
//	TIM_SetIC2Prescaler(HW_PEDELEC_TIMER,TIM_ICPSC_DIV8);

	TIM_PrescalerConfig(HW_PEDELEC_TIMER,1000,TIM_PSCReloadMode_Immediate);
	/* Select the slave Mode: Reset Mode */
	TIM_SelectSlaveMode(HW_PEDELEC_TIMER, TIM_SlaveMode_Reset);
	TIM_SelectMasterSlaveMode(HW_PEDELEC_TIMER,TIM_MasterSlaveMode_Enable);

	/* TIM enable counter */
	TIM_Cmd(HW_PEDELEC_TIMER, ENABLE);

	/* Enable the CC2 Interrupt Request */
	TIM_ITConfig(HW_PEDELEC_TIMER, TIM_IT_CC2, ENABLE);

}

void pedelec_set_pulse_detected_flag(bool value){
	pedelec_pulse_detected_flag = value;
}

bool pedelec_get_pulse_detected_flag(void){
	return pedelec_pulse_detected_flag;
}


void pedelec_tim_isr(void){

	pedelec_pulse_detected_flag = true;

	/* Get the Input Capture value */
	pedelec_timer_C2_value = TIM_GetCapture2(HW_PEDELEC_TIMER);
	pedelec_timer_C1_value = TIM_GetCapture1(HW_PEDELEC_TIMER);

}

float pedelec_get_frecuency(void){
	return pedelec_frecuency/1000;
}

float pedelec_get_rpm( float frecuency, uint8_t magnets){
	float ret;
	if(magnets > 0){
		ret = frecuency * 60 / magnets;
	}else{
		ret = 0;
	}
	return ret;
}

void pedelec_periodic_task( uint32_t delta_ms ){
	static volatile uint32_t time_between_pulses_ms = 0;
	static volatile uint32_t time_between_pulses_limit_ms = 500;
	static volatile bool low_frecuency_flag = true;
	static volatile uint8_t low_frecuency_flag_hysteresis = 0;


	if( low_frecuency_flag ){
		pedelec_frecuency = 0;
	}else{
		if (pedelec_timer_C2_value != 0)
		{
			RCC_ClocksTypeDef RCC_Clocks;
			RCC_GetClocksFreq(&RCC_Clocks);

			pedelec_frecuency = (RCC_Clocks.HCLK_Frequency)/2 / pedelec_timer_C2_value;

			//clear this value just to make the system calculate the frecuency only when a new value was read
			pedelec_timer_C2_value = 0;
		}

	}

	if(pedelec_get_pulse_detected_flag()){
		pedelec_set_pulse_detected_flag(false);
		if(time_between_pulses_ms < time_between_pulses_limit_ms){
			if(low_frecuency_flag_hysteresis > 1)
			{
				low_frecuency_flag = false;
			}else{
				low_frecuency_flag_hysteresis++;
			}
		}
		time_between_pulses_ms = 0;
	}else{
		if( time_between_pulses_ms > time_between_pulses_limit_ms){
			low_frecuency_flag = true;
			low_frecuency_flag_hysteresis = 0;
		}else{
			time_between_pulses_ms += delta_ms;
		}
	}

}

void pedelec_init(void){
	TIM_PWMinput_config();
}




