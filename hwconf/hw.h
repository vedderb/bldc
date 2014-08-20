/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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

/*
 * hw.h
 *
 *  Created on: 12 apr 2014
 *      Author: benjamin
 */

#ifndef HW_H_
#define HW_H_

#include "conf_general.h"

#ifdef HW_VERSION_40
#include "hw_40.h"
#elif defined HW_VERSION_R2
#include "hw_r2.h"
#elif defined HW_VERSION_BW
#include "hw_bw.h"
#elif defined HW_VERSION_PL
#include "hw_pl.h"
#else
#error "No hardware version defined"
#endif

// MCPWM timers
#if (PWM_TIMER==1)
//TIM1
#define TIM_PWM								TIM1
#define TIM_PWM_UP_IRQn						TIM1_UP_TIM10_IRQn
#define RCC_APB2Periph_TIM_PWM				RCC_APB2Periph_TIM1
#define TIM_PWM_UP_IRQHandler				TIM1_UP_IRQHandler
#define ADC_ExternalTrigInjecConv_T_PWM_CC4	ADC_ExternalTrigInjecConv_T1_CC4
//TIM4 slave of TIM1
#define TIM_ADC								TIM3
#define TIM_TS_PWM 							TIM_TS_ITR0
#define ADC_ExternalTrigConv_T_ADC_CCx		ADC_ExternalTrigConv_T3_CC1
#define ADC_ExternalTrigInjecConv_T_ADC_CC4	ADC_ExternalTrigInjecConv_T3_CC4
#define RCC_APB1Periph_TIM_ADC				RCC_APB1Periph_TIM3

#elif (PWM_TIMER==8)
//TIM8
// on STM32F4xx TIM8 update is shared with TIM13 global interrupt
#define TIM_PWM								TIM8
#define TIM_PWM_UP_IRQn						TIM8_UP_IRQn
#define RCC_APB2Periph_TIM_PWM				RCC_APB2Periph_TIM8
#define TIM_PWM_UP_IRQHandler				TIM8_UP_IRQHandler
#define ADC_ExternalTrigInjecConv_T_PWM_CC4	ADC_ExternalTrigInjecConv_T8_CC4
//TIM4 slave of TIM8
#define TIM_ADC								TIM5
#define TIM_TS_PWM							TIM_TS_ITR3
#define ADC_ExternalTrigConv_T_ADC_CC1		ADC_ExternalTrigConv_T5_CC1
#define ADC_ExternalTrigInjecConv_T_ADC_CC4	ADC_ExternalTrigInjecConv_T5_CC4
#define RCC_APB1Periph_TIM_ADC				RCC_APB1Periph_TIM5
	
#endif


// Functions
void hw_init_gpio(void);
void hw_setup_adc_channels(void);
void hw_setup_servo_outputs(void);

#endif /* HW_H_ */
