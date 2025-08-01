/*
	Copyright 2018 Benjamin Vedder	benjamin@vedder.se

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

#include "hw.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils_math.h"
#include <math.h>
#include "mc_interface.h"
#include "stm32f4xx_rcc.h"
#include "terminal.h"
#include "commands.h"
#include "mc_interface.h"
#include "stdio.h"
#include <math.h>
#include "minilzo.h"
#include "i2c_bb.h"
#include "buffer.h"

// Variables
static volatile bool i2c_running = false;

// Private functions
static volatile uint8_t a50s_dual_id = 0;


// I2C configuration
static const I2CConfig i2cfg = {
		OPMODE_I2C,
		100000,
		STD_DUTY_CYCLE
};

void hw_init_gpio(void) {
	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOE, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOH, ENABLE);

	// LEDs
	palSetPadMode(GPIOD, 7, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 4, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	palSetPadMode(GPIOA, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOB, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	// Phase filters
	palSetPadMode(GPIOE, 11, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOE, 9, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 2, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);	
	
	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);			
		
	// DAC as voltage reference for shunt amps
	palSetPadMode(GPIOA, 4, PAL_MODE_INPUT_ANALOG);
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_DAC, ENABLE);
	DAC->CR |= DAC_CR_EN1 | DAC_CR_BOFF1;
	DAC->DHR12R1 = 2047;
	
	hw_a50s_get_id_from_pins();
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 		1, ADC_SampleTime_15Cycles);	// 0 - ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0,  		2, ADC_SampleTime_15Cycles);	// 3 - ADC_IND_SENS1	
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5,  		3, ADC_SampleTime_15Cycles);	// 6 - ADC_IND_VIN_SENS	

	// ADC2 regular channels																
	ADC_RegularChannelConfig(ADC2, ADC_Channel_12, 		1, ADC_SampleTime_15Cycles);	// 1 - ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 		2, ADC_SampleTime_15Cycles);	// 4 - ADC_IND_SENS2	
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 		3, ADC_SampleTime_15Cycles);	// 7 - unsued	

	// ADC3 regular channels - only a subset of channels avaliable											
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 		1, ADC_SampleTime_15Cycles);	// 2 - ADC_IND_CURR3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 		2, ADC_SampleTime_15Cycles);	// 5 - ADC_IND_SENS3	
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 		3, ADC_SampleTime_15Cycles);	// 8 - ADC_IND_TEMP_MOS		
	


	// Injected channels																	
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);			// ADC_IND_CURR1
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);			// ADC_IND_CURR2
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_13, 1, ADC_SampleTime_15Cycles);			// ADC_IND_CURR3
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);			// ADC_IND_CURR1
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);			// ADC_IND_CURR2
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_13, 2, ADC_SampleTime_15Cycles);			// ADC_IND_CURR3
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 3, ADC_SampleTime_15Cycles);			// ADC_IND_CURR1
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_12, 3, ADC_SampleTime_15Cycles);			// ADC_IND_CURR2
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_13, 3, ADC_SampleTime_15Cycles);			// ADC_IND_CURR3
	
}
void hw_start_i2c(void){}
void hw_stop_i2c(void){}
void hw_try_restore_i2c(void){}

void hw_a50s_get_id_from_pins(void) {
	// ID pins for CAN ID
	uint8_t id0 = palReadPad(GPIOD, 4);
	uint8_t id1 = palReadPad(GPIOC, 12);
	
	a50s_dual_id = id0 | (id1 << 1);
}

uint8_t get_a50s_dual_id()
{
	return a50s_dual_id;
}

float get_cal1(){
	if(get_a50s_dual_id() & 1)
		return 0.909384;
	else
		return 0.894089;
	return 1;
}

float get_cal2(){
	if(get_a50s_dual_id() & 1)
		return 0.919358;
	else
		return 0.895433;
	return 1;
}

float get_cal3(){
	if(get_a50s_dual_id() & 1)
		return 0.907427;
	else
		return 0.903154;
	return 1;
}