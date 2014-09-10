/*
	Copyright 2014 Cyril Holweck	cyril.holweck@free.fr

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
 * hw_pl.c
 *
 *  Created on: 04 jul 2014
 *      Author: cyrilh
 */

#include "hw.h"
#ifdef HW_VERSION_PL

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "servo.h"

void hw_init_gpio(void) {
	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);

	// LEDs
	palSetPadMode(GPIOB, 3,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 4,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

	// GPIOC (ENABLE_GATE)
	/** TODO: add second gate */
	palSetPadMode(GPIOC, 13,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	DISABLE_GATE();

	// GPIOB (DCCAL)
	palSetPadMode(GPIOB, 12,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

	// DEBUG:
	palSetPadMode(GPIOA, 0, PAL_MODE_ALTERNATE(GPIO_AF_TIM5) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
			
	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	/** TODO: add second gate */
	palSetPadMode(GPIOC, 6, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOC, 7, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOC, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOA, 7, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 0, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 1, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	// Fault pin
	palSetPadMode(GPIOB, 8, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(GPIOB, 9, PAL_MODE_INPUT_PULLUP);

	// ADC Pins
	//palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	//palSetPadMode(GPIOA, 4, PAL_MODE_INPUT_ANALOG); RFU
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 5, PAL_MODE_INPUT_ANALOG);
	
	// Enable internal temperature sensor
	ADC_TempSensorVrefintCmd(ENABLE);
}

/*
 * ADC Vector
 *
 * 0:	IN12	M2_SENS1	(phase connected to TIM8_CH1, but called M2_SENS3 on schematic)
 * 1:	IN11	M2_SENS2	(phase connected to TIM8_CH2)
 * 2:	IN10	M2_SENS3	(phase connected to TIM8_CH3, but called M2_SENS1 on schematic)
 * 3:	IN0		M1_SENS1
 * 4:	IN1		M1_SENS2
 * 5:	IN2		M1_SENS3
 * 6:	IN14	M2_CURR
 * 7:	IN16	TEMP_PCB (STM32 internal temperature sensor)
 * 8:	IN15	M2_SHUNT2
 * 9:	IN6		AN_IN (HV_SENS)
 * 10:	IN3		M1_CURR
 * 11:	IN13	ADC_EXT
 
 * RFU:	IN4		RFU (M1_SHUNT2)
 */
 
void hw_setup_adc_channels(void) {
	// ADC1 regular channels 12, 0, 14, 6
	ADC_RegularChannelConfig(ADC1, ADC_Channel_12, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_6, 4, ADC_SampleTime_3Cycles);

	// ADC2 regular channels 11, 1, TempSensor, 3, 
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_TempSensor, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_3, 4, ADC_SampleTime_3Cycles);
	
	// ADC3 regular channels 10, 2, 15, 13, 
	ADC_RegularChannelConfig(ADC3, ADC_Channel_10, 1, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 2, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_15, 3, ADC_SampleTime_3Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_3Cycles);

	// Injected channels
	/** TODO: figure out if all sens for sensorless control should be injected */
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_14, 1, ADC_SampleTime_3Cycles);
}

void hw_setup_servo_outputs(void) {
	// Set up GPIO ports
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

	servos[0].gpio = GPIOB;
	servos[0].pin = 5;
	servos[0].offset = 0;
	servos[0].pos = 128;

	servos[1].gpio = GPIOD;
	servos[1].pin = 2;
	servos[1].offset = 0;
	servos[1].pos = 0;
}

#endif
