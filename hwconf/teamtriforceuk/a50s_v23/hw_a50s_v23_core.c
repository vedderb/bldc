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

// Variables
static volatile bool i2c_running = false;

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
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOH, ENABLE);

	// LEDs
	palSetPadMode(GPIOB, 0, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 1, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

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

	// Hall sensors	
	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_INPUT_PULLUP);
	
	// Phase filters
	palSetPadMode(GPIOC, 2, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOC, 14, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOC, 15, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);
	
	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);	
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);          // 0 -  ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);	         // 3 -  ADC_IND_SENS3	
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 3, ADC_SampleTime_15Cycles);          // 6 -  ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 4, ADC_SampleTime_15Cycles);          // 9 -  ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 5, ADC_SampleTime_15Cycles);          // 12 -  ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 6, ADC_SampleTime_15Cycles);          // 15 -  ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 7, ADC_SampleTime_15Cycles);          // 18 -  ADC_IND_CURR2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 8, ADC_SampleTime_15Cycles);           // 21 -  ADC_IND_EXT	
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 9, ADC_SampleTime_56Cycles);     // 24 - ADC_IND_VREFINT     
	
	// ADC2 regular channels                                                             
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);          // 1 -  ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 2, ADC_SampleTime_15Cycles);	         // 4 -  ADC_IND_SENS2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 3, ADC_SampleTime_15Cycles);          // 7 -  ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 4, ADC_SampleTime_15Cycles);          // 10 -  ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 5, ADC_SampleTime_15Cycles);          // 13 -  ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 6, ADC_SampleTime_15Cycles);          // 16 -  ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 7, ADC_SampleTime_15Cycles);          // 19 -  ADC_IND_CURR1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 8, ADC_SampleTime_15Cycles);           // 22 -  ADC_IND_EXT2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_14, 9, ADC_SampleTime_56Cycles);     	 // 25 -  ADC_IND_TEMP_MOTOR
		
	// ADC3 regular channels	
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 1, ADC_SampleTime_15Cycles);          // 2 -  ADC_IND_VIN_SENS
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 2, ADC_SampleTime_15Cycles);	         // 5 -  ADC_IND_SENS1	
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 3, ADC_SampleTime_15Cycles);          // 8 -  UNUSED
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);          // 11 -  UNUSED
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 5, ADC_SampleTime_15Cycles);          // 14 -  UNUSED
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 6, ADC_SampleTime_15Cycles);          // 17 -  UNUSED
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 7, ADC_SampleTime_15Cycles);          // 20 -  UNUSED
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 8, ADC_SampleTime_15Cycles);           // 23 -  ADC_IND_TEMP_MOS
	ADC_RegularChannelConfig(ADC3, ADC_Channel_15, 9, ADC_SampleTime_56Cycles);     	 // 26 - UNUSED

	// Injected channels 	
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);         // ADC_IND_CURR2
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);         // ADC_IND_CURR1	
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);         // ADC_IND_CURR1
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);         // ADC_IND_CURR2	
	
}


void hw_start_i2c(void) {
	i2cAcquireBus(&HW_I2C_DEV);

	if (!i2c_running) {
		palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN,
				PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1 |
				PAL_STM32_PUDR_PULLUP);
		palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1 |
				PAL_STM32_PUDR_PULLUP);

		i2cStart(&HW_I2C_DEV, &i2cfg);
		i2c_running = true;
	}

	i2cReleaseBus(&HW_I2C_DEV);
}

void hw_stop_i2c(void) {
	i2cAcquireBus(&HW_I2C_DEV);

	if (i2c_running) {
		palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN, PAL_MODE_INPUT);
		palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN, PAL_MODE_INPUT);

		i2cStop(&HW_I2C_DEV);
		i2c_running = false;

	}

	i2cReleaseBus(&HW_I2C_DEV);
}

/**
 * Try to restore the i2c bus
 */
void hw_try_restore_i2c(void) {
	if (i2c_running) {
		i2cAcquireBus(&HW_I2C_DEV);

		palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN,
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1 |
				PAL_STM32_PUDR_PULLUP);

		palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1 |
				PAL_STM32_PUDR_PULLUP);

		palSetPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
		palSetPad(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN);

		chThdSleep(1);

		for(int i = 0;i < 16;i++) {
			palClearPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
			chThdSleep(1);
			palSetPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
			chThdSleep(1);
		}

		// Generate start then stop condition
		palClearPad(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN);
		chThdSleep(1);
		palClearPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
		chThdSleep(1);
		palSetPad(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN);
		chThdSleep(1);
		palSetPad(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN);

		palSetPadMode(HW_I2C_SCL_PORT, HW_I2C_SCL_PIN,
				PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1 |
				PAL_STM32_PUDR_PULLUP);

		palSetPadMode(HW_I2C_SDA_PORT, HW_I2C_SDA_PIN,
				PAL_MODE_ALTERNATE(HW_I2C_GPIO_AF) |
				PAL_STM32_OTYPE_OPENDRAIN |
				PAL_STM32_OSPEED_MID1 |
				PAL_STM32_PUDR_PULLUP);

		HW_I2C_DEV.state = I2C_STOP;
		i2cStart(&HW_I2C_DEV, &i2cfg);

		i2cReleaseBus(&HW_I2C_DEV);
	}
}
