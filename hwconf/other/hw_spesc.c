/*
	Copyright 2012-2016 Benjamin Vedder	benjamin@vedder.se

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
#include "utils.h"
#include "terminal.h"
#include "commands.h"
#include "mc_interface.h"
#include "ledpwm.h"
// Variables
static volatile bool i2c_running = false;
// fan control thread
static THD_WORKING_AREA(fan_control_thread_wa, 128);
static THD_FUNCTION(fan_control_thread, arg);

// I2C configuration
static const I2CConfig i2cfg = {
	OPMODE_I2C,
	100000,
	STD_DUTY_CYCLE};

void hw_init_gpio(void)
{
	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

	// LEDs
	palSetPadMode(GPIOB, 0,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 1,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST);

	// External Buzzer (using servo pin!)
	palSetPadMode(GPIOB, 6,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST);
	EXT_BUZZER_ON();
	chThdSleepMilliseconds(200);
	EXT_BUZZER_OFF();
	chThdSleepMilliseconds(200);
	EXT_BUZZER_ON();
	chThdSleepMilliseconds(200);
	EXT_BUZZER_OFF();
	chThdSleepMilliseconds(200);
	EXT_BUZZER_ON();
	chThdSleepMilliseconds(200);
	EXT_BUZZER_OFF();

	palSetPadMode(LIGHT_BACK_GPIO , LIGHT_BACK_PIN,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST); // Rear light
	palSetPadMode(LIGHT_FORNT_GPIO, LIGHT_FORNT_PIN,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST); // front light
	palSetPadMode(FAN_GPIO, FAN_PIN,
				  PAL_MODE_OUTPUT_PUSHPULL |
					  PAL_STM32_OSPEED_HIGHEST); // fan// power on DC-DC Converter pull high to enable , pull low disable
	palSetPadMode(EXTERNAL_DCDC_GPIO, EXTERNAL_DCDC_PIN, 
				  PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST); // external dc-dc board power on control 
	LIGHT_FRONT_ON();
	LIGHT_BACK_ON();
	chThdSleepMilliseconds(200);
	LIGHT_BACK_OFF();
	LIGHT_FRONT_OFF();
	chThdSleepMilliseconds(200);
	LIGHT_FRONT_ON();
	LIGHT_BACK_ON();
	chThdSleepMilliseconds(200);
	LIGHT_BACK_OFF();
	LIGHT_FRONT_OFF();
	// FAN on
	FAN_OFF();
	// power on DC-DC Converter pull high to enable , pull low disable
	palSetPadMode(GPIOD, 2, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	palSetPadMode(GPIOA, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOB, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	// Hall sensors
	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_INPUT_PULLUP);

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);
}

void hw_setup_adc_channels(void)
{
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 5, ADC_SampleTime_15Cycles);

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_0, 5, ADC_SampleTime_15Cycles);

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_1, 5, ADC_SampleTime_15Cycles);

	// Injected channels
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_10, 3, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_11, 3, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC3, ADC_Channel_12, 3, ADC_SampleTime_15Cycles);
	// fan control thread
	chThdCreateStatic(fan_control_thread_wa, sizeof(fan_control_thread_wa), LOWPRIO, fan_control_thread, NULL);
}

void hw_start_i2c(void)
{
	i2cAcquireBus(&HW_I2C_DEV);

	if (!i2c_running)
	{
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

void hw_stop_i2c(void)
{
	i2cAcquireBus(&HW_I2C_DEV);

	if (i2c_running)
	{
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
void hw_try_restore_i2c(void)
{
	if (i2c_running)
	{
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

		for (int i = 0; i < 16; i++)
		{
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
static THD_FUNCTION(fan_control_thread, arg)
{
	(void)arg;
	chRegSetThreadName("fan_control_thread");
	float temp_t;
	for (;;)
	{
		temp_t = mc_interface_temp_fet_filtered();
		if ( temp_t > mc_interface_get_configuration()->bms.t_limit_start )
		{
			FAN_ON();
		}
		else
		{
			FAN_OFF();
		}

		chThdSleepMilliseconds(1000);
	}
}
