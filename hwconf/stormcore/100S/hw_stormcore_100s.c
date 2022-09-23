/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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
#include "ledpwm.h"
#include "drv8323s.h"

#if defined (HW_VER_IS_100S_V2)
static THD_WORKING_AREA(switch_color_thread_wa, 128);
static THD_FUNCTION(switch_color_thread, arg);
static volatile float switch_bright = 1.0;
#endif

// I2C configuration
static const I2CConfig i2cfg = {
								OPMODE_I2C,
								100000,
								STD_DUTY_CYCLE
};
static volatile bool i2c_running = false;

void hw_init_gpio(void) {
	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

#if defined (HW_VER_IS_100S_V2)
	palSetPadMode(PHASE_FILTER_GPIO, PHASE_FILTER_PIN,
				  PAL_MODE_OUTPUT_PUSHPULL |
				  PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();
	palSetPadMode(SWITCH_LED_1_GPIO,SWITCH_LED_1_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SWITCH_LED_2_GPIO,SWITCH_LED_2_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	chThdCreateStatic(switch_color_thread_wa, sizeof(switch_color_thread_wa), LOWPRIO, switch_color_thread, NULL);
#endif

	// LEDs
	palSetPadMode(GPIOB, 0,
				  PAL_MODE_OUTPUT_PUSHPULL |
				  PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 1,
				  PAL_MODE_OUTPUT_PUSHPULL |
				  PAL_STM32_OSPEED_HIGHEST);

	// ENABLE_GATE
	palSetPadMode(GPIOB, 5,
				  PAL_MODE_OUTPUT_PUSHPULL |
				  PAL_STM32_OSPEED_HIGHEST);
	ENABLE_GATE();

	// Disable BMI160
	palSetPadMode(GPIOA, 15,
				  PAL_MODE_OUTPUT_PUSHPULL |
				  PAL_STM32_OSPEED_HIGHEST);
	palSetPad(GPIOA, 15);

	// Disable DCCAL
	palSetPadMode(GPIOD, 2,
				  PAL_MODE_OUTPUT_PUSHPULL |
				  PAL_STM32_OSPEED_HIGHEST);
	palClearPad(GPIOD, 2);

	ENABLE_GATE();

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

	// Fault pin
	palSetPadMode(GPIOB, 7, PAL_MODE_INPUT_PULLUP);

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
	palSetPadMode(GPIOC, 5, PAL_MODE_INPUT_ANALOG);

	drv8323s_init();

}

void hw_setup_adc_channels(void) {
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

#if defined (HW_VER_IS_100S_V2)
static THD_FUNCTION(switch_color_thread, arg) {
	(void)arg;
	chRegSetThreadName("switch_color");
	float switch_red = 0.0;
	float switch_green = 0.0;
	float switch_blue = 0.0;

	for(int i = 0; i < 400; i++) {
		float angle = i*3.14/400.0;
		float s,c;
		utils_fast_sincos_better(angle, &s, &c);
		switch_blue = 0.75* c*c;
		ledpwm_set_intensity(LED_HW1,switch_bright*switch_blue);
		utils_fast_sincos_better(angle + 3.14/3.0, &s, &c);
		switch_green = 0.75* c*c;
		ledpwm_set_intensity(LED_HW2,switch_bright*switch_green);
		utils_fast_sincos_better(angle + 6.28/3.0, &s, &c);
		switch_red = 0.75* c*c;
		ledpwm_set_intensity(LED_HW3,switch_bright*switch_red);
		chThdSleepMilliseconds(4);
	}
	float switch_red_old = switch_red_old;
	float switch_green_old = switch_green;
	float switch_blue_old = switch_blue;
	float wh_left;
	float left = mc_interface_get_battery_level(&wh_left);
	if(left < 0.5){
		float intense = utils_map(left,0.0, 0.5, 0.0, 1.0);
		utils_truncate_number(&intense,0,1);
		switch_blue = intense;
		switch_red  = 1.0-intense;
	}else{
		float intense = utils_map(left , 0.5, 1.0, 0.0, 1.0);
		utils_truncate_number(&intense,0,1);
		switch_green = intense;
		switch_blue  = 1.0-intense;
	}
	for(int i = 0; i < 100; i++) {
		float red_now = utils_map((float) i,0.0, 100.0, switch_red_old, switch_red);
		float blue_now = utils_map((float) i,0.0, 100.0, switch_blue_old, switch_blue);
		float green_now = utils_map((float) i,0.0, 100.0, switch_green_old, switch_green);
		ledpwm_set_intensity(LED_HW1, switch_bright*blue_now);
		ledpwm_set_intensity(LED_HW2, switch_bright*green_now);
		ledpwm_set_intensity(LED_HW3, switch_bright*red_now);
		chThdSleepMilliseconds(2);
	}

	for (;;) {
		mc_fault_code fault = mc_interface_get_fault();
		mc_interface_select_motor_thread(2);
		mc_fault_code fault2 = mc_interface_get_fault();
		mc_interface_select_motor_thread(1);
		if (fault != FAULT_CODE_NONE || fault2 != FAULT_CODE_NONE) {
			ledpwm_set_intensity(LED_HW2, 0);
			ledpwm_set_intensity(LED_HW1, 0);
			for (int i = 0;i < (int)fault;i++) {

				ledpwm_set_intensity(LED_HW3, 1.0);
				chThdSleepMilliseconds(250);
				ledpwm_set_intensity(LED_HW3, 0.0);
				chThdSleepMilliseconds(250);
			}

			chThdSleepMilliseconds(500);

			for (int i = 0;i < (int)fault2;i++) {
				ledpwm_set_intensity(LED_HW3, 1.0);
				chThdSleepMilliseconds(250);
				ledpwm_set_intensity(LED_HW3, 0.0);
				chThdSleepMilliseconds(250);
			}

			chThdSleepMilliseconds(500);
		} else {
			left = mc_interface_get_battery_level(&wh_left);
			if(HW_SAMPLE_SHUTDOWN()){
				switch_bright = 0.5;
			}else{
				switch_bright = 1.0;
			}

			if(left < 0.5){
				float intense = utils_map(left,0.0, 0.5, 0.0, 1.0);
				utils_truncate_number(&intense,0,1);
				switch_blue = intense;
				switch_red  = 1.0-intense;
				switch_green = 0;
			}else{
				float intense = utils_map(left , 0.5, 1.0, 0.0, 1.0);
				utils_truncate_number(&intense,0,1);
				switch_green = intense;
				switch_blue  = 1.0-intense;
				switch_red = 0;
			}
			ledpwm_set_intensity(LED_HW1, switch_bright*switch_blue);
			ledpwm_set_intensity(LED_HW2, switch_bright*switch_green);
			ledpwm_set_intensity(LED_HW3, switch_bright*switch_red);
		}

		chThdSleepMilliseconds(20);
	}
}
#endif

