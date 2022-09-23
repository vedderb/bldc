/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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
#include "drv8301.h"
#include "comm_can.h"
#include "mc_interface.h"
#include "ledpwm.h"
#include "utils_math.h"
#include "main.h"

typedef enum {
	SWITCH_BOOTED = 0,
	SWITCH_TURN_ON_DELAY_ACTIVE,
	SWITCH_HELD_AFTER_TURN_ON,
	SWITCH_TURNED_ON,
	SWITCH_SHUTTING_DOWN,
} switch_states;

// Variables
static volatile bool i2c_running = false;
static THD_WORKING_AREA(smart_switch_thread_wa, 128);
static THD_WORKING_AREA(mux_thread_wa, 256);
static THD_FUNCTION(mux_thread, arg);
static volatile switch_states switch_state = SWITCH_BOOTED;

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

	// LEDs
	palSetPadMode(GPIOD, 3,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOD, 7,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

	//Temp switches
	palSetPadMode(GPIOE, 7,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOD, 8,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOD, 9,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 12,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

	ENABLE_MOS_TEMP1();

	// GPIOC (ENABLE_GATE)
	palSetPadMode(GPIOE, 2,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOD, 4,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOB, 8,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	DISABLE_GATE();
	// GPIOB (DCCAL)

    				// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	palSetPadMode(GPIOE, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOE, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOE, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOE, 11, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOE, 12, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOE, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOC, 6, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOC, 7, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOC, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 7, PAL_MODE_ALTERNATE(GPIO_AF_TIM8) |
			PAL_STM32_OSPEED_HIGHEST |
			PAL_STM32_PUDR_FLOATING);

	// Hall sensors
	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO4, HW_HALL_ENC_PIN4, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO5, HW_HALL_ENC_PIN5, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO6, HW_HALL_ENC_PIN6, PAL_MODE_INPUT_PULLUP);

	// Fault pin
	palSetPadMode(GPIOE, 3, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(GPIOE, 1, PAL_MODE_INPUT_PULLUP);

	// ADC Pins
	palSetPadMode(GPIOA, 4, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);

	mc_interface_select_motor_thread(1);
	drv8301_init();
	mc_interface_select_motor_thread(2);
	drv8301_init();
	mc_interface_select_motor_thread(1);
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 1, ADC_SampleTime_15Cycles); //0
	ADC_RegularChannelConfig(ADC1, ADC_Channel_9, 2, ADC_SampleTime_15Cycles);//3
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 3, ADC_SampleTime_15Cycles);//6
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5 , 4, ADC_SampleTime_15Cycles);//9
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 5, ADC_SampleTime_15Cycles);//12

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 1, ADC_SampleTime_15Cycles);//1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_8, 2, ADC_SampleTime_15Cycles);//4
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 3, ADC_SampleTime_15Cycles);//7
	ADC_RegularChannelConfig(ADC2, ADC_Channel_4, 4, ADC_SampleTime_15Cycles);//10
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 5, ADC_SampleTime_15Cycles);//13

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 1, ADC_SampleTime_15Cycles);//2
	ADC_RegularChannelConfig(ADC3, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);//5
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 3, ADC_SampleTime_15Cycles);//8
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);//11
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 5, ADC_SampleTime_15Cycles);//14

	// Injected channels
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_9, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC1, ADC_Channel_8, 2, ADC_SampleTime_15Cycles);


	ADC_InjectedChannelConfig(ADC2, ADC_Channel_5, 1, ADC_SampleTime_15Cycles);
	ADC_InjectedChannelConfig(ADC2, ADC_Channel_4, 2, ADC_SampleTime_15Cycles);

	//  ADC_InjectedChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_15Cycles);
	// ADC_InjectedChannelConfig(ADC3, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);
	//ADC_InjectedChannelConfig(ADC3, ADC_Channel_1, 3, ADC_SampleTime_15Cycles);

	chThdCreateStatic(mux_thread_wa, sizeof(mux_thread_wa), NORMALPRIO, mux_thread, NULL);
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

static THD_FUNCTION(mux_thread, arg) {
	chRegSetThreadName("adc_mux");
	(void)arg;

#define TEMP_FILTER_LEN				9
	uint16_t mot1_temp_samples[TEMP_FILTER_LEN] = {0};
	uint16_t mot2_temp_samples[TEMP_FILTER_LEN] = {0};
	unsigned int mot1_temp_samp_ptr = 0;
	unsigned int mot2_temp_samp_ptr = 0;

	for (;;) {
		ENABLE_MOS_TEMP1();
		chThdSleepMicroseconds(400);
		ADC_Value[ADC_IND_TEMP_MOS] = ADC_Value[ADC_IND_ADC_MUX];

		ENABLE_MOS_TEMP2();
		chThdSleepMicroseconds(400);
		ADC_Value[ADC_IND_TEMP_MOS_M2] = ADC_Value[ADC_IND_ADC_MUX];

		ENABLE_MOT_TEMP1();
		chThdSleepMicroseconds(400);
		ADC_Value[ADC_IND_TEMP_MOTOR] = utils_median_filter_uint16_run(
				mot1_temp_samples, &mot1_temp_samp_ptr, TEMP_FILTER_LEN, ADC_Value[ADC_IND_ADC_MUX]);

		ENABLE_MOT_TEMP2();
		chThdSleepMicroseconds(400);
		ADC_Value[ADC_IND_TEMP_MOTOR_2] = utils_median_filter_uint16_run(
				mot2_temp_samples, &mot2_temp_samp_ptr, TEMP_FILTER_LEN, ADC_Value[ADC_IND_ADC_MUX]);
	}
}

void smart_switch_keep_on(void) {
	palSetPad(SWITCH_OUT_GPIO, SWITCH_OUT_PIN);

//#ifdef HW_HAS_RGB_SWITCH
//	LED_SWITCH_B_ON();
//	ledpwm_set_intensity(SWITCH_LED_B, 1.0);
//#else
//	ledpwm_set_intensity(SWITCH_LED, 1.0);
//	ledpwm_set_switch_intensity(0.6);
//#endif
}

void smart_switch_shut_down(void) {
	switch_state = SWITCH_SHUTTING_DOWN;
	palClearPad(SWITCH_OUT_GPIO, SWITCH_OUT_PIN);
	return;
}

bool smart_switch_is_pressed(void) {
	if(palReadPad(SWITCH_IN_GPIO, SWITCH_IN_PIN) == 1)
		return true;
	else
		return false;
}

static THD_FUNCTION(smart_switch_thread, arg) {
	(void)arg;
	chRegSetThreadName("smart_switch");

	unsigned int millis_switch_pressed = 0;

	for (;;) {
		switch (switch_state) {
		case SWITCH_BOOTED:
			ledpwm_set_intensity(LED_HW1, 1.0);
			switch_state = SWITCH_TURN_ON_DELAY_ACTIVE;
			break;
		case SWITCH_TURN_ON_DELAY_ACTIVE:
			chThdSleepMilliseconds(500);
			switch_state = SWITCH_HELD_AFTER_TURN_ON;
			smart_switch_keep_on();
			ledpwm_set_intensity(LED_HW1, 1.0);
			//Wait for other systems to boot up before proceeding
			while (!main_init_done()) {
				chThdSleepMilliseconds(200);
			}
			break;
		case SWITCH_HELD_AFTER_TURN_ON:
			if(smart_switch_is_pressed()){
				switch_state = SWITCH_HELD_AFTER_TURN_ON;
			} else {
				switch_state = SWITCH_TURNED_ON;
			}
			break;
		case SWITCH_TURNED_ON:
			if (smart_switch_is_pressed()) {
				millis_switch_pressed++;
				ledpwm_set_intensity(LED_HW1, 0.6);
			} else {
				millis_switch_pressed = 0;
				ledpwm_set_intensity(LED_HW1, 1.0);
			}

			if (millis_switch_pressed > SMART_SWITCH_MSECS_PRESSED_OFF) {
				switch_state = SWITCH_SHUTTING_DOWN;
			}
			break;
		case SWITCH_SHUTTING_DOWN:
			ledpwm_set_intensity(LED_HW1, 0.0);
			comm_can_shutdown(255);
			smart_switch_shut_down();
			chThdSleepMilliseconds(10000);
			smart_switch_keep_on();
			switch_state = SWITCH_TURN_ON_DELAY_ACTIVE;
			break;
		default:
			break;
		}
		chThdSleepMilliseconds(1);
	}
}

void smart_switch_thread_start(void) {
	chThdCreateStatic(smart_switch_thread_wa, sizeof(smart_switch_thread_wa),
			NORMALPRIO, smart_switch_thread, NULL);
}

void smart_switch_pin_init(void) {
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOE, ENABLE);

	palSetPadMode(SWITCH_IN_GPIO, SWITCH_IN_PIN, PAL_MODE_INPUT);
	palSetPadMode(SWITCH_OUT_GPIO,SWITCH_OUT_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SWITCH_LED_1_GPIO,SWITCH_LED_1_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);	LED_PWM1_ON();
	return;
}
