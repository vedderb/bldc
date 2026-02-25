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
#include "ledpwm.h"
#include "utils_math.h"
#include "main.h"
#include "app.h"
#include "comm_can.h"

typedef enum {
	SWITCH_BOOTED = 0,
	SWITCH_TURN_ON_DELAY_ACTIVE,
	SWITCH_HELD_AFTER_TURN_ON,
	SWITCH_TURNED_ON,
	SWITCH_SHUTTING_DOWN,
} switch_states;

// Variables
static THD_WORKING_AREA(smart_switch_thread_wa, 256);
static THD_WORKING_AREA(switch_color_thread_wa, 256);
static THD_FUNCTION(switch_color_thread, arg);
static volatile switch_states switch_state = SWITCH_BOOTED;

static volatile float switch_bright = 0.75;
static bool switch_color_thd_running = false;
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

	// LEDs
	palSetPadMode(LED_GREEN_GPIO, LED_GREEN_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(LED_RED_GPIO, LED_RED_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

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
	palSetPadMode(GPIOB, 12, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOC, 14, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(GPIOC, 15, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();

	palSetPadMode(CURRENT_FILTER_GPIO, CURRENT_FILTER_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	CURRENT_FILTER_OFF();

	// AUX pins
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 4, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 7, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);			// 0 Curr 1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);			// 3 Volt 1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_7, 3, ADC_SampleTime_15Cycles);			// 6 EXT
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles);			// 9 EXT4
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 5, ADC_SampleTime_15Cycles);			// 12 EXT4
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 6, ADC_SampleTime_15Cycles);			// 15 TEMP_MOS
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8, 7, ADC_SampleTime_15Cycles);			// 18 Smart Switch

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);			// 1 Curr 2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 2, ADC_SampleTime_15Cycles);			// 4 Volt 2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_15Cycles);			// 7 EXT2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 4, ADC_SampleTime_15Cycles);			// 10 EXT5
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 5, ADC_SampleTime_15Cycles);			// 13 EXT5
	ADC_RegularChannelConfig(ADC2, ADC_Channel_4, 6, ADC_SampleTime_15Cycles);			// 16 Temp Motor
	ADC_RegularChannelConfig(ADC2, ADC_Channel_4, 7, ADC_SampleTime_15Cycles);			// 19 Temp Motor

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);			// 2 Curr 3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 2, ADC_SampleTime_15Cycles);			// 5 Volt 3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 3, ADC_SampleTime_15Cycles);			// 8 Volt In
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);			// 11 Volt In
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 5, ADC_SampleTime_15Cycles);			// 14 Volt In
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 6, ADC_SampleTime_15Cycles);			// 17 Volt In
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 7, ADC_SampleTime_15Cycles);			// 20 Volt In

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

	if (!switch_color_thd_running) {
		chThdCreateStatic(switch_color_thread_wa, sizeof(switch_color_thread_wa), LOWPRIO, switch_color_thread, NULL);
		switch_color_thd_running = true;
	}
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
	mc_interface_select_motor_thread(2);
	mc_interface_set_current(0);
	mc_interface_lock();
	mc_interface_select_motor_thread(1);
	mc_interface_set_current(0);
	mc_interface_lock();
	switch_state = SWITCH_SHUTTING_DOWN;
	palClearPad(SWITCH_OUT_GPIO, SWITCH_OUT_PIN);
	return;
}

bool smart_switch_is_pressed(void) {
	if (ADC_VOLTS(ADC_IND_SW_DET) > 0.9 &&
			(mc_interface_temp_fet_filtered() < 68.0) /* why?? */) {
		return true;
	} else {
		return false;
	}
}

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

	if (left < 0.5) {
		float intense = utils_map(left,0.0, 0.5, 0.0, 1.0);
		utils_truncate_number(&intense,0,1);
		switch_blue = intense;
		switch_red  = 1.0-intense;
	} else {
		float intense = utils_map(left , 0.5, 1.0, 0.0, 1.0);
		utils_truncate_number(&intense,0,1);
		switch_green = intense;
		switch_blue  = 1.0-intense;
	}

	for (int i = 0; i < 100; i++) {
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

		// Config check
		mc_configuration *mcconf = (mc_configuration*)mc_interface_get_configuration();

		if (mcconf->motor_type == MOTOR_TYPE_FOC &&
				mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
			// In hall sensor mode we use the ADC pins on the comm-port as additional
			// pull-ups as the voltage dividers take the voltage down otherwise.
			palSetPadMode(HW_ADC_EXT4_GPIO, HW_ADC_EXT4_PIN, PAL_MODE_OUTPUT_PUSHPULL);
			palSetPadMode(HW_ADC_EXT5_GPIO, HW_ADC_EXT5_PIN, PAL_MODE_OUTPUT_PUSHPULL);
			palSetPad(HW_ADC_EXT4_GPIO, HW_ADC_EXT4_PIN);
			palSetPad(HW_ADC_EXT5_GPIO, HW_ADC_EXT5_PIN);
		} else if (mcconf->motor_type == MOTOR_TYPE_FOC &&
				mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
			// Ensure that the sin/cos pins are in ADC mode
			if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_SINCOS) {
				palSetPadMode(HW_ADC_EXT4_GPIO, HW_ADC_EXT4_PIN, PAL_MODE_INPUT_ANALOG);
				palSetPadMode(HW_ADC_EXT5_GPIO, HW_ADC_EXT5_PIN, PAL_MODE_INPUT_ANALOG);
			}
		}

		chThdSleepMilliseconds(20);
	}
}

static THD_FUNCTION(smart_switch_thread, arg) {
	(void)arg;
	chRegSetThreadName("smart_switch");
	unsigned int millis_switch_pressed = 0;

	for (;;) {
		const app_configuration *conf = app_get_configuration();

		switch (switch_state) {
		case SWITCH_BOOTED:
			switch_state = SWITCH_TURN_ON_DELAY_ACTIVE;
			break;

		case SWITCH_TURN_ON_DELAY_ACTIVE:
			switch_state = SWITCH_HELD_AFTER_TURN_ON;
			mc_interface_select_motor_thread(2);
			mc_interface_set_current(0);
			mc_interface_lock();
			mc_interface_select_motor_thread(1);
			mc_interface_set_current(0);
			mc_interface_lock();

			mc_interface_select_motor_thread(2);
			mc_interface_unlock();
			mc_interface_select_motor_thread(1);
			mc_interface_unlock();

			// Wait for other systems to boot up before proceeding
			while (!main_init_done()) {
				chThdSleepMilliseconds(200);
			}
			break;

		case SWITCH_HELD_AFTER_TURN_ON:
			if (smart_switch_is_pressed()) {
				switch_state = SWITCH_HELD_AFTER_TURN_ON;
			} else {
				switch_state = SWITCH_TURNED_ON;
			}
			break;

		case SWITCH_TURNED_ON:
			if (conf->shutdown_mode == SHUTDOWN_MODE_ALWAYS_OFF) {
				if (!smart_switch_is_pressed()) {
					switch_state = SWITCH_SHUTTING_DOWN;
				}
			} else {
				if (smart_switch_is_pressed()) {
					millis_switch_pressed++;
					switch_bright = 0.5;
				} else {
					millis_switch_pressed = 0;
					switch_bright = 1.0;
				}

				if (millis_switch_pressed > SMART_SWITCH_MSECS_PRESSED_OFF) {
					switch_state = SWITCH_SHUTTING_DOWN;
				}
			}
			break;

		case SWITCH_SHUTTING_DOWN:
			switch_bright = 0;
			while (smart_switch_is_pressed()) {
				chThdSleepMilliseconds(10);
			}
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

	palSetPadMode(SWITCH_OUT_GPIO,SWITCH_OUT_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SWITCH_LED_1_GPIO,SWITCH_LED_1_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SWITCH_LED_2_GPIO,SWITCH_LED_2_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(SWITCH_LED_3_GPIO,SWITCH_LED_3_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPad(SWITCH_OUT_GPIO, SWITCH_OUT_PIN);
	LED_SWITCH_B_ON();
	LED_SWITCH_R_OFF();
	LED_SWITCH_G_OFF();
	return;
}
