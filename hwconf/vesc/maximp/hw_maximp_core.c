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
#include "lispif.h"
#include "lispbm.h"

// Variables
static volatile bool i2c_running = false;
static THD_WORKING_AREA(mux_thread_wa, 256);
static THD_FUNCTION(mux_thread, arg);
static volatile bool mux_thd_running = false;

// I2C configuration
static const I2CConfig i2cfg = {
		OPMODE_I2C,
		100000,
		STD_DUTY_CYCLE
};

static lbm_value ext_reg_v(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	float adc = (float)ADC_Value[ADC_IND_12V_SENSE_V];
	// V-div 22k - 2.2k
	return lbm_enc_float(adc * (V_REG / 4095.0) * ((22.0 + 2.2) / 2.2));
}

static lbm_value ext_reg_i(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	float adc = (float)ADC_Value[ADC_IND_12V_SENSE_I];
	// 0.01 ohm, shunt amp same as rest of hw
	return lbm_enc_float((adc * (V_REG / 4095.0) - (V_REG / 2.0)) / (CURRENT_AMP_GAIN * 0.01));
}

static lbm_value ext_reg_t(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(NTC_TEMP_DCDC());
}

static lbm_value ext_reg5_v(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	float adc = (float)ADC_Value[ADC_IND_5V_SENSE_V];
	// V-div 10k - 10k
	return lbm_enc_float(adc * (V_REG / 4095.0) * ((10.0 + 10.0) / 10.0));
}

static lbm_value ext_reg5_en(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	if (lbm_dec_as_i32(args[0]) == 0) {
		REG_5V_OFF();
	} else {
		REG_5V_ON();
	}
	return ENC_SYM_TRUE;
}

static void load_extensions(bool main_found) {
	if (!main_found) {
		lbm_add_extension("hw-reg-v", ext_reg_v);
		lbm_add_extension("hw-reg-i", ext_reg_i);
		lbm_add_extension("hw-reg-t", ext_reg_t);
		lbm_add_extension("hw-reg5-v", ext_reg5_v);
		lbm_add_extension("hw-reg5-en", ext_reg5_en);
	}
}

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
	palSetPadMode(PHASE_FILTER_GPIO, PHASE_FILTER_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();

	CURRENT_FILTER_OFF();

	// AUX pins
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	AUX2_OFF();
	palSetPadMode(AUX2_GPIO, AUX2_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	REG_5V_ON();
	palSetPadMode(REG_5V_GPIO, REG_5V_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 7, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);

	// DAC as voltage reference for shunt amps
	palSetPadMode(GPIOA, 4, PAL_MODE_INPUT_ANALOG);
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_DAC, ENABLE);
	DAC->CR |= DAC_CR_EN1;
	DAC->DHR12R1 = 2047;

	lispif_add_ext_load_callback(load_extensions);
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);			// 0
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);			// 3
	ADC_RegularChannelConfig(ADC1, ADC_Channel_7, 3, ADC_SampleTime_15Cycles);			// 6
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles);			// 9
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 5, ADC_SampleTime_15Cycles);			// 12
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8, 6, ADC_SampleTime_15Cycles);			// 15

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);			// 1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 2, ADC_SampleTime_15Cycles);			// 4
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_15Cycles);			// 7
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_15Cycles);			// 10
	ADC_RegularChannelConfig(ADC2, ADC_Channel_0, 5, ADC_SampleTime_15Cycles);			// 13
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 6, ADC_SampleTime_15Cycles);			// 16

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);			// 2
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 2, ADC_SampleTime_15Cycles);			// 5
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 3, ADC_SampleTime_15Cycles);			// 8
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 4, ADC_SampleTime_15Cycles);			// 11
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 5, ADC_SampleTime_15Cycles);			// 14
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 6, ADC_SampleTime_15Cycles);			// 17

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

	if (!mux_thd_running) {
		chThdCreateStatic(mux_thread_wa, sizeof(mux_thread_wa), NORMALPRIO, mux_thread, NULL);
		mux_thd_running = true;
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

static THD_FUNCTION(mux_thread, arg) {
	(void)arg;

	chRegSetThreadName("adc_mux");

	palSetPadMode(ADC_SW_1_PORT, ADC_SW_1_PIN ,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(ADC_SW_2_PORT, ADC_SW_2_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(ADC_SW_3_PORT, ADC_SW_3_PIN ,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

#define T_SAMP_US		400

	for (;;) {
		ADCMUX_MOT_TEMP();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_TEMP_MOTOR] = ADC_Value[ADC_IND_ADC_MUX];

		ADCMUX_12V_SENSE_V();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_12V_SENSE_V] = ADC_Value[ADC_IND_ADC_MUX];

		ADCMUX_MOS_TEMP1();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_TEMP_MOS] = ADC_Value[ADC_IND_ADC_MUX];

		ADCMUX_MOS_TEMP2();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_TEMP_MOS_2] = ADC_Value[ADC_IND_ADC_MUX];

		ADCMUX_12V_SENSE_I();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_12V_SENSE_I] = ADC_Value[ADC_IND_ADC_MUX];

		ADCMUX_5V_SENSE_V();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_5V_SENSE_V] = ADC_Value[ADC_IND_ADC_MUX];

		ADCMUX_MOS_TEMP3();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_TEMP_MOS_3] = ADC_Value[ADC_IND_ADC_MUX];

		ADCMUX_TEMP_DCDC();
		chThdSleepMicroseconds(T_SAMP_US);
		ADC_Value[ADC_IND_TEMP_DCDC] = ADC_Value[ADC_IND_ADC_MUX];
	}
}

float hw100_400_get_temp(void) {
	float t1 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float t2 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS_2]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float t3 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS_3]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float res = 0.0;

	if (t1 > t2 && t1 > t3) {
		res = t1;
	} else if (t2 > t1 && t2 > t3) {
		res = t2;
	} else {
		res = t3;
	}

	return res;
}
