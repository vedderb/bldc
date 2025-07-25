/*
	Copyright 2023 Benjamin Vedder	benjamin@vedder.se

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
#include "utils.h"

// Variables
static volatile bool i2c_running = false;
static THD_WORKING_AREA(sense_thread_wa, 256);
static THD_FUNCTION(sense_thread, arg);
static volatile bool sense_thd_running = false;

static volatile float pas_time = 0.0;
static volatile systime_t pas_update = 0;
static volatile float speed_time = 0.0;
static volatile systime_t speed_update = 0;
static volatile uint32_t pas_pulse_cnt = 0.0;

// I2C configuration
static const I2CConfig i2cfg = {
		OPMODE_I2C,
		100000,
		STD_DUTY_CYCLE
};

static lbm_value ext_read_brake(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(ADC_VOLTS(ADC_IND_BRAKE) < 0.5 ? 1 : 0);
}

static lbm_value ext_read_gear(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(ADC_VOLTS(ADC_IND_GEAR) < 0.5 ? 0 : 1);
}

static lbm_value ext_read_pas1(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(palReadPad(HW_PAS1_PORT, HW_PAS1_PIN) ? 1 : 0);
}

static lbm_value ext_read_pas2(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(palReadPad(HW_PAS2_PORT, HW_PAS2_PIN) ? 1 : 0);
}

static lbm_value ext_read_speed(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(palReadPad(HW_SPEED_PORT, HW_SPEED_PIN) ? 1 : 0);
}

static lbm_value ext_pas_last_time(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(pas_time);
}

static lbm_value ext_pas_age(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(UTILS_AGE_S(pas_update));
}

static lbm_value ext_pas_pulse_cnt(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(pas_pulse_cnt);
}

static lbm_value ext_speed_last_time(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(speed_time);
}

static lbm_value ext_speed_age(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(UTILS_AGE_S(speed_update));
}

static lbm_value ext_pwr_hold(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	if (lbm_dec_as_i32(args[0])) {
		PWR_HOLD_ON();
	} else {
		PWR_HOLD_OFF();
	}
	return ENC_SYM_TRUE;
}

static lbm_value ext_pwr_read(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_i(palReadPad(HW_PWR_GPIO, HW_PWR_PIN) ? 1 : 0);
}

static void load_extensions(bool main_found) {
	if (!main_found) {
		lbm_add_extension("read-brake", ext_read_brake);
		lbm_add_extension("read-gear", ext_read_gear);
		lbm_add_extension("read-pas1", ext_read_pas1);
		lbm_add_extension("read-pas2", ext_read_pas2);
		lbm_add_extension("read-speed", ext_read_speed);
		lbm_add_extension("pas-last-time", ext_pas_last_time);
		lbm_add_extension("pas-age", ext_pas_age);
		lbm_add_extension("pas-pulse-cnt", ext_pas_pulse_cnt);
		lbm_add_extension("speed-last-time", ext_speed_last_time);
		lbm_add_extension("speed-age", ext_speed_age);
		lbm_add_extension("pwr-hold", ext_pwr_hold);
		lbm_add_extension("pwr-read", ext_pwr_read);
	}
}

void hw_init_gpio(void) {
	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

	// ENABLE_GATE
	palSetPadMode(GPIOC, 5, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	ENABLE_GATE();

	// Fault
	palSetPadMode(GPIOC, 7, PAL_MODE_INPUT);

	// Hold power on for now
	palSetPadMode(HW_PWR_GPIO, HW_PWR_PIN, PAL_MODE_INPUT);
	palSetPadMode(HW_PWR_HOLD_GPIO, HW_PWR_HOLD_PIN,
				PAL_MODE_OUTPUT_PUSHPULL |PAL_STM32_OSPEED_HIGHEST);
	PWR_HOLD_ON();

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

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);

	// AUX pins LIGHT1 LIGHT2
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	AUX2_OFF();
	palSetPadMode(AUX2_GPIO, AUX2_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

	palSetPadMode(HW_PAS1_PORT, HW_PAS1_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_PAS2_PORT, HW_PAS2_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_SPEED_PORT, HW_SPEED_PIN, PAL_MODE_INPUT_PULLUP);

	drv8301_init();

	lispif_add_ext_load_callback(load_extensions);
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles); //0 Curr 2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);  //3 VC
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 3, ADC_SampleTime_15Cycles);  //6 TRQ
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles); //9 Temp Motor
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 5, ADC_SampleTime_15Cycles); //12
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8, 6, ADC_SampleTime_15Cycles);  //15 Brake

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles); //1 Curr 1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 2, ADC_SampleTime_15Cycles);  //4 VB
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_15Cycles);  //7 ADC1 THR
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 4, ADC_SampleTime_15Cycles); //10 ADC1
	ADC_RegularChannelConfig(ADC2, ADC_Channel_0, 5, ADC_SampleTime_15Cycles);  //13
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 6, ADC_SampleTime_15Cycles);  //16 Gear Sense

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles); //2 Curr 3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 2, ADC_SampleTime_15Cycles);  //5 VA
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 3, ADC_SampleTime_15Cycles);  //8 Temp FET
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles); //11 V_IN
	ADC_RegularChannelConfig(ADC3, ADC_Channel_1, 5, ADC_SampleTime_15Cycles);  //14
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 6, ADC_SampleTime_15Cycles);  //17

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

	if (!sense_thd_running) {
		chThdCreateStatic(sense_thread_wa, sizeof(sense_thread_wa), NORMALPRIO, sense_thread, NULL);
		sense_thd_running = true;
	}
}

static THD_FUNCTION(sense_thread, arg) {
	(void)arg;

	chRegSetThreadName("pas-wheel");

	int pas_last = palReadPad(HW_PAS1_PORT, HW_PAS1_PIN);
	int speed_last = palReadPad(HW_SPEED_PORT, HW_SPEED_PIN);

	for (;;) {
		int pas = palReadPad(HW_PAS1_PORT, HW_PAS1_PIN);
		int speed = palReadPad(HW_SPEED_PORT, HW_SPEED_PIN);

		if (pas == 1 && pas != pas_last) {
			float time = UTILS_AGE_S(pas_update);
			if (time > 0.01) { // Max 120 RPM (30 PPR)
				pas_pulse_cnt += 1;
				pas_time = time;
				pas_update = chVTGetSystemTimeX();
			}
		}

		if (speed == 1 && speed != speed_last) {
			float time = UTILS_AGE_S(speed_update);
			if (time > 0.05) { // Max 900 RPM
				speed_time = time;
				speed_update = chVTGetSystemTimeX();
			}
		}

		pas_last = pas;
		speed_last = speed;
		chThdSleep(1);
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

