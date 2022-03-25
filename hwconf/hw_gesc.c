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
#include "commands.h"
#include "terminal.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "gpdrive.h"
#include "app.h"
#include "mempools.h"
#include "timeout.h"
#include "stdio.h"

#ifdef HW_HAS_LORA
#include "lora/lora.h"
#endif

THD_FUNCTION(mag_thread, arg);
static THD_WORKING_AREA(mag_thread_wa, 512);
static bool mag_thread_running = false;

// Variables
static volatile bool i2c_running = false;
static volatile int angle = 0;

//private functions
static void terminal_cmd_doublepulse(int argc, const char** argv);


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
	palSetPadMode(PHASE_FILTER_GPIO, PHASE_FILTER_PIN,
		PAL_MODE_OUTPUT_PUSHPULL |
		PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();

	// Current filter
	palSetPadMode(GPIOD, 2,
		PAL_MODE_OUTPUT_PUSHPULL |
		PAL_STM32_OSPEED_HIGHEST);

	CURRENT_FILTER_OFF();

	// AUX pin
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN,
		PAL_MODE_OUTPUT_PUSHPULL |
		PAL_STM32_OSPEED_HIGHEST);

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

#ifdef HW_HAS_LORA
	lora_init();
#endif
#ifndef HW_HAS_DUAL_MOTORS
	//register terminal callbacks
	//double pulse not possible with dual motor setup
	terminal_register_command_callback(
		"double_pulse",
		"Start a double pulse test",
		0,
		terminal_cmd_doublepulse);
#endif
}

void hw_setup_adc_channels(void) {
	/*
	 * ADC Vector
	 *
	 * 0  (1):  IN0     SENS1
	 * 1  (2):  IN1     SENS2
	 * 2  (3):  IN2     SENS3
	 * 3  (1):  IN10    CURR1
	 * 4  (2):  IN11    CURR2
	 * 5  (3):  IN12    CURR3
	 * 6  (1):  IN5     ADC_EXT1
	 * 7  (2):  IN6     ADC_EXT2
	 * 8  (3):  IN3     TEMP_MOS
	 * 9  (1):  IN14    TEMP_MOTOR
	 * 10 (2):  IN15
	 * 11 (3):  IN13    AN_IN
	 * 12 (1):  Vrefint
	 */


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
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 5, ADC_SampleTime_15Cycles);

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_5, 5, ADC_SampleTime_15Cycles);

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

	// Setup i2c mag sensor here
	if (!mag_thread_running) {
		chThdCreateStatic(mag_thread_wa, sizeof(mag_thread_wa), NORMALPRIO, mag_thread, NULL);
		mag_thread_running = true;
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

		for (int i = 0;i < 16;i++) {
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


THD_FUNCTION(mag_thread, arg) {
	(void)arg;

	chRegSetThreadName("I2C MagAngle");

	uint8_t rxbuf[10];
	uint8_t txbuf[10];
	msg_t status = MSG_OK;
	systime_t tmo = MS2ST(5);
	i2caddr_t temp_addr = 0x36;

	hw_start_i2c();
	chThdSleepMilliseconds(10);

	for (;;) {
		if (i2c_running) {
			txbuf[0] = 0x0c;
			txbuf[1] = 0x0d;
			i2cAcquireBus(&HW_I2C_DEV);
			status = i2cMasterTransmitTimeout(&HW_I2C_DEV, temp_addr, txbuf, 2, rxbuf, 2, tmo);
			i2cReleaseBus(&HW_I2C_DEV);

			if (status == MSG_OK) {
				angle = rxbuf[0] | rxbuf[1] << 8;
			} else {
				hw_try_restore_i2c();
			}
		}
		chThdSleepMilliseconds(100);
	}
}

int gsvesc_get_angle() {
	return angle;
}

static void terminal_cmd_doublepulse(int argc, const char** argv)
{
	(void)argc;
	(void)argv;

	int preface, pulse1, breaktime, pulse2;
	int utick;
	int deadtime = -1;

	TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
	TIM_OCInitTypeDef  TIM_OCInitStructure;
	TIM_BDTRInitTypeDef TIM_BDTRInitStructure;

	if (argc < 5) {
		commands_printf("Usage: double_pulse <preface> <pulse1> <break> <pulse2> [deadtime]");
		commands_printf("   preface: idle time in  µs");
		commands_printf("    pulse1: high time of pulse 1 in µs");
		commands_printf("     break: break between pulses in µs\n");
		commands_printf("    pulse2: high time of pulse 2 in µs");
		commands_printf("  deadtime: overwrite deadtime, in ns");
		return;
	}
	sscanf(argv[1], "%d", &preface);
	sscanf(argv[2], "%d", &pulse1);
	sscanf(argv[3], "%d", &breaktime);
	sscanf(argv[4], "%d", &pulse2);
	if (argc == 6) {
		sscanf(argv[5], "%d", &deadtime);
	}
	timeout_configure_IWDT_slowest();

	utick = (int)(SYSTEM_CORE_CLOCK / 1000000);
	mcpwm_deinit();
	mcpwm_foc_deinit();
	gpdrive_deinit();

	TIM_Cmd(TIM1, DISABLE);
	TIM_Cmd(TIM4, DISABLE);
	//TIM4 als Trigger Timer
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM4, ENABLE);

	TIM_TimeBaseStructure.TIM_Period = (SYSTEM_CORE_CLOCK / 20000);
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseInit(TIM4, &TIM_TimeBaseStructure);
	TIM_SelectMasterSlaveMode(TIM4, TIM_MasterSlaveMode_Enable);
	TIM_SelectOutputTrigger(TIM4, TIM_TRGOSource_Enable);
	TIM4->CNT = 0;

	// TIM1
	// TIM1 clock enable
	RCC_APB2PeriphClockCmd(RCC_APB2Periph_TIM1, ENABLE);

	// Time Base configuration
	TIM_TimeBaseStructure.TIM_Prescaler = 0;
	TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
	TIM_TimeBaseStructure.TIM_Period = (preface + pulse1) * utick;
	TIM_TimeBaseStructure.TIM_ClockDivision = 0;
	TIM_TimeBaseStructure.TIM_RepetitionCounter = 0;
	TIM_TimeBaseInit(TIM1, &TIM_TimeBaseStructure);

	// Channel 1, 2 and 3 Configuration in PWM mode
	TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_PWM2;
	TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
	TIM_OCInitStructure.TIM_OutputNState = TIM_OutputNState_Enable;
	TIM_OCInitStructure.TIM_Pulse = preface * utick;
	TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
	TIM_OCInitStructure.TIM_OCNPolarity = TIM_OCNPolarity_High;
	TIM_OCInitStructure.TIM_OCIdleState = TIM_OCIdleState_Set;
	TIM_OCInitStructure.TIM_OCNIdleState = TIM_OCNIdleState_Set;

	TIM_OC1Init(TIM1, &TIM_OCInitStructure);
	TIM_OC1PreloadConfig(TIM1, TIM_OCPreload_Enable);
	TIM_OC2Init(TIM1, &TIM_OCInitStructure);
	TIM_OC2PreloadConfig(TIM1, TIM_OCPreload_Enable);
	TIM_OC3Init(TIM1, &TIM_OCInitStructure);
	TIM_OC3PreloadConfig(TIM1, TIM_OCPreload_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_1, TIM_OCMode_PWM2);
	TIM_CCxCmd(TIM1, TIM_Channel_1, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_1, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_2, TIM_OCMode_Inactive);
	TIM_CCxCmd(TIM1, TIM_Channel_2, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_2, TIM_CCxN_Enable);

	TIM_SelectOCxM(TIM1, TIM_Channel_3, TIM_OCMode_Inactive);
	TIM_CCxCmd(TIM1, TIM_Channel_3, TIM_CCx_Enable);
	TIM_CCxNCmd(TIM1, TIM_Channel_3, TIM_CCxN_Enable);
	TIM_GenerateEvent(TIM1, TIM_EventSource_COM);


	// Automatic Output enable, Break, dead time and lock configuration
	TIM_BDTRInitStructure.TIM_OSSRState = TIM_OSSRState_Enable;
	TIM_BDTRInitStructure.TIM_OSSIState = TIM_OSSIState_Enable;
	TIM_BDTRInitStructure.TIM_LOCKLevel = TIM_LOCKLevel_OFF;
	if (deadtime < 0) {
		TIM_BDTRInitStructure.TIM_DeadTime = conf_general_calculate_deadtime(HW_DEAD_TIME_NSEC, SYSTEM_CORE_CLOCK);
	} else {
		TIM_BDTRInitStructure.TIM_DeadTime = conf_general_calculate_deadtime(deadtime, SYSTEM_CORE_CLOCK);
	}
	TIM_BDTRInitStructure.TIM_Break = TIM_Break_Disable;
	TIM_BDTRInitStructure.TIM_BreakPolarity = TIM_BreakPolarity_High;
	TIM_BDTRInitStructure.TIM_AutomaticOutput = TIM_AutomaticOutput_Disable;
	TIM_BDTRConfig(TIM1, &TIM_BDTRInitStructure);

	TIM_CCPreloadControl(TIM1, ENABLE);
	TIM_ARRPreloadConfig(TIM1, ENABLE);

	TIM1->CNT = 0;
	TIM1->EGR = TIM_EGR_UG;

	TIM_SelectSlaveMode(TIM1, TIM_SlaveMode_Trigger);
	TIM_SelectInputTrigger(TIM1, TIM_TS_ITR3);
	TIM_SelectOnePulseMode(TIM1, TIM_OPMode_Single);
	TIM_CtrlPWMOutputs(TIM1, ENABLE);

	TIM_Cmd(TIM1, ENABLE);
	//Timer 4 triggert Timer 1
	TIM_Cmd(TIM4, ENABLE);
	TIM_Cmd(TIM4, DISABLE);
	TIM1->ARR = (breaktime + pulse2) * utick;
	TIM1->CCR1 = breaktime * utick;
	while (TIM1->CNT != 0);
	TIM_Cmd(TIM4, ENABLE);

	chThdSleepMilliseconds(1);
	TIM_CtrlPWMOutputs(TIM1, DISABLE);
	mc_configuration* mcconf = mempools_alloc_mcconf();
	*mcconf = *mc_interface_get_configuration();

	switch (mcconf->motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_init(mcconf);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_init(mcconf, mcconf);
		break;

	case MOTOR_TYPE_GPD:
		gpdrive_init(mcconf);
		break;

	default:
		break;
	}
	commands_printf("Done");
	return;
}

