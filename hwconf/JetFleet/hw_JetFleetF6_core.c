/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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
#include "terminal.h"
#include "commands.h"
#include "mc_interface.h"

// Variables
typedef enum {
	BT_SHUTDOWN_IDLE = 0,
	BT_SHUTDOWN_HOLDING,
	BT_SHUTDOWN_ARMED,
	BT_SHUTDOWN_FORCE_ARMED,
	BT_SHUTDOWN_REQUESTED,
} bt_shutdown_state;

static bool bt_raw_pressed = false;
static bool bt_last_raw_pressed = false;
static bool bt_pressed = false;
static bt_shutdown_state bt_state = BT_SHUTDOWN_IDLE;
static unsigned int bt_hold_counter = 0;
static unsigned int bt_debounce_counter = 0;

#define EXT_BUZZER_ON()    palSetPad(HW_ICU_GPIO, HW_ICU_PIN)
#define EXT_BUZZER_OFF()   palClearPad(HW_ICU_GPIO, HW_ICU_PIN)

void buzzer_init(void) {
    // External Buzzer (using servo pin!)
    palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN,
                  PAL_MODE_OUTPUT_PUSHPULL |
                  PAL_STM32_OSPEED_HIGHEST);
	EXT_BUZZER_ON();
    chThdSleepMilliseconds(30);
    EXT_BUZZER_OFF();
}

static void beep_off(void)
{
	EXT_BUZZER_OFF();
}

static void beep_on(void)
{
	EXT_BUZZER_ON();
}

// Private functions
static void terminal_button_test(int argc, const char **argv);

void hw_init_gpio(void) {
	
	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

	#ifdef HW_USE_BRK
	// BRK Fault pin
	palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_ALTERNATE(GPIO_AF_TIM1));
	#else	
	// Soft Lockout
	palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_INPUT);
	#endif

	
	// AUX
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);	

	// LEDs
	palSetPadMode(LED_GREEN_GPIO, LED_GREEN_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(LED_RED_GPIO, LED_RED_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);

	// On-board Buzzer (using servo pin!)
	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN,
			PAL_MODE_OUTPUT_PUSHPULL |
			PAL_STM32_OSPEED_HIGHEST);
	EXT_BUZZER_OFF();

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
	palSetPadMode(GPIOC, 15, PAL_MODE_OUTPUT_OPENDRAIN);
	palSetPadMode(GPIOC, 14, PAL_MODE_OUTPUT_OPENDRAIN);
	palSetPadMode(GPIOC, 13, PAL_MODE_OUTPUT_OPENDRAIN);
	PHASE_FILTER_OFF();

	// ShutDown
	palSetPadMode(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN, PAL_MODE_OUTPUT_OPENDRAIN);
	palSetPadMode(HW_SHUTDOWN_SENSE_GPIO, HW_SHUTDOWN_SENSE_PIN, PAL_MODE_INPUT);

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 6, PAL_MODE_INPUT_ANALOG);

	//palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);

	terminal_register_command_callback(
			"test_button",
			"Try sampling the shutdown button",
			0,
			terminal_button_test);
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_Vrefint, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8, 6, ADC_SampleTime_15Cycles);

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_0, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC2, ADC_Channel_9, 6, ADC_SampleTime_15Cycles);

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 1, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 2, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3, 3, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_1, 5, ADC_SampleTime_15Cycles);
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2, 6, ADC_SampleTime_15Cycles);

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

// Generic hardware I2C is not routed on JetFleetF6: HW_I2C_SDA shares PC5 with
// shutdown sense, and PC5 has no valid I2C alternate function on STM32F4. The
// IMU has its own bit-banged path via LSM6DS3_SDA_*, so these stay no-ops.
void hw_start_i2c(void) {
}

void hw_stop_i2c(void) {
}

void hw_try_restore_i2c(void) {
}

#define TIME_500MS 50
#define TIME_3S 300
#define ERPM_THRESHOLD 100
#define BUTTON_DEBOUNCE_SAMPLES 2

static bool shutdown_button_pressed(void) {
	bt_raw_pressed = palReadPad(HW_SHUTDOWN_SENSE_GPIO, HW_SHUTDOWN_SENSE_PIN) == PAL_HIGH;

	if (bt_raw_pressed == bt_last_raw_pressed) {
		if (bt_debounce_counter < BUTTON_DEBOUNCE_SAMPLES) {
			bt_debounce_counter++;
		}
	} else {
		bt_last_raw_pressed = bt_raw_pressed;
		bt_debounce_counter = 1;
	}

	if (bt_debounce_counter >= BUTTON_DEBOUNCE_SAMPLES) {
		bt_pressed = bt_raw_pressed;
	}

	return bt_pressed;
}

/**
 * hw_sample_shutdown_button - return false if shutdown is requested, true otherwise
 *
 * Behavior: sample the shutdown button as a digital input. The input is expected
 * to be low when released and high when pressed.
 *
 * Once the hold counter reaches the threshold, provided that the ERPM is below
 * 100, a very short (20ms) beep will go off. Shutdown actually happens on the
 * release edge when the press is over.
 *
 * If the motor is spinning faster, then a 3s press is required. Buzzer will beep once the
 * time has been reached. Again, shutdown happens on release.
 *
 * Normal shutdown time:    0.5s
 * Emergency shutdown time: 3.0s
 */

bool hw_sample_shutdown_button(void) {
	bool pressed = shutdown_button_pressed();

	switch (bt_state) {
	case BT_SHUTDOWN_IDLE:
		if (pressed) {
			bt_hold_counter = 1;
			bt_state = BT_SHUTDOWN_HOLDING;
		} else {
			bt_hold_counter = 0;
		}
		break;

	case BT_SHUTDOWN_HOLDING:
		if (!pressed) {
			bt_hold_counter = 0;
			beep_off();
			bt_state = BT_SHUTDOWN_IDLE;
			break;
		}

		bt_hold_counter++;

		if (bt_hold_counter > TIME_500MS) {
			if (fabsf(mc_interface_get_rpm()) < ERPM_THRESHOLD) {
				// Power-down is triggered by releasing the button after arming.
				bt_hold_counter = 0;
				bt_state = BT_SHUTDOWN_ARMED;

				beep_on();
				chThdSleepMilliseconds(20);
				beep_off();
			} else if (bt_hold_counter > TIME_3S) {
				// Emergency Power-Down - beep to let the user know it's ready.
				beep_on();
				bt_hold_counter = 0;
				bt_state = BT_SHUTDOWN_FORCE_ARMED;
			}
		}
		break;

	case BT_SHUTDOWN_ARMED:
		if (fabsf(mc_interface_get_rpm()) > ERPM_THRESHOLD) {
			bt_hold_counter = 0;
			beep_off();
			bt_state = BT_SHUTDOWN_IDLE;
			break;
		}

		if (!pressed) {
			beep_off();
			bt_state = BT_SHUTDOWN_REQUESTED;
			return false;
		}
		break;

	case BT_SHUTDOWN_FORCE_ARMED:
		if (!pressed) {
			beep_off();
			bt_state = BT_SHUTDOWN_REQUESTED;
			return false;
		}
		break;

	case BT_SHUTDOWN_REQUESTED:
		beep_off();
		return false;

	default:
		bt_hold_counter = 0;
		beep_off();
		bt_state = BT_SHUTDOWN_IDLE;
		break;
	}

	return true;
}


float hw_JetFleet_get_temp(void) {
	float t1 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float t3 = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS_3]) / 10000.0) / 3380.0) + (1.0 / 298.15)) - 273.15);
	float res = 0.0;

	if (t1 >= t3) {
		res = t1;
	} else {
		res = t3;
	} 
	return res;
}

static void terminal_button_test(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	for (int i = 0;i < 40;i++) {
		commands_printf("BT: %d:%d raw=%d pressed=%d state=%d RPM=%.1f",
				HW_SAMPLE_SHUTDOWN(), bt_hold_counter, (int)bt_raw_pressed,
				(int)bt_pressed, (int)bt_state, (double)mc_interface_get_rpm());
		chThdSleepMilliseconds(100);
	}
}
