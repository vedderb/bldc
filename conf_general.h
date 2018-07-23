/*
	Copyright 2017 - 2018 Benjamin Vedder	benjamin@vedder.se

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

#ifndef CONF_GENERAL_H_
#define CONF_GENERAL_H_

// Firmware version
#define FW_VERSION_MAJOR		3
#define FW_VERSION_MINOR		40

#include "datatypes.h"

// Settings and parameters to override
//#define VIN_R1						33000.0
//#define VIN_R1						39200.0
//#define VIN_R2						2200.0
//#define CURRENT_AMP_GAIN			10.0
//#define CURRENT_SHUNT_RES			0.005
//#define WS2811_ENABLE				1
//#define WS2811_TEST					1
//#define CURR1_DOUBLE_SAMPLE			0
//#define CURR2_DOUBLE_SAMPLE			0
//#define AS5047_USE_HW_SPI_PINS		1

// Disable hardware limits on configuration parameters
//#define DISABLE_HW_LIMITS

// Benjamins first HW60 PCB with PB5 and PB6 swapped
//#define HW60_VEDDER_FIRST_PCB

// Benjamins first HW75_300 PCB with different LED pins and motor temp error
//#define HW75_300_VEDDER_FIRST_PCB

/*
 * Select only one hardware version
 */
#if !defined(HW_VERSION_40) && !defined(HW_VERSION_45) && !defined(HW_VERSION_46) && \
	!defined(HW_VERSION_48) && !defined(HW_VERSION_49) && !defined(HW_VERSION_410) && \
	!defined(HW_VERSION_60) && !defined(HW_VERSION_R2) && !defined(HW_VERSION_VICTOR_R1A) && \
	!defined(HW_VERSION_DAS_RS) && !defined(HW_VERSION_PALTA) && !defined(HW_VERSION_RH) && \
	!defined(HW_VERSION_TP) && !defined(HW_VERSION_75_300) && !defined(HW_VERSION_MINI4) && \
	!defined(HW_VERSION_DAS_MINI)
//#define HW_VERSION_40
//#define HW_VERSION_45
//#define HW_VERSION_46 // Also for 4.7
//#define HW_VERSION_48
//#define HW_VERSION_49
//#define HW_VERSION_410 // Also for 4.11 and 4.12
#define HW_VERSION_60
//#define HW_VERSION_R2
//#define HW_VERSION_VICTOR_R1A
//#define HW_VERSION_DAS_RS
//#define HW_VERSION_PALTA
//#define HW_VERSION_RH
//#define HW_VERSION_TP
//#define HW_VERSION_75_300
//#define HW_VERSION_MINI4
//#define HW_VERSION_DAS_MINI
#endif

/*
 * Select default user motor configuration
 */
//#define MCCONF_DEFAULT_USER			"mcconf_sten.h"
//#define MCCONF_DEFAULT_USER			"mcconf_sp_540kv.h"
//#define MCCONF_DEFAULT_USER			"mcconf_castle_2028.h"
//#define MCCONF_DEFAULT_USER			"mcconf_ellwee.h"

/*
 * Select default user app configuration
 */
//#define APPCONF_DEFAULT_USER		"appconf_example_ppm.h"
//#define APPCONF_DEFAULT_USER		"appconf_custom.h"
//#define APPCONF_DEFAULT_USER		"appconf_ellwee.h"

/*
 * Set APP_CUSTOM_TO_USE to the name of the main C file of the custom application.
 */
//#define APP_CUSTOM_TO_USE			"app_ellwee.c"

/*
 * Enable CAN-bus
 */
#define CAN_ENABLE					1

/*
 * Settings for the external LEDs (hardcoded for now)
 */
#define LED_EXT_BATT_LOW			28.0
#define LED_EXT_BATT_HIGH			33.0

/*
 * Output WS2811 signal on the HALL1 pin. Notice that hall sensors can't be used
 * at the same time.
 */
#ifndef WS2811_ENABLE
#define WS2811_ENABLE				0
#endif
#define WS2811_CLK_HZ				800000
#define WS2811_LED_NUM				28
#define WS2811_USE_CH2				1		// 0: CH1 (PB6) 1: CH2 (PB7)
#ifndef WS2811_TEST
#define WS2811_TEST					0		// Show a test pattern
#endif

/*
 * Servo output driver
 */
#ifndef SERVO_OUT_ENABLE
#define SERVO_OUT_ENABLE			0		// Enable servo output
#endif
#define SERVO_OUT_PULSE_MIN_US		1000	// Minimum pulse length in microseconds
#define SERVO_OUT_PULSE_MAX_US		2000	// Maximum pulse length in microseconds
#define SERVO_OUT_RATE_HZ			50		// Update rate in Hz

// Correction factor for computations that depend on the old resistor division factor
#define VDIV_CORR					((VIN_R2 / (VIN_R2 + VIN_R1)) / (2.2 / (2.2 + 33.0)))

// Current ADC to amperes factor
#define FAC_CURRENT					((V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN))

// Actual voltage on 3.3V net based on internal reference
//#define V_REG						(1.21 / ((float)ADC_Value[ADC_IND_VREFINT] / 4095.0))
#define V_REG						3.3

// Use the pins for the hardware SPI port instead of the hall/encoder pins for the AS5047
#ifndef AS5047_USE_HW_SPI_PINS
#define AS5047_USE_HW_SPI_PINS		0
#endif

/*
 * MCU
 */
#define SYSTEM_CORE_CLOCK			168000000
#define STM32_UUID					((uint32_t*)0x1FFF7A10)
#define STM32_UUID_8				((uint8_t*)0x1FFF7A10)

/*
 *	Run the BLDC speed controller in current mode instead of duty cycle mode. This will
 *	make it behave like the FOC speed controller. The duty cycle mode has the advantage
 *	that it does not require the extra current controller since bldc inherently runs
 *	with duty cycle control. The current controller also outputs a duty cycle in the
 *	end, and then the speed controller might as well do the same without the current
 *	controller dynamics in between. FOC on the other hand is inherently based on current
 *	control.
 */
#define BLDC_SPEED_CONTROL_CURRENT	1

// Global configuration variables
extern bool conf_general_permanent_nrf_found;

// Functions
void conf_general_init(void);
void conf_general_get_default_app_configuration(app_configuration *conf);
void conf_general_get_default_mc_configuration(mc_configuration *conf);
void conf_general_read_app_configuration(app_configuration *conf);
bool conf_general_store_app_configuration(app_configuration *conf);
void conf_general_read_mc_configuration(mc_configuration *conf);
bool conf_general_store_mc_configuration(mc_configuration *conf);
bool conf_general_detect_motor_param(float current, float min_rpm, float low_duty,
		float *int_limit, float *bemf_coupling_k, int8_t *hall_table, int *hall_res);
bool conf_general_measure_flux_linkage(float current, float duty,
		float min_erpm, float res, float *linkage);

#endif /* CONF_GENERAL_H_ */
