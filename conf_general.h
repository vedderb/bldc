/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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

/*
 * conf_general.h
 *
 *  Created on: 14 apr 2014
 *      Author: benjamin
 */

#ifndef CONF_GENERAL_H_
#define CONF_GENERAL_H_

// Firmware version
#define FW_VERSION_MAJOR	2
#define FW_VERSION_MINOR	18

#include "datatypes.h"

/*
 * Settings
 */
#define SYSTEM_CORE_CLOCK		168000000

// Settings and parameters to override
//#define VIN_R1				33000.0
//#define VIN_R1				39200.0
//#define VIN_R2				2200.0
//#define CURRENT_AMP_GAIN	10.0
//#define CURRENT_SHUNT_RES	0.0005
//#define WS2811_ENABLE			1
//#define CURR1_DOUBLE_SAMPLE		0
//#define CURR2_DOUBLE_SAMPLE		0

/*
 * Select only one hardware version
 */
#if !defined(HW_VERSION_40) && !defined(HW_VERSION_45) && !defined(HW_VERSION_46) && \
	!defined(HW_VERSION_48) && !defined(HW_VERSION_49) && !defined(HW_VERSION_410) && \
	!defined(HW_VERSION_R2) && !defined(HW_VERSION_VICTOR_R1A)
//#define HW_VERSION_40
//#define HW_VERSION_45
//#define HW_VERSION_46 // Also for 4.7
//#define HW_VERSION_48
//#define HW_VERSION_49
#define HW_VERSION_410 // Also for 4.11 and 4.12
//#define HW_VERSION_R2
//#define HW_VERSION_VICTOR_R1A
#endif

/*
 * Select default user motor configuration
 */
//#define MCCONF_DEFAULT_USER		"mcconf_sten.h"
//#define MCCONF_DEFAULT_USER		"mcconf_sp_540kv.h"
//#define MCCONF_DEFAULT_USER		"mcconf_castle_2028.h"

/*
 * Select default user app configuration
 */
//#define APPCONF_DEFAULT_USER		"appconf_example_ppm.h"
//#define APPCONF_DEFAULT_USER		"appconf_custom.h"

/*
 * Select which custom application to use. To configure the default applications and
 * their settings, go to conf_general_read_app_configuration and enter the default init
 * values.
 */
//#define USE_APP_STEN

/*
 * Enable CAN-bus
 */
#define CAN_ENABLE				1

/*
 * Settings for the external LEDs (hardcoded for now)
 */
#define LED_EXT_BATT_LOW		28.0
#define LED_EXT_BATT_HIGH		33.0

/*
 * Output WS2811 signal on the HALL1 pin. Notice that hall sensors can't be used
 * at the same time.
 */
#ifndef WS2811_ENABLE
#define WS2811_ENABLE			0
#endif
#define WS2811_CLK_HZ			800000
#define WS2811_LED_NUM			28
#define WS2811_USE_CH2			1		// 0: CH1 (PB6) 1: CH2 (PB7)

/*
 * Servo output driver
 */
#ifndef SERVO_OUT_ENABLE
#define SERVO_OUT_ENABLE		0		// Enable servo output
#endif
#define SERVO_OUT_SIMPLE		1		// Use simple HW-based driver (recommended)
#define SERVO_OUT_PULSE_MIN_US	1000	// Minimum pulse length in microseconds
#define SERVO_OUT_PULSE_MAX_US	2000	// Maximum pulse length in microseconds
#define SERVO_OUT_RATE_HZ		50		// Update rate in Hz

// Correction factor for computations that depend on the old resistor division factor
#define VDIV_CORR			((VIN_R2 / (VIN_R2 + VIN_R1)) / (2.2 / (2.2 + 33.0)))

// Actual voltage on 3.3V net based on internal reference
//#define V_REG				(1.21 / ((float)ADC_Value[ADC_IND_VREFINT] / 4095.0))
#define V_REG				3.3

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
