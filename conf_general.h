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
#define FW_VERSION_MAJOR	1
#define FW_VERSION_MINOR	7

#include "datatypes.h"

/*
 * Settings
 */
#define AUTO_PRINT_FAULTS		0
#define SYSTEM_CORE_CLOCK		168000000

// Component parameters to override
//#define V_REG				3.3
#define VIN_R1				39000.0
//#define VIN_R2				2200.0
//#define CURRENT_AMP_GAIN	10.0
//#define CURRENT_SHUNT_RES	0.001

// Correction factor for computations that depend on the old resistor division factor
#define VDIV_CORR			((VIN_R2 / (VIN_R2 + VIN_R1)) / (2.2 / (2.2 + 33.0)))

/*
 * Select only one hardware version
 */
//#define HW_VERSION_BW
//#define HW_VERSION_40
//#define HW_VERSION_45
#define HW_VERSION_46 // Also for 4.7
//#define HW_VERSION_R2
//#define HW_VERSION_VICTOR_R1A

/*
 * Select only one (default) motor configuration
 */
//#define MCCONF_OUTRUNNER1
#define MCCONF_OUTRUNNER2
//#define MCCONF_STEN

/*
 * Select which custom application to use. To configure the default applications and
 * their settings, go to conf_general_read_app_configuration and enter the default init
 * values.
 */
//#define USE_APP_STEN
//#define USE_APP_GURGALOF

/*
 * Use encoder
 */
#define ENCODER_ENABLE			0
#define ENCODER_COUNTS			14400
//#define ENCODER_COUNTS			10000

/*
 * Enable CAN-bus
 */
#define CAN_ENABLE				1

/*
 * Settings for the external LEDs (hardcoded for now)
 */
#define LED_EXT_BATT_LOW		25.6
#define LED_EXT_BATT_HIGH		33.0

/*
 * Output WS2811 signal on the HALL1 pin. Notice that hall sensors can't be used
 * at the same time.
 */
#define WS2811_ENABLE			0
#define WS2811_CLK_HZ			800000
#define WS2811_LED_NUM			14
#define WS2811_USE_CH2			1		// 0: CH1 (PB6) 1: CH2 (PB7)

// Functions
void conf_general_init(void);
void conf_general_read_app_configuration(app_configuration *conf);
bool conf_general_store_app_configuration(app_configuration *conf);
void conf_general_read_mc_configuration(mc_configuration *conf);
bool conf_general_store_mc_configuration(mc_configuration *conf);
bool conf_general_detect_motor_param(float current, float min_rpm, float low_duty,
		float *int_limit, float *bemf_coupling_k, int8_t *hall_table, int *hall_res);

#endif /* CONF_GENERAL_H_ */
