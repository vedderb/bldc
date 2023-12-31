/*
	Copyright 2017 - 2022 Benjamin Vedder	benjamin@vedder.se

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
#define FW_VERSION_MAJOR			6
#define FW_VERSION_MINOR			05
// Set to 0 for building a release and iterate during beta test builds
#define FW_TEST_VERSION_NUMBER		25

#include "datatypes.h"

// Disable hardware limits on configuration parameters
//#define DISABLE_HW_LIMITS

#if !defined(HW_SOURCE) && !defined(HW_SOURCE_ALT)
#error "No hardware source file set"
#endif

#ifndef HW_HEADER
#error "No hardware header file set"
#endif

#ifdef USER_MC_CONF
#include USER_MC_CONF
#endif

#ifdef USER_APP_CONF
#include USER_APP_CONF
#endif

// This is how to provide a custom UI in VESC Tool. The UI can be created and tested in the
// scripting page, then the source files can be exported. The defines below use the exported
// files to provide the custom UI when VESC Tool connects.
//
// The intention if the HW gui is to be part of the HW-file and the app gui is for custom apps.
// Both can be used at the same time.
//
// Defining QMLUI_HW_FULLSCREEN and/or QMLUI_APP_FULLSCREEN will disable the other pages in the
// mobile version of VESC Tool.
//
//#define QMLUI_SOURCE_HW		"qmlui/hw/qmlui_example_hw.c"
//#define QMLUI_HEADER_HW		"qmlui/hw/qmlui_example_hw.h"
//#define QMLUI_HW_FULLSCREEN
//
//#define QMLUI_SOURCE_APP	"qmlui/app/qmlui_example_app.c"
//#define QMLUI_HEADER_APP	"qmlui/app/qmlui_example_app.h"
//#define QMLUI_APP_FULLSCREEN

/*
 * Select default user motor configuration
 */
//#include			"mcconf_default.h"
//#include 			"mcconf_china_60kv.h"

/*
 * Select default user app configuration
 */
//#include			"appconf_example_ppm.h"
//#include			"appconf_custom.h"

/*
 * Set APP_CUSTOM_TO_USE to the name of the main C file of the custom application.
 */
//#define APP_CUSTOM_TO_USE			"app_custom_template.c"
//#define APP_CUSTOM_TO_USE			"app_motor_heater.c"
//#include "er/app_erockit_conf_v2.h"
//#include "finn/app_finn_az_conf.h"

#include "hw.h"
#include "mcconf_default.h"
#include "appconf_default.h"

/*
 * Enable blackmagic probe output on SWD port
 */
#ifndef HAS_BLACKMAGIC
#define HAS_BLACKMAGIC				1
#endif

/*
 * Enable CAN-bus
 */
#ifndef CAN_ENABLE
#define CAN_ENABLE					1
#endif

#ifdef HW_HAS_NO_CAN
#undef CAN_ENABLE
#define CAN_ENABLE 					0
#endif

/*
 * Servo output driver
 */
#define SERVO_OUT_PULSE_MIN_US		1000	// Minimum pulse length in microseconds
#define SERVO_OUT_PULSE_MAX_US		2000	// Maximum pulse length in microseconds
#define SERVO_OUT_RATE_HZ			50		// Update rate in Hz

// Correction factor for computations that depend on the old resistor division factor
#define VDIV_CORR					((VIN_R2 / (VIN_R2 + VIN_R1)) / (2.2 / (2.2 + 33.0)))

// Current ADC to amperes factor
#define FAC_CURRENT					((V_REG / 4095.0) / (CURRENT_SHUNT_RES * CURRENT_AMP_GAIN))

#define VOLTAGE_TO_ADC_FACTOR	( VIN_R2 / (VIN_R2 + VIN_R1) ) * ( 4096.0 / V_REG )

// Actual voltage on 3.3V net based on internal reference
//#define V_REG						(1.21 / ((float)ADC_Value[ADC_IND_VREFINT] / 4095.0))
//#define V_REG						3.3

// Use the pins for the hardware SPI port instead of the hall/encoder pins for the AS5047
#ifndef AS504x_USE_SW_MOSI_PIN
#define AS504x_USE_SW_MOSI_PIN 		0
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

/*
 *	Run the FOC loop once every N ADC ISR requests. This way the pwm frequency is
 *	detached from the FOC calculation, which because it takes ~25usec it can't work
 *	at >40khz. To set a 100kHz pwm FOC_CONTROL_LOOP_FREQ_DIVIDER can be set at 3
 *	so it skips 2 ISR calls and execute the control loop in the 3rd call.
 */
#ifndef FOC_CONTROL_LOOP_FREQ_DIVIDER
#define FOC_CONTROL_LOOP_FREQ_DIVIDER	1
#endif

// Global configuration variables
extern bool conf_general_permanent_nrf_found;
extern volatile backup_data g_backup;

// Functions
void conf_general_init(void);
bool conf_general_store_backup_data(void);
bool conf_general_read_eeprom_var_hw(eeprom_var *v, int address);
bool conf_general_read_eeprom_var_custom(eeprom_var *v, int address);
bool conf_general_store_eeprom_var_hw(eeprom_var *v, int address);
bool conf_general_store_eeprom_var_custom(eeprom_var *v, int address);
void conf_general_read_app_configuration(app_configuration *conf);
bool conf_general_store_app_configuration(app_configuration *conf);
void conf_general_read_mc_configuration(mc_configuration *conf, bool is_motor_2);
bool conf_general_store_mc_configuration(mc_configuration *conf, bool is_motor_2);
bool conf_general_detect_motor_param(float current, float min_rpm, float low_duty,
									 float *int_limit, float *bemf_coupling_k, int8_t *hall_table, int *hall_res);
bool conf_general_measure_flux_linkage(float current, float duty,
									   float min_erpm, float res, float *linkage);
uint8_t conf_general_calculate_deadtime(float deadtime_ns, float core_clock_freq);
int conf_general_measure_flux_linkage_openloop(float current, float duty,
											   float erpm_per_sec, float res, float ind, float *linkage,
											   float *linkage_undriven, float *undriven_samples, bool *result);
int conf_general_autodetect_apply_sensors_foc(float current,
											  bool store_mcconf_on_success, bool send_mcconf_on_success, int *result);
void conf_general_calc_apply_foc_cc_kp_ki_gain(mc_configuration *mcconf, float tc);
int conf_general_detect_apply_all_foc(float max_power_loss,
									  bool store_mcconf_on_success, bool send_mcconf_on_success);
int conf_general_detect_apply_all_foc_can(bool detect_can, float max_power_loss,
										  float min_current_in, float max_current_in,
										  float openloop_rpm, float sl_erpm,
										  void(*reply_func)(unsigned char* data, unsigned int len));


#endif /* CONF_GENERAL_H_ */
