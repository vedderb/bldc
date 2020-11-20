/*
	Copyright 2017 - 2019 Benjamin Vedder	benjamin@vedder.se

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
#define FW_VERSION_MAJOR			5
#define FW_VERSION_MINOR			01
// Set to 0 for building a release and iterate during beta test builds
#define FW_TEST_VERSION_NUMBER		0

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

/*
 * Select only one hardware version, if it is not passed
 * as an argument.
 */
#if !defined(HW_SOURCE) && !defined(HW_HEADER)
//#define HW_SOURCE "hw_40.c"
//#define HW_HEADER "hw_40.h"

//#define HW_SOURCE "hw_45.c"
//#define HW_HEADER "hw_45.h"

//#define HW_SOURCE "hw_46.c" // Also for 4.7
//#define HW_HEADER "hw_46.h" // Also for 4.7

//#define HW_SOURCE "hw_48.c"
//#define HW_HEADER "hw_48.h"

//#define HW_SOURCE "hw_49.c"
//#define HW_HEADER "hw_49.h"

//#define HW_SOURCE "hw_410.c" // Also for 4.11 and 4.12
//#define HW_HEADER "hw_410.h" // Also for 4.11 and 4.12

// Benjamins first HW60 PCB with PB5 and PB6 swapped
//#define HW60_VEDDER_FIRST_PCB

// Mark3 version of HW60 with power switch and separate NRF UART.
//#define HW60_IS_MK3
#define HW60_IS_MK4

#define HW_SOURCE "hw_60.c"
#define HW_HEADER "hw_60.h"

//#define HW_SOURCE "hw_r2.c"
//#define HW_HEADER "hw_r2.h"

//#define HW_SOURCE "hw_victor_r1a.c"
//#define HW_HEADER "hw_victor_r1a.h"

//#define HW_SOURCE "hw_das_rs.c"
//#define HW_HEADER "hw_das_rs.h"

//#define HW_SOURCE "hw_axiom.c"
//#define HW_HEADER "hw_axiom.h"

//#define HW_SOURCE "hw_luna_bbshd.c"
//#define HW_HEADER "hw_luna_bbshd.h"

//#define HW_SOURCE "hw_rh.c"
//#define HW_HEADER "hw_rh.h"

//#define HW_SOURCE "hw_tp.c"
//#define HW_HEADER "hw_tp.h"

// Benjamins first HW75_300 PCB with different LED pins and motor temp error
//#define HW75_300_VEDDER_FIRST_PCB

// Second revision with separate UART for NRF51
//#define HW75_300_REV_2
//#define HW75_300_REV_3

//#define HW_SOURCE "hw_75_300.c"
//#define HW_HEADER "hw_75_300.h"

//#define HW_SOURCE "hw_mini4.c"
//#define HW_HEADER "hw_mini4.h"

//#define HW_SOURCE "hw_das_mini.c"
//#define HW_HEADER "hw_das_mini.h"

//#define HW_SOURCE "hw_uavc_qcube.c"
//#define HW_HEADER "hw_uavc_qcube.h"

//#define HW_SOURCE "hw_uavc_omega.c"
//#define HW_HEADER "hw_uavc_omega.h"

//#define HW_SOURCE "hw_binar_v1.c"
//#define HW_HEADER "hw_binar_v1.h"

//#define HW_SOURCE "hw_hd.c"
//#define HW_HEADER "hw_hd.h"

//#define HW_SOURCE "hw_a200s_v2.c"
//#define HW_HEADER "hw_a200s_v2.h"

//#define HW_SOURCE "hw_rd2.c"
//#define HW_HEADER "hw_rd2.h"

//#define HW_SOURCE "hw_100_250.c"
//#define HW_HEADER "hw_100_250.h"

//#define HW_SOURCE "hw_unity.c"
//#define HW_HEADER "hw_unity.h"

//#define HW_DUAL_CONFIG_PARALLEL
//#define HW_SOURCE "hw_stormcore_100d.c"
//#define HW_HEADER "hw_stormcore_100d.h"

//#define HW_SOURCE "hw_stormcore_60d.c"
//#define HW_HEADER "hw_stormcore_60d.h"
//
//#define HW_SOURCE "hw_stormcore_100s.c"
//#define HW_HEADER "hw_stormcore_100s.h"
#endif

#ifndef HW_SOURCE
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

/*
 * Select default user motor configuration
 */
//#include			"mcconf_sten.h"
//#include			"mcconf_sp_540kv.h"
//#include			"mcconf_castle_2028.h"
//#include			"mcconf_ellwee.h"
//#include			"conf_test.h"

/*
 * Select default user app configuration
 */
//#include			"appconf_example_ppm.h"
//#include			"appconf_custom.h"
//#include			"appconf_ellwee.h"

/*
 * Set APP_CUSTOM_TO_USE to the name of the main C file of the custom application.
 */
//#define APP_CUSTOM_TO_USE			"app_custom_template.c"
//#define APP_CUSTOM_TO_USE			"app_motor_heater.c"
//#include "app_erockit_conf.h"

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

#define VOLTAGE_TO_ADC_FACTOR	( VIN_R2 / (VIN_R2 + VIN_R1) ) * ( 4096.0 / V_REG )

// Actual voltage on 3.3V net based on internal reference
//#define V_REG						(1.21 / ((float)ADC_Value[ADC_IND_VREFINT] / 4095.0))
//#define V_REG						3.3

// Use the pins for the hardware SPI port instead of the hall/encoder pins for the AS5047
#ifndef AS5047_USE_HW_SPI_PINS
#define AS5047_USE_HW_SPI_PINS		0
#endif
#ifndef AD2S1205_USE_HW_SPI_PINS
#define AD2S1205_USE_HW_SPI_PINS	0
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

// Functions
void conf_general_init(void);
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
bool conf_general_measure_flux_linkage_openloop(float current, float duty,
		float erpm_per_sec, float res, float ind, float *linkage,
		float *linkage_undriven, float *undriven_samples);
int conf_general_autodetect_apply_sensors_foc(float current,
		bool store_mcconf_on_success, bool send_mcconf_on_success);
void conf_general_calc_apply_foc_cc_kp_ki_gain(mc_configuration *mcconf, float tc);
int conf_general_detect_apply_all_foc(float max_power_loss,
		bool store_mcconf_on_success, bool send_mcconf_on_success);
int conf_general_detect_apply_all_foc_can(bool detect_can, float max_power_loss,
		float min_current_in, float max_current_in, float openloop_rpm, float sl_erpm);

#endif /* CONF_GENERAL_H_ */
