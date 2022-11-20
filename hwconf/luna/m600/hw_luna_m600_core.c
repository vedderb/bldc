/*
	Copyright 2020 Marcos Chaparro	mchaparro@powerdesigns.ca
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
#include "luna_m600_display.h"
#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "utils.h"
#include <math.h>
#include "mc_interface.h"
#include "mempools.h"
#include "mcpwm_foc.h"
#include "app.h"

// for double pulse test:
#include "stdio.h"
#include "string.h"
#include "commands.h"
#include "terminal.h"

#define EEPROM_ADDR_FIXED_THROTTLE_LEVEL	2

// Variables
static volatile bool i2c_running = false;

// Private functions
static void terminal_cmd_set_m600_use_fixed_throttle_level(int argc, const char **argv);
static void terminal_cmd_m600_correct_encoder_offset(int argc, const char **argv);
static void hw_override_pairing_done(void);

// I2C configuration
static const I2CConfig i2cfg = {
		OPMODE_I2C,
		100000,
		STD_DUTY_CYCLE
};

// Backup data that we want to preserve across firmware updates
typedef struct __attribute__((packed)) {
	uint8_t encoder_offset_calibration_done;
	float encoder_offset;
	char fw_version[16];
}m600_backup_t;

// The config is stored in the backup struct so that it is stored while sleeping.
static volatile m600_backup_t *m600_backup = (m600_backup_t*)g_backup.hw_config;

void hw_init_gpio(void) {
	
	//we can use __TIMESTAMP__ if we redefine it in the makefile gcc ... -Wno-builtin-macro-redefined -D__TIMESTAMP__=$(date +'"%Y-%m-%dT%H:%M:%S"') ...
	// https://stackoverflow.com/questions/17498556/c-preprocessor-timestamp-in-iso-86012004/

	// GPIO clock enable
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOA, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOB, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOC, ENABLE);
	RCC_AHB1PeriphClockCmd(RCC_AHB1Periph_GPIOD, ENABLE);

	// LEDs
	palSetPadMode(LED_GREEN_GPIO, LED_GREEN_PIN, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(LED_RED_GPIO, LED_RED_PIN, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);

	// GPIOA Configuration: Channel 1 to 3 as alternate function push-pull
	palSetPadMode(GPIOA, 8, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 9, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOA, 10, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	palSetPadMode(GPIOB, 13, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 14, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);
	palSetPadMode(GPIOB, 15, PAL_MODE_ALTERNATE(GPIO_AF_TIM1) | PAL_STM32_OSPEED_HIGHEST | PAL_STM32_PUDR_FLOATING);

	// Hall sensors
	palSetPadMode(HW_HALL_ENC_GPIO1, HW_HALL_ENC_PIN1, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO2, HW_HALL_ENC_PIN2, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_HALL_ENC_GPIO3, HW_HALL_ENC_PIN3, PAL_MODE_INPUT_PULLUP);

#ifdef HW_USE_BRK
	// BRK Fault pin
	palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_ALTERNATE(GPIO_AF_TIM1));
#endif

#ifdef HW_HAS_WHEEL_SPEED_SENSOR
	palSetPadMode(HW_SPEED_SENSOR_PORT, HW_SPEED_SENSOR_PIN, PAL_MODE_INPUT_PULLUP);
#endif
	// Current filter
	palSetPadMode(GPIOC, 13, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);
	CURRENT_FILTER_OFF();
	
#ifdef M600_Rev5
	// Phase voltage filter. Disabled by default
	palSetPadMode(HW_PHASE_A_FILTER_GPIO, HW_PHASE_A_FILTER_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(HW_PHASE_B_FILTER_GPIO, HW_PHASE_B_FILTER_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	palSetPadMode(HW_PHASE_C_FILTER_GPIO, HW_PHASE_C_FILTER_PIN, PAL_MODE_OUTPUT_OPENDRAIN | PAL_STM32_OSPEED_HIGHEST);
	PHASE_FILTER_OFF();
#else
	//MT6816 program enable
	palSetPadMode(MT6816_PROG_EN_GPIO, MT6816_PROG_EN_PIN, PAL_MODE_OUTPUT_PUSHPULL | PAL_STM32_OSPEED_HIGHEST);
	MT6816_PROG_DISABLE();
#endif

	// AUX pin
	AUX_OFF();
	palSetPadMode(AUX_GPIO, AUX_PIN, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);

	// Shutdown latch
	palSetPadMode(HW_SHUTDOWN_GPIO, HW_SHUTDOWN_PIN, PAL_MODE_OUTPUT_PUSHPULL |	PAL_STM32_OSPEED_HIGHEST);
	HW_SHUTDOWN_HOLD_ON();

	// ADC Pins
	palSetPadMode(GPIOA, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOA, 3, PAL_MODE_INPUT_ANALOG);		// ON-OFF button sense
	palSetPadMode(GPIOA, 5, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOB, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOB, 1, PAL_MODE_INPUT_ANALOG);

	palSetPadMode(GPIOC, 0, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 1, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 2, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 3, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 4, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(GPIOC, 5, PAL_MODE_INPUT_ANALOG);
	
	terminal_register_command_callback(
			"fix_throttle",
			"Usage: fix_throttle [1 or 0]",
			0,
			terminal_cmd_set_m600_use_fixed_throttle_level);

	terminal_register_command_callback(
			"correct_encoder",
			"Detect and apply encoder offset",
			0,
			terminal_cmd_m600_correct_encoder_offset);

	hw_override_pairing_done();

	luna_canbus_start();
}

void hw_setup_adc_channels(void) {
	// ADC1 regular channels
	ADC_RegularChannelConfig(ADC1, ADC_Channel_0,  1, ADC_SampleTime_15Cycles);	// 0	SENS1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_10, 2, ADC_SampleTime_15Cycles);	// 3	CURR1
	ADC_RegularChannelConfig(ADC1, ADC_Channel_8,  3, ADC_SampleTime_15Cycles);	// 6	ADC_IND_EXT2
	ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 4, ADC_SampleTime_15Cycles); // 9	TEMP_MOTOR
	ADC_RegularChannelConfig(ADC1, ADC_Channel_9,  5, ADC_SampleTime_15Cycles);	// 12	V_GATE_DRIVER
	ADC_RegularChannelConfig(ADC1, ADC_Channel_5,  6, ADC_SampleTime_15Cycles);	// 15	TEMP_FET

	// ADC2 regular channels
	ADC_RegularChannelConfig(ADC2, ADC_Channel_1,  1, ADC_SampleTime_15Cycles);	// 1	SENS2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_11, 2, ADC_SampleTime_15Cycles);	// 4	CURR2
	ADC_RegularChannelConfig(ADC2, ADC_Channel_6,  3, ADC_SampleTime_15Cycles);	// 7	UNUSED
	ADC_RegularChannelConfig(ADC2, ADC_Channel_15, 4, ADC_SampleTime_15Cycles);	// 10	ADC_IND_EXT
	ADC_RegularChannelConfig(ADC2, ADC_Channel_7,  5, ADC_SampleTime_15Cycles);	// 13	ADC_IND_EXT3
	ADC_RegularChannelConfig(ADC2, ADC_Channel_Vrefint, 6, ADC_SampleTime_15Cycles);// 16	ADC_IND_VREFINT

	// ADC3 regular channels
	ADC_RegularChannelConfig(ADC3, ADC_Channel_2,  1, ADC_SampleTime_15Cycles);	// 2	SENS3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_12, 2, ADC_SampleTime_15Cycles);	// 5	CURR3
	ADC_RegularChannelConfig(ADC3, ADC_Channel_3,  3, ADC_SampleTime_15Cycles);	// 8	ON-OFF button
	ADC_RegularChannelConfig(ADC3, ADC_Channel_13, 4, ADC_SampleTime_15Cycles);	// 11	VBUS
	ADC_RegularChannelConfig(ADC3, ADC_Channel_1,  5, ADC_SampleTime_15Cycles);	// 14	UNUSED
	ADC_RegularChannelConfig(ADC3, ADC_Channel_Vrefint, 6, ADC_SampleTime_15Cycles);// 18	UNUSED

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

volatile float wheel_rpm_filtered = 0;
volatile float trip_odometer = 1.0; //avoids huge consumption numbers in the gauges

void hw_update_speed_sensor(void) {
	static float wheel_rpm = 0;
	static uint8_t sensor_state = 0;
	static uint8_t sensor_state_old = 0;
	static float last_sensor_event_time = 0;
	float current_time = (float)chVTGetSystemTimeX() / (float)CH_CFG_ST_FREQUENCY;

	sensor_state = palReadPad(HW_SPEED_SENSOR_PORT, HW_SPEED_SENSOR_PIN);

	if(sensor_state == 0 && sensor_state_old == 1 ) {
		float revolution_duration = current_time - last_sensor_event_time;

		if (revolution_duration > 0.11) {	//ignore periods <110ms, which is about 68km/h
			last_sensor_event_time = current_time;
			wheel_rpm = 60.0 / revolution_duration;
			UTILS_LP_FAST(wheel_rpm_filtered, (float)wheel_rpm, 0.5);

			// For some reason a race condition on startup crashes the OS if this is executed too soon.
			// So don't track odometer for the first 4 seconds
			if(current_time > 4.0) {
				trip_odometer += mc_interface_get_configuration()->si_wheel_diameter * M_PI;
			}
		}
	} else {
		// After 3 seconds without sensor signal, set RPM as zero
		if ( (current_time - last_sensor_event_time) > 3.0) {
			wheel_rpm_filtered = 0.0;
		}
	}
	sensor_state_old = sensor_state;
}

/* Get speed in m/s */
float hw_get_speed(void) {
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	float speed = wheel_rpm_filtered * conf->si_wheel_diameter * M_PI / 60.0;
	return speed;
}

/* Get trip distance in meters */
float hw_get_distance(void) {
	return trip_odometer;
}

float hw_get_distance_abs(void) {
	return trip_odometer;
}

float hw_get_mosfet_temp_filtered(void) {
	static float mosfet_temp_filtered = 25.0;
	float mosfet_temp = (1.0 / ((logf(NTC_RES(ADC_Value[ADC_IND_TEMP_MOS]) / 10000.0) / 3455.0) + (1.0 / 298.15)) - 273.15);

	UTILS_LP_FAST(mosfet_temp_filtered, (float)mosfet_temp, 0.1);
	return mosfet_temp_filtered;
}

bool hw_luna_m600_shutdown_button_down(void) {
	static float button_filtered = 0.0;
	UTILS_LP_FAST(button_filtered, GET_ON_OFF_BUTTON_VOLTAGE(), 0.01);
	return (button_filtered < 1.35);
}

bool hw_luna_m600_minus_button_down(void) {
	static float button_filtered = 0.0;
	UTILS_LP_FAST(button_filtered, GET_ON_OFF_BUTTON_VOLTAGE(), 0.05);
	return (button_filtered >= 1.35 && button_filtered < 2.0);
}

static void terminal_cmd_set_m600_use_fixed_throttle_level(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	eeprom_var use_fixed_throttle;
	if( argc == 2 ) {
		char throttle_type[32];

		sscanf(argv[1], "%s", throttle_type);

		use_fixed_throttle.as_i32 = (throttle_type[0] == '1') ? 1 : 0;

		// Store data in eeprom
		conf_general_store_eeprom_var_hw(&use_fixed_throttle, EEPROM_ADDR_FIXED_THROTTLE_LEVEL);
	}
	else {
		commands_printf("1 argument required: 1 (fixed) or 0 (follow display level)");
	}
	return;
}

bool hw_m600_has_fixed_throttle_level(void) {
	eeprom_var use_fixed_throttle;
	bool var_not_found = !conf_general_read_eeprom_var_hw(&use_fixed_throttle, EEPROM_ADDR_FIXED_THROTTLE_LEVEL);

	if( (use_fixed_throttle.as_i32 != 1) || var_not_found) {
		return false;
	} else {
		return true;
	}
}

float hw_get_PAS_torque(void) {
	return luna_canbus_get_PAS_torque();
}

float hw_get_encoder_error(void) {
	static float angle_diff_filtered = 0.0;
	float angle_diff = 0.0;
	
	// some batches have only 2 current sensors, so better rely only in
	// the phase voltage tracker which runs with no modulation and is
	// accurate at mid-high rpm
	if(mc_interface_get_state() != MC_STATE_OFF && mc_interface_get_duty_cycle_now() > 0.4) {
		angle_diff = utils_angle_difference(mcpwm_foc_get_phase_encoder(), mcpwm_foc_get_phase_observer());
	}

	UTILS_LP_FAST(angle_diff_filtered, angle_diff, 0.005);
	return angle_diff_filtered;
}

void hw_recover_encoder_offset(void) {
	if (mc_interface_get_configuration()->foc_encoder_offset > 360.0 ) {
		//invalid encoder offset found in flash, probably its the first boot after a fw update
		if (m600_backup->encoder_offset_calibration_done && m600_backup->encoder_offset <= 360.0) {
			//invalid offset found in flash, but there is a valid offset backed up. Use it.
			mc_configuration *mcconf = mempools_alloc_mcconf();
			*mcconf = *mc_interface_get_configuration();

			mcconf->foc_encoder_offset = m600_backup->encoder_offset;
			mc_interface_set_configuration(mcconf);
			conf_general_store_mc_configuration(mcconf, false);

			mempools_free_mcconf(mcconf);
		}
	}

	else {
		// encoder offset is valid, lets check that the backed up value is the same
		float mc_conf_encoder_offset = mc_interface_get_configuration()->foc_encoder_offset;

		if(mc_conf_encoder_offset <= 360.0) {
			//user could have written a new offset angle. Update the backup.
			if(m600_backup->encoder_offset_calibration_done == false || mc_conf_encoder_offset != m600_backup->encoder_offset) {
				m600_backup->encoder_offset = mc_conf_encoder_offset;
				m600_backup->encoder_offset_calibration_done = true;
				conf_general_store_backup_data();
			}
		}
	}
}

static void terminal_cmd_m600_correct_encoder_offset(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	mc_configuration *mcconf = mempools_alloc_mcconf();
	*mcconf = *mc_interface_get_configuration();
	mc_configuration *mcconf_old = mempools_alloc_mcconf();
	*mcconf_old = *mcconf;

	float current = 15.0;

	mcconf->motor_type = MOTOR_TYPE_FOC;
	mcconf->foc_f_zv = 10000.0;
	mcconf->foc_current_kp = 0.01;
	mcconf->foc_current_ki = 10.0;
	mc_interface_set_configuration(mcconf);

	float offset = 0.0;
	float ratio = 0.0;
	bool inverted = false;
	mcpwm_foc_encoder_detect(current, false, &offset, &ratio, &inverted);

	mcconf_old->foc_encoder_offset = offset;
	mcconf->foc_encoder_offset = offset;
	mc_interface_set_configuration(mcconf_old);
	mc_interface_set_configuration(mcconf);

	mempools_free_mcconf(mcconf);
	mempools_free_mcconf(mcconf_old);

	return;
}

// Users are getting locked out because they don't know what pairing means. Lets disable the pairing
static void hw_override_pairing_done(void) {
	if( app_get_configuration()->pairing_done == true) {
		app_configuration *appconf = mempools_alloc_appconf();
		*appconf = *app_get_configuration();
		appconf->pairing_done = false;

		conf_general_store_app_configuration(appconf);
		app_set_configuration(appconf);

		mempools_free_appconf(appconf);
	}
}
