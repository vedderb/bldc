/*
 * conf_general.c
 *
 *  Created on: 14 sep 2014
 *      Author: benjamin
 */

#include "conf_general.h"
#include "ch.h"
#include "eeprom.h"
#include "mcpwm.h"
#include "hw.h"
#include "utils.h"

#include <string.h>

// Default configuration file
#ifdef MCCONF_OUTRUNNER1
#include "mcconf_outrunner1.h"
#elif defined MCCONF_OUTRUNNER2
#include "mcconf_outrunner2.h"
#elif defined MCCONF_STEN
#include "mcconf_sten.h"
#endif

// Parameters that can be overridden
#ifndef MC_DEFAULT_MOTOR_TYPE
#define MC_DEFAULT_MOTOR_TYPE			MOTOR_TYPE_BLDC
#endif
#ifndef MCPWM_PWM_MODE
#define MCPWM_PWM_MODE					PWM_MODE_SYNCHRONOUS // Default PWM mode
#endif
#ifndef MCPWM_MIN_VOLTAGE
#define MCPWM_MIN_VOLTAGE				8.0		// Minimum input voltage
#endif
#ifndef MCPWM_MAX_VOLTAGE
#define MCPWM_MAX_VOLTAGE				50.0	// Maximum input voltage
#endif
#ifndef MCPWM_RPM_MAX
#define MCPWM_RPM_MAX					100000.0	// The motor speed limit (Upper)
#endif
#ifndef MCPWM_RPM_MIN
#define MCPWM_RPM_MIN					-100000.0	// The motor speed limit (Lower)
#endif
#ifndef MCPWM_CURRENT_STARTUP_BOOST
#define MCPWM_CURRENT_STARTUP_BOOST		0.01	// The lowest duty cycle to use in current control mode (has to be > MCPWM_MIN_DUTY_CYCLE)
#endif
#ifndef MCPWM_RPM_LIMIT_NEG_TORQUE
#define MCPWM_RPM_LIMIT_NEG_TORQUE		true		// Use negative torque to limit the RPM
#endif
#ifndef MCPWM_CURR_MAX_RPM_FBRAKE
#define MCPWM_CURR_MAX_RPM_FBRAKE		300	// Maximum electrical RPM to use full brake at
#endif
#ifndef MCPWM_CURR_MAX_RPM_FBRAKE_CC
#define MCPWM_CURR_MAX_RPM_FBRAKE_CC	1500	// Maximum electrical RPM to use full brake at with current control
#endif
#ifndef MCPWM_SLOW_ABS_OVERCURRENT
#define MCPWM_SLOW_ABS_OVERCURRENT		false	// Use the filtered (and hence slower) current for the overcurrent fault detection
#endif
#ifndef MCPWM_COMM_MODE
#define MCPWM_COMM_MODE					COMM_MODE_INTEGRATE	// The commutation mode to use
#endif
#ifndef MCPWM_CYCLE_INT_LIMIT_HIGH_FAC
#define MCPWM_CYCLE_INT_LIMIT_HIGH_FAC	0.8		// Flux integrator limit percentage at MCPWM_CYCLE_INT_START_RPM_BR ERPM
#endif
#ifndef MCPWM_CYCLE_INT_START_RPM_BR
#define MCPWM_CYCLE_INT_START_RPM_BR	80000.0	// RPM border between the START and LOW interval
#endif
#ifndef MCPWM_FAULT_STOP_TIME
#define MCPWM_FAULT_STOP_TIME			3000	// Ignore commands for this duration in msec when faults occur
#endif
#ifndef MCPWM_LIM_TEMP_FET_START
#define MCPWM_LIM_TEMP_FET_START		80.0	// MOSFET temperature where current limiting should begin
#endif
#ifndef MCPWM_LIM_TEMP_FET_END
#define MCPWM_LIM_TEMP_FET_END			100.0	// MOSFET temperature where everything should be shut off
#endif
#ifndef MCPWM_LIM_TEMP_MOTOR_START
#define MCPWM_LIM_TEMP_MOTOR_START		80.0	// MOTOR temperature where current limiting should begin
#endif
#ifndef MCPWM_LIM_TEMP_MOTOR_END
#define MCPWM_LIM_TEMP_MOTOR_END		100.0	// MOTOR temperature where everything should be shut off
#endif
#ifndef MCPWM_MAX_FB_CURR_DIR_CHANGE
#define MCPWM_MAX_FB_CURR_DIR_CHANGE	10.0	// Maximum current during full brake during which a direction change is allowed
#endif
#ifndef MCPWM_MIN_DUTY
#define MCPWM_MIN_DUTY					0.005	// Minimum duty cycle
#endif
#ifndef MCPWM_MAX_DUTY
#define MCPWM_MAX_DUTY					0.95	// Maximum duty cycle
#endif
#ifndef MCPWM_HALL_ERPM
#define MCPWM_HALL_ERPM					2000.0	// ERPM above which sensorless commutation is used in hybrid mode
#endif
// Default hall sensor table
#ifndef MCPWM_HALL_TAB_0
#define MCPWM_HALL_TAB_0				-1
#endif
#ifndef MCPWM_HALL_TAB_1
#define MCPWM_HALL_TAB_1				1
#endif
#ifndef MCPWM_HALL_TAB_2
#define MCPWM_HALL_TAB_2				3
#endif
#ifndef MCPWM_HALL_TAB_3
#define MCPWM_HALL_TAB_3				2
#endif
#ifndef MCPWM_HALL_TAB_4
#define MCPWM_HALL_TAB_4				5
#endif
#ifndef MCPWM_HALL_TAB_5
#define MCPWM_HALL_TAB_5				6
#endif
#ifndef MCPWM_HALL_TAB_6
#define MCPWM_HALL_TAB_6				4
#endif
#ifndef MCPWM_HALL_TAB_7
#define MCPWM_HALL_TAB_7				-1
#endif

// EEPROM settings
#define EEPROM_BASE_MCCONF		1000
#define EEPROM_BASE_APPCONF		2000

// Global variables
uint16_t VirtAddVarTab[NB_OF_VAR];

void conf_general_init(void) {
	// First, make sure that all relevant virtual addresses are assigned for page swapping.
	memset(VirtAddVarTab, 0, sizeof(VirtAddVarTab));

	int ind = 0;
	for (unsigned int i = 0;i < (sizeof(app_configuration) / 2);i++) {
		VirtAddVarTab[ind++] = EEPROM_BASE_MCCONF + i;
	}

	for (unsigned int i = 0;i < (sizeof(app_configuration) / 2);i++) {
		VirtAddVarTab[ind++] = EEPROM_BASE_APPCONF + i;
	}

	FLASH_Unlock();
	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);
	EE_Init();
}

/**
 * Read app_configuration from EEPROM. If this fails, default values will be used.
 *
 * @param conf
 * A pointer to a app_configuration struct to write the read configuration to.
 */
void conf_general_read_app_configuration(app_configuration *conf) {
	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;
	uint16_t var;

	for (unsigned int i = 0;i < (sizeof(app_configuration) / 2);i++) {
		if (EE_ReadVariable(EEPROM_BASE_APPCONF + i, &var) == 0) {
			conf_addr[2 * i] = (var >> 8) & 0xFF;
			conf_addr[2 * i + 1] = var & 0xFF;
		} else {
			is_ok = false;
			break;
		}
	}

	// Set the default configuration
	if (!is_ok) {
		memset(conf, 0, sizeof(app_configuration));
		conf->controller_id = 0;
		conf->timeout_msec = 1000;
		conf->timeout_brake_current = 0.0;
		conf->send_can_status = false;
		conf->send_can_status_rate_hz = 500;

		// The default app is UART in case the UART port is used for
		// firmware updates.
		conf->app_to_use = APP_UART;

		conf->app_ppm_conf.ctrl_type = PPM_CTRL_TYPE_NONE;
		conf->app_ppm_conf.pid_max_erpm = 15000;
		conf->app_ppm_conf.hyst = 0.15;
		conf->app_ppm_conf.pulse_start = 1.0;
		conf->app_ppm_conf.pulse_end = 2.0;
		conf->app_ppm_conf.median_filter = false;
		conf->app_ppm_conf.safe_start = true;
		conf->app_ppm_conf.rpm_lim_start = 150000.0;
		conf->app_ppm_conf.rpm_lim_end = 200000.0;
		conf->app_ppm_conf.multi_esc = false;
		conf->app_ppm_conf.tc = false;
		conf->app_ppm_conf.tc_max_diff = 3000.0;

		conf->app_adc_conf.ctrl_type = ADC_CTRL_TYPE_NONE;
		conf->app_adc_conf.hyst = 0.15;
		conf->app_adc_conf.voltage_start = 0.9;
		conf->app_adc_conf.voltage_end = 3.0;
		conf->app_adc_conf.use_filter = true;
		conf->app_adc_conf.safe_start = true;
		conf->app_adc_conf.button_inverted = false;
		conf->app_adc_conf.voltage_inverted = false;
		conf->app_adc_conf.rpm_lim_start = 150000;
		conf->app_adc_conf.rpm_lim_end = 200000;
		conf->app_adc_conf.multi_esc = false;
		conf->app_adc_conf.tc = false;
		conf->app_adc_conf.tc_max_diff = 3000.0;
		conf->app_adc_conf.update_rate_hz = 500;

		conf->app_uart_baudrate = 115200;

		conf->app_chuk_conf.ctrl_type = CHUK_CTRL_TYPE_CURRENT;
		conf->app_chuk_conf.hyst = 0.15;
		conf->app_chuk_conf.rpm_lim_start = 150000.0;
		conf->app_chuk_conf.rpm_lim_end = 250000.0;
		conf->app_chuk_conf.ramp_time_pos = 0.5;
		conf->app_chuk_conf.ramp_time_neg = 0.25;
		conf->app_chuk_conf.multi_esc = true;
		conf->app_chuk_conf.tc = false;
		conf->app_chuk_conf.tc_max_diff = 3000.0;
	}
}

/**
 * Write app_configuration to EEPROM.
 *
 * @param conf
 * A pointer to the configuration that should be stored.
 */
bool conf_general_store_app_configuration(app_configuration *conf) {
	mcpwm_unlock();
	mcpwm_release_motor();

	utils_sys_lock_cnt();
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, DISABLE);

	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;
	uint16_t var;

	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

	for (unsigned int i = 0;i < (sizeof(app_configuration) / 2);i++) {
		var = (conf_addr[2 * i] << 8) & 0xFF00;
		var |= conf_addr[2 * i + 1] & 0xFF;

		if (EE_WriteVariable(EEPROM_BASE_APPCONF + i, var) != FLASH_COMPLETE) {
			is_ok = false;
			break;
		}
	}

	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, ENABLE);
	utils_sys_unlock_cnt();

	return is_ok;
}

/**
 * Read mc_configuration from EEPROM. If this fails, default values will be used.
 *
 * @param conf
 * A pointer to a mc_configuration struct to write the read configuration to.
 */
void conf_general_read_mc_configuration(mc_configuration *conf) {
	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;
	uint16_t var;

	for (unsigned int i = 0;i < (sizeof(mc_configuration) / 2);i++) {
		if (EE_ReadVariable(EEPROM_BASE_MCCONF + i, &var) == 0) {
			conf_addr[2 * i] = (var >> 8) & 0xFF;
			conf_addr[2 * i + 1] = var & 0xFF;
		} else {
			is_ok = false;
			break;
		}
	}

	if (!is_ok) {
		conf->pwm_mode = MCPWM_PWM_MODE;
		conf->comm_mode = MCPWM_COMM_MODE;
		conf->motor_type = MC_DEFAULT_MOTOR_TYPE;
		conf->sensor_mode = MCPWM_SENSOR_MODE;

		conf->l_current_max = MCPWM_CURRENT_MAX;
		conf->l_current_min = MCPWM_CURRENT_MIN;
		conf->l_in_current_max = MCPWM_IN_CURRENT_MAX;
		conf->l_in_current_min = MCPWM_IN_CURRENT_MIN;
		conf->l_abs_current_max = MCPWM_MAX_ABS_CURRENT;
		conf->l_min_erpm = MCPWM_RPM_MIN;
		conf->l_max_erpm = MCPWM_RPM_MAX;
		conf->l_max_erpm_fbrake = MCPWM_CURR_MAX_RPM_FBRAKE;
		conf->l_max_erpm_fbrake_cc = MCPWM_CURR_MAX_RPM_FBRAKE_CC;
		conf->l_min_vin = MCPWM_MIN_VOLTAGE;
		conf->l_max_vin = MCPWM_MAX_VOLTAGE;
		conf->l_slow_abs_current = MCPWM_SLOW_ABS_OVERCURRENT;
		conf->l_rpm_lim_neg_torque = MCPWM_RPM_LIMIT_NEG_TORQUE;
		conf->l_temp_fet_start = MCPWM_LIM_TEMP_FET_START;
		conf->l_temp_fet_end = MCPWM_LIM_TEMP_FET_END;
		conf->l_temp_motor_start = MCPWM_LIM_TEMP_MOTOR_START;
		conf->l_temp_motor_end = MCPWM_LIM_TEMP_MOTOR_END;
		conf->l_min_duty = MCPWM_MIN_DUTY;
		conf->l_max_duty = MCPWM_MAX_DUTY;

		conf->lo_current_max = conf->l_current_max;
		conf->lo_current_min = conf->l_current_min;
		conf->lo_in_current_max = conf->l_in_current_max;
		conf->lo_in_current_min = conf->l_in_current_min;

		conf->sl_min_erpm = MCPWM_MIN_RPM;
		conf->sl_max_fullbreak_current_dir_change = MCPWM_MAX_FB_CURR_DIR_CHANGE;
		conf->sl_min_erpm_cycle_int_limit = MCPWM_CYCLE_INT_LIMIT_MIN_RPM;
		conf->sl_cycle_int_limit = MCPWM_CYCLE_INT_LIMIT;
		conf->sl_phase_advance_at_br = MCPWM_CYCLE_INT_LIMIT_HIGH_FAC;
		conf->sl_cycle_int_rpm_br = MCPWM_CYCLE_INT_START_RPM_BR;
		conf->sl_bemf_coupling_k = MCPWM_BEMF_INPUT_COUPLING_K;

		conf->hall_table[0] = MCPWM_HALL_TAB_0;
		conf->hall_table[1] = MCPWM_HALL_TAB_1;
		conf->hall_table[2] = MCPWM_HALL_TAB_2;
		conf->hall_table[3] = MCPWM_HALL_TAB_3;
		conf->hall_table[4] = MCPWM_HALL_TAB_4;
		conf->hall_table[5] = MCPWM_HALL_TAB_5;
		conf->hall_table[6] = MCPWM_HALL_TAB_6;
		conf->hall_table[7] = MCPWM_HALL_TAB_7;
		conf->hall_sl_erpm = MCPWM_HALL_ERPM;

		conf->s_pid_kp = MCPWM_PID_KP;
		conf->s_pid_ki = MCPWM_PID_KI;
		conf->s_pid_kd = MCPWM_PID_KD;
		conf->s_pid_min_rpm = MCPWM_PID_MIN_RPM;

		conf->p_pid_kp = MCPWM_P_PID_KP;
		conf->p_pid_ki = MCPWM_P_PID_KI;
		conf->p_pid_kd = MCPWM_P_PID_KD;

		conf->cc_startup_boost_duty = MCPWM_CURRENT_STARTUP_BOOST;
		conf->cc_min_current = MCPWM_CURRENT_CONTROL_MIN;
		conf->cc_gain = MCPWM_CURRENT_CONTROL_GAIN;
		conf->cc_ramp_step_max = 0.04;

		conf->m_fault_stop_time_ms = MCPWM_FAULT_STOP_TIME;
	}
}

/**
 * Write mc_configuration to EEPROM.
 *
 * @param conf
 * A pointer to the configuration that should be stored.
 */
bool conf_general_store_mc_configuration(mc_configuration *conf) {
	mcpwm_unlock();
	mcpwm_release_motor();

	utils_sys_lock_cnt();
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, DISABLE);

	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;
	uint16_t var;

	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

	for (unsigned int i = 0;i < (sizeof(mc_configuration) / 2);i++) {
		var = (conf_addr[2 * i] << 8) & 0xFF00;
		var |= conf_addr[2 * i + 1] & 0xFF;

		if (EE_WriteVariable(EEPROM_BASE_MCCONF + i, var) != FLASH_COMPLETE) {
			is_ok = false;
			break;
		}
	}

	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, ENABLE);
	utils_sys_unlock_cnt();

	return is_ok;
}

bool conf_general_detect_motor_param(float current, float min_rpm, float low_duty,
		float *int_limit, float *bemf_coupling_k, int8_t *hall_table, int *hall_res) {

	int ok_steps = 0;
	const float spinup_to_duty = 0.6;

	mc_configuration mcconf_old = *mcpwm_get_configuration();
	mc_configuration mcconf = *mcpwm_get_configuration();

	mcconf.sensor_mode = SENSOR_MODE_SENSORLESS;
	mcconf.comm_mode = COMM_MODE_DELAY;
	mcconf.sl_phase_advance_at_br = 1.0;
	mcconf.sl_min_erpm = min_rpm;
	mcpwm_set_configuration(&mcconf);

	mcpwm_lock();

	mcpwm_lock_override_once();
	mcpwm_set_current(current);

	// Spin up the motor
	for (int i = 0;i < 5000;i++) {
		if (mcpwm_get_duty_cycle_now() < spinup_to_duty) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	// Reset hall sensor samples
	mcpwm_reset_hall_detect_table();

	// Run for a while to get hall sensor samples
	mcpwm_lock_override_once();
	mcpwm_set_duty(spinup_to_duty);
	chThdSleepMilliseconds(400);

	// Release the motor and wait a few commutations
	mcpwm_lock_override_once();
	mcpwm_set_current(0.0);
	int tacho = mcpwm_get_tachometer_value(0);
	for (int i = 0;i < 2000;i++) {
		if ((mcpwm_get_tachometer_value(0) - tacho) < 3) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	// Average the cycle integrator for 50 commutations
	mcpwm_read_reset_avg_cycle_integrator();
	tacho = mcpwm_get_tachometer_value(0);
	for (int i = 0;i < 3000;i++) {
		if ((mcpwm_get_tachometer_value(0) - tacho) < 50) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	// Get hall detect result
	*hall_res = mcpwm_get_hall_detect_result(hall_table);

	*int_limit = mcpwm_read_reset_avg_cycle_integrator();

	// Wait for the motor to slow down
	for (int i = 0;i < 5000;i++) {
		if (mcpwm_get_duty_cycle_now() > low_duty) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	mcpwm_lock_override_once();
	mcpwm_set_duty(low_duty);

	// Average the cycle integrator for 100 commutations
	mcpwm_read_reset_avg_cycle_integrator();
	tacho = mcpwm_get_tachometer_value(0);
	float rpm_sum = 0.0;
	float rpm_iterations = 0.0;
	for (int i = 0;i < 3000;i++) {
		if ((mcpwm_get_tachometer_value(0) - tacho) < 100) {
			rpm_sum += mcpwm_get_rpm();
			rpm_iterations += 1;
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	float avg_cycle_integrator_running = mcpwm_read_reset_avg_cycle_integrator();
	float rpm = rpm_sum / rpm_iterations;

	mcpwm_lock_override_once();
	mcpwm_release_motor();

	// Try to figure out the coupling factor
	avg_cycle_integrator_running -= *int_limit;
	avg_cycle_integrator_running /= (float)ADC_Value[ADC_IND_VIN_SENS];
	avg_cycle_integrator_running *= rpm;
	*bemf_coupling_k = avg_cycle_integrator_running;

	// Restore settings
	mcpwm_set_configuration(&mcconf_old);

	mcpwm_unlock();

	return ok_steps == 5 ? true : false;
}
