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
#include "mc_interface.h"
#include "hw.h"
#include "utils.h"
#include "stm32f4xx_conf.h"
#include "timeout.h"

#include <string.h>
#include <math.h>

// User defined default motor configuration file
#ifdef MCCONF_DEFAULT_USER
#include MCCONF_DEFAULT_USER
#endif

// User defined default app configuration file
#ifdef APPCONF_DEFAULT_USER
#include APPCONF_DEFAULT_USER
#endif

// Default configuration parameters that can be overridden
#include "mcconf_default.h"
#include "appconf_default.h"

// EEPROM settings
#define EEPROM_BASE_MCCONF		1000
#define EEPROM_BASE_APPCONF		2000

// Global variables
uint16_t VirtAddVarTab[NB_OF_VAR];

// Private variables
mc_configuration mcconf, mcconf_old;

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
 * Load the compiled default app_configuration.
 *
 * @param conf
 * A pointer to store the default configuration to.
 */
void conf_general_get_default_app_configuration(app_configuration *conf) {
	memset(conf, 0, sizeof(app_configuration));
	conf->controller_id = APPCONF_CONTROLLER_ID;
	conf->timeout_msec = APPCONF_TIMEOUT_MSEC;
	conf->timeout_brake_current = APPCONF_TIMEOUT_BRAKE_CURRENT;
	conf->send_can_status = APPCONF_SEND_CAN_STATUS;
	conf->send_can_status_rate_hz = APPCONF_SEND_CAN_STATUS_RATE_HZ;

	conf->app_to_use = APPCONF_APP_TO_USE;

	conf->app_ppm_conf.ctrl_type = APPCONF_PPM_CTRL_TYPE;
	conf->app_ppm_conf.pid_max_erpm = APPCONF_PPM_PID_MAX_ERPM;
	conf->app_ppm_conf.hyst = APPCONF_PPM_HYST;
	conf->app_ppm_conf.pulse_start = APPCONF_PPM_PULSE_START;
	conf->app_ppm_conf.pulse_end = APPCONF_PPM_PULSE_END;
	conf->app_ppm_conf.median_filter = APPCONF_PPM_MEDIAN_FILTER;
	conf->app_ppm_conf.safe_start = APPCONF_PPM_SAFE_START;
	conf->app_ppm_conf.rpm_lim_start = APPCONF_PPM_RPM_LIM_START;
	conf->app_ppm_conf.rpm_lim_end = APPCONF_PPM_RPM_LIM_END;
	conf->app_ppm_conf.multi_esc = APPCONF_PPM_MULTI_ESC;
	conf->app_ppm_conf.tc = APPCONF_PPM_TC;
	conf->app_ppm_conf.tc_max_diff = APPCONF_PPM_TC_MAX_DIFF;

	conf->app_adc_conf.ctrl_type = APPCONF_ADC_CTRL_TYPE;
	conf->app_adc_conf.hyst = APPCONF_ADC_HYST;
	conf->app_adc_conf.voltage_start = APPCONF_ADC_VOLTAGE_START;
	conf->app_adc_conf.voltage_end = APPCONF_ADC_VOLTAGE_END;
	conf->app_adc_conf.use_filter = APPCONF_ADC_USE_FILTER;
	conf->app_adc_conf.safe_start = APPCONF_ADC_SAFE_START;
	conf->app_adc_conf.cc_button_inverted = APPCONF_ADC_CC_BUTTON_INVERTED;
	conf->app_adc_conf.rev_button_inverted = APPCONF_ADC_REV_BUTTON_INVERTED;
	conf->app_adc_conf.voltage_inverted = APPCONF_ADC_VOLTAGE_INVERTED;
	conf->app_adc_conf.rpm_lim_start = APPCONF_ADC_RPM_LIM_START;
	conf->app_adc_conf.rpm_lim_end = APPCONF_ADC_RPM_LIM_END;
	conf->app_adc_conf.multi_esc = APPCONF_ADC_MULTI_ESC;
	conf->app_adc_conf.tc = APPCONF_ADC_TC;
	conf->app_adc_conf.tc_max_diff = APPCONF_ADC_TC_MAX_DIFF;
	conf->app_adc_conf.update_rate_hz = APPCONF_ADC_UPDATE_RATE_HZ;

	conf->app_uart_baudrate = APPCONF_UART_BAUDRATE;

	conf->app_chuk_conf.ctrl_type = APPCONF_CHUK_CTRL_TYPE;
	conf->app_chuk_conf.hyst = APPCONF_CHUK_HYST;
	conf->app_chuk_conf.rpm_lim_start = APPCONF_CHUK_RPM_LIM_START;
	conf->app_chuk_conf.rpm_lim_end = APPCONF_CHUK_RPM_LIM_END;
	conf->app_chuk_conf.ramp_time_pos = APPCONF_CHUK_RAMP_TIME_POS;
	conf->app_chuk_conf.ramp_time_neg = APPCONF_CHUK_RAMP_TIME_NEG;
	conf->app_chuk_conf.stick_erpm_per_s_in_cc = APPCONF_STICK_ERPM_PER_S_IN_CC;
	conf->app_chuk_conf.multi_esc = APPCONF_CHUK_MULTI_ESC;
	conf->app_chuk_conf.tc = APPCONF_CHUK_TC;
	conf->app_chuk_conf.tc_max_diff = APPCONF_CHUK_TC_MAX_DIFF;
}

/**
 * Load the compiled default mc_configuration.
 *
 * @param conf
 * A pointer to store the default configuration to.
 */
void conf_general_get_default_mc_configuration(mc_configuration *conf) {
	memset(conf, 0, sizeof(mc_configuration));
	conf->pwm_mode = MCCONF_PWM_MODE;
	conf->comm_mode = MCCONF_COMM_MODE;
	conf->motor_type = MCCONF_DEFAULT_MOTOR_TYPE;
	conf->sensor_mode = MCCONF_SENSOR_MODE;

	conf->l_current_max = MCCONF_L_CURRENT_MAX;
	conf->l_current_min = MCCONF_L_CURRENT_MIN;
	conf->l_in_current_max = MCCONF_L_IN_CURRENT_MAX;
	conf->l_in_current_min = MCCONF_L_IN_CURRENT_MIN;
	conf->l_abs_current_max = MCCONF_L_MAX_ABS_CURRENT;
	conf->l_min_erpm = MCCONF_L_RPM_MIN;
	conf->l_max_erpm = MCCONF_L_RPM_MAX;
	conf->l_max_erpm_fbrake = MCCONF_L_CURR_MAX_RPM_FBRAKE;
	conf->l_max_erpm_fbrake_cc = MCCONF_L_CURR_MAX_RPM_FBRAKE_CC;
	conf->l_min_vin = MCCONF_L_MIN_VOLTAGE;
	conf->l_max_vin = MCCONF_L_MAX_VOLTAGE;
	conf->l_battery_cut_start = MCCONF_L_BATTERY_CUT_START;
	conf->l_battery_cut_end = MCCONF_L_BATTERY_CUT_END;
	conf->l_slow_abs_current = MCCONF_L_SLOW_ABS_OVERCURRENT;
	conf->l_rpm_lim_neg_torque = MCCONF_L_RPM_LIMIT_NEG_TORQUE;
	conf->l_temp_fet_start = MCCONF_L_LIM_TEMP_FET_START;
	conf->l_temp_fet_end = MCCONF_L_LIM_TEMP_FET_END;
	conf->l_temp_motor_start = MCCONF_L_LIM_TEMP_MOTOR_START;
	conf->l_temp_motor_end = MCCONF_L_LIM_TEMP_MOTOR_END;
	conf->l_min_duty = MCCONF_L_MIN_DUTY;
	conf->l_max_duty = MCCONF_L_MAX_DUTY;

	conf->lo_current_max = conf->l_current_max;
	conf->lo_current_min = conf->l_current_min;
	conf->lo_in_current_max = conf->l_in_current_max;
	conf->lo_in_current_min = conf->l_in_current_min;

	conf->sl_min_erpm = MCCONF_SL_MIN_RPM;
	conf->sl_max_fullbreak_current_dir_change = MCCONF_SL_MAX_FB_CURR_DIR_CHANGE;
	conf->sl_min_erpm_cycle_int_limit = MCCONF_SL_MIN_ERPM_CYCLE_INT_LIMIT;
	conf->sl_cycle_int_limit = MCCONF_SL_CYCLE_INT_LIMIT;
	conf->sl_phase_advance_at_br = MCCONF_SL_PHASE_ADVANCE_AT_BR;
	conf->sl_cycle_int_rpm_br = MCCONF_SL_CYCLE_INT_BR;
	conf->sl_bemf_coupling_k = MCCONF_SL_BEMF_COUPLING_K;

	conf->hall_table[0] = MCCONF_HALL_TAB_0;
	conf->hall_table[1] = MCCONF_HALL_TAB_1;
	conf->hall_table[2] = MCCONF_HALL_TAB_2;
	conf->hall_table[3] = MCCONF_HALL_TAB_3;
	conf->hall_table[4] = MCCONF_HALL_TAB_4;
	conf->hall_table[5] = MCCONF_HALL_TAB_5;
	conf->hall_table[6] = MCCONF_HALL_TAB_6;
	conf->hall_table[7] = MCCONF_HALL_TAB_7;
	conf->hall_sl_erpm = MCCONF_HALL_ERPM;

	conf->foc_current_kp = MCCONF_FOC_CURRENT_KP;
	conf->foc_current_ki = MCCONF_FOC_CURRENT_KI;
	conf->foc_f_sw = MCCONF_FOC_F_SW;
	conf->foc_dt_us = MCCONF_FOC_DT_US;
	conf->foc_encoder_inverted = MCCONF_FOC_ENCODER_INVERTED;
	conf->foc_encoder_offset = MCCONF_FOC_ENCODER_OFFSET;
	conf->foc_encoder_ratio = MCCONF_FOC_ENCODER_RATIO;
	conf->foc_sensor_mode = MCCONF_FOC_SENSOR_MODE;
	conf->foc_pll_kp = MCCONF_FOC_PLL_KP;
	conf->foc_pll_ki = MCCONF_FOC_PLL_KI;
	conf->foc_motor_l = MCCONF_FOC_MOTOR_L;
	conf->foc_motor_r = MCCONF_FOC_MOTOR_R;
	conf->foc_motor_flux_linkage = MCCONF_FOC_MOTOR_FLUX_LINKAGE;
	conf->foc_observer_gain = MCCONF_FOC_OBSERVER_GAIN;
	conf->foc_duty_dowmramp_kp = MCCONF_FOC_DUTY_DOWNRAMP_KP;
	conf->foc_duty_dowmramp_ki = MCCONF_FOC_DUTY_DOWNRAMP_KI;
	conf->foc_openloop_rpm = MCCONF_FOC_OPENLOOP_RPM;
	conf->foc_sl_openloop_hyst = MCCONF_FOC_SL_OPENLOOP_HYST;
	conf->foc_sl_openloop_time = MCCONF_FOC_SL_OPENLOOP_TIME;
	conf->foc_sl_d_current_duty = MCCONF_FOC_SL_D_CURRENT_DUTY;
	conf->foc_sl_d_current_factor = MCCONF_FOC_SL_D_CURRENT_FACTOR;
	conf->foc_hall_table[0] = MCCONF_FOC_HALL_TAB_0;
	conf->foc_hall_table[1] = MCCONF_FOC_HALL_TAB_1;
	conf->foc_hall_table[2] = MCCONF_FOC_HALL_TAB_2;
	conf->foc_hall_table[3] = MCCONF_FOC_HALL_TAB_3;
	conf->foc_hall_table[4] = MCCONF_FOC_HALL_TAB_4;
	conf->foc_hall_table[5] = MCCONF_FOC_HALL_TAB_5;
	conf->foc_hall_table[6] = MCCONF_FOC_HALL_TAB_6;
	conf->foc_hall_table[7] = MCCONF_FOC_HALL_TAB_7;
	conf->foc_hall_sl_erpm = MCCONF_FOC_HALL_ERPM;

	conf->s_pid_kp = MCCONF_S_PID_KP;
	conf->s_pid_ki = MCCONF_S_PID_KI;
	conf->s_pid_kd = MCCONF_S_PID_KD;
	conf->s_pid_min_erpm = MCCONF_S_PID_MIN_RPM;

	conf->p_pid_kp = MCCONF_P_PID_KP;
	conf->p_pid_ki = MCCONF_P_PID_KI;
	conf->p_pid_kd = MCCONF_P_PID_KD;

	conf->cc_startup_boost_duty = MCCONF_CC_STARTUP_BOOST_DUTY;
	conf->cc_min_current = MCCONF_CC_MIN_CURRENT;
	conf->cc_gain = MCCONF_CC_GAIN;
	conf->cc_ramp_step_max = MCCONF_CC_RAMP_STEP;

	conf->m_fault_stop_time_ms = MCCONF_M_FAULT_STOP_TIME;
	conf->m_duty_ramp_step = MCCONF_M_RAMP_STEP;
	conf->m_duty_ramp_step_rpm_lim = MCCONF_M_RAMP_STEP_RPM_LIM;
	conf->m_current_backoff_gain = MCCONF_M_CURRENT_BACKOFF_GAIN;
	conf->m_encoder_counts = MCCONF_M_ENCODER_COUNTS;
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
		conf_general_get_default_app_configuration(conf);
	}
}

/**
 * Write app_configuration to EEPROM.
 *
 * @param conf
 * A pointer to the configuration that should be stored.
 */
bool conf_general_store_app_configuration(app_configuration *conf) {
	mc_interface_unlock();
	mc_interface_release_motor();

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
		conf_general_get_default_mc_configuration(conf);
	}
}

/**
 * Write mc_configuration to EEPROM.
 *
 * @param conf
 * A pointer to the configuration that should be stored.
 */
bool conf_general_store_mc_configuration(mc_configuration *conf) {
	mc_interface_unlock();
	mc_interface_release_motor();

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

	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_BLDC;
	mcconf.sensor_mode = SENSOR_MODE_SENSORLESS;
	mcconf.comm_mode = COMM_MODE_DELAY;
	mcconf.sl_phase_advance_at_br = 1.0;
	mcconf.sl_min_erpm = min_rpm;
	mc_interface_set_configuration(&mcconf);

	mc_interface_lock();

	mc_interface_lock_override_once();
	mc_interface_set_current(current);

	// Spin up the motor
	for (int i = 0;i < 5000;i++) {
		if (mc_interface_get_duty_cycle_now() < spinup_to_duty) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	// Reset hall sensor samples
	mcpwm_reset_hall_detect_table();

	// Run for a while to get hall sensor samples
	mc_interface_lock_override_once();
	mc_interface_set_duty(spinup_to_duty);
	chThdSleepMilliseconds(400);

	// Release the motor and wait a few commutations
	mc_interface_lock_override_once();
	mc_interface_set_current(0.0);
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
		if (mc_interface_get_duty_cycle_now() > low_duty) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	mc_interface_lock_override_once();
	mc_interface_set_duty(low_duty);

	// Average the cycle integrator for 100 commutations
	mcpwm_read_reset_avg_cycle_integrator();
	tacho = mcpwm_get_tachometer_value(0);
	float rpm_sum = 0.0;
	float rpm_iterations = 0.0;
	for (int i = 0;i < 3000;i++) {
		if ((mcpwm_get_tachometer_value(0) - tacho) < 100) {
			rpm_sum += mc_interface_get_rpm();
			rpm_iterations += 1;
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	float avg_cycle_integrator_running = mcpwm_read_reset_avg_cycle_integrator();
	float rpm = rpm_sum / rpm_iterations;

	mc_interface_lock_override_once();
	mc_interface_release_motor();

	// Try to figure out the coupling factor
	avg_cycle_integrator_running -= *int_limit;
	avg_cycle_integrator_running /= (float)ADC_Value[ADC_IND_VIN_SENS];
	avg_cycle_integrator_running *= rpm;
	*bemf_coupling_k = avg_cycle_integrator_running;

	// Restore settings
	mc_interface_set_configuration(&mcconf_old);

	mc_interface_unlock();

	return ok_steps == 5 ? true : false;
}

/**
 * Try to measure the motor flux linkage.
 *
 * @param current
 * The current so spin up the motor with.
 *
 * @param duty
 * The duty cycle to maintain.
 *
 * @param min_erpm
 * The minimum ERPM for the delay commutation mode.
 *
 * @param res
 * The motor phase resistance.
 *
 * @param linkage
 * The calculated flux linkage.
 *
 * @return
 * True for success, false otherwise.
 */
bool conf_general_measure_flux_linkage(float current, float duty,
		float min_erpm, float res, float *linkage) {
	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_BLDC;
	mcconf.sensor_mode = SENSOR_MODE_SENSORLESS;
	mcconf.comm_mode = COMM_MODE_DELAY;
	mcconf.sl_phase_advance_at_br = 1.0;
	mcconf.sl_min_erpm = min_erpm;
	mc_interface_set_configuration(&mcconf);

	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	mc_interface_lock_override_once();
	mc_interface_set_current(current);

	int cnt = 0;
	while (mc_interface_get_duty_cycle_now() < duty) {
		chThdSleepMilliseconds(1);
		cnt++;
		if (cnt >= 5000) {
			mc_interface_set_current(0.0);
			timeout_configure(tout, tout_c);
			mc_interface_set_configuration(&mcconf_old);
			mc_interface_unlock();
			return false;
		}
	}

	mc_interface_lock_override_once();
	mc_interface_set_duty(duty);

	float avg_voltage = 0.0;
	float avg_rpm = 0.0;
	float avg_current = 0.0;
	float samples = 0.0;
	for (int i = 0;i < 2000;i++) {
		avg_voltage += GET_INPUT_VOLTAGE() * mc_interface_get_duty_cycle_now();
		avg_rpm += mc_interface_get_rpm();
		avg_current += mc_interface_get_tot_current();
		samples += 1.0;
		chThdSleepMilliseconds(1.0);
	}

	mc_interface_set_configuration(&mcconf_old);
	mc_interface_unlock();
	mc_interface_set_current(0.0);

	avg_voltage /= samples;
	avg_rpm /= samples;
	avg_current /= samples;
	avg_voltage -= avg_current * res * 2.0;

	*linkage = avg_voltage * 60.0 / (sqrtf(3.0) * 2.0 * M_PI * avg_rpm);

	return true;
}
