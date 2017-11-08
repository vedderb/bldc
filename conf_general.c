/*
	Copyright 2016 - 2017 Benjamin Vedder	benjamin@vedder.se

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
bool conf_general_permanent_nrf_found = false;

// Private variables
mc_configuration mcconf, mcconf_old;

void conf_general_init(void) {
	// First, make sure that all relevant virtual addresses are assigned for page swapping.
	memset(VirtAddVarTab, 0, sizeof(VirtAddVarTab));

	int ind = 0;
	for (unsigned int i = 0;i < (sizeof(mc_configuration) / 2);i++) {
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
	conf->can_baud_rate = APPCONF_CAN_BAUD_RATE;

	conf->app_to_use = APPCONF_APP_TO_USE;

	conf->app_ppm_conf.ctrl_type = APPCONF_PPM_CTRL_TYPE;
	conf->app_ppm_conf.pid_max_erpm = APPCONF_PPM_PID_MAX_ERPM;
	conf->app_ppm_conf.hyst = APPCONF_PPM_HYST;
	conf->app_ppm_conf.pulse_start = APPCONF_PPM_PULSE_START;
	conf->app_ppm_conf.pulse_end = APPCONF_PPM_PULSE_END;
	conf->app_ppm_conf.pulse_center = APPCONF_PPM_PULSE_CENTER;
	conf->app_ppm_conf.median_filter = APPCONF_PPM_MEDIAN_FILTER;
	conf->app_ppm_conf.safe_start = APPCONF_PPM_SAFE_START;
	conf->app_ppm_conf.throttle_exp = APPCONF_PPM_THROTTLE_EXP;
	conf->app_ppm_conf.throttle_exp_brake = APPCONF_PPM_THROTTLE_EXP_BRAKE;
	conf->app_ppm_conf.throttle_exp_mode = APPCONF_PPM_THROTTLE_EXP_MODE;
	conf->app_ppm_conf.ramp_time_pos = APPCONF_PPM_RAMP_TIME_POS;
	conf->app_ppm_conf.ramp_time_neg = APPCONF_PPM_RAMP_TIME_NEG;
	conf->app_ppm_conf.multi_esc = APPCONF_PPM_MULTI_ESC;
	conf->app_ppm_conf.tc = APPCONF_PPM_TC;
	conf->app_ppm_conf.tc_max_diff = APPCONF_PPM_TC_MAX_DIFF;

	conf->app_adc_conf.ctrl_type = APPCONF_ADC_CTRL_TYPE;
	conf->app_adc_conf.hyst = APPCONF_ADC_HYST;
	conf->app_adc_conf.voltage_start = APPCONF_ADC_VOLTAGE_START;
	conf->app_adc_conf.voltage_end = APPCONF_ADC_VOLTAGE_END;
	conf->app_adc_conf.voltage_center = APPCONF_ADC_VOLTAGE_CENTER;
	conf->app_adc_conf.voltage2_start = APPCONF_ADC_VOLTAGE2_START;
	conf->app_adc_conf.voltage2_end = APPCONF_ADC_VOLTAGE2_END;
	conf->app_adc_conf.use_filter = APPCONF_ADC_USE_FILTER;
	conf->app_adc_conf.safe_start = APPCONF_ADC_SAFE_START;
	conf->app_adc_conf.cc_button_inverted = APPCONF_ADC_CC_BUTTON_INVERTED;
	conf->app_adc_conf.rev_button_inverted = APPCONF_ADC_REV_BUTTON_INVERTED;
	conf->app_adc_conf.voltage_inverted = APPCONF_ADC_VOLTAGE_INVERTED;
	conf->app_adc_conf.voltage2_inverted = APPCONF_ADC_VOLTAGE2_INVERTED;
	conf->app_adc_conf.throttle_exp = APPCONF_ADC_THROTTLE_EXP;
	conf->app_adc_conf.throttle_exp_brake = APPCONF_ADC_THROTTLE_EXP_BRAKE;
	conf->app_adc_conf.throttle_exp_mode = APPCONF_ADC_THROTTLE_EXP_MODE;
	conf->app_adc_conf.ramp_time_pos = APPCONF_ADC_RAMP_TIME_POS;
	conf->app_adc_conf.ramp_time_neg = APPCONF_ADC_RAMP_TIME_NEG;
	conf->app_adc_conf.multi_esc = APPCONF_ADC_MULTI_ESC;
	conf->app_adc_conf.tc = APPCONF_ADC_TC;
	conf->app_adc_conf.tc_max_diff = APPCONF_ADC_TC_MAX_DIFF;
	conf->app_adc_conf.update_rate_hz = APPCONF_ADC_UPDATE_RATE_HZ;

	conf->app_uart_baudrate = APPCONF_UART_BAUDRATE;

	conf->app_chuk_conf.ctrl_type = APPCONF_CHUK_CTRL_TYPE;
	conf->app_chuk_conf.hyst = APPCONF_CHUK_HYST;
	conf->app_chuk_conf.ramp_time_pos = APPCONF_CHUK_RAMP_TIME_POS;
	conf->app_chuk_conf.ramp_time_neg = APPCONF_CHUK_RAMP_TIME_NEG;
	conf->app_chuk_conf.stick_erpm_per_s_in_cc = APPCONF_STICK_ERPM_PER_S_IN_CC;
	conf->app_chuk_conf.throttle_exp = APPCONF_CHUK_THROTTLE_EXP;
	conf->app_chuk_conf.throttle_exp_brake = APPCONF_CHUK_THROTTLE_EXP_BRAKE;
	conf->app_chuk_conf.throttle_exp_mode = APPCONF_CHUK_THROTTLE_EXP_MODE;
	conf->app_chuk_conf.multi_esc = APPCONF_CHUK_MULTI_ESC;
	conf->app_chuk_conf.tc = APPCONF_CHUK_TC;
	conf->app_chuk_conf.tc_max_diff = APPCONF_CHUK_TC_MAX_DIFF;

	conf->app_nrf_conf.speed = APPCONF_NRF_SPEED;
	conf->app_nrf_conf.power = APPCONF_NRF_POWER;
	conf->app_nrf_conf.crc_type = APPCONF_NRF_CRC;
	conf->app_nrf_conf.retry_delay = APPCONF_NRF_RETR_DELAY;
	conf->app_nrf_conf.retries = APPCONF_NRF_RETRIES;
	conf->app_nrf_conf.channel = APPCONF_NRF_CHANNEL;
	conf->app_nrf_conf.address[0] = APPCONF_NRF_ADDR_B0;
	conf->app_nrf_conf.address[1] = APPCONF_NRF_ADDR_B1;
	conf->app_nrf_conf.address[2] = APPCONF_NRF_ADDR_B2;
	conf->app_nrf_conf.send_crc_ack = APPCONF_NRF_SEND_CRC_ACK;
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
	conf->l_erpm_start = MCCONF_L_RPM_START;
	conf->l_max_erpm_fbrake = MCCONF_L_CURR_MAX_RPM_FBRAKE;
	conf->l_max_erpm_fbrake_cc = MCCONF_L_CURR_MAX_RPM_FBRAKE_CC;
	conf->l_min_vin = MCCONF_L_MIN_VOLTAGE;
	conf->l_max_vin = MCCONF_L_MAX_VOLTAGE;
	conf->l_battery_cut_start = MCCONF_L_BATTERY_CUT_START;
	conf->l_battery_cut_end = MCCONF_L_BATTERY_CUT_END;
	conf->l_slow_abs_current = MCCONF_L_SLOW_ABS_OVERCURRENT;
	conf->l_temp_fet_start = MCCONF_L_LIM_TEMP_FET_START;
	conf->l_temp_fet_end = MCCONF_L_LIM_TEMP_FET_END;
	conf->l_temp_motor_start = MCCONF_L_LIM_TEMP_MOTOR_START;
	conf->l_temp_motor_end = MCCONF_L_LIM_TEMP_MOTOR_END;
	conf->l_temp_accel_dec = MCCONF_L_LIM_TEMP_ACCEL_DEC;
	conf->l_min_duty = MCCONF_L_MIN_DUTY;
	conf->l_max_duty = MCCONF_L_MAX_DUTY;
	conf->l_watt_max = MCCONF_L_WATT_MAX;
	conf->l_watt_min = MCCONF_L_WATT_MIN;

	conf->lo_current_max = conf->l_current_max;
	conf->lo_current_min = conf->l_current_min;
	conf->lo_in_current_max = conf->l_in_current_max;
	conf->lo_in_current_min = conf->l_in_current_min;
	conf->lo_current_motor_max_now = conf->l_current_max;
	conf->lo_current_motor_min_now = conf->l_current_min;

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
	conf->foc_observer_gain_slow = MCCONF_FOC_OBSERVER_GAIN_SLOW;
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
	conf->foc_sl_erpm = MCCONF_FOC_SL_ERPM;
	conf->foc_sample_v0_v7 = MCCONF_FOC_SAMPLE_V0_V7;
	conf->foc_sample_high_current = MCCONF_FOC_SAMPLE_HIGH_CURRENT;
	conf->foc_sat_comp = MCCONF_FOC_SAT_COMP;
	conf->foc_temp_comp = MCCONF_FOC_TEMP_COMP;
	conf->foc_temp_comp_base_temp = MCCONF_FOC_TEMP_COMP_BASE_TEMP;

	conf->s_pid_kp = MCCONF_S_PID_KP;
	conf->s_pid_ki = MCCONF_S_PID_KI;
	conf->s_pid_kd = MCCONF_S_PID_KD;
	conf->s_pid_min_erpm = MCCONF_S_PID_MIN_RPM;
	conf->s_pid_allow_braking = MCCONF_S_PID_ALLOW_BRAKING;

	conf->p_pid_kp = MCCONF_P_PID_KP;
	conf->p_pid_ki = MCCONF_P_PID_KI;
	conf->p_pid_kd = MCCONF_P_PID_KD;
	conf->p_pid_ang_div = MCCONF_P_PID_ANG_DIV;

	conf->cc_startup_boost_duty = MCCONF_CC_STARTUP_BOOST_DUTY;
	conf->cc_min_current = MCCONF_CC_MIN_CURRENT;
	conf->cc_gain = MCCONF_CC_GAIN;
	conf->cc_ramp_step_max = MCCONF_CC_RAMP_STEP;

	conf->m_fault_stop_time_ms = MCCONF_M_FAULT_STOP_TIME;
	conf->m_duty_ramp_step = MCCONF_M_RAMP_STEP;
	conf->m_current_backoff_gain = MCCONF_M_CURRENT_BACKOFF_GAIN;
	conf->m_encoder_counts = MCCONF_M_ENCODER_COUNTS;
	conf->m_sensor_port_mode = MCCONF_M_SENSOR_PORT_MODE;
	conf->m_invert_direction = MCCONF_M_INVERT_DIRECTION;
	conf->m_drv8301_oc_mode = MCCONF_M_DRV8301_OC_MODE;
	conf->m_drv8301_oc_adj = MCCONF_M_DRV8301_OC_ADJ;
	conf->m_bldc_f_sw_min = MCCONF_M_BLDC_F_SW_MIN;
	conf->m_bldc_f_sw_max = MCCONF_M_BLDC_F_SW_MAX;
	conf->m_dc_f_sw = MCCONF_M_DC_F_SW;
	conf->m_ntc_motor_beta = MCCONF_M_NTC_MOTOR_BETA;
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
	mc_interface_lock();

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

	chThdSleepMilliseconds(100);
	mc_interface_unlock();
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
	mc_interface_lock();

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

	chThdSleepMilliseconds(100);
	mc_interface_unlock();
	utils_sys_unlock_cnt();

	return is_ok;
}

bool conf_general_detect_motor_param(float current, float min_rpm, float low_duty,
		float *int_limit, float *bemf_coupling_k, int8_t *hall_table, int *hall_res) {

	int ok_steps = 0;
	const float spinup_to_duty = 0.5;

	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_BLDC;
	mcconf.sensor_mode = SENSOR_MODE_SENSORLESS;
	mcconf.comm_mode = COMM_MODE_INTEGRATE;
	mcconf.sl_phase_advance_at_br = 1.0;
	mcconf.sl_min_erpm = min_rpm;
	mcconf.sl_bemf_coupling_k = 300;
	mcconf.sl_cycle_int_limit = 50;
	mcconf.sl_min_erpm_cycle_int_limit = 1100;
	mcconf.m_invert_direction = false;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	// Wait one second for things to get ready after
	// the fault disappears. (will fry things otherwise...)
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	mc_interface_lock_override_once();
	mc_interface_set_current(current);

	// Try to spin up the motor. Up to three attempts with different settings are made.
	bool started = false;
	for (int i = 0;i < 3;i++) {
		if (i == 1) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 2 * min_rpm;
			mcconf.sl_cycle_int_limit = 20;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		} else if (i == 2) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 4 * min_rpm;
			mcconf.comm_mode = COMM_MODE_DELAY;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		}

		int cnt = 0;
		bool switch_done = false;
		started = true;

		while (mc_interface_get_duty_cycle_now() < spinup_to_duty) {
			chThdSleepMilliseconds(1);
			cnt++;

			if (mc_interface_get_duty_cycle_now() >= (spinup_to_duty / 2.0) && !switch_done) {
				mcpwm_switch_comm_mode(COMM_MODE_DELAY);
				switch_done = true;
			}

			if (cnt > 2000 && !switch_done) {
				started = false;
				break;
			}

			if (cnt >= 5000) {
				started = false;
				break;
			}
		}

		if (switch_done) {
			break;
		}
	}

	if (!started) {
		mc_interface_set_current(0.0);
		timeout_configure(tout, tout_c);
		mc_interface_set_configuration(&mcconf_old);
		mc_interface_unlock();
		return false;
	}

	ok_steps++;

	// Reset hall sensor samples
	mcpwm_reset_hall_detect_table();

	// Run for a while to get hall sensor samples
	mc_interface_lock_override_once();
	mc_interface_set_duty(spinup_to_duty);
	chThdSleepMilliseconds(400);

	// Release the motor and wait a few commutations
	mc_interface_lock_override_once();
	mc_interface_set_current(0.0);
	int tacho = mc_interface_get_tachometer_value(0);
	for (int i = 0;i < 2000;i++) {
		if ((mc_interface_get_tachometer_value(0) - tacho) < 3) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	// Average the cycle integrator for 50 commutations
	mcpwm_read_reset_avg_cycle_integrator();
	tacho = mc_interface_get_tachometer_value(false);
	for (int i = 0;i < 3000;i++) {
		if ((mc_interface_get_tachometer_value(false) - tacho) < 50) {
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
	tacho = mc_interface_get_tachometer_value(0);
	float rpm_sum = 0.0;
	float rpm_iterations = 0.0;
	for (int i = 0;i < 3000;i++) {
		if ((mc_interface_get_tachometer_value(0) - tacho) < 100) {
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
	timeout_configure(tout, tout_c);

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
	mcconf.comm_mode = COMM_MODE_INTEGRATE;
	mcconf.sl_phase_advance_at_br = 1.0;
	mcconf.sl_min_erpm = min_erpm;
	mcconf.m_bldc_f_sw_min = 10000.0;
	mcconf.sl_bemf_coupling_k = 300;
	mcconf.sl_cycle_int_limit = 50;
	mcconf.sl_min_erpm_cycle_int_limit = 1100;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	// Wait one second for things to get ready after
	// the fault disapears. (will fry things otherwise...)
	// TODO: Add FAULT_INIT_NOT_DONE
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	mc_interface_lock_override_once();
	mc_interface_set_current(current);

	// Try to spin up the motor. Up to three attempts with different settings are made.
	bool started = false;
	for (int i = 0;i < 4;i++) {
		if (i == 1) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_cycle_int_limit = 250;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		} else if (i == 2) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 2 * min_erpm;
			mcconf.sl_cycle_int_limit = 20;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		} else if (i == 3) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 4 * min_erpm;
			mcconf.comm_mode = COMM_MODE_DELAY;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		}

		int cnt = 0;
		bool switch_done = false;
		started = true;

		while (mc_interface_get_duty_cycle_now() < duty) {
			chThdSleepMilliseconds(1);
			cnt++;

			if (mc_interface_get_duty_cycle_now() >= (duty / 2.0) && !switch_done) {
				mcpwm_switch_comm_mode(COMM_MODE_DELAY);
				switch_done = true;
			}

			if (cnt > 2000 && !switch_done) {
				started = false;
				break;
			}

			if (cnt >= 5000) {
				started = false;
				break;
			}
		}

		if (switch_done) {
			break;
		}
	}

	if (!started) {
		mc_interface_set_current(0.0);
		timeout_configure(tout, tout_c);
		mc_interface_set_configuration(&mcconf_old);
		mc_interface_unlock();
		return false;
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

	timeout_configure(tout, tout_c);
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
