/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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
#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "hw.h"
#include "utils.h"
#include "stm32f4xx_conf.h"
#include "timeout.h"
#include "commands.h"
#include "encoder.h"
#include "comm_can.h"
#include "app.h"

#include <string.h>
#include <math.h>

#include "conf_mc_app_default.h"

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
	conf->controller_id = HW_DEFAULT_ID;
	conf->timeout_msec = APPCONF_TIMEOUT_MSEC;
	conf->timeout_brake_current = APPCONF_TIMEOUT_BRAKE_CURRENT;
	conf->send_can_status = APPCONF_SEND_CAN_STATUS;
	conf->send_can_status_rate_hz = APPCONF_SEND_CAN_STATUS_RATE_HZ;
	conf->can_baud_rate = APPCONF_CAN_BAUD_RATE;
	conf->pairing_done = APPCONF_PAIRING_DONE;

	conf->uavcan_enable = APPCONF_UAVCAN_ENABLE;
	conf->uavcan_esc_index = APPCONF_UAVCAN_ESC_INDEX;

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
	conf->l_current_max_scale = MCCONF_L_CURRENT_MAX_SCALE;
	conf->l_current_min_scale = MCCONF_L_CURRENT_MIN_SCALE;

	conf->lo_current_max = conf->l_current_max * conf->l_current_max_scale;
	conf->lo_current_min = conf->l_current_min * conf->l_current_min_scale;
	conf->lo_in_current_max = conf->l_in_current_max;
	conf->lo_in_current_min = conf->l_in_current_min;
	conf->lo_current_motor_max_now = conf->lo_current_max;
	conf->lo_current_motor_min_now = conf->lo_current_min;

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
	conf->foc_current_filter_const = MCCONF_FOC_CURRENT_FILTER_CONST;

	conf->gpd_buffer_notify_left = MCCONF_GPD_BUFFER_NOTIFY_LEFT;
	conf->gpd_buffer_interpol = MCCONF_GPD_BUFFER_INTERPOL;
	conf->gpd_current_filter_const = MCCONF_GPD_CURRENT_FILTER_CONST;
	conf->gpd_current_kp = MCCONF_GPD_CURRENT_KP;
	conf->gpd_current_ki = MCCONF_GPD_CURRENT_KI;

	conf->s_pid_kp = MCCONF_S_PID_KP;
	conf->s_pid_ki = MCCONF_S_PID_KI;
	conf->s_pid_kd = MCCONF_S_PID_KD;
	conf->s_pid_kd_filter = MCCONF_S_PID_KD_FILTER;
	conf->s_pid_min_erpm = MCCONF_S_PID_MIN_RPM;
	conf->s_pid_allow_braking = MCCONF_S_PID_ALLOW_BRAKING;

	conf->p_pid_kp = MCCONF_P_PID_KP;
	conf->p_pid_ki = MCCONF_P_PID_KI;
	conf->p_pid_kd = MCCONF_P_PID_KD;
	conf->p_pid_kd_filter = MCCONF_P_PID_KD_FILTER;
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
	conf->m_out_aux_mode = MCCONF_M_OUT_AUX_MODE;

	conf->si_motor_poles = MCCONF_SI_MOTOR_POLES;
	conf->si_gear_ratio = MCCONF_SI_GEAR_RATIO;
	conf->si_wheel_diameter = MCCONF_SI_WHEEL_DIAMETER;
	conf->si_battery_type = MCCONF_SI_BATTERY_TYPE;
	conf->si_battery_cells = MCCONF_SI_BATTERY_CELLS;
	conf->si_battery_ah = MCCONF_SI_BATTERY_AH;
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
	timeout_configure_IWDT_slowest();

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
	timeout_configure_IWDT();

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
	timeout_configure_IWDT_slowest();

	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;

	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

	for (unsigned int i = 0;i < (sizeof(mc_configuration) / 2);i++) {
		uint16_t var = (conf_addr[2 * i] << 8) & 0xFF00;
		var |= conf_addr[2 * i + 1] & 0xFF;

		if (EE_WriteVariable(EEPROM_BASE_MCCONF + i, var) != FLASH_COMPLETE) {
			is_ok = false;
			break;
		}
	}

	RCC_APB1PeriphClockCmd(RCC_APB1Periph_WWDG, ENABLE);
	timeout_configure_IWDT();

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

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old);
		return false;
	}

	// Wait one second for things to get ready after
	// the fault disapears.
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

/* Calculate DTG register */
uint8_t conf_general_calculate_deadtime(float deadtime_ns, float core_clock_freq) {
	uint8_t DTG = 0;
	float timebase = 1.0 / (core_clock_freq / 1000000.0) * 1000.0;

	if (deadtime_ns <= (timebase * 127.0)) {
		DTG = deadtime_ns / timebase;
	} else {
		if (deadtime_ns <= ((63.0 + 64.0) * 2.0 * timebase)) {
			DTG = deadtime_ns / (2.0 * timebase) - 64.0;
			DTG |= 0x80;
		} else {
			if (deadtime_ns <= ((31.0 + 32.0) * 8.0 * timebase)) {
				DTG = deadtime_ns / (8.0 * timebase) - 32.0;
				DTG |= 0xC0;
			} else {
				if (deadtime_ns <= ((31.0 + 32) * 16 * timebase)) {
					DTG = deadtime_ns / (16.0 * timebase) - 32.0;
					DTG |= 0xE0;
				} else {
					// Deadtime requested is longer than max achievable. Set deadtime at
					// longest possible value
					DTG = 0xFF;
					assert_param(1); //catch this
				}
			}
		}
	}

	return DTG;
}

/**
 * Try to measure the motor flux linkage using open loop FOC control.
 *
 * @param current
 * The Q-axis current to spin up the motor.
 *
 * @param duty
 * Duty cycle % to measure at
 *
 * @param erpm_per_sec
 * Acceleration rate
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
bool conf_general_measure_flux_linkage_openloop(float current, float duty,
		float erpm_per_sec, float res, float *linkage) {
	bool result = false;

	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_FOC;
	mcconf.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
	mcconf.foc_current_kp = 0.0005;
	mcconf.foc_current_ki = 1.0;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old);
		return false;
	}

	// Wait one second for things to get ready after
	// the fault disapears.
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	int cnt = 0;
	float rpm_now = 0;

	// Start by locking the motor
	mcpwm_foc_set_openloop(current, rpm_now);

	float duty_still = 0;
	float samples = 0;
	for (int i = 0;i < 1000;i++) {
		duty_still += fabsf(mc_interface_get_duty_cycle_now());
		samples += 1.0;
		chThdSleepMilliseconds(1);
	}

	duty_still /= samples;
	float duty_max = 0.0;
	const int max_time = 15000;

	while (fabsf(mc_interface_get_duty_cycle_now()) < duty) {
		mcpwm_foc_set_openloop(current, mcconf.m_invert_direction ? -rpm_now : rpm_now);
		rpm_now += erpm_per_sec / 1000.0;

		chThdSleepMilliseconds(1);
		cnt++;

		float duty_now = fabsf(mc_interface_get_duty_cycle_now());

		if (duty_now > duty_max) {
			duty_max = duty_now;
		}

		if (cnt >= max_time) {
			*linkage = -1.0;
			break;
		}

		if (cnt > 4000 && duty_now < (duty_max * 0.7)) {
			cnt = max_time;
			*linkage = -2.0;
			break;
		}

		if (cnt > 4000 && duty < duty_still * 1.1) {
			cnt = max_time;
			*linkage = -3.0;
			break;
		}

		if (rpm_now >= 20000) {
			break;
		}
	}

	chThdSleepMilliseconds(1000);

	if (cnt < max_time) {
		float vq_avg = 0.0;
		float vd_avg = 0.0;
		float iq_avg = 0.0;
		float id_avg = 0.0;
		float samples2 = 0.0;

		for (int i = 0;i < 1000;i++) {
			vq_avg += mcpwm_foc_get_vq();
			vd_avg += mcpwm_foc_get_vd();
			iq_avg += mcpwm_foc_get_iq();
			id_avg += mcpwm_foc_get_id();
			samples2 += 1.0;
			chThdSleepMilliseconds(1);
		}

		vq_avg /= samples2;
		vd_avg /= samples2;
		iq_avg /= samples2;
		id_avg /= samples2;

		*linkage = (sqrtf(SQ(vq_avg) + SQ(vd_avg)) - res *
				sqrtf(SQ(iq_avg) + SQ(id_avg))) / (rpm_now * ((2.0 * M_PI) / 60.0));

		result = true;
	}

	timeout_configure(tout, tout_c);
	mc_interface_unlock();
	mc_interface_release_motor();
	mc_interface_set_configuration(&mcconf_old);
	return result;
}

/**
 * Automatically detect sensors and apply settings in FOC mode.
 *
 * @param current
 * Current to use for detection.
 *
 * @param store_mcconf_on_success
 * Store motor configuration in emulated EEPROM if the detection succeeds.
 *
 * @param send_mcconf_on_success
 * Send motor configuration if the detection succeeds.
 *
 * @return
 * 2: AS5147 detected successfully
 * 1: Hall sensors detected successfully
 * 0: No sensors detected and sensorless mode applied successfully
 * -1: Detection failed
 */
int conf_general_autodetect_apply_sensors_foc(float current,
		bool store_mcconf_on_success, bool send_mcconf_on_success) {
	int result = -1;

	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_FOC;
	mcconf.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
	mcconf.foc_current_kp = 0.0005;
	mcconf.foc_current_ki = 1.0;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old);
		return -1;
	}

	// Wait one second for things to get ready after
	// the fault disapears.
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	// Hall sensors
	mcconf.m_sensor_port_mode = SENSOR_PORT_MODE_HALL;
	mc_interface_set_configuration(&mcconf);

	uint8_t hall_table[8];
	bool res = mcpwm_foc_hall_detect(current, hall_table);

	// Disable timeout and lock, as hall detection will undo the lock
	timeout_reset();
	timeout_configure(60000, 0.0);
	mc_interface_lock();

	if (res) {
		mcconf_old.m_sensor_port_mode = SENSOR_PORT_MODE_HALL;
		mcconf_old.foc_sensor_mode = FOC_SENSOR_MODE_HALL;
		for (int i = 0;i < 8;i++) {
			mcconf_old.foc_hall_table[i] = hall_table[i];
		}

		result = 1;
	}

	// AS5047 encoder
	if (!res) {
		mcconf.m_sensor_port_mode = SENSOR_PORT_MODE_AS5047_SPI;
		mc_interface_set_configuration(&mcconf);

		mcpwm_foc_set_openloop_phase(current, 0.0);
		chThdSleepMilliseconds(1000);

		float phase_start = encoder_read_deg();
		float phase_mid = 0.0;
		float phase_end = 0.0;

		for (int i = 0;i < 180.0;i++) {
			mcpwm_foc_set_openloop_phase(current, i);
			chThdSleepMilliseconds(5);

			if (i == 90) {
				phase_mid = encoder_read_deg();
			}
		}

		phase_end = encoder_read_deg();
		float diff = fabsf(utils_angle_difference(phase_start, phase_end));
		float diff_mid = fabsf(utils_angle_difference(phase_mid, phase_end));

		if (diff > 2.0 && (diff_mid - diff / 2.0) < (diff / 4)) {
			float offset, ratio;
			bool inverted;
			mcpwm_foc_encoder_detect(current, false, &offset, &ratio, &inverted);
			mcconf_old.m_sensor_port_mode = SENSOR_PORT_MODE_AS5047_SPI;
			mcconf_old.foc_sensor_mode = FOC_SENSOR_MODE_ENCODER;
			mcconf_old.foc_encoder_offset = offset;
			mcconf_old.foc_encoder_ratio = ratio;
			mcconf_old.foc_encoder_inverted = inverted;

			res = true;
			result = 2;
		}
	}

	// Sensorless
	if (!res) {
		mcconf_old.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
		result = 0;
		res = true;
	}

	timeout_configure(tout, tout_c);
	mc_interface_unlock();
	mc_interface_release_motor();
	mc_interface_set_configuration(&mcconf_old);

	// On success store the mc configuration, also send it to VESC Tool.
	if (res) {
		if (store_mcconf_on_success) {
			conf_general_store_mc_configuration(&mcconf_old);
		}

		if (send_mcconf_on_success) {
			commands_send_mcconf(COMM_GET_MCCONF, &mcconf_old);
		}
	}

	return result;
}

/**
 * Detect and apply all parameters, current limits and sensors.
 *
 * @param max_power_loss
 * The maximum power loss to derive current limits, as well as detection currents, from.
 *
 * @param store_mcconf_on_success
 * Store motor configuration in emulated EEPROM if the detection succeeds.
 *
 * @param send_mcconf_on_success
 * Send motor configuration if the detection succeeds.
 *
 * @return
 * >=0: Success, see conf_general_autodetect_apply_sensors_foc codes
 * -10: Flux linkage detection failed
 *  -x: see conf_general_autodetect_apply_sensors_foc faults
 */
int conf_general_detect_apply_all_foc(float max_power_loss,
		bool store_mcconf_on_success, bool send_mcconf_on_success) {
	int result = -1;

	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_FOC;
	mcconf.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
	mcconf.foc_current_kp = 0.0005;
	mcconf.foc_current_ki = 1.0;
	mcconf.l_current_max = MCCONF_L_CURRENT_MAX;
	mcconf.l_current_min = MCCONF_L_CURRENT_MIN;
	mcconf.l_current_max_scale = MCCONF_L_CURRENT_MAX_SCALE;
	mcconf.l_current_min_scale = MCCONF_L_CURRENT_MIN_SCALE;
	mcconf.l_watt_max = MCCONF_L_WATT_MAX;
	mcconf.l_watt_min = MCCONF_L_WATT_MIN;
	mcconf.l_max_erpm = MCCONF_L_RPM_MAX;
	mcconf.l_min_erpm = MCCONF_L_RPM_MIN;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old);
		return -1;
	}

	// Wait one second for things to get ready after
	// the fault disappears.
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	float current_start = mcconf.l_current_max / 50;
	utils_truncate_number_abs(&current_start, mcconf.cc_min_current * 1.1);

	float i_last = 0.0;
	for (float i = current_start;i < mcconf.l_current_max;i *= 1.5) {
		float res_tmp = mcpwm_foc_measure_resistance(i, 5);
		i_last = i;

		if (mc_interface_get_fault() != FAULT_CODE_NONE) {
			timeout_configure(tout, tout_c);
			mc_interface_unlock();
			mc_interface_release_motor();
			mc_interface_set_configuration(&mcconf_old);
			return -11;
		}

		if ((i * i * res_tmp) >= (max_power_loss / 3.0)) {
			break;
		}
	}

	float r = mcpwm_foc_measure_resistance(i_last, 100);
	float l = mcpwm_foc_measure_inductance_current(i_last, 100, 0) * 1e-6;
	float i_max = sqrtf(max_power_loss / r);
	utils_truncate_number(&i_max, HW_LIM_CURRENT);

	float lambda = 0.0;
	int res = conf_general_measure_flux_linkage_openloop(i_max / 2.5, 0.3, 1800, r, &lambda);

	float old_r = mcconf_old.foc_motor_r;
	float old_l = mcconf_old.foc_motor_l;
	float old_flux_linkage = mcconf_old.foc_motor_flux_linkage;
	float old_kp = mcconf_old.foc_current_kp;
	float old_ki = mcconf_old.foc_current_ki;
	float old_observer_gain = mcconf_old.foc_observer_gain;
	bool old_temp_comp = mcconf_old.foc_temp_comp;
	float old_temp_comp_base_temp = mcconf_old.foc_temp_comp_base_temp;

	if (res) {
		mcconf_old.l_current_max = i_max;
		mcconf_old.l_current_min = -i_max;

		float tc = 1000.0;
		float bw = 1.0 / (tc * 1e-6);
		float kp = l * bw;
		float ki = r * bw;
		float gain = 0.001 / (lambda * lambda);

		mcconf_old.foc_motor_r = r;
		mcconf_old.foc_motor_l = l;
		mcconf_old.foc_motor_flux_linkage = lambda;
		mcconf_old.foc_current_kp = kp;
		mcconf_old.foc_current_ki = ki;
		mcconf_old.foc_observer_gain = gain * 1e6;

		// Temperature compensation
		if (mc_interface_temp_motor_filtered() > 0.0) {
			mcconf_old.foc_temp_comp = true;
			mcconf_old.foc_temp_comp_base_temp = mc_interface_temp_motor_filtered();
		} else {
			mcconf_old.foc_temp_comp = false;
		}
	} else {
		result = -10;
	}

	timeout_configure(tout, tout_c);
	mc_interface_unlock();
	mc_interface_release_motor();
	mc_interface_set_configuration(&mcconf_old);

	// Restore initial settings on sensor detection failure
	if (res) {
		// Wait for motor to stop
		chThdSleepMilliseconds(100);
		for (int i = 0;i < 1000;i++) {
			if (fabsf(mc_interface_get_rpm()) > 100.0) {
				chThdSleepMilliseconds(10);
			} else {
				break;
			}
		}

		// This will also store the settings to emulated eeprom and send them to vesc tool
		result = conf_general_autodetect_apply_sensors_foc(i_max / 3.0,
				store_mcconf_on_success, send_mcconf_on_success);
	} else {
		mcconf_old.foc_motor_r = old_r;
		mcconf_old.foc_motor_l = old_l;
		mcconf_old.foc_motor_flux_linkage = old_flux_linkage;
		mcconf_old.foc_current_kp = old_kp;
		mcconf_old.foc_current_ki = old_ki;
		mcconf_old.foc_observer_gain = old_observer_gain;
		mcconf_old.foc_temp_comp = old_temp_comp;
		mcconf_old.foc_temp_comp_base_temp = old_temp_comp_base_temp;
		mc_interface_set_configuration(&mcconf_old);
	}

	return result;
}

/**
 * Same as conf_general_detect_apply_all_foc, but also start detection in VESCs found on the CAN-bus.
 *
 * @param detect_can
 * Run detection on VESCs found on the CAN-bus as well. Setting this to false makes
 * this function behave like conf_general_detect_apply_all_foc, with the convenience
 * of also applying the settings.
 *
 * @param max_power_loss
 * The maximum power loss to derive current limits, as well as detection currents, from.
 *
 * @param min_current_in
 * Minimum input current (negative value). 0 means leave it unchanged.
 *
 * @param max_current_in
 * MAximum input current. 0 means leave it unchanged.
 *
 * @param openloop_rpm
 * FOC openloop ERPM in sensorless mode. 0 means leave it unchanged.
 *
 * @param sl_erpm
 * FOC ERPM above which sensorless should be used in sensored modes. 0 means leave it unchanged.
 *
 * @return
 * Same as conf_general_detect_apply_all_foc, and
 * -50: CAN detection timed out
 * -51: CAN detection failed
 */
int conf_general_detect_apply_all_foc_can(bool detect_can, float max_power_loss,
		float min_current_in, float max_current_in, float openloop_rpm, float sl_erpm) {
	app_configuration appconf = *app_get_configuration();
	uint8_t id_new = appconf.controller_id;

	mcconf = *mc_interface_get_configuration();

	if (fabsf(min_current_in) > 0.001) {
		mcconf.l_in_current_min = min_current_in;
	} else {
		mcconf.l_in_current_min = MCCONF_L_IN_CURRENT_MIN;
	}

	if (fabsf(max_current_in) > 0.001) {
		mcconf.l_in_current_max = max_current_in;
	} else {
		mcconf.l_in_current_max = MCCONF_L_IN_CURRENT_MAX;
	}

	if (fabsf(openloop_rpm) > 0.001) {
		mcconf.foc_openloop_rpm = openloop_rpm;
	} else {
		mcconf.foc_openloop_rpm = MCCONF_FOC_OPENLOOP_RPM;
	}

	if (fabsf(sl_erpm) > 0.001) {
		mcconf.foc_sl_erpm = sl_erpm;
	} else {
		mcconf.foc_sl_erpm = MCCONF_FOC_SL_ERPM;
	}

	mc_interface_set_configuration(&mcconf);
	int can_devs = 0;
	comm_can_detect_all_foc_res_clear();

	if (detect_can) {
		for (int i = 0;i < 255;i++) {
			if (comm_can_ping(i)) {
				comm_can_conf_current_limits_in(i, false, mcconf.l_in_current_min, mcconf.l_in_current_max);
				comm_can_conf_foc_erpms(i, false, mcconf.foc_openloop_rpm, mcconf.foc_sl_erpm);
				comm_can_detect_apply_all_foc(i, true, max_power_loss);
				can_devs++;

				if (i == id_new) {
					id_new++;
				}

				if (id_new == 255) {
					id_new = 0;
				}
			}
		}
	}

	int res = conf_general_detect_apply_all_foc(max_power_loss, false, false);

	// Wait for all VESCs on the CAN-bus to finish detection
	int timeout = true;
	for (int i = 0;i < 4000;i++) {
		if (comm_can_detect_all_foc_res_size() >= can_devs) {
			timeout = false;
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (timeout) {
		res = -50;
	} else {
		for (int i = 0;i < can_devs;i++) {
			if (comm_can_detect_all_foc_res(i) < 0) {
				res = -51;
			}
		}
	}

	// Store and send settings
	if (res >= 0) {
		if (appconf.controller_id != id_new || appconf.send_can_status != CAN_STATUS_1_2_3_4) {
			appconf.controller_id = id_new;
			appconf.send_can_status = CAN_STATUS_1_2_3_4;
			conf_general_store_app_configuration(&appconf);
			app_set_configuration(&appconf);
			commands_send_appconf(COMM_GET_APPCONF, &appconf);
			chThdSleepMilliseconds(1000);
		}

		mcconf = *mc_interface_get_configuration();
		conf_general_store_mc_configuration(&mcconf);
		commands_send_mcconf(COMM_GET_MCCONF, &mcconf);
		chThdSleepMilliseconds(1000);
	}

	return res;
}
