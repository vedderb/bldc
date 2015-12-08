/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

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
 * commands.c
 *
 *  Created on: 19 sep 2014
 *      Author: benjamin
 */

#include "commands.h"
#include "ch.h"
#include "hal.h"
#include "main.h"
#include "stm32f4xx_conf.h"
#include "servo.h"
#include "servo_simple.h"
#include "buffer.h"
#include "terminal.h"
#include "hw.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "app.h"
#include "timeout.h"
#include "servo_dec.h"
#include "comm_can.h"
#include "flash_helper.h"
#include "utils.h"
#include "packet.h"
#include "encoder.h"

#include <math.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

// Threads
static THD_FUNCTION(detect_thread, arg);
static THD_WORKING_AREA(detect_thread_wa, 2048);
static thread_t *detect_tp;

// Private variables
static uint8_t send_buffer[PACKET_MAX_PL_LEN];
static float detect_cycle_int_limit;
static float detect_coupling_k;
static float detect_current;
static float detect_min_rpm;
static float detect_low_duty;
static int8_t detect_hall_table[8];
static int detect_hall_res;
static void(*send_func)(unsigned char *data, unsigned int len) = 0;

void commands_init(void) {
	chThdCreateStatic(detect_thread_wa, sizeof(detect_thread_wa), NORMALPRIO, detect_thread, NULL);
}

/**
 * Provide a function to use the next time there are packets to be sent.
 *
 * @param func
 * A pointer to the packet sending function.
 */
void commands_set_send_func(void(*func)(unsigned char *data, unsigned int len)) {
	send_func = func;
}

/**
 * Send a packet using the set send function.
 *
 * @param data
 * The packet data.
 *
 * @param len
 * The data length.
 */
void commands_send_packet(unsigned char *data, unsigned int len) {
	if (send_func) {
		send_func(data, len);
	}
}

/**
 * Process a received buffer with commands and data.
 *
 * @param data
 * The buffer to process.
 *
 * @param len
 * The length of the buffer.
 */
void commands_process_packet(unsigned char *data, unsigned int len) {
	if (!len) {
		return;
	}

	COMM_PACKET_ID packet_id;
	int32_t ind = 0;
	uint16_t sample_len;
	uint8_t decimation;
	bool at_start;
	static mc_configuration mcconf, mcconf_old; // Static to save some stack space
	app_configuration appconf;
	uint16_t flash_res;
	uint32_t new_app_offset;

	packet_id = data[0];
	data++;
	len--;

	switch (packet_id) {
	case COMM_FW_VERSION:
		ind = 0;
		send_buffer[ind++] = COMM_FW_VERSION;
		send_buffer[ind++] = FW_VERSION_MAJOR;
		send_buffer[ind++] = FW_VERSION_MINOR;
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_JUMP_TO_BOOTLOADER:
		flash_helper_jump_to_bootloader();
		break;

	case COMM_ERASE_NEW_APP:
		ind = 0;
		flash_res = flash_helper_erase_new_app(buffer_get_uint32(data, &ind));

		ind = 0;
		send_buffer[ind++] = COMM_ERASE_NEW_APP;
		send_buffer[ind++] = flash_res == FLASH_COMPLETE ? 1 : 0;
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_WRITE_NEW_APP_DATA:
		ind = 0;
		new_app_offset = buffer_get_uint32(data, &ind);
		flash_res = flash_helper_write_new_app_data(new_app_offset, data + ind, len - ind);

		ind = 0;
		send_buffer[ind++] = COMM_WRITE_NEW_APP_DATA;
		send_buffer[ind++] = flash_res == FLASH_COMPLETE ? 1 : 0;
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GET_VALUES:
		ind = 0;
		send_buffer[ind++] = COMM_GET_VALUES;
		buffer_append_int16(send_buffer, (int16_t)(NTC_TEMP(ADC_IND_TEMP_MOS1) * 10.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(NTC_TEMP(ADC_IND_TEMP_MOS2) * 10.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(NTC_TEMP(ADC_IND_TEMP_MOS3) * 10.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(NTC_TEMP(ADC_IND_TEMP_MOS4) * 10.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(NTC_TEMP(ADC_IND_TEMP_MOS5) * 10.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(NTC_TEMP(ADC_IND_TEMP_MOS6) * 10.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(NTC_TEMP(ADC_IND_TEMP_PCB) * 10.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mc_interface_read_reset_avg_motor_current() * 100.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mc_interface_read_reset_avg_input_current() * 100.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(mc_interface_get_duty_cycle_now() * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)mc_interface_get_rpm(), &ind);
		buffer_append_int16(send_buffer, (int16_t)(GET_INPUT_VOLTAGE() * 10.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mc_interface_get_amp_hours(false) * 10000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mc_interface_get_amp_hours_charged(false) * 10000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mc_interface_get_watt_hours(false) * 10000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mc_interface_get_watt_hours_charged(false) * 10000.0), &ind);
		buffer_append_int32(send_buffer, mc_interface_get_tachometer_value(false), &ind);
		buffer_append_int32(send_buffer, mc_interface_get_tachometer_abs_value(false), &ind);
		send_buffer[ind++] = mc_interface_get_fault();
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_SET_DUTY:
		ind = 0;
		mc_interface_set_duty((float)buffer_get_int32(data, &ind) / 100000.0);
		timeout_reset();
		break;

	case COMM_SET_CURRENT:
		ind = 0;
		mc_interface_set_current((float)buffer_get_int32(data, &ind) / 1000.0);
		timeout_reset();
		break;

	case COMM_SET_CURRENT_BRAKE:
		ind = 0;
		mc_interface_set_brake_current((float)buffer_get_int32(data, &ind) / 1000.0);
		timeout_reset();
		break;

	case COMM_SET_RPM:
		ind = 0;
		mc_interface_set_pid_speed((float)buffer_get_int32(data, &ind));
		timeout_reset();
		break;

	case COMM_SET_POS:
		ind = 0;
		mc_interface_set_pid_pos((float)buffer_get_int32(data, &ind) / 1000000.0);
		timeout_reset();
		break;

	case COMM_SET_DETECT:
		mcpwm_set_detect();
		timeout_reset();
		break;

	case COMM_SET_SERVO_POS:
#if SERVO_OUT_ENABLE
		ind = 0;
#if SERVO_OUT_SIMPLE
		servo_simple_set_output(buffer_get_float16(data, 1000.0, &ind));
#else
		servos[0].pos = (int16_t)(buffer_get_float16(data, 1000.0, &ind) * 255.0);
#endif
#endif
		break;

	case COMM_SET_MCCONF:
		mcconf = *mc_interface_get_configuration();

		ind = 0;
		mcconf.pwm_mode = data[ind++];
		mcconf.comm_mode = data[ind++];
		mcconf.motor_type = data[ind++];
		mcconf.sensor_mode = data[ind++];

		mcconf.l_current_max = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_current_min = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_in_current_max = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_in_current_min = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_abs_current_max = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_min_erpm = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_max_erpm = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_max_erpm_fbrake = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_max_erpm_fbrake_cc = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_min_vin = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_max_vin = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_battery_cut_start = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_battery_cut_end = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_slow_abs_current = data[ind++];
		mcconf.l_rpm_lim_neg_torque = data[ind++];
		mcconf.l_temp_fet_start = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_temp_fet_end = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_temp_motor_start = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_temp_motor_end = buffer_get_float32(data, 1000.0, &ind);
		mcconf.l_min_duty = buffer_get_float32(data, 1000000.0, &ind);
		mcconf.l_max_duty = buffer_get_float32(data, 1000000.0, &ind);

		mcconf.lo_current_max = mcconf.l_current_max;
		mcconf.lo_current_min = mcconf.l_current_min;
		mcconf.lo_in_current_max = mcconf.l_in_current_max;
		mcconf.lo_in_current_min = mcconf.l_in_current_min;

		mcconf.sl_min_erpm = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_min_erpm_cycle_int_limit = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_max_fullbreak_current_dir_change = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_cycle_int_limit = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_phase_advance_at_br = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_cycle_int_rpm_br = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_bemf_coupling_k = (float)buffer_get_int32(data, &ind) / 1000.0;

		memcpy(mcconf.hall_table, data + ind, 8);
		ind += 8;
		mcconf.hall_sl_erpm = (float)buffer_get_int32(data, &ind) / 1000.0;

		mcconf.foc_current_kp = buffer_get_float32(data, 1e5, &ind);
		mcconf.foc_current_ki = buffer_get_float32(data, 1e5, &ind);
		mcconf.foc_f_sw = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_dt_us = buffer_get_float32(data, 1e6, &ind);
		mcconf.foc_encoder_inverted = data[ind++];
		mcconf.foc_encoder_offset = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_encoder_ratio = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_sensor_mode = data[ind++];
		mcconf.foc_pll_kp = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_pll_ki = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_motor_l = buffer_get_float32(data, 1e8, &ind);
		mcconf.foc_motor_r = buffer_get_float32(data, 1e5, &ind);
		mcconf.foc_motor_flux_linkage = buffer_get_float32(data, 1e5, &ind);
		mcconf.foc_observer_gain = buffer_get_float32(data, 1e0, &ind);
		mcconf.foc_duty_dowmramp_kp = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_duty_dowmramp_ki = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_openloop_rpm = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_sl_openloop_hyst = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_sl_openloop_time = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_sl_d_current_duty = buffer_get_float32(data, 1e3, &ind);
		mcconf.foc_sl_d_current_factor = buffer_get_float32(data, 1e3, &ind);

		mcconf.s_pid_kp = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.s_pid_ki = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.s_pid_kd = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.s_pid_min_erpm = (float)buffer_get_int32(data, &ind) / 1000.0;

		mcconf.p_pid_kp = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.p_pid_ki = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.p_pid_kd = (float)buffer_get_int32(data, &ind) / 1000000.0;

		mcconf.cc_startup_boost_duty = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.cc_min_current = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.cc_gain = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.cc_ramp_step_max = (float)buffer_get_int32(data, &ind) / 1000000.0;

		mcconf.m_fault_stop_time_ms = buffer_get_int32(data, &ind);
		mcconf.m_duty_ramp_step = (float)buffer_get_float32(data, 1000000.0, &ind);
		mcconf.m_duty_ramp_step_rpm_lim = (float)buffer_get_float32(data, 1000000.0, &ind);
		mcconf.m_current_backoff_gain = (float)buffer_get_float32(data, 1000000.0, &ind);
		mcconf.m_encoder_counts = buffer_get_uint32(data, &ind);

		conf_general_store_mc_configuration(&mcconf);
		mc_interface_set_configuration(&mcconf);

#if ENCODER_ENABLE
		encoder_set_counts(mcconf.m_encoder_counts);
#endif

		ind = 0;
		send_buffer[ind++] = packet_id;
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GET_MCCONF:
	case COMM_GET_MCCONF_DEFAULT:
		if (packet_id == COMM_GET_MCCONF) {
			mcconf = *mc_interface_get_configuration();
		} else {
			conf_general_get_default_mc_configuration(&mcconf);
		}

		ind = 0;
		send_buffer[ind++] = packet_id;

		send_buffer[ind++] = mcconf.pwm_mode;
		send_buffer[ind++] = mcconf.comm_mode;
		send_buffer[ind++] = mcconf.motor_type;
		send_buffer[ind++] = mcconf.sensor_mode;

		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_current_max * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_current_min * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_in_current_max * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_in_current_min * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_abs_current_max * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_min_erpm * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_erpm * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_erpm_fbrake * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_erpm_fbrake_cc * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_min_vin * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_vin * 1000.0), &ind);
		buffer_append_float32(send_buffer, mcconf.l_battery_cut_start, 1000.0, &ind);
		buffer_append_float32(send_buffer, mcconf.l_battery_cut_end, 1000.0, &ind);
		send_buffer[ind++] = mcconf.l_slow_abs_current;
		send_buffer[ind++] = mcconf.l_rpm_lim_neg_torque;
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_temp_fet_start * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_temp_fet_end * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_temp_motor_start * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_temp_motor_end * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_min_duty * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_duty * 1000000.0), &ind);

		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_min_erpm * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_min_erpm_cycle_int_limit * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_max_fullbreak_current_dir_change * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_cycle_int_limit * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_phase_advance_at_br * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_cycle_int_rpm_br * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_bemf_coupling_k * 1000.0), &ind);

		memcpy(send_buffer + ind, mcconf.hall_table, 8);
		ind += 8;
		buffer_append_int32(send_buffer, (int32_t)(mcconf.hall_sl_erpm * 1000.0), &ind);

		buffer_append_float32(send_buffer, mcconf.foc_current_kp, 1e5, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_current_ki, 1e5, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_f_sw, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_dt_us, 1e6, &ind);
		send_buffer[ind++] = mcconf.foc_encoder_inverted;
		buffer_append_float32(send_buffer, mcconf.foc_encoder_offset, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_encoder_ratio, 1e3, &ind);
		send_buffer[ind++] = mcconf.foc_sensor_mode;
		buffer_append_float32(send_buffer, mcconf.foc_pll_kp, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_pll_ki, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_motor_l, 1e8, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_motor_r, 1e5, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_motor_flux_linkage, 1e5, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_observer_gain, 1e0, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_duty_dowmramp_kp, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_duty_dowmramp_ki, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_openloop_rpm, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_sl_openloop_hyst, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_sl_openloop_time, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_sl_d_current_duty, 1e3, &ind);
		buffer_append_float32(send_buffer, mcconf.foc_sl_d_current_factor, 1e3, &ind);

		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_kp * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_ki * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_kd * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_min_erpm * 1000.0), &ind);

		buffer_append_int32(send_buffer, (int32_t)(mcconf.p_pid_kp * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.p_pid_ki * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.p_pid_kd * 1000000.0), &ind);

		buffer_append_int32(send_buffer, (int32_t)(mcconf.cc_startup_boost_duty * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.cc_min_current * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.cc_gain * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.cc_ramp_step_max * 1000000.0), &ind);

		buffer_append_int32(send_buffer, mcconf.m_fault_stop_time_ms, &ind);
		buffer_append_float32(send_buffer, mcconf.m_duty_ramp_step, 1000000.0, &ind);
		buffer_append_float32(send_buffer, mcconf.m_duty_ramp_step_rpm_lim, 1000000.0, &ind);
		buffer_append_float32(send_buffer, mcconf.m_current_backoff_gain, 1000000.0, &ind);
		buffer_append_uint32(send_buffer, mcconf.m_encoder_counts, &ind);

		commands_send_packet(send_buffer, ind);
		break;

	case COMM_SET_APPCONF:
		appconf = *app_get_configuration();

		ind = 0;
		appconf.controller_id = data[ind++];
		appconf.timeout_msec = buffer_get_uint32(data, &ind);
		appconf.timeout_brake_current = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.send_can_status = data[ind++];
		appconf.send_can_status_rate_hz = buffer_get_uint16(data, &ind);

		appconf.app_to_use = data[ind++];

		appconf.app_ppm_conf.ctrl_type = data[ind++];
		appconf.app_ppm_conf.pid_max_erpm = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_ppm_conf.hyst = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_ppm_conf.pulse_start = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_ppm_conf.pulse_end = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_ppm_conf.median_filter = data[ind++];
		appconf.app_ppm_conf.safe_start = data[ind++];
		appconf.app_ppm_conf.rpm_lim_start = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_ppm_conf.rpm_lim_end = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_ppm_conf.multi_esc = data[ind++];
		appconf.app_ppm_conf.tc = data[ind++];
		appconf.app_ppm_conf.tc_max_diff = (float)buffer_get_int32(data, &ind) / 1000.0;

		appconf.app_adc_conf.ctrl_type = data[ind++];
		appconf.app_adc_conf.hyst = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_adc_conf.voltage_start = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_adc_conf.voltage_end = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_adc_conf.use_filter = data[ind++];
		appconf.app_adc_conf.safe_start = data[ind++];
		appconf.app_adc_conf.cc_button_inverted = data[ind++];
		appconf.app_adc_conf.rev_button_inverted = data[ind++];
		appconf.app_adc_conf.voltage_inverted = data[ind++];
		appconf.app_adc_conf.rpm_lim_start = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_adc_conf.rpm_lim_end = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_adc_conf.multi_esc = data[ind++];
		appconf.app_adc_conf.tc = data[ind++];
		appconf.app_adc_conf.tc_max_diff = (float)buffer_get_int32(data, &ind) / 1000.0;
		appconf.app_adc_conf.update_rate_hz = buffer_get_uint16(data, &ind);

		appconf.app_uart_baudrate = buffer_get_uint32(data, &ind);

		appconf.app_chuk_conf.ctrl_type = data[ind++];
		appconf.app_chuk_conf.hyst = buffer_get_float32(data, 1000.0, &ind);
		appconf.app_chuk_conf.rpm_lim_start = buffer_get_float32(data, 1000.0, &ind);
		appconf.app_chuk_conf.rpm_lim_end = buffer_get_float32(data, 1000.0, &ind);
		appconf.app_chuk_conf.ramp_time_pos = buffer_get_float32(data, 1000.0, &ind);
		appconf.app_chuk_conf.ramp_time_neg = buffer_get_float32(data, 1000.0, &ind);
		appconf.app_chuk_conf.stick_erpm_per_s_in_cc = buffer_get_float32(data, 1000.0, &ind);
		appconf.app_chuk_conf.multi_esc = data[ind++];
		appconf.app_chuk_conf.tc = data[ind++];
		appconf.app_chuk_conf.tc_max_diff = buffer_get_float32(data, 1000.0, &ind);

		conf_general_store_app_configuration(&appconf);
		app_set_configuration(&appconf);
		timeout_configure(appconf.timeout_msec, appconf.timeout_brake_current);

		ind = 0;
		send_buffer[ind++] = packet_id;
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GET_APPCONF:
	case COMM_GET_APPCONF_DEFAULT:
		if (packet_id == COMM_GET_APPCONF) {
			appconf = *app_get_configuration();
		} else {
			conf_general_get_default_app_configuration(&appconf);
		}

		ind = 0;
		send_buffer[ind++] = packet_id;
		send_buffer[ind++] = appconf.controller_id;
		buffer_append_uint32(send_buffer, appconf.timeout_msec, &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.timeout_brake_current * 1000.0), &ind);
		send_buffer[ind++] = appconf.send_can_status;
		buffer_append_uint16(send_buffer, appconf.send_can_status_rate_hz, &ind);

		send_buffer[ind++] = appconf.app_to_use;

		send_buffer[ind++] = appconf.app_ppm_conf.ctrl_type;
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_ppm_conf.pid_max_erpm * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_ppm_conf.hyst * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_ppm_conf.pulse_start * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_ppm_conf.pulse_end * 1000.0), &ind);
		send_buffer[ind++] = appconf.app_ppm_conf.median_filter;
		send_buffer[ind++] = appconf.app_ppm_conf.safe_start;
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_ppm_conf.rpm_lim_start * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_ppm_conf.rpm_lim_end * 1000.0), &ind);
		send_buffer[ind++] = appconf.app_ppm_conf.multi_esc;
		send_buffer[ind++] = appconf.app_ppm_conf.tc;
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_ppm_conf.tc_max_diff * 1000.0), &ind);

		send_buffer[ind++] = appconf.app_adc_conf.ctrl_type;
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_adc_conf.hyst * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_adc_conf.voltage_start * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_adc_conf.voltage_end * 1000.0), &ind);
		send_buffer[ind++] = appconf.app_adc_conf.use_filter;
		send_buffer[ind++] = appconf.app_adc_conf.safe_start;
		send_buffer[ind++] = appconf.app_adc_conf.cc_button_inverted;
		send_buffer[ind++] = appconf.app_adc_conf.rev_button_inverted;
		send_buffer[ind++] = appconf.app_adc_conf.voltage_inverted;
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_adc_conf.rpm_lim_start * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_adc_conf.rpm_lim_end * 1000.0), &ind);
		send_buffer[ind++] = appconf.app_adc_conf.multi_esc;
		send_buffer[ind++] = appconf.app_adc_conf.tc;
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_adc_conf.tc_max_diff * 1000.0), &ind);
		buffer_append_uint16(send_buffer, appconf.app_adc_conf.update_rate_hz, &ind);

		buffer_append_uint32(send_buffer, appconf.app_uart_baudrate, &ind);

		send_buffer[ind++] = appconf.app_chuk_conf.ctrl_type;
		buffer_append_float32(send_buffer, appconf.app_chuk_conf.hyst, 1000.0, &ind);
		buffer_append_float32(send_buffer, appconf.app_chuk_conf.rpm_lim_start, 1000.0, &ind);
		buffer_append_float32(send_buffer, appconf.app_chuk_conf.rpm_lim_end, 1000.0, &ind);
		buffer_append_float32(send_buffer, appconf.app_chuk_conf.ramp_time_pos, 1000.0, &ind);
		buffer_append_float32(send_buffer, appconf.app_chuk_conf.ramp_time_neg, 1000.0, &ind);
		buffer_append_float32(send_buffer, appconf.app_chuk_conf.stick_erpm_per_s_in_cc, 1000.0, &ind);
		send_buffer[ind++] = appconf.app_chuk_conf.multi_esc;
		send_buffer[ind++] = appconf.app_chuk_conf.tc;
		buffer_append_int32(send_buffer, (int32_t)(appconf.app_chuk_conf.tc_max_diff * 1000.0), &ind);

		commands_send_packet(send_buffer, ind);
		break;

	case COMM_SAMPLE_PRINT:
		ind = 0;
		at_start = data[ind++];
		sample_len = buffer_get_uint16(data, &ind);
		decimation = data[ind++];
		main_sample_print_data(at_start, sample_len, decimation);
		break;

	case COMM_TERMINAL_CMD:
		data[len] = '\0';
		terminal_process_string((char*)data);
		break;

	case COMM_DETECT_MOTOR_PARAM:
		ind = 0;
		detect_current = buffer_get_float32(data, 1e3, &ind);
		detect_min_rpm = buffer_get_float32(data, 1e3, &ind);
		detect_low_duty = buffer_get_float32(data, 1e3, &ind);

		chEvtSignal(detect_tp, (eventmask_t) 1);
		break;

	case COMM_DETECT_MOTOR_R_L: {
		mcconf = *mc_interface_get_configuration();
		mcconf_old = mcconf;

		ind = 0;
		send_buffer[ind++] = COMM_DETECT_MOTOR_R_L;

		mcconf.motor_type = MOTOR_TYPE_FOC;
		mc_interface_set_configuration(&mcconf);

		float r = 0.0;
		float l = 0.0;
		bool res = mcpwm_foc_measure_res_ind(&r, &l);
		mc_interface_set_configuration(&mcconf_old);

		if (!res) {
			r = 0.0;
			l = 0.0;
		}

		buffer_append_float32(send_buffer, r, 1e6, &ind);
		buffer_append_float32(send_buffer, l, 1e3, &ind);
		commands_send_packet(send_buffer, ind);
	}
	break;

	case COMM_DETECT_MOTOR_FLUX_LINKAGE: {
		ind = 0;
		float current = buffer_get_float32(data, 1e3, &ind);
		float min_rpm = buffer_get_float32(data, 1e3, &ind);
		float duty = buffer_get_float32(data, 1e3, &ind);
		float resistance = buffer_get_float32(data, 1e6, &ind);

		float linkage;
		bool res = conf_general_measure_flux_linkage(current, duty, min_rpm, resistance, &linkage);

		if (!res) {
			linkage = 0.0;
		}

		ind = 0;
		send_buffer[ind++] = COMM_DETECT_MOTOR_FLUX_LINKAGE;
		buffer_append_float32(send_buffer, linkage, 1e7, &ind);
		commands_send_packet(send_buffer, ind);
	}
	break;

	case COMM_REBOOT:
		// Lock the system and enter an infinite loop. The watchdog will reboot.
		__disable_irq();
		for(;;){};
		break;

	case COMM_ALIVE:
		timeout_reset();
		break;

	case COMM_GET_DECODED_PPM:
		ind = 0;
		send_buffer[ind++] = COMM_GET_DECODED_PPM;
		buffer_append_int32(send_buffer, (int32_t)(servodec_get_servo(0) * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(servodec_get_last_pulse_len(0) * 1000000.0), &ind);
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GET_DECODED_ADC:
		ind = 0;
		send_buffer[ind++] = COMM_GET_DECODED_ADC;
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_decoded_level() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_voltage() * 1000000.0), &ind);
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GET_DECODED_CHUK:
		ind = 0;
		send_buffer[ind++] = COMM_GET_DECODED_CHUK;
		buffer_append_int32(send_buffer, (int32_t)(app_nunchuk_get_decoded_chuk() * 1000000.0), &ind);
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_FORWARD_CAN:
		comm_can_send_buffer(data[0], data + 1, len - 1, false);
		break;

	default:
		break;
	}
}

void commands_printf(char* format, ...) {
	va_list arg;
	va_start (arg, format);
	int len;
	static char print_buffer[255];

	print_buffer[0] = COMM_PRINT;
	len = vsnprintf(print_buffer+1, 254, format, arg);
	va_end (arg);

	if(len>0) {
		commands_send_packet((unsigned char*)print_buffer, (len<254)? len+1: 255);
	}
}

void commands_send_samples(uint8_t *data, int len) {
	uint8_t buffer[len + 1];
	int index = 0;

	buffer[index++] = COMM_SAMPLE_PRINT;

	for (int i = 0;i < len;i++) {
		buffer[index++] = data[i];
	}

	commands_send_packet(buffer, index);
}

void commands_send_rotor_pos(float rotor_pos) {
	uint8_t buffer[5];
	int32_t index = 0;

	buffer[index++] = COMM_ROTOR_POSITION;
	buffer_append_int32(buffer, (int32_t)(rotor_pos * 100000.0), &index);

	commands_send_packet(buffer, index);
}

void commands_send_experiment_samples(float *samples, int len) {
	if ((len * 4 + 1) > 256) {
		return;
	}

	uint8_t buffer[len * 4 + 1];
	int32_t index = 0;

	buffer[index++] = COMM_EXPERIMENT_SAMPLE;

	for (int i = 0;i < len;i++) {
		buffer_append_int32(buffer, (int32_t)(samples[i] * 10000.0), &index);
	}

	commands_send_packet(buffer, index);
}

static THD_FUNCTION(detect_thread, arg) {
	(void)arg;

	chRegSetThreadName("Detect");

	detect_tp = chThdGetSelfX();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		if (!conf_general_detect_motor_param(detect_current, detect_min_rpm,
				detect_low_duty, &detect_cycle_int_limit, &detect_coupling_k,
				detect_hall_table, &detect_hall_res)) {
			detect_cycle_int_limit = 0.0;
			detect_coupling_k = 0.0;
		}

		int32_t ind = 0;
		send_buffer[ind++] = COMM_DETECT_MOTOR_PARAM;
		buffer_append_int32(send_buffer, (int32_t)(detect_cycle_int_limit * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(detect_coupling_k * 1000.0), &ind);
		memcpy(send_buffer + ind, detect_hall_table, 8);
		ind += 8;
		send_buffer[ind++] = detect_hall_res;
		commands_send_packet(send_buffer, ind);
	}
}
