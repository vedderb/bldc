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

#include "commands.h"
#include "ch.h"
#include "hal.h"
#include "mc_interface.h"
#include "stm32f4xx_conf.h"
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
#include "nrf_driver.h"
#include "gpdrive.h"

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
static uint8_t detect_thread_cmd_buffer[50];
static volatile bool is_detecting = false;
static void(* volatile send_func)(unsigned char *data, unsigned int len) = 0;
static void(* volatile send_func_detect)(unsigned char *data, unsigned int len) = 0;
static void(* volatile send_func_last)(unsigned char *data, unsigned int len) = 0;
static void(* volatile appdata_func)(unsigned char *data, unsigned int len) = 0;
static void(* volatile send_func_nrf)(unsigned char *data, unsigned int len) = 0;
static disp_pos_mode display_position_mode;

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
	send_func_last = send_func;
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
 * Send a packet using the set NRF51 send function. The NRF51 send function
 * is set when the COMM_EXT_NRF_PRESENT and COMM_EXT_NRF_ESB_RX_DATA commands
 * are received, at which point the previous send function is restored. The
 * intention behind that is to make the NRF51-related communication only with
 * the interface that has an NRF51, and prevent the NRF51 communication from
 * interfering with other communication.
 *
 * @param data
 * The packet data.
 *
 * @param len
 * The data length.
 */
void commands_send_packet_nrf(unsigned char *data, unsigned int len) {
	if (send_func_nrf) {
		send_func_nrf(data, len);
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
	static mc_configuration mcconf; // Static to save some stack space
	static app_configuration appconf;

	packet_id = data[0];
	data++;
	len--;

	switch (packet_id) {
	case COMM_FW_VERSION:
		ind = 0;
		send_buffer[ind++] = COMM_FW_VERSION;
		send_buffer[ind++] = FW_VERSION_MAJOR;
		send_buffer[ind++] = FW_VERSION_MINOR;

		strcpy((char*)(send_buffer + ind), HW_NAME);
		ind += strlen(HW_NAME) + 1;

		memcpy(send_buffer + ind, STM32_UUID_8, 12);
		ind += 12;

		send_buffer[ind++] = app_get_configuration()->pairing_done;

		commands_send_packet(send_buffer, ind);
		break;

	case COMM_JUMP_TO_BOOTLOADER_ALL_CAN:
		data[-1] = COMM_JUMP_TO_BOOTLOADER;
		comm_can_send_buffer(255, data - 1, len + 1, 2);
		chThdSleepMilliseconds(100);
		/* Falls through. */
		/* no break */
	case COMM_JUMP_TO_BOOTLOADER:
		flash_helper_jump_to_bootloader();
		break;

	case COMM_ERASE_NEW_APP_ALL_CAN:
		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(6000);
		}

		data[-1] = COMM_ERASE_NEW_APP;
		comm_can_send_buffer(255, data - 1, len + 1, 2);
		chThdSleepMilliseconds(1500);
		/* Falls through. */
		/* no break */
	case COMM_ERASE_NEW_APP: {
		ind = 0;

		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(6000);
		}
		uint16_t flash_res = flash_helper_erase_new_app(buffer_get_uint32(data, &ind));

		ind = 0;
		send_buffer[ind++] = COMM_ERASE_NEW_APP;
		send_buffer[ind++] = flash_res == FLASH_COMPLETE ? 1 : 0;
		commands_send_packet(send_buffer, ind);
	} break;

	case COMM_WRITE_NEW_APP_DATA_ALL_CAN:
		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(2000);
		}

		data[-1] = COMM_WRITE_NEW_APP_DATA;
		comm_can_send_buffer(255, data - 1, len + 1, 2);
		/* Falls through. */
		/* no break */
	case COMM_WRITE_NEW_APP_DATA: {
		ind = 0;
		uint32_t new_app_offset = buffer_get_uint32(data, &ind);

		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(2000);
		}
		uint16_t flash_res = flash_helper_write_new_app_data(new_app_offset, data + ind, len - ind);

		ind = 0;
		send_buffer[ind++] = COMM_WRITE_NEW_APP_DATA;
		send_buffer[ind++] = flash_res == FLASH_COMPLETE ? 1 : 0;
		commands_send_packet(send_buffer, ind);
	} break;

	case COMM_GET_VALUES:
	case COMM_GET_VALUES_SELECTIVE: {
		ind = 0;
		send_buffer[ind++] = packet_id;

		uint32_t mask = 0xFFFFFFFF;
		if (packet_id == COMM_GET_VALUES_SELECTIVE) {
			int32_t ind2 = 0;
			mask = buffer_get_uint32(data, &ind2);
			buffer_append_uint32(send_buffer, mask, &ind);
		}

		if (mask & ((uint32_t)1 << 0)) {
			buffer_append_float16(send_buffer, mc_interface_temp_fet_filtered(), 1e1, &ind);
		}
		if (mask & ((uint32_t)1 << 1)) {
			buffer_append_float16(send_buffer, mc_interface_temp_motor_filtered(), 1e1, &ind);
		}
		if (mask & ((uint32_t)1 << 2)) {
			buffer_append_float32(send_buffer, mc_interface_read_reset_avg_motor_current(), 1e2, &ind);
		}
		if (mask & ((uint32_t)1 << 3)) {
			buffer_append_float32(send_buffer, mc_interface_read_reset_avg_input_current(), 1e2, &ind);
		}
		if (mask & ((uint32_t)1 << 4)) {
			buffer_append_float32(send_buffer, mc_interface_read_reset_avg_id(), 1e2, &ind);
		}
		if (mask & ((uint32_t)1 << 5)) {
			buffer_append_float32(send_buffer, mc_interface_read_reset_avg_iq(), 1e2, &ind);
		}
		if (mask & ((uint32_t)1 << 6)) {
			buffer_append_float16(send_buffer, mc_interface_get_duty_cycle_now(), 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 7)) {
			buffer_append_float32(send_buffer, mc_interface_get_rpm(), 1e0, &ind);
		}
		if (mask & ((uint32_t)1 << 8)) {
			buffer_append_float16(send_buffer, GET_INPUT_VOLTAGE(), 1e1, &ind);
		}
		if (mask & ((uint32_t)1 << 9)) {
			buffer_append_float32(send_buffer, mc_interface_get_amp_hours(false), 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 10)) {
			buffer_append_float32(send_buffer, mc_interface_get_amp_hours_charged(false), 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 11)) {
			buffer_append_float32(send_buffer, mc_interface_get_watt_hours(false), 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 12)) {
			buffer_append_float32(send_buffer, mc_interface_get_watt_hours_charged(false), 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 13)) {
			buffer_append_int32(send_buffer, mc_interface_get_tachometer_value(false), &ind);
		}
		if (mask && ((uint32_t)1 << 14)) {
			buffer_append_int32(send_buffer, mc_interface_get_tachometer_abs_value(false), &ind);
		}
		if (mask & ((uint32_t)1 << 15)) {
			send_buffer[ind++] = mc_interface_get_fault();
		}
		if (mask & ((uint32_t)1 << 16)) {
			buffer_append_float32(send_buffer, mc_interface_get_pid_pos_now(), 1e6, &ind);
		}
		if (mask & ((uint32_t)1 << 17)) {
			send_buffer[ind++] = app_get_configuration()->controller_id;
		}
		if (mask & ((uint32_t)1 << 18)) {
			buffer_append_float16(send_buffer, NTC_TEMP_MOS1(), 1e1, &ind);
			buffer_append_float16(send_buffer, NTC_TEMP_MOS2(), 1e1, &ind);
			buffer_append_float16(send_buffer, NTC_TEMP_MOS3(), 1e1, &ind);
		}

		commands_send_packet(send_buffer, ind);
	} break;

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

	case COMM_SET_HANDBRAKE:
		ind = 0;
		mc_interface_set_handbrake(buffer_get_float32(data, 1e3, &ind));
		timeout_reset();
		break;

	case COMM_SET_DETECT:
		mcconf = *mc_interface_get_configuration();

		ind = 0;
		display_position_mode = data[ind++];

		if (mcconf.motor_type == MOTOR_TYPE_BLDC) {
			if (display_position_mode == DISP_POS_MODE_NONE) {
				mc_interface_release_motor();
			} else if (display_position_mode == DISP_POS_MODE_INDUCTANCE) {
				mcpwm_set_detect();
			}
		}

		timeout_reset();
		break;

	case COMM_SET_SERVO_POS:
#if SERVO_OUT_ENABLE
		ind = 0;
		servo_simple_set_output(buffer_get_float16(data, 1000.0, &ind));
#endif
		break;

	case COMM_SET_MCCONF:
		mcconf = *mc_interface_get_configuration();

		ind = 0;
		mcconf.pwm_mode = data[ind++];
		mcconf.comm_mode = data[ind++];
		mcconf.motor_type = data[ind++];
		mcconf.sensor_mode = data[ind++];

		mcconf.l_current_max = buffer_get_float32_auto(data, &ind);
		mcconf.l_current_min = buffer_get_float32_auto(data, &ind);
		mcconf.l_in_current_max = buffer_get_float32_auto(data, &ind);
		mcconf.l_in_current_min = buffer_get_float32_auto(data, &ind);
		mcconf.l_abs_current_max = buffer_get_float32_auto(data, &ind);
		mcconf.l_min_erpm = buffer_get_float32_auto(data, &ind);
		mcconf.l_max_erpm = buffer_get_float32_auto(data, &ind);
		mcconf.l_erpm_start = buffer_get_float32_auto(data, &ind);
		mcconf.l_max_erpm_fbrake = buffer_get_float32_auto(data, &ind);
		mcconf.l_max_erpm_fbrake_cc = buffer_get_float32_auto(data, &ind);
		mcconf.l_min_vin = buffer_get_float32_auto(data, &ind);
		mcconf.l_max_vin = buffer_get_float32_auto(data, &ind);
		mcconf.l_battery_cut_start = buffer_get_float32_auto(data, &ind);
		mcconf.l_battery_cut_end = buffer_get_float32_auto(data, &ind);
		mcconf.l_slow_abs_current = data[ind++];
		mcconf.l_temp_fet_start = buffer_get_float32_auto(data, &ind);
		mcconf.l_temp_fet_end = buffer_get_float32_auto(data, &ind);
		mcconf.l_temp_motor_start = buffer_get_float32_auto(data, &ind);
		mcconf.l_temp_motor_end = buffer_get_float32_auto(data, &ind);
		mcconf.l_temp_accel_dec = buffer_get_float32_auto(data, &ind);
		mcconf.l_min_duty = buffer_get_float32_auto(data, &ind);
		mcconf.l_max_duty = buffer_get_float32_auto(data, &ind);
		mcconf.l_watt_max = buffer_get_float32_auto(data, &ind);
		mcconf.l_watt_min = buffer_get_float32_auto(data, &ind);
		mcconf.l_current_max_scale = buffer_get_float32_auto(data, &ind);
		mcconf.l_current_min_scale = buffer_get_float32_auto(data, &ind);

		utils_truncate_number(&mcconf.l_current_max_scale , 0.0, 1.0);
		utils_truncate_number(&mcconf.l_current_min_scale , 0.0, 1.0);

		mcconf.lo_current_max = mcconf.l_current_max * mcconf.l_current_max_scale;
		mcconf.lo_current_min = mcconf.l_current_min * mcconf.l_current_min_scale;
		mcconf.lo_in_current_max = mcconf.l_in_current_max;
		mcconf.lo_in_current_min = mcconf.l_in_current_min;
		mcconf.lo_current_motor_max_now = mcconf.lo_current_max;
		mcconf.lo_current_motor_min_now = mcconf.lo_current_min;

		mcconf.sl_min_erpm = buffer_get_float32_auto(data, &ind);
		mcconf.sl_min_erpm_cycle_int_limit = buffer_get_float32_auto(data, &ind);
		mcconf.sl_max_fullbreak_current_dir_change = buffer_get_float32_auto(data, &ind);
		mcconf.sl_cycle_int_limit = buffer_get_float32_auto(data, &ind);
		mcconf.sl_phase_advance_at_br = buffer_get_float32_auto(data, &ind);
		mcconf.sl_cycle_int_rpm_br = buffer_get_float32_auto(data, &ind);
		mcconf.sl_bemf_coupling_k = buffer_get_float32_auto(data, &ind);

		memcpy(mcconf.hall_table, data + ind, 8);
		ind += 8;
		mcconf.hall_sl_erpm = buffer_get_float32_auto(data, &ind);

		mcconf.foc_current_kp = buffer_get_float32_auto(data, &ind);
		mcconf.foc_current_ki = buffer_get_float32_auto(data, &ind);
		mcconf.foc_f_sw = buffer_get_float32_auto(data, &ind);
		mcconf.foc_dt_us = buffer_get_float32_auto(data, &ind);
		mcconf.foc_encoder_inverted = data[ind++];
		mcconf.foc_encoder_offset = buffer_get_float32_auto(data, &ind);
		mcconf.foc_encoder_ratio = buffer_get_float32_auto(data, &ind);
		mcconf.foc_sensor_mode = data[ind++];
		mcconf.foc_pll_kp = buffer_get_float32_auto(data, &ind);
		mcconf.foc_pll_ki = buffer_get_float32_auto(data, &ind);
		mcconf.foc_motor_l = buffer_get_float32_auto(data, &ind);
		mcconf.foc_motor_r = buffer_get_float32_auto(data, &ind);
		mcconf.foc_motor_flux_linkage = buffer_get_float32_auto(data, &ind);
		mcconf.foc_observer_gain = buffer_get_float32_auto(data, &ind);
		mcconf.foc_observer_gain_slow = buffer_get_float32_auto(data, &ind);
		mcconf.foc_duty_dowmramp_kp = buffer_get_float32_auto(data, &ind);
		mcconf.foc_duty_dowmramp_ki = buffer_get_float32_auto(data, &ind);
		mcconf.foc_openloop_rpm = buffer_get_float32_auto(data, &ind);
		mcconf.foc_sl_openloop_hyst = buffer_get_float32_auto(data, &ind);
		mcconf.foc_sl_openloop_time = buffer_get_float32_auto(data, &ind);
		mcconf.foc_sl_d_current_duty = buffer_get_float32_auto(data, &ind);
		mcconf.foc_sl_d_current_factor = buffer_get_float32_auto(data, &ind);
		memcpy(mcconf.foc_hall_table, data + ind, 8);
		ind += 8;
		mcconf.foc_sl_erpm = buffer_get_float32_auto(data, &ind);
		mcconf.foc_sample_v0_v7 = data[ind++];
		mcconf.foc_sample_high_current = data[ind++];
		mcconf.foc_sat_comp = buffer_get_float32_auto(data, &ind);
		mcconf.foc_temp_comp = data[ind++];
		mcconf.foc_temp_comp_base_temp = buffer_get_float32_auto(data, &ind);
		mcconf.foc_current_filter_const = buffer_get_float32_auto(data, &ind);

		mcconf.gpd_buffer_notify_left = buffer_get_int16(data, &ind);
		mcconf.gpd_buffer_interpol = buffer_get_int16(data, &ind);
		mcconf.gpd_current_filter_const = buffer_get_float32_auto(data, &ind);
		mcconf.gpd_current_kp = buffer_get_float32_auto(data, &ind);
		mcconf.gpd_current_ki = buffer_get_float32_auto(data, &ind);

		mcconf.s_pid_kp = buffer_get_float32_auto(data, &ind);
		mcconf.s_pid_ki = buffer_get_float32_auto(data, &ind);
		mcconf.s_pid_kd = buffer_get_float32_auto(data, &ind);
		mcconf.s_pid_kd_filter = buffer_get_float32_auto(data, &ind);
		mcconf.s_pid_min_erpm = buffer_get_float32_auto(data, &ind);
		mcconf.s_pid_allow_braking = data[ind++];

		mcconf.p_pid_kp = buffer_get_float32_auto(data, &ind);
		mcconf.p_pid_ki = buffer_get_float32_auto(data, &ind);
		mcconf.p_pid_kd = buffer_get_float32_auto(data, &ind);
		mcconf.p_pid_kd_filter = buffer_get_float32_auto(data, &ind);
		mcconf.p_pid_ang_div = buffer_get_float32_auto(data, &ind);

		mcconf.cc_startup_boost_duty = buffer_get_float32_auto(data, &ind);
		mcconf.cc_min_current = buffer_get_float32_auto(data, &ind);
		mcconf.cc_gain = buffer_get_float32_auto(data, &ind);
		mcconf.cc_ramp_step_max = buffer_get_float32_auto(data, &ind);

		mcconf.m_fault_stop_time_ms = buffer_get_int32(data, &ind);
		mcconf.m_duty_ramp_step = buffer_get_float32_auto(data, &ind);
		mcconf.m_current_backoff_gain = buffer_get_float32_auto(data, &ind);
		mcconf.m_encoder_counts = buffer_get_uint32(data, &ind);
		mcconf.m_sensor_port_mode = data[ind++];
		mcconf.m_invert_direction = data[ind++];
		mcconf.m_drv8301_oc_mode = data[ind++];
		mcconf.m_drv8301_oc_adj = data[ind++];
		mcconf.m_bldc_f_sw_min = buffer_get_float32_auto(data, &ind);
		mcconf.m_bldc_f_sw_max = buffer_get_float32_auto(data, &ind);
		mcconf.m_dc_f_sw = buffer_get_float32_auto(data, &ind);
		mcconf.m_ntc_motor_beta = buffer_get_float32_auto(data, &ind);
		mcconf.m_out_aux_mode = data[ind++];

		mcconf.si_motor_poles = data[ind++];
		mcconf.si_gear_ratio = buffer_get_float32_auto(data, &ind);
		mcconf.si_wheel_diameter = buffer_get_float32_auto(data, &ind);
		mcconf.si_battery_type = data[ind++];
		mcconf.si_battery_cells = data[ind++];
		mcconf.si_battery_ah = buffer_get_float32_auto(data, &ind);

		commands_apply_mcconf_hw_limits(&mcconf);
		conf_general_store_mc_configuration(&mcconf);
		mc_interface_set_configuration(&mcconf);
		chThdSleepMilliseconds(200);

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

		commands_send_mcconf(packet_id, &mcconf);
		break;

	case COMM_SET_APPCONF:
		appconf = *app_get_configuration();

		ind = 0;
		appconf.controller_id = data[ind++];
		appconf.timeout_msec = buffer_get_uint32(data, &ind);
		appconf.timeout_brake_current = buffer_get_float32_auto(data, &ind);
		appconf.send_can_status = data[ind++];
		appconf.send_can_status_rate_hz = buffer_get_uint16(data, &ind);
		appconf.can_baud_rate = data[ind++];
		appconf.pairing_done = data[ind++];

		appconf.uavcan_enable = data[ind++];
		appconf.uavcan_esc_index = data[ind++];

		appconf.app_to_use = data[ind++];

		appconf.app_ppm_conf.ctrl_type = data[ind++];
		appconf.app_ppm_conf.pid_max_erpm = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.hyst = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.pulse_start = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.pulse_end = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.pulse_center = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.median_filter = data[ind++];
		appconf.app_ppm_conf.safe_start = data[ind++];
		appconf.app_ppm_conf.throttle_exp = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.throttle_exp_brake = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.throttle_exp_mode = data[ind++];
		appconf.app_ppm_conf.ramp_time_pos = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.ramp_time_neg = buffer_get_float32_auto(data, &ind);
		appconf.app_ppm_conf.multi_esc = data[ind++];
		appconf.app_ppm_conf.tc = data[ind++];
		appconf.app_ppm_conf.tc_max_diff = buffer_get_float32_auto(data, &ind);

		appconf.app_adc_conf.ctrl_type = data[ind++];
		appconf.app_adc_conf.hyst = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.voltage_start = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.voltage_end = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.voltage_center = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.voltage2_start = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.voltage2_end = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.use_filter = data[ind++];
		appconf.app_adc_conf.safe_start = data[ind++];
		appconf.app_adc_conf.cc_button_inverted = data[ind++];
		appconf.app_adc_conf.rev_button_inverted = data[ind++];
		appconf.app_adc_conf.voltage_inverted = data[ind++];
		appconf.app_adc_conf.voltage2_inverted = data[ind++];
		appconf.app_adc_conf.throttle_exp = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.throttle_exp_brake = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.throttle_exp_mode = data[ind++];
		appconf.app_adc_conf.ramp_time_pos = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.ramp_time_neg = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.multi_esc = data[ind++];
		appconf.app_adc_conf.tc = data[ind++];
		appconf.app_adc_conf.tc_max_diff = buffer_get_float32_auto(data, &ind);
		appconf.app_adc_conf.update_rate_hz = buffer_get_uint16(data, &ind);

		appconf.app_uart_baudrate = buffer_get_uint32(data, &ind);

		appconf.app_chuk_conf.ctrl_type = data[ind++];
		appconf.app_chuk_conf.hyst = buffer_get_float32_auto(data, &ind);
		appconf.app_chuk_conf.ramp_time_pos = buffer_get_float32_auto(data, &ind);
		appconf.app_chuk_conf.ramp_time_neg = buffer_get_float32_auto(data, &ind);
		appconf.app_chuk_conf.stick_erpm_per_s_in_cc = buffer_get_float32_auto(data, &ind);
		appconf.app_chuk_conf.throttle_exp = buffer_get_float32_auto(data, &ind);
		appconf.app_chuk_conf.throttle_exp_brake = buffer_get_float32_auto(data, &ind);
		appconf.app_chuk_conf.throttle_exp_mode = data[ind++];
		appconf.app_chuk_conf.multi_esc = data[ind++];
		appconf.app_chuk_conf.tc = data[ind++];
		appconf.app_chuk_conf.tc_max_diff = buffer_get_float32_auto(data, &ind);

		appconf.app_nrf_conf.speed = data[ind++];
		appconf.app_nrf_conf.power = data[ind++];
		appconf.app_nrf_conf.crc_type = data[ind++];
		appconf.app_nrf_conf.retry_delay = data[ind++];
		appconf.app_nrf_conf.retries = data[ind++];
		appconf.app_nrf_conf.channel = data[ind++];
		memcpy(appconf.app_nrf_conf.address, data + ind, 3);
		ind += 3;
		appconf.app_nrf_conf.send_crc_ack = data[ind++];

		conf_general_store_app_configuration(&appconf);
		app_set_configuration(&appconf);
		timeout_configure(appconf.timeout_msec, appconf.timeout_brake_current);
		chThdSleepMilliseconds(200);

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

		commands_send_appconf(packet_id, &appconf);
		break;

	case COMM_SAMPLE_PRINT: {
		uint16_t sample_len;
		uint8_t decimation;
		debug_sampling_mode mode;

		ind = 0;
		mode = data[ind++];
		sample_len = buffer_get_uint16(data, &ind);
		decimation = data[ind++];
		mc_interface_sample_print_data(mode, sample_len, decimation);
	} break;

	case COMM_TERMINAL_CMD:
		data[len] = '\0';
		terminal_process_string((char*)data);
		break;

	case COMM_DETECT_MOTOR_PARAM:
	case COMM_DETECT_MOTOR_R_L:
	case COMM_DETECT_MOTOR_FLUX_LINKAGE:
	case COMM_DETECT_ENCODER:
	case COMM_DETECT_HALL_FOC:
	case COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP:
	case COMM_DETECT_APPLY_ALL_FOC:
		if (!is_detecting && len < sizeof(detect_thread_cmd_buffer)) {
			memcpy(detect_thread_cmd_buffer, data - 1, len + 1);
			is_detecting = true;
			send_func_detect = send_func;
			chEvtSignal(detect_tp, (eventmask_t)1);
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
		buffer_append_int32(send_buffer, (int32_t)(app_ppm_get_decoded_level() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(servodec_get_last_pulse_len(0) * 1000000.0), &ind);
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GET_DECODED_ADC:
		ind = 0;
		send_buffer[ind++] = COMM_GET_DECODED_ADC;
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_decoded_level() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_voltage() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_decoded_level2() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_voltage2() * 1000000.0), &ind);
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GET_DECODED_CHUK:
		ind = 0;
		send_buffer[ind++] = COMM_GET_DECODED_CHUK;
		buffer_append_int32(send_buffer, (int32_t)(app_nunchuk_get_decoded_chuk() * 1000000.0), &ind);
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_FORWARD_CAN:
		comm_can_send_buffer(data[0], data + 1, len - 1, 0);
		break;

	case COMM_SET_CHUCK_DATA: {
		chuck_data chuck_d_tmp;

		ind = 0;
		chuck_d_tmp.js_x = data[ind++];
		chuck_d_tmp.js_y = data[ind++];
		chuck_d_tmp.bt_c = data[ind++];
		chuck_d_tmp.bt_z = data[ind++];
		chuck_d_tmp.acc_x = buffer_get_int16(data, &ind);
		chuck_d_tmp.acc_y = buffer_get_int16(data, &ind);
		chuck_d_tmp.acc_z = buffer_get_int16(data, &ind);

		if (len >= (unsigned int)ind + 2) {
			chuck_d_tmp.rev_has_state = data[ind++];
			chuck_d_tmp.is_rev = data[ind++];
		} else {
			chuck_d_tmp.rev_has_state = false;
			chuck_d_tmp.is_rev = false;
		}
		app_nunchuk_update_output(&chuck_d_tmp);
	} break;

	case COMM_CUSTOM_APP_DATA:
		if (appdata_func) {
			appdata_func(data, len);
		}
		break;

	case COMM_NRF_START_PAIRING:
		ind = 0;
		nrf_driver_start_pairing(buffer_get_int32(data, &ind));

		ind = 0;
		send_buffer[ind++] = packet_id;
		send_buffer[ind++] = NRF_PAIR_STARTED;
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GPD_SET_FSW:
		timeout_reset();
		ind = 0;
		gpdrive_set_switching_frequency((float)buffer_get_int32(data, &ind));
		break;

	case COMM_GPD_BUFFER_SIZE_LEFT:
		ind = 0;
		send_buffer[ind++] = COMM_GPD_BUFFER_SIZE_LEFT;
		buffer_append_int32(send_buffer, gpdrive_buffer_size_left(), &ind);
		commands_send_packet(send_buffer, ind);
		break;

	case COMM_GPD_FILL_BUFFER:
		timeout_reset();
		ind = 0;
		while (ind < (int)len) {
			gpdrive_add_buffer_sample(buffer_get_float32_auto(data, &ind));
		}
		break;

	case COMM_GPD_OUTPUT_SAMPLE:
		timeout_reset();
		ind = 0;
		gpdrive_add_buffer_sample(buffer_get_float32_auto(data, &ind));
		break;

	case COMM_GPD_SET_MODE:
		timeout_reset();
		ind = 0;
		gpdrive_set_mode(data[ind++]);
		break;

	case COMM_GPD_FILL_BUFFER_INT8:
		timeout_reset();
		ind = 0;
		while (ind < (int)len) {
			gpdrive_add_buffer_sample_int((int8_t)data[ind++]);
		}
		break;

	case COMM_GPD_FILL_BUFFER_INT16:
		timeout_reset();
		ind = 0;
		while (ind < (int)len) {
			gpdrive_add_buffer_sample_int(buffer_get_int16(data, &ind));
		}
		break;

	case COMM_GPD_SET_BUFFER_INT_SCALE:
		ind = 0;
		gpdrive_set_buffer_int_scale(buffer_get_float32_auto(data, &ind));
		break;

	case COMM_GET_VALUES_SETUP:
	case COMM_GET_VALUES_SETUP_SELECTIVE: {
		float ah_tot = 0.0;
		float ah_charge_tot = 0.0;
		float wh_tot = 0.0;
		float wh_charge_tot = 0.0;
		float current_tot = 0.0;
		float current_in_tot = 0.0;
		uint8_t num_vescs = 1;

		ah_tot += mc_interface_get_amp_hours(false);
		ah_charge_tot += mc_interface_get_amp_hours_charged(false);
		wh_tot += mc_interface_get_watt_hours(false);
		wh_charge_tot += mc_interface_get_watt_hours_charged(false);
		current_tot += mc_interface_get_tot_current_filtered();
		current_in_tot += mc_interface_get_tot_current_in_filtered();

		for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
			can_status_msg *msg = comm_can_get_status_msg_index(i);
			if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < 0.1) {
				current_tot += msg->current;
				num_vescs++;
			}

			can_status_msg_2 *msg2 = comm_can_get_status_msg_2_index(i);
			if (msg2->id >= 0 && UTILS_AGE_S(msg2->rx_time) < 0.1) {
				ah_tot += msg2->amp_hours;
				ah_charge_tot += msg2->amp_hours_charged;
			}

			can_status_msg_3 *msg3 = comm_can_get_status_msg_3_index(i);
			if (msg3->id >= 0 && UTILS_AGE_S(msg3->rx_time) < 0.1) {
				wh_tot += msg3->watt_hours;
				wh_charge_tot += msg3->watt_hours_charged;
			}

			can_status_msg_4 *msg4 = comm_can_get_status_msg_4_index(i);
			if (msg4->id >= 0 && UTILS_AGE_S(msg4->rx_time) < 0.1) {
				current_in_tot += msg4->current_in;
			}
		}

		float wh_batt_left = 0.0;
		float battery_level = mc_interface_get_battery_level(&wh_batt_left);

		ind = 0;
		send_buffer[ind++] = packet_id;

		uint32_t mask = 0xFFFFFFFF;
		if (packet_id == COMM_GET_VALUES_SETUP_SELECTIVE) {
			int32_t ind2 = 0;
			mask = buffer_get_uint32(data, &ind2);
			buffer_append_uint32(send_buffer, mask, &ind);
		}

		if (mask & ((uint32_t)1 << 0)) {
			buffer_append_float16(send_buffer, mc_interface_temp_fet_filtered(), 1e1, &ind);
		}
		if (mask & ((uint32_t)1 << 1)) {
			buffer_append_float16(send_buffer, mc_interface_temp_motor_filtered(), 1e1, &ind);
		}
		if (mask & ((uint32_t)1 << 2)) {
			buffer_append_float32(send_buffer, current_tot, 1e2, &ind);
		}
		if (mask & ((uint32_t)1 << 3)) {
			buffer_append_float32(send_buffer, current_in_tot, 1e2, &ind);
		}
		if (mask & ((uint32_t)1 << 4)) {
			buffer_append_float16(send_buffer, mc_interface_get_duty_cycle_now(), 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 5)) {
			buffer_append_float32(send_buffer, mc_interface_get_rpm(), 1e0, &ind);
		}
		if (mask & ((uint32_t)1 << 6)) {
			buffer_append_float32(send_buffer, mc_interface_get_speed(), 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 7)) {
			buffer_append_float16(send_buffer, GET_INPUT_VOLTAGE(), 1e1, &ind);
		}
		if (mask & ((uint32_t)1 << 8)) {
			buffer_append_float16(send_buffer, battery_level, 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 9)) {
			buffer_append_float32(send_buffer, ah_tot, 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 10)) {
			buffer_append_float32(send_buffer, ah_charge_tot, 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 11)) {
			buffer_append_float32(send_buffer, wh_tot, 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 12)) {
			buffer_append_float32(send_buffer, wh_charge_tot, 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 13)) {
			buffer_append_float32(send_buffer, mc_interface_get_distance(), 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 14)) {
			buffer_append_float32(send_buffer, mc_interface_get_distance_abs(), 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 15)) {
			buffer_append_float32(send_buffer, mc_interface_get_pid_pos_now(), 1e6, &ind);
		}
		if (mask & ((uint32_t)1 << 16)) {
			send_buffer[ind++] = mc_interface_get_fault();
		}
		if (mask & ((uint32_t)1 << 17)) {
			send_buffer[ind++] = app_get_configuration()->controller_id;
		}
		if (mask & ((uint32_t)1 << 18)) {
			send_buffer[ind++] = num_vescs;
		}
		if (mask & ((uint32_t)1 << 19)) {
			buffer_append_float32(send_buffer, wh_batt_left, 1e3, &ind);
		}

		commands_send_packet(send_buffer, ind);
	} break;

	case COMM_SET_MCCONF_TEMP:
	case COMM_SET_MCCONF_TEMP_SETUP: {
		mcconf = *mc_interface_get_configuration();

		ind = 0;
		bool store = data[ind++];
		bool forward_can = data[ind++];
		bool ack = data[ind++];
		bool divide_by_controllers = data[ind++];

		float controller_num = 1.0;

		if (divide_by_controllers) {
			for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);
				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < 0.1) {
					controller_num += 1.0;
				}
			}
		}

		mcconf.l_current_min_scale = buffer_get_float32_auto(data, &ind);
		mcconf.l_current_max_scale = buffer_get_float32_auto(data, &ind);

		if (packet_id == COMM_SET_MCCONF_TEMP_SETUP) {
			const float fact = ((mcconf.si_motor_poles / 2.0) * 60.0 *
					mcconf.si_gear_ratio) / (mcconf.si_wheel_diameter * M_PI);

			mcconf.l_min_erpm = buffer_get_float32_auto(data, &ind) * fact;
			mcconf.l_max_erpm = buffer_get_float32_auto(data, &ind) * fact;

			// Write computed RPM back and change forwarded packet id to
			// COMM_SET_MCCONF_TEMP. This way only the master has to be
			// aware of the setup information.
			ind -= 8;
			buffer_append_float32_auto(data, mcconf.l_min_erpm, &ind);
			buffer_append_float32_auto(data, mcconf.l_max_erpm, &ind);
		} else {
			mcconf.l_min_erpm = buffer_get_float32_auto(data, &ind);
			mcconf.l_max_erpm = buffer_get_float32_auto(data, &ind);
		}

		mcconf.l_min_duty = buffer_get_float32_auto(data, &ind);
		mcconf.l_max_duty = buffer_get_float32_auto(data, &ind);
		mcconf.l_watt_min = buffer_get_float32_auto(data, &ind) / controller_num;
		mcconf.l_watt_max = buffer_get_float32_auto(data, &ind) / controller_num;

		// Write divided data back to the buffer, as the other controllers have no way to tell
		// how many controllers are on the bus and thus need pre-divided data.
		// We set divide by controllers to false before forwarding.
		ind -= 8;
		buffer_append_float32_auto(data, mcconf.l_watt_min, &ind);
		buffer_append_float32_auto(data, mcconf.l_watt_max, &ind);

		mcconf.lo_current_min = mcconf.l_current_min * mcconf.l_current_min_scale;
		mcconf.lo_current_max = mcconf.l_current_max * mcconf.l_current_max_scale;
		mcconf.lo_current_motor_min_now = mcconf.lo_current_min;
		mcconf.lo_current_motor_max_now = mcconf.lo_current_max;

		commands_apply_mcconf_hw_limits(&mcconf);

		if (store) {
			conf_general_store_mc_configuration(&mcconf);
		}

		mc_interface_set_configuration(&mcconf);

		if (forward_can) {
			data[-1] = COMM_SET_MCCONF_TEMP;
			data[1] = 0; // No more forward
			data[2] = 0; // No ack
			data[3] = 0; // No dividing, see comment above

			// TODO: Maybe broadcast on CAN-bus?
			for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);
				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < 0.1) {
					comm_can_send_buffer(msg->id, data - 1, len + 1, 0);
				}
			}
		}

		if (ack) {
			ind = 0;
			send_buffer[ind++] = packet_id;
			commands_send_packet(send_buffer, ind);
		}
	} break;

	case COMM_EXT_NRF_PRESENT: {
		send_func_nrf = send_func;
		send_func = send_func_last;

		if (!conf_general_permanent_nrf_found) {
			nrf_driver_init_ext_nrf();
			if (!nrf_driver_is_pairing()) {
				const app_configuration *appconf_ptr = app_get_configuration();
				send_buffer[0] = COMM_EXT_NRF_ESB_SET_CH_ADDR;
				send_buffer[1] = appconf_ptr->app_nrf_conf.channel;
				send_buffer[2] = appconf_ptr->app_nrf_conf.address[0];
				send_buffer[3] = appconf_ptr->app_nrf_conf.address[1];
				send_buffer[4] = appconf_ptr->app_nrf_conf.address[2];
				commands_send_packet_nrf(send_buffer, 5);
			}
		}
	} break;

	case COMM_EXT_NRF_ESB_RX_DATA: {
		send_func_nrf = send_func;
		send_func = send_func_last;
		nrf_driver_process_packet(data, len);
	} break;

	case COMM_PING_CAN:
		ind = 0;
		// Send buffer cannot be used, as it might be altered while waiting
		// for pings.
		uint8_t buffer[256];
		buffer[ind++] = COMM_PING_CAN;

		for (uint8_t i = 0;i < 255;i++) {
			if (comm_can_ping(i)) {
				buffer[ind++] = i;
			}
		}

		commands_send_packet(buffer, ind);
		break;

	case COMM_APP_DISABLE_OUTPUT: {
		ind = 0;
		bool fwd_can = data[ind++];
		int time = buffer_get_int32(data, &ind);
		app_disable_output(time);

		if (fwd_can) {
			data[0] = 0; // Don't continue forwarding
			comm_can_send_buffer(255, data - 1, len + 1, 0);
		}
	} break;

	default:
		break;
	}
}

void commands_printf(const char* format, ...) {
	va_list arg;
	va_start (arg, format);
	int len;
	static char print_buffer[255];

	print_buffer[0] = COMM_PRINT;
	len = vsnprintf(print_buffer+1, 254, format, arg);
	va_end (arg);

	if(len > 0) {
		commands_send_packet((unsigned char*)print_buffer, (len<254)? len+1: 255);
	}
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

disp_pos_mode commands_get_disp_pos_mode(void) {
	return display_position_mode;
}

void commands_set_app_data_handler(void(*func)(unsigned char *data, unsigned int len)) {
	appdata_func = func;
}

void commands_send_app_data(unsigned char *data, unsigned int len) {
	int32_t index = 0;

	send_buffer[index++] = COMM_CUSTOM_APP_DATA;
	memcpy(send_buffer + index, data, len);
	index += len;

	commands_send_packet(send_buffer, index);
}

void commands_send_gpd_buffer_notify(void) {
	int32_t index = 0;
	send_buffer[index++] = COMM_GPD_BUFFER_NOTIFY;
	commands_send_packet(send_buffer, index);
}

void commands_send_mcconf(COMM_PACKET_ID packet_id, mc_configuration *mcconf) {
	int32_t ind = 0;
	send_buffer[ind++] = packet_id;

	send_buffer[ind++] = mcconf->pwm_mode;
	send_buffer[ind++] = mcconf->comm_mode;
	send_buffer[ind++] = mcconf->motor_type;
	send_buffer[ind++] = mcconf->sensor_mode;

	buffer_append_float32_auto(send_buffer, mcconf->l_current_max, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_current_min, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_in_current_max, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_in_current_min, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_abs_current_max, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_min_erpm, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_max_erpm, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_erpm_start, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_max_erpm_fbrake, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_max_erpm_fbrake_cc, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_min_vin, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_max_vin, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_battery_cut_start, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_battery_cut_end, &ind);
	send_buffer[ind++] = mcconf->l_slow_abs_current;
	buffer_append_float32_auto(send_buffer, mcconf->l_temp_fet_start, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_temp_fet_end, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_temp_motor_start, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_temp_motor_end, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_temp_accel_dec, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_min_duty, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_max_duty, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_watt_max, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_watt_min, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_current_max_scale, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->l_current_min_scale, &ind);

	buffer_append_float32_auto(send_buffer, mcconf->sl_min_erpm, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->sl_min_erpm_cycle_int_limit, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->sl_max_fullbreak_current_dir_change, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->sl_cycle_int_limit, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->sl_phase_advance_at_br, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->sl_cycle_int_rpm_br, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->sl_bemf_coupling_k, &ind);

	memcpy(send_buffer + ind, mcconf->hall_table, 8);
	ind += 8;
	buffer_append_float32_auto(send_buffer, mcconf->hall_sl_erpm, &ind);

	buffer_append_float32_auto(send_buffer, mcconf->foc_current_kp, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_current_ki, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_f_sw, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_dt_us, &ind);
	send_buffer[ind++] = mcconf->foc_encoder_inverted;
	buffer_append_float32_auto(send_buffer, mcconf->foc_encoder_offset, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_encoder_ratio, &ind);
	send_buffer[ind++] = mcconf->foc_sensor_mode;
	buffer_append_float32_auto(send_buffer, mcconf->foc_pll_kp, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_pll_ki, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_motor_l, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_motor_r, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_motor_flux_linkage, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_observer_gain, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_observer_gain_slow, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_duty_dowmramp_kp, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_duty_dowmramp_ki, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_openloop_rpm, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_sl_openloop_hyst, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_sl_openloop_time, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_sl_d_current_duty, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_sl_d_current_factor, &ind);
	memcpy(send_buffer + ind, mcconf->foc_hall_table, 8);
	ind += 8;
	buffer_append_float32_auto(send_buffer, mcconf->foc_sl_erpm, &ind);
	send_buffer[ind++] = mcconf->foc_sample_v0_v7;
	send_buffer[ind++] = mcconf->foc_sample_high_current;
	buffer_append_float32_auto(send_buffer, mcconf->foc_sat_comp, &ind);
	send_buffer[ind++] = mcconf->foc_temp_comp;
	buffer_append_float32_auto(send_buffer, mcconf->foc_temp_comp_base_temp, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->foc_current_filter_const, &ind);

	buffer_append_int16(send_buffer, mcconf->gpd_buffer_notify_left, &ind);
	buffer_append_int16(send_buffer, mcconf->gpd_buffer_interpol, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->gpd_current_filter_const, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->gpd_current_kp, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->gpd_current_ki, &ind);

	buffer_append_float32_auto(send_buffer, mcconf->s_pid_kp, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->s_pid_ki, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->s_pid_kd, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->s_pid_kd_filter, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->s_pid_min_erpm, &ind);
	send_buffer[ind++] = mcconf->s_pid_allow_braking;

	buffer_append_float32_auto(send_buffer, mcconf->p_pid_kp, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->p_pid_ki, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->p_pid_kd, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->p_pid_kd_filter, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->p_pid_ang_div, &ind);

	buffer_append_float32_auto(send_buffer, mcconf->cc_startup_boost_duty, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->cc_min_current, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->cc_gain, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->cc_ramp_step_max, &ind);

	buffer_append_int32(send_buffer, mcconf->m_fault_stop_time_ms, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->m_duty_ramp_step, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->m_current_backoff_gain, &ind);
	buffer_append_uint32(send_buffer, mcconf->m_encoder_counts, &ind);
	send_buffer[ind++] = mcconf->m_sensor_port_mode;
	send_buffer[ind++] = mcconf->m_invert_direction;
	send_buffer[ind++] = mcconf->m_drv8301_oc_mode;
	send_buffer[ind++] = mcconf->m_drv8301_oc_adj;
	buffer_append_float32_auto(send_buffer, mcconf->m_bldc_f_sw_min, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->m_bldc_f_sw_max, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->m_dc_f_sw, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->m_ntc_motor_beta, &ind);
	send_buffer[ind++] = mcconf->m_out_aux_mode;

	send_buffer[ind++] = mcconf->si_motor_poles;
	buffer_append_float32_auto(send_buffer, mcconf->si_gear_ratio, &ind);
	buffer_append_float32_auto(send_buffer, mcconf->si_wheel_diameter, &ind);
	send_buffer[ind++] = mcconf->si_battery_type;
	send_buffer[ind++] = mcconf->si_battery_cells;
	buffer_append_float32_auto(send_buffer, mcconf->si_battery_ah, &ind);

	commands_send_packet(send_buffer, ind);
}

void commands_send_appconf(COMM_PACKET_ID packet_id, app_configuration *appconf) {
	int32_t ind = 0;
	send_buffer[ind++] = packet_id;
	send_buffer[ind++] = appconf->controller_id;
	buffer_append_uint32(send_buffer, appconf->timeout_msec, &ind);
	buffer_append_float32_auto(send_buffer, appconf->timeout_brake_current, &ind);
	send_buffer[ind++] = appconf->send_can_status;
	buffer_append_uint16(send_buffer, appconf->send_can_status_rate_hz, &ind);
	send_buffer[ind++] = appconf->can_baud_rate;
	send_buffer[ind++] = appconf->pairing_done;

	send_buffer[ind++] = appconf->uavcan_enable;
	send_buffer[ind++] = appconf->uavcan_esc_index;

	send_buffer[ind++] = appconf->app_to_use;

	send_buffer[ind++] = appconf->app_ppm_conf.ctrl_type;
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.pid_max_erpm, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.hyst, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.pulse_start, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.pulse_end, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.pulse_center, &ind);
	send_buffer[ind++] = appconf->app_ppm_conf.median_filter;
	send_buffer[ind++] = appconf->app_ppm_conf.safe_start;
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.throttle_exp, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.throttle_exp_brake, &ind);
	send_buffer[ind++] = appconf->app_ppm_conf.throttle_exp_mode;
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.ramp_time_pos, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.ramp_time_neg, &ind);
	send_buffer[ind++] = appconf->app_ppm_conf.multi_esc;
	send_buffer[ind++] = appconf->app_ppm_conf.tc;
	buffer_append_float32_auto(send_buffer, appconf->app_ppm_conf.tc_max_diff, &ind);

	send_buffer[ind++] = appconf->app_adc_conf.ctrl_type;
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.hyst, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.voltage_start, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.voltage_end, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.voltage_center, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.voltage2_start, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.voltage2_end, &ind);
	send_buffer[ind++] = appconf->app_adc_conf.use_filter;
	send_buffer[ind++] = appconf->app_adc_conf.safe_start;
	send_buffer[ind++] = appconf->app_adc_conf.cc_button_inverted;
	send_buffer[ind++] = appconf->app_adc_conf.rev_button_inverted;
	send_buffer[ind++] = appconf->app_adc_conf.voltage_inverted;
	send_buffer[ind++] = appconf->app_adc_conf.voltage2_inverted;
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.throttle_exp, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.throttle_exp_brake, &ind);
	send_buffer[ind++] = appconf->app_adc_conf.throttle_exp_mode;
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.ramp_time_pos, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.ramp_time_neg, &ind);
	send_buffer[ind++] = appconf->app_adc_conf.multi_esc;
	send_buffer[ind++] = appconf->app_adc_conf.tc;
	buffer_append_float32_auto(send_buffer, appconf->app_adc_conf.tc_max_diff, &ind);
	buffer_append_uint16(send_buffer, appconf->app_adc_conf.update_rate_hz, &ind);

	buffer_append_uint32(send_buffer, appconf->app_uart_baudrate, &ind);

	send_buffer[ind++] = appconf->app_chuk_conf.ctrl_type;
	buffer_append_float32_auto(send_buffer, appconf->app_chuk_conf.hyst, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_chuk_conf.ramp_time_pos, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_chuk_conf.ramp_time_neg, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_chuk_conf.stick_erpm_per_s_in_cc, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_chuk_conf.throttle_exp, &ind);
	buffer_append_float32_auto(send_buffer, appconf->app_chuk_conf.throttle_exp_brake, &ind);
	send_buffer[ind++] = appconf->app_chuk_conf.throttle_exp_mode;
	send_buffer[ind++] = appconf->app_chuk_conf.multi_esc;
	send_buffer[ind++] = appconf->app_chuk_conf.tc;
	buffer_append_float32_auto(send_buffer, appconf->app_chuk_conf.tc_max_diff, &ind);

	send_buffer[ind++] = appconf->app_nrf_conf.speed;
	send_buffer[ind++] = appconf->app_nrf_conf.power;
	send_buffer[ind++] = appconf->app_nrf_conf.crc_type;
	send_buffer[ind++] = appconf->app_nrf_conf.retry_delay;
	send_buffer[ind++] = appconf->app_nrf_conf.retries;
	send_buffer[ind++] = appconf->app_nrf_conf.channel;
	memcpy(send_buffer + ind, appconf->app_nrf_conf.address, 3);
	ind += 3;
	send_buffer[ind++] = appconf->app_nrf_conf.send_crc_ack;

	commands_send_packet(send_buffer, ind);
}

void commands_apply_mcconf_hw_limits(mc_configuration *mcconf) {
	utils_truncate_number(&mcconf->l_current_max_scale, 0.0, 1.0);
	utils_truncate_number(&mcconf->l_current_min_scale, 0.0, 1.0);

#ifndef DISABLE_HW_LIMITS
#ifdef HW_LIM_CURRENT
	utils_truncate_number(&mcconf->l_current_max, HW_LIM_CURRENT);
	utils_truncate_number(&mcconf->l_current_min, HW_LIM_CURRENT);
#endif
#ifdef HW_LIM_CURRENT_IN
	utils_truncate_number(&mcconf->l_in_current_max, HW_LIM_CURRENT_IN);
	utils_truncate_number(&mcconf->l_in_current_min, HW_LIM_CURRENT);
#endif
#ifdef HW_LIM_CURRENT_ABS
	utils_truncate_number(&mcconf->l_abs_current_max, HW_LIM_CURRENT_ABS);
#endif
#ifdef HW_LIM_VIN
	utils_truncate_number(&mcconf->l_max_vin, HW_LIM_VIN);
	utils_truncate_number(&mcconf->l_min_vin, HW_LIM_VIN);
#endif
#ifdef HW_LIM_ERPM
	utils_truncate_number(&mcconf->l_max_erpm, HW_LIM_ERPM);
	utils_truncate_number(&mcconf->l_min_erpm, HW_LIM_ERPM);
#endif
#ifdef HW_LIM_DUTY_MIN
	utils_truncate_number(&mcconf->l_min_duty, HW_LIM_DUTY_MIN);
#endif
#ifdef HW_LIM_DUTY_MAX
	utils_truncate_number(&mcconf->l_max_duty, HW_LIM_DUTY_MAX);
#endif
#ifdef HW_LIM_TEMP_FET
	utils_truncate_number(&mcconf->l_temp_fet_start, HW_LIM_TEMP_FET);
	utils_truncate_number(&mcconf->l_temp_fet_end, HW_LIM_TEMP_FET);
#endif
#endif
}

static THD_FUNCTION(detect_thread, arg) {
	(void)arg;

	chRegSetThreadName("Detect");

	detect_tp = chThdGetSelfX();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		uint8_t *data = detect_thread_cmd_buffer;

		COMM_PACKET_ID packet_id;
		int32_t ind = 0;
		static mc_configuration mcconf, mcconf_old;
		static uint8_t send_buffer[50];

		packet_id = data[0];
		data++;

		switch (packet_id) {
		case COMM_DETECT_MOTOR_PARAM: {
			ind = 0;
			float detect_current = buffer_get_float32(data, 1e3, &ind);
			float detect_min_rpm = buffer_get_float32(data, 1e3, &ind);
			float detect_low_duty = buffer_get_float32(data, 1e3, &ind);
			float detect_cycle_int_limit;
			float detect_coupling_k;
			int8_t detect_hall_table[8];
			int detect_hall_res;

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

			if (send_func_detect) {
				send_func_detect(send_buffer, ind);
			}
		} break;

		case COMM_DETECT_MOTOR_R_L: {
			mcconf = *mc_interface_get_configuration();
			mcconf_old = mcconf;

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

			ind = 0;
			send_buffer[ind++] = COMM_DETECT_MOTOR_R_L;
			buffer_append_float32(send_buffer, r, 1e6, &ind);
			buffer_append_float32(send_buffer, l, 1e3, &ind);
			if (send_func_detect) {
				send_func_detect(send_buffer, ind);
			}
		} break;

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
			if (send_func_detect) {
				send_func_detect(send_buffer, ind);
			}
		} break;

		case COMM_DETECT_ENCODER: {
			if (encoder_is_configured()) {
				mcconf = *mc_interface_get_configuration();
				mcconf_old = mcconf;

				ind = 0;
				float current = buffer_get_float32(data, 1e3, &ind);

				mcconf.motor_type = MOTOR_TYPE_FOC;
				mcconf.foc_f_sw = 10000.0;
				mcconf.foc_current_kp = 0.01;
				mcconf.foc_current_ki = 10.0;
				mc_interface_set_configuration(&mcconf);

				float offset = 0.0;
				float ratio = 0.0;
				bool inverted = false;
				mcpwm_foc_encoder_detect(current, false, &offset, &ratio, &inverted);
				mc_interface_set_configuration(&mcconf_old);

				ind = 0;
				send_buffer[ind++] = COMM_DETECT_ENCODER;
				buffer_append_float32(send_buffer, offset, 1e6, &ind);
				buffer_append_float32(send_buffer, ratio, 1e6, &ind);
				send_buffer[ind++] = inverted;

				if (send_func_detect) {
					send_func_detect(send_buffer, ind);
				}
			} else {
				ind = 0;
				send_buffer[ind++] = COMM_DETECT_ENCODER;
				buffer_append_float32(send_buffer, 1001.0, 1e6, &ind);
				buffer_append_float32(send_buffer, 0.0, 1e6, &ind);
				send_buffer[ind++] = false;

				if (send_func_detect) {
					send_func_detect(send_buffer, ind);
				}
			}
		} break;

		case COMM_DETECT_HALL_FOC: {
			mcconf = *mc_interface_get_configuration();

			if (mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_HALL) {
				mcconf_old = mcconf;
				ind = 0;
				float current = buffer_get_float32(data, 1e3, &ind);

				mcconf.motor_type = MOTOR_TYPE_FOC;
				mcconf.foc_f_sw = 10000.0;
				mcconf.foc_current_kp = 0.01;
				mcconf.foc_current_ki = 10.0;
				mc_interface_set_configuration(&mcconf);

				uint8_t hall_tab[8];
				bool res = mcpwm_foc_hall_detect(current, hall_tab);
				mc_interface_set_configuration(&mcconf_old);

				ind = 0;
				send_buffer[ind++] = COMM_DETECT_HALL_FOC;
				memcpy(send_buffer + ind, hall_tab, 8);
				ind += 8;
				send_buffer[ind++] = res ? 0 : 1;

				if (send_func_detect) {
					send_func_detect(send_buffer, ind);
				}
			} else {
				ind = 0;
				send_buffer[ind++] = COMM_DETECT_HALL_FOC;
				memset(send_buffer, 255, 8);
				ind += 8;
				send_buffer[ind++] = 0;
				if (send_func_detect) {
					send_func_detect(send_buffer, ind);
				}
			}
		} break;

		case COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP: {
			ind = 0;
			float current = buffer_get_float32(data, 1e3, &ind);
			float erpm_per_sec = buffer_get_float32(data, 1e3, &ind);
			float duty = buffer_get_float32(data, 1e3, &ind);
			float resistance = buffer_get_float32(data, 1e6, &ind);

			float linkage;
			bool res = conf_general_measure_flux_linkage_openloop(current, duty, erpm_per_sec, resistance, &linkage);

			if (!res) {
				linkage = 0.0;
			}

			ind = 0;
			send_buffer[ind++] = COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP;
			buffer_append_float32(send_buffer, linkage, 1e7, &ind);
			if (send_func_detect) {
				send_func_detect(send_buffer, ind);
			}
		} break;

		case COMM_DETECT_APPLY_ALL_FOC: {
			ind = 0;
			bool detect_can = data[ind++];
			float max_power_loss = buffer_get_float32(data, 1e3, &ind);
			float min_current_in = buffer_get_float32(data, 1e3, &ind);
			float max_current_in = buffer_get_float32(data, 1e3, &ind);
			float openloop_rpm = buffer_get_float32(data, 1e3, &ind);
			float sl_erpm = buffer_get_float32(data, 1e3, &ind);

			int res = conf_general_detect_apply_all_foc_can(detect_can, max_power_loss,
					min_current_in, max_current_in, openloop_rpm, sl_erpm);

			ind = 0;
			send_buffer[ind++] = COMM_DETECT_APPLY_ALL_FOC;
			buffer_append_int16(send_buffer, res, &ind);
			if (send_func_detect) {
				send_func_detect(send_buffer, ind);
			}
		} break;

		default:
			break;
		}

		is_detecting = false;
	}
}
