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
#include "confgenerator.h"
#include "imu.h"
#include "shutdown.h"
#if HAS_BLACKMAGIC
#include "bm_if.h"
#endif
#include "minilzo.h"
#include "mempools.h"
#include "bms.h"

#include <math.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

// Threads
static THD_FUNCTION(blocking_thread, arg);
static THD_WORKING_AREA(blocking_thread_wa, 2048);
static thread_t *blocking_tp;

// Private variables
static uint8_t send_buffer_global[PACKET_MAX_PL_LEN];
static uint8_t blocking_thread_cmd_buffer[PACKET_MAX_PL_LEN];
static volatile unsigned int blocking_thread_cmd_len = 0;
static volatile bool is_blocking = false;
static volatile int blocking_thread_motor = 1;
static void(* volatile send_func)(unsigned char *data, unsigned int len) = 0;
static void(* volatile send_func_blocking)(unsigned char *data, unsigned int len) = 0;
static void(* volatile send_func_nrf)(unsigned char *data, unsigned int len) = 0;
static void(* volatile send_func_can_fwd)(unsigned char *data, unsigned int len) = 0;
static void(* volatile appdata_func)(unsigned char *data, unsigned int len) = 0;
static disp_pos_mode display_position_mode;
static mutex_t print_mutex;
static mutex_t send_buffer_mutex;
static mutex_t terminal_mutex;
static volatile int fw_version_sent_cnt = 0;

void commands_init(void) {
	chMtxObjectInit(&print_mutex);
	chMtxObjectInit(&send_buffer_mutex);
	chMtxObjectInit(&terminal_mutex);
	chThdCreateStatic(blocking_thread_wa, sizeof(blocking_thread_wa), NORMALPRIO, blocking_thread, NULL);
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
 * Send a packet using the last can fwd function.
 *
 * @param data
 * The packet data.
 *
 * @param len
 * The data length.
 */
void commands_send_packet_can_last(unsigned char *data, unsigned int len) {
	if (send_func_can_fwd) {
		send_func_can_fwd(data, len);
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
 * Send data using the function last used by the blocking thread.
 *
 * @param data
 * The packet data.
 *
 * @param len
 * The data length.
 */
void commands_send_packet_last_blocking(unsigned char *data, unsigned int len) {
	if (send_func_blocking) {
		send_func_blocking(data, len);
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
void commands_process_packet(unsigned char *data, unsigned int len,
		void(*reply_func)(unsigned char *data, unsigned int len)) {

	if (!len) {
		return;
	}

	COMM_PACKET_ID packet_id;

	packet_id = data[0];
	data++;
	len--;

	// The NRF51 ESB implementation is treated like it has its own
	// independent communication interface.
	if (packet_id == COMM_EXT_NRF_PRESENT ||
			packet_id == COMM_EXT_NRF_ESB_RX_DATA) {
		send_func_nrf = reply_func;
	} else {
		send_func = reply_func;
	}

	// Avoid calling invalid function pointer if it is null.
	// commands_send_packet will make the check.
	if (!reply_func) {
		reply_func = commands_send_packet;
	}

	switch (packet_id) {
	case COMM_FW_VERSION: {
		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_FW_VERSION;
		send_buffer[ind++] = FW_VERSION_MAJOR;
		send_buffer[ind++] = FW_VERSION_MINOR;

		strcpy((char*)(send_buffer + ind), HW_NAME);
		ind += strlen(HW_NAME) + 1;

		memcpy(send_buffer + ind, STM32_UUID_8, 12);
		ind += 12;

		// Add 1 to the UUID for the second motor, so that configuration backup and
		// restore works.
		if (mc_interface_get_motor_thread() == 2) {
			send_buffer[ind - 1]++;
		}

		send_buffer[ind++] = app_get_configuration()->pairing_done;
		send_buffer[ind++] = FW_TEST_VERSION_NUMBER;

		send_buffer[ind++] = HW_TYPE_VESC;

		send_buffer[ind++] = 0; // No custom config

		fw_version_sent_cnt++;

		reply_func(send_buffer, ind);
	} break;

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
		int32_t ind = 0;

		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(6000);
		}
		uint16_t flash_res = flash_helper_erase_new_app(buffer_get_uint32(data, &ind));

		ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_ERASE_NEW_APP;
		send_buffer[ind++] = flash_res == FLASH_COMPLETE ? 1 : 0;
		reply_func(send_buffer, ind);
	} break;

	case COMM_WRITE_NEW_APP_DATA_ALL_CAN_LZO:
	case COMM_WRITE_NEW_APP_DATA_ALL_CAN:
		if (packet_id == COMM_WRITE_NEW_APP_DATA_ALL_CAN_LZO) {
			chMtxLock(&send_buffer_mutex);
			memcpy(send_buffer_global, data + 6, len - 6);
			int32_t ind = 4;
			lzo_uint decompressed_len = buffer_get_uint16(data, &ind);
			lzo1x_decompress_safe(send_buffer_global, len - 6, data + 4, &decompressed_len, NULL);
			chMtxUnlock(&send_buffer_mutex);
			len = decompressed_len + 4;
		}

		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(2000);
		}

		data[-1] = COMM_WRITE_NEW_APP_DATA;

		comm_can_send_buffer(255, data - 1, len + 1, 2);
		/* Falls through. */
		/* no break */
	case COMM_WRITE_NEW_APP_DATA_LZO:
	case COMM_WRITE_NEW_APP_DATA: {
		if (packet_id == COMM_WRITE_NEW_APP_DATA_LZO) {
			chMtxLock(&send_buffer_mutex);
			memcpy(send_buffer_global, data + 6, len - 6);
			int32_t ind = 4;
			lzo_uint decompressed_len = buffer_get_uint16(data, &ind);
			lzo1x_decompress_safe(send_buffer_global, len - 6, data + 4, &decompressed_len, NULL);
			chMtxUnlock(&send_buffer_mutex);
			len = decompressed_len + 4;
		}

		int32_t ind = 0;
		uint32_t new_app_offset = buffer_get_uint32(data, &ind);

		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(2000);
		}
		uint16_t flash_res = flash_helper_write_new_app_data(new_app_offset, data + ind, len - ind);

		SHUTDOWN_RESET();

		ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_WRITE_NEW_APP_DATA;
		send_buffer[ind++] = flash_res == FLASH_COMPLETE ? 1 : 0;
		buffer_append_uint32(send_buffer, new_app_offset, &ind);
		reply_func(send_buffer, ind);
	} break;

	case COMM_GET_VALUES:
	case COMM_GET_VALUES_SELECTIVE: {
		int32_t ind = 0;
		chMtxLock(&send_buffer_mutex);
		uint8_t *send_buffer = send_buffer_global;
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
		if (mask & ((uint32_t)1 << 14)) {
			buffer_append_int32(send_buffer, mc_interface_get_tachometer_abs_value(false), &ind);
		}
		if (mask & ((uint32_t)1 << 15)) {
			send_buffer[ind++] = mc_interface_get_fault();
		}
		if (mask & ((uint32_t)1 << 16)) {
			buffer_append_float32(send_buffer, mc_interface_get_pid_pos_now(), 1e6, &ind);
		}
		if (mask & ((uint32_t)1 << 17)) {
			uint8_t current_controller_id = app_get_configuration()->controller_id;
#ifdef HW_HAS_DUAL_MOTORS
			if (mc_interface_get_motor_thread() == 2) {
				current_controller_id = utils_second_motor_id();
			}
#endif
			send_buffer[ind++] = current_controller_id;
		}
		if (mask & ((uint32_t)1 << 18)) {
			buffer_append_float16(send_buffer, NTC_TEMP_MOS1(), 1e1, &ind);
			buffer_append_float16(send_buffer, NTC_TEMP_MOS2(), 1e1, &ind);
			buffer_append_float16(send_buffer, NTC_TEMP_MOS3(), 1e1, &ind);
		}
		if (mask & ((uint32_t)1 << 19)) {
			buffer_append_float32(send_buffer, mc_interface_read_reset_avg_vd(), 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 20)) {
			buffer_append_float32(send_buffer, mc_interface_read_reset_avg_vq(), 1e3, &ind);
		}

		reply_func(send_buffer, ind);
		chMtxUnlock(&send_buffer_mutex);
	} break;

	case COMM_SET_DUTY: {
		int32_t ind = 0;
		mc_interface_set_duty((float)buffer_get_int32(data, &ind) / 100000.0);
		timeout_reset();
	} break;

	case COMM_SET_CURRENT: {
		int32_t ind = 0;
		mc_interface_set_current((float)buffer_get_int32(data, &ind) / 1000.0);
		timeout_reset();
	} break;

	case COMM_SET_CURRENT_BRAKE: {
		int32_t ind = 0;
		mc_interface_set_brake_current((float)buffer_get_int32(data, &ind) / 1000.0);
		timeout_reset();
	} break;

	case COMM_SET_RPM: {
		int32_t ind = 0;
		mc_interface_set_pid_speed((float)buffer_get_int32(data, &ind));
		timeout_reset();
	} break;

	case COMM_SET_POS: {
		int32_t ind = 0;
		mc_interface_set_pid_pos((float)buffer_get_int32(data, &ind) / 1000000.0);
		timeout_reset();
	} break;

	case COMM_SET_HANDBRAKE: {
		int32_t ind = 0;
		mc_interface_set_handbrake(buffer_get_float32(data, 1e3, &ind));
		timeout_reset();
	} break;

	case COMM_SET_DETECT: {
		int32_t ind = 0;
		display_position_mode = data[ind++];

		if (mc_interface_get_configuration()->motor_type == MOTOR_TYPE_BLDC) {
			if (display_position_mode == DISP_POS_MODE_NONE) {
				mc_interface_release_motor();
			} else if (display_position_mode == DISP_POS_MODE_INDUCTANCE) {
				mcpwm_set_detect();
			}
		}

		timeout_reset();
	} break;

	case COMM_SET_SERVO_POS: {
#if SERVO_OUT_ENABLE
		int32_t ind = 0;
		servo_simple_set_output(buffer_get_float16(data, 1000.0, &ind));
#endif
	} break;

	case COMM_SET_MCCONF: {
		mc_configuration *mcconf = mempools_alloc_mcconf();
		*mcconf = *mc_interface_get_configuration();

		if (confgenerator_deserialize_mcconf(data, mcconf)) {
			utils_truncate_number(&mcconf->l_current_max_scale , 0.0, 1.0);
			utils_truncate_number(&mcconf->l_current_min_scale , 0.0, 1.0);

#ifdef HW_HAS_DUAL_MOTORS
			mcconf->motor_type = MOTOR_TYPE_FOC;
#endif

			mcconf->lo_current_max = mcconf->l_current_max * mcconf->l_current_max_scale;
			mcconf->lo_current_min = mcconf->l_current_min * mcconf->l_current_min_scale;
			mcconf->lo_in_current_max = mcconf->l_in_current_max;
			mcconf->lo_in_current_min = mcconf->l_in_current_min;
			mcconf->lo_current_motor_max_now = mcconf->lo_current_max;
			mcconf->lo_current_motor_min_now = mcconf->lo_current_min;

			commands_apply_mcconf_hw_limits(mcconf);
			conf_general_store_mc_configuration(mcconf, mc_interface_get_motor_thread() == 2);
			mc_interface_set_configuration(mcconf);
			chThdSleepMilliseconds(200);

			int32_t ind = 0;
			uint8_t send_buffer[50];
			send_buffer[ind++] = packet_id;
			reply_func(send_buffer, ind);
		} else {
			commands_printf("Warning: Could not set mcconf due to wrong signature");
		}

		mempools_free_mcconf(mcconf);
	} break;

	case COMM_GET_MCCONF:
	case COMM_GET_MCCONF_DEFAULT: {
		mc_configuration *mcconf = mempools_alloc_mcconf();

		if (packet_id == COMM_GET_MCCONF) {
			*mcconf = *mc_interface_get_configuration();
		} else {
			confgenerator_set_defaults_mcconf(mcconf);
		}

		commands_send_mcconf(packet_id, mcconf);
		mempools_free_mcconf(mcconf);
	} break;

	case COMM_SET_APPCONF: {
		app_configuration *appconf = mempools_alloc_appconf();
		*appconf = *app_get_configuration();

		if (confgenerator_deserialize_appconf(data, appconf)) {
#ifdef HW_HAS_DUAL_MOTORS
			// Ignore ID when setting second motor config
			if (mc_interface_get_motor_thread() == 2) {
				appconf->controller_id = app_get_configuration()->controller_id;
			}
#endif

			conf_general_store_app_configuration(appconf);
			app_set_configuration(appconf);
			timeout_configure(appconf->timeout_msec, appconf->timeout_brake_current);
			chThdSleepMilliseconds(200);

			int32_t ind = 0;
			uint8_t send_buffer[50];
			send_buffer[ind++] = packet_id;
			reply_func(send_buffer, ind);
		} else {
			commands_printf("Warning: Could not set appconf due to wrong signature");
		}

		mempools_free_appconf(appconf);
	} break;

	case COMM_GET_APPCONF:
	case COMM_GET_APPCONF_DEFAULT: {
		app_configuration *appconf = mempools_alloc_appconf();

		if (packet_id == COMM_GET_APPCONF) {
			*appconf = *app_get_configuration();
		} else {
			confgenerator_set_defaults_appconf(appconf);
		}

#ifdef HW_HAS_DUAL_MOTORS
		if (mc_interface_get_motor_thread() == 2) {
			appconf->controller_id = utils_second_motor_id();
		}
#endif

		commands_send_appconf(packet_id, appconf);

		mempools_free_appconf(appconf);
	} break;

	case COMM_SAMPLE_PRINT: {
		uint16_t sample_len;
		uint8_t decimation;
		debug_sampling_mode mode;

		int32_t ind = 0;
		mode = data[ind++];
		sample_len = buffer_get_uint16(data, &ind);
		decimation = data[ind++];
		mc_interface_sample_print_data(mode, sample_len, decimation);
	} break;

	case COMM_REBOOT:
		// Lock the system and enter an infinite loop. The watchdog will reboot.
		__disable_irq();
		for(;;){};
		break;

	case COMM_ALIVE:
		SHUTDOWN_RESET();
		timeout_reset();
		break;

	case COMM_GET_DECODED_PPM: {
		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_GET_DECODED_PPM;
		buffer_append_int32(send_buffer, (int32_t)(app_ppm_get_decoded_level() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(servodec_get_last_pulse_len(0) * 1000000.0), &ind);
		reply_func(send_buffer, ind);
	} break;

	case COMM_GET_DECODED_ADC: {
		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_GET_DECODED_ADC;
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_decoded_level() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_voltage() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_decoded_level2() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_adc_get_voltage2() * 1000000.0), &ind);
		reply_func(send_buffer, ind);
	} break;

	case COMM_GET_DECODED_CHUK: {
		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_GET_DECODED_CHUK;
		buffer_append_int32(send_buffer, (int32_t)(app_nunchuk_get_decoded_chuk() * 1000000.0), &ind);
		reply_func(send_buffer, ind);
	} break;

	case COMM_GET_DECODED_BALANCE: {
		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_GET_DECODED_BALANCE;
		buffer_append_int32(send_buffer, (int32_t)(app_balance_get_pid_output() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_balance_get_pitch_angle() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_balance_get_roll_angle() * 1000000.0), &ind);
		buffer_append_uint32(send_buffer, app_balance_get_diff_time(), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_balance_get_motor_current() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_balance_get_motor_position() * 1000000.0), &ind);
		buffer_append_uint16(send_buffer, app_balance_get_state(), &ind);
		buffer_append_uint16(send_buffer, app_balance_get_switch_state(), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_balance_get_adc1() * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(app_balance_get_adc2() * 1000000.0), &ind);
		reply_func(send_buffer, ind);
	} break;

	case COMM_FORWARD_CAN: {
		send_func_can_fwd = reply_func;

#ifdef HW_HAS_DUAL_MOTORS
		if (data[0] == utils_second_motor_id()) {
			mc_interface_select_motor_thread(2);
			commands_process_packet(data + 1, len - 1, reply_func);
			mc_interface_select_motor_thread(1);
		} else {
			comm_can_send_buffer(data[0], data + 1, len - 1, 0);
		}
#else
		comm_can_send_buffer(data[0], data + 1, len - 1, 0);
#endif
	} break;

	case COMM_SET_CHUCK_DATA: {
		chuck_data chuck_d_tmp;

		int32_t ind = 0;
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

	case COMM_NRF_START_PAIRING: {
		int32_t ind = 0;
		nrf_driver_start_pairing(buffer_get_int32(data, &ind));

		ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = packet_id;
		send_buffer[ind++] = NRF_PAIR_STARTED;
		reply_func(send_buffer, ind);
	} break;

	case COMM_GPD_SET_FSW: {
		timeout_reset();
		int32_t ind = 0;
		gpdrive_set_switching_frequency((float)buffer_get_int32(data, &ind));
	} break;

	case COMM_GPD_BUFFER_SIZE_LEFT: {
		int32_t ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_GPD_BUFFER_SIZE_LEFT;
		buffer_append_int32(send_buffer, gpdrive_buffer_size_left(), &ind);
		reply_func(send_buffer, ind);
	} break;

	case COMM_GPD_FILL_BUFFER: {
		timeout_reset();
		int32_t ind = 0;
		while (ind < (int)len) {
			gpdrive_add_buffer_sample(buffer_get_float32_auto(data, &ind));
		}
	} break;

	case COMM_GPD_OUTPUT_SAMPLE: {
		timeout_reset();
		int32_t ind = 0;
		gpdrive_output_sample(buffer_get_float32_auto(data, &ind));
	} break;

	case COMM_GPD_SET_MODE: {
		timeout_reset();
		int32_t ind = 0;
		gpdrive_set_mode(data[ind++]);
	} break;

	case COMM_GPD_FILL_BUFFER_INT8: {
		timeout_reset();
		int32_t ind = 0;
		while (ind < (int)len) {
			gpdrive_add_buffer_sample_int((int8_t)data[ind++]);
		}
	} break;

	case COMM_GPD_FILL_BUFFER_INT16: {
		timeout_reset();
		int32_t ind = 0;
		while (ind < (int)len) {
			gpdrive_add_buffer_sample_int(buffer_get_int16(data, &ind));
		}
	} break;

	case COMM_GPD_SET_BUFFER_INT_SCALE: {
		int32_t ind = 0;
		gpdrive_set_buffer_int_scale(buffer_get_float32_auto(data, &ind));
	} break;

	case COMM_GET_VALUES_SETUP:
	case COMM_GET_VALUES_SETUP_SELECTIVE: {
		setup_values val = mc_interface_get_setup_values();

		float wh_batt_left = 0.0;
		float battery_level = mc_interface_get_battery_level(&wh_batt_left);

		int32_t ind = 0;
		chMtxLock(&send_buffer_mutex);
		uint8_t *send_buffer = send_buffer_global;
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
			buffer_append_float32(send_buffer, val.current_tot, 1e2, &ind);
		}
		if (mask & ((uint32_t)1 << 3)) {
			buffer_append_float32(send_buffer, val.current_in_tot, 1e2, &ind);
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
			buffer_append_float32(send_buffer, val.ah_tot, 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 10)) {
			buffer_append_float32(send_buffer, val.ah_charge_tot, 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 11)) {
			buffer_append_float32(send_buffer, val.wh_tot, 1e4, &ind);
		}
		if (mask & ((uint32_t)1 << 12)) {
			buffer_append_float32(send_buffer, val.wh_charge_tot, 1e4, &ind);
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
			uint8_t current_controller_id = app_get_configuration()->controller_id;
#ifdef HW_HAS_DUAL_MOTORS
			if (mc_interface_get_motor_thread() == 2) {
				current_controller_id = utils_second_motor_id();
			}
#endif
			send_buffer[ind++] = current_controller_id;
		}
		if (mask & ((uint32_t)1 << 18)) {
			send_buffer[ind++] = val.num_vescs;
		}
		if (mask & ((uint32_t)1 << 19)) {
			buffer_append_float32(send_buffer, wh_batt_left, 1e3, &ind);
		}
		if (mask & ((uint32_t)1 << 20)) {
			buffer_append_uint32(send_buffer, mc_interface_get_odometer(), &ind);
		}

		reply_func(send_buffer, ind);
		chMtxUnlock(&send_buffer_mutex);
	    } break;

	case COMM_SET_ODOMETER: {
		int32_t ind = 0;
		mc_interface_set_odometer(buffer_get_uint32(data, &ind));
		timeout_reset();
	} break;

	case COMM_SET_MCCONF_TEMP:
	case COMM_SET_MCCONF_TEMP_SETUP: {
		mc_configuration *mcconf = mempools_alloc_mcconf();
		*mcconf = *mc_interface_get_configuration();

		int32_t ind = 0;
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

		mcconf->l_current_min_scale = buffer_get_float32_auto(data, &ind);
		mcconf->l_current_max_scale = buffer_get_float32_auto(data, &ind);

		if (packet_id == COMM_SET_MCCONF_TEMP_SETUP) {
			const float fact = ((mcconf->si_motor_poles / 2.0) * 60.0 *
					mcconf->si_gear_ratio) / (mcconf->si_wheel_diameter * M_PI);

			mcconf->l_min_erpm = buffer_get_float32_auto(data, &ind) * fact;
			mcconf->l_max_erpm = buffer_get_float32_auto(data, &ind) * fact;

			// Write computed RPM back and change forwarded packet id to
			// COMM_SET_MCCONF_TEMP. This way only the master has to be
			// aware of the setup information.
			ind -= 8;
			buffer_append_float32_auto(data, mcconf->l_min_erpm, &ind);
			buffer_append_float32_auto(data, mcconf->l_max_erpm, &ind);
		} else {
			mcconf->l_min_erpm = buffer_get_float32_auto(data, &ind);
			mcconf->l_max_erpm = buffer_get_float32_auto(data, &ind);
		}

		mcconf->l_min_duty = buffer_get_float32_auto(data, &ind);
		mcconf->l_max_duty = buffer_get_float32_auto(data, &ind);
		mcconf->l_watt_min = buffer_get_float32_auto(data, &ind) / controller_num;
		mcconf->l_watt_max = buffer_get_float32_auto(data, &ind) / controller_num;

		// Write divided data back to the buffer, as the other controllers have no way to tell
		// how many controllers are on the bus and thus need pre-divided data.
		// We set divide by controllers to false before forwarding.
		ind -= 8;
		buffer_append_float32_auto(data, mcconf->l_watt_min, &ind);
		buffer_append_float32_auto(data, mcconf->l_watt_max, &ind);

		// Battery limits can be set optionally in a backwards-compatible way.
		if ((int32_t)len >= (ind + 8)) {
			mcconf->l_in_current_min = buffer_get_float32_auto(data, &ind);
			mcconf->l_in_current_max = buffer_get_float32_auto(data, &ind);
		}

		mcconf->lo_current_min = mcconf->l_current_min * mcconf->l_current_min_scale;
		mcconf->lo_current_max = mcconf->l_current_max * mcconf->l_current_max_scale;
		mcconf->lo_current_motor_min_now = mcconf->lo_current_min;
		mcconf->lo_current_motor_max_now = mcconf->lo_current_max;
		mcconf->lo_in_current_min = mcconf->l_in_current_min;
		mcconf->lo_in_current_max = mcconf->l_in_current_max;

		commands_apply_mcconf_hw_limits(mcconf);

		if (store) {
			conf_general_store_mc_configuration(mcconf, mc_interface_get_motor_thread() == 2);
		}

		mc_interface_set_configuration(mcconf);

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
			uint8_t send_buffer[50];
			send_buffer[ind++] = packet_id;
			reply_func(send_buffer, ind);
		}

		mempools_free_mcconf(mcconf);
	} break;

	case COMM_GET_MCCONF_TEMP: {
		mc_configuration *mcconf = mempools_alloc_mcconf();
		*mcconf = *mc_interface_get_configuration();
		int32_t ind = 0;
		uint8_t send_buffer[60];

		send_buffer[ind++] = packet_id;
		buffer_append_float32_auto(send_buffer, mcconf->l_current_min_scale, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_current_max_scale, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_min_erpm, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_max_erpm, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_min_duty, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_max_duty, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_watt_min, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_watt_max, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_in_current_min, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->l_in_current_max, &ind);
		// Setup config needed for speed calculation
		send_buffer[ind++] = (uint8_t)mcconf->si_motor_poles;
		buffer_append_float32_auto(send_buffer, mcconf->si_gear_ratio, &ind);
		buffer_append_float32_auto(send_buffer, mcconf->si_wheel_diameter, &ind);

		mempools_free_mcconf(mcconf);
		reply_func(send_buffer, ind);
	} break;

	case COMM_EXT_NRF_PRESENT: {
		if (!conf_general_permanent_nrf_found) {
			nrf_driver_init_ext_nrf();
			if (!nrf_driver_is_pairing()) {
				const app_configuration *appconf = app_get_configuration();
				uint8_t send_buffer[50];
				send_buffer[0] = COMM_EXT_NRF_ESB_SET_CH_ADDR;
				send_buffer[1] = appconf->app_nrf_conf.channel;
				send_buffer[2] = appconf->app_nrf_conf.address[0];
				send_buffer[3] = appconf->app_nrf_conf.address[1];
				send_buffer[4] = appconf->app_nrf_conf.address[2];
				commands_send_packet_nrf(send_buffer, 5);
			}
		}
	} break;

	case COMM_EXT_NRF_ESB_RX_DATA: {
		nrf_driver_process_packet(data, len);
	} break;

	case COMM_APP_DISABLE_OUTPUT: {
		int32_t ind = 0;
		bool fwd_can = data[ind++];
		int time = buffer_get_int32(data, &ind);
		app_disable_output(time);

		if (fwd_can) {
			data[0] = 0; // Don't continue forwarding
			comm_can_send_buffer(255, data - 1, len + 1, 0);
		}
	} break;

	case COMM_TERMINAL_CMD_SYNC:
		data[len] = '\0';
		chMtxLock(&terminal_mutex);
		terminal_process_string((char*)data);
		chMtxUnlock(&terminal_mutex);
		break;

	case COMM_GET_IMU_DATA: {
		int32_t ind = 0;
		uint8_t send_buffer[70];
		send_buffer[ind++] = packet_id;

		int32_t ind2 = 0;
		uint32_t mask = buffer_get_uint16(data, &ind2);

		float rpy[3], acc[3], gyro[3], mag[3], q[4];
		imu_get_rpy(rpy);
		imu_get_accel(acc);
		imu_get_gyro(gyro);
		imu_get_mag(mag);
		imu_get_quaternions(q);

		buffer_append_uint16(send_buffer, mask, &ind);

		if (mask & ((uint32_t)1 << 0)) {
			buffer_append_float32_auto(send_buffer, rpy[0], &ind);
		}
		if (mask & ((uint32_t)1 << 1)) {
			buffer_append_float32_auto(send_buffer, rpy[1], &ind);
		}
		if (mask & ((uint32_t)1 << 2)) {
			buffer_append_float32_auto(send_buffer, rpy[2], &ind);
		}

		if (mask & ((uint32_t)1 << 3)) {
			buffer_append_float32_auto(send_buffer, acc[0], &ind);
		}
		if (mask & ((uint32_t)1 << 4)) {
			buffer_append_float32_auto(send_buffer, acc[1], &ind);
		}
		if (mask & ((uint32_t)1 << 5)) {
			buffer_append_float32_auto(send_buffer, acc[2], &ind);
		}

		if (mask & ((uint32_t)1 << 6)) {
			buffer_append_float32_auto(send_buffer, gyro[0], &ind);
		}
		if (mask & ((uint32_t)1 << 7)) {
			buffer_append_float32_auto(send_buffer, gyro[1], &ind);
		}
		if (mask & ((uint32_t)1 << 8)) {
			buffer_append_float32_auto(send_buffer, gyro[2], &ind);
		}

		if (mask & ((uint32_t)1 << 9)) {
			buffer_append_float32_auto(send_buffer, mag[0], &ind);
		}
		if (mask & ((uint32_t)1 << 10)) {
			buffer_append_float32_auto(send_buffer, mag[1], &ind);
		}
		if (mask & ((uint32_t)1 << 11)) {
			buffer_append_float32_auto(send_buffer, mag[2], &ind);
		}

		if (mask & ((uint32_t)1 << 12)) {
			buffer_append_float32_auto(send_buffer, q[0], &ind);
		}
		if (mask & ((uint32_t)1 << 13)) {
			buffer_append_float32_auto(send_buffer, q[1], &ind);
		}
		if (mask & ((uint32_t)1 << 14)) {
			buffer_append_float32_auto(send_buffer, q[2], &ind);
		}
		if (mask & ((uint32_t)1 << 15)) {
			buffer_append_float32_auto(send_buffer, q[3], &ind);
		}

		reply_func(send_buffer, ind);
	} break;

	case COMM_ERASE_BOOTLOADER_ALL_CAN:
		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(6000);
		}

		data[-1] = COMM_ERASE_BOOTLOADER;
		comm_can_send_buffer(255, data - 1, len + 1, 2);
		chThdSleepMilliseconds(1500);
		/* Falls through. */
		/* no break */
	case COMM_ERASE_BOOTLOADER: {
		int32_t ind = 0;

		if (nrf_driver_ext_nrf_running()) {
			nrf_driver_pause(6000);
		}
		uint16_t flash_res = flash_helper_erase_bootloader();

		ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = COMM_ERASE_BOOTLOADER;
		send_buffer[ind++] = flash_res == FLASH_COMPLETE ? 1 : 0;
		reply_func(send_buffer, ind);
	} break;

	case COMM_SET_CURRENT_REL: {
		int32_t ind = 0;
		mc_interface_set_current_rel(buffer_get_float32(data, 1e5, &ind));
		timeout_reset();
	} break;

	case COMM_CAN_FWD_FRAME: {
		int32_t ind = 0;
		uint32_t id = buffer_get_uint32(data, &ind);
		bool is_ext = data[ind++];

		if (is_ext) {
			comm_can_transmit_eid(id, data + ind, len - ind);
		} else {
			comm_can_transmit_sid(id, data + ind, len - ind);
		}
	} break;

	case COMM_SET_BATTERY_CUT: {
		int32_t ind = 0;
		float start = buffer_get_float32(data, 1e3, &ind);
		float end = buffer_get_float32(data, 1e3, &ind);
		bool store = data[ind++];
		bool fwd_can = data[ind++];

		if (fwd_can) {
			comm_can_conf_battery_cut(255, store, start, end);
		}

		mc_configuration *mcconf = mempools_alloc_mcconf();
		*mcconf = *mc_interface_get_configuration();

		if (mcconf->l_battery_cut_start != start || mcconf->l_battery_cut_end != end) {
			mcconf->l_battery_cut_start = start;
			mcconf->l_battery_cut_end = end;

			if (store) {
				conf_general_store_mc_configuration(mcconf,
						mc_interface_get_motor_thread() == 2);
			}

			mc_interface_set_configuration(mcconf);
		}

		mempools_free_mcconf(mcconf);

		// Send ack
		ind = 0;
		uint8_t send_buffer[50];
		send_buffer[ind++] = packet_id;
		reply_func(send_buffer, ind);
	} break;

	case COMM_SET_CAN_MODE: {
		int32_t ind = 0;
		bool store = data[ind++];
		bool ack = data[ind++];
		int mode = data[ind++];

		app_configuration *appconf = mempools_alloc_appconf();
		*appconf = *app_get_configuration();
		appconf->can_mode = mode;

		if (store) {
			conf_general_store_app_configuration(appconf);
		}

		app_set_configuration(appconf);

		mempools_free_appconf(appconf);

		if (ack) {
			ind = 0;
			uint8_t send_buffer[50];
			send_buffer[ind++] = packet_id;
			reply_func(send_buffer, ind);
		}
	} break;

	case COMM_BMS_GET_VALUES:
	case COMM_BMS_SET_CHARGE_ALLOWED:
	case COMM_BMS_SET_BALANCE_OVERRIDE:
	case COMM_BMS_RESET_COUNTERS:
	case COMM_BMS_FORCE_BALANCE:
	case COMM_BMS_ZERO_CURRENT_OFFSET: {
		bms_process_cmd(data - 1, len + 1, reply_func);
		break;
	}

	// Blocking commands. Only one of them runs at any given time, in their
	// own thread. If other blocking commands come before the previous one has
	// finished, they are discarded.
	case COMM_TERMINAL_CMD:
	case COMM_DETECT_MOTOR_PARAM:
	case COMM_DETECT_MOTOR_R_L:
	case COMM_DETECT_MOTOR_FLUX_LINKAGE:
	case COMM_DETECT_ENCODER:
	case COMM_DETECT_HALL_FOC:
	case COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP:
	case COMM_DETECT_APPLY_ALL_FOC:
	case COMM_PING_CAN:
	case COMM_BM_CONNECT:
	case COMM_BM_ERASE_FLASH_ALL:
	case COMM_BM_WRITE_FLASH_LZO:
	case COMM_BM_WRITE_FLASH:
	case COMM_BM_REBOOT:
	case COMM_BM_DISCONNECT:
	case COMM_BM_MAP_PINS_DEFAULT:
	case COMM_BM_MAP_PINS_NRF5X:
	case COMM_BM_MEM_READ:
	case COMM_GET_IMU_CALIBRATION:
		if (!is_blocking) {
			memcpy(blocking_thread_cmd_buffer, data - 1, len + 1);
			blocking_thread_cmd_len = len + 1;
			is_blocking = true;
			blocking_thread_motor = mc_interface_get_motor_thread();
			send_func_blocking = reply_func;
			chEvtSignal(blocking_tp, (eventmask_t)1);
		}
		break;

	default:
		break;
	}
}

void commands_printf(const char* format, ...) {
	chMtxLock(&print_mutex);

	va_list arg;
	va_start (arg, format);
	int len;
	static char print_buffer[255];

	print_buffer[0] = COMM_PRINT;
	len = vsnprintf(print_buffer + 1, 254, format, arg);
	va_end (arg);

	if(len > 0) {
		commands_send_packet_last_blocking((unsigned char*)print_buffer,
				(len < 254) ? len + 1 : 255);
	}

	chMtxUnlock(&print_mutex);
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

void commands_fwd_can_frame(int len, unsigned char *data, uint32_t id, bool is_extended) {
	if (len > 8) {
		len = 8;
	}

	uint8_t buffer[len + 6];
	int32_t index = 0;
	buffer[index++] = COMM_CAN_FWD_FRAME;
	buffer_append_uint32(buffer, id, &index);
	buffer[index++] = is_extended;
	memcpy(buffer + index, data, len);
	index += len;
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
	chMtxLock(&send_buffer_mutex);
	send_buffer_global[index++] = COMM_CUSTOM_APP_DATA;
	memcpy(send_buffer_global + index, data, len);
	index += len;
	commands_send_packet(send_buffer_global, index);
	chMtxUnlock(&send_buffer_mutex);
}

void commands_send_gpd_buffer_notify(void) {
	int32_t index = 0;
	uint8_t buffer[1];
	buffer[index++] = COMM_GPD_BUFFER_NOTIFY;
	commands_send_packet(buffer, index);
}

void commands_send_mcconf(COMM_PACKET_ID packet_id, mc_configuration *mcconf) {
	chMtxLock(&send_buffer_mutex);
	send_buffer_global[0] = packet_id;
	int32_t len = confgenerator_serialize_mcconf(send_buffer_global + 1, mcconf);
	commands_send_packet(send_buffer_global, len + 1);
	chMtxUnlock(&send_buffer_mutex);
}

void commands_send_appconf(COMM_PACKET_ID packet_id, app_configuration *appconf) {
	chMtxLock(&send_buffer_mutex);
	send_buffer_global[0] = packet_id;
	int32_t len = confgenerator_serialize_appconf(send_buffer_global + 1, appconf);
	commands_send_packet(send_buffer_global, len + 1);
	chMtxUnlock(&send_buffer_mutex);
}

inline static float hw_lim_upper(float l, float h) {(void)l; return h;}

void commands_apply_mcconf_hw_limits(mc_configuration *mcconf) {
	utils_truncate_number(&mcconf->l_current_max_scale, 0.0, 1.0);
	utils_truncate_number(&mcconf->l_current_min_scale, 0.0, 1.0);

	float ctrl_loop_freq = 0.0;

	// This limit should always be active, as starving the threads never
	// makes sense.
#ifdef HW_LIM_FOC_CTRL_LOOP_FREQ
    if (mcconf->foc_sample_v0_v7 == true) {
    	//control loop executes twice per pwm cycle when sampling in v0 and v7
		utils_truncate_number(&mcconf->foc_f_sw, HW_LIM_FOC_CTRL_LOOP_FREQ);
		ctrl_loop_freq = mcconf->foc_f_sw;
    } else {
#ifdef HW_HAS_DUAL_MOTORS
    	utils_truncate_number(&mcconf->foc_f_sw, HW_LIM_FOC_CTRL_LOOP_FREQ);
    	ctrl_loop_freq = mcconf->foc_f_sw;
#else
		utils_truncate_number(&mcconf->foc_f_sw, HW_LIM_FOC_CTRL_LOOP_FREQ * 2.0);
		ctrl_loop_freq = mcconf->foc_f_sw / 2.0;
#endif
    }
#endif

    if (ctrl_loop_freq >= (hw_lim_upper(HW_LIM_FOC_CTRL_LOOP_FREQ) * 0.9)) {
    	utils_truncate_number_int(&mcconf->m_hall_extra_samples, 0, 2);
    } else if (ctrl_loop_freq >= (hw_lim_upper(HW_LIM_FOC_CTRL_LOOP_FREQ) * 0.7)) {
    	utils_truncate_number_int(&mcconf->m_hall_extra_samples, 0, 4);
    } else {
    	utils_truncate_number_int(&mcconf->m_hall_extra_samples, 0, 10);
    }

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
#ifdef HW_FOC_CURRENT_FILTER_LIM
	utils_truncate_number(&mcconf->foc_current_filter_const, HW_FOC_CURRENT_FILTER_LIM);
#endif
#endif
}

void commands_init_plot(char *namex, char *namey) {
	int ind = 0;
	chMtxLock(&send_buffer_mutex);
	send_buffer_global[ind++] = COMM_PLOT_INIT;
	memcpy(send_buffer_global + ind, namex, strlen(namex));
	ind += strlen(namex);
	send_buffer_global[ind++] = '\0';
	memcpy(send_buffer_global + ind, namey, strlen(namey));
	ind += strlen(namey);
	send_buffer_global[ind++] = '\0';
	commands_send_packet(send_buffer_global, ind);
	chMtxUnlock(&send_buffer_mutex);
}

void commands_plot_add_graph(char *name) {
	int ind = 0;
	chMtxLock(&send_buffer_mutex);
	send_buffer_global[ind++] = COMM_PLOT_ADD_GRAPH;
	memcpy(send_buffer_global + ind, name, strlen(name));
	ind += strlen(name);
	send_buffer_global[ind++] = '\0';
	commands_send_packet(send_buffer_global, ind);
	chMtxUnlock(&send_buffer_mutex);
}

void commands_plot_set_graph(int graph) {
	int ind = 0;
	uint8_t buffer[2];
	buffer[ind++] = COMM_PLOT_SET_GRAPH;
	buffer[ind++] = graph;
	commands_send_packet(buffer, ind);
}

void commands_send_plot_points(float x, float y) {
	int32_t ind = 0;
	uint8_t buffer[10];
	buffer[ind++] = COMM_PLOT_DATA;
	buffer_append_float32_auto(buffer, x, &ind);
	buffer_append_float32_auto(buffer, y, &ind);
	commands_send_packet(buffer, ind);
}

int commands_get_fw_version_sent_cnt(void) {
	return fw_version_sent_cnt;
}

// TODO: The commands_set_ble_name and commands_set_ble_pin are not
// tested. Test them, and remove this comment when done!

void commands_set_ble_name(char* name) {
	int ind = 0;
	int name_len = strlen(name);
	if (name_len > 27) {
		name_len = 27;
	}

	uint8_t buffer[name_len + 2];
	buffer[ind++] = COMM_SET_BLE_NAME;
	memcpy(buffer + ind, name, name_len);
	ind += name_len;
	buffer[ind++] = '\0';

#ifdef HW_UART_P_DEV
	app_uartcomm_send_packet_p(buffer, ind);
#else
	app_uartcomm_send_packet(buffer, ind);
#endif
}

void commands_set_ble_pin(char* pin) {
	int ind = 0;
	int pin_len = strlen(pin);
	if (pin_len > 27) {
		pin_len = 27;
	}

	uint8_t buffer[pin_len + 2];
	buffer[ind++] = COMM_SET_BLE_NAME;
	memcpy(buffer + ind, pin, pin_len);
	ind += pin_len;
	buffer[ind++] = '\0';
#ifdef HW_UART_P_DEV
	app_uartcomm_send_packet_p(buffer, ind);
#else
	app_uartcomm_send_packet(buffer, ind);
#endif
}

static THD_FUNCTION(blocking_thread, arg) {
	(void)arg;

	chRegSetThreadName("comm_block");

	blocking_tp = chThdGetSelfX();

	for(;;) {
		is_blocking = false;

		chEvtWaitAny((eventmask_t) 1);

		mc_interface_select_motor_thread(blocking_thread_motor);

		uint8_t *data = blocking_thread_cmd_buffer;
		unsigned int len = blocking_thread_cmd_len;

		COMM_PACKET_ID packet_id;
		static uint8_t send_buffer[512];

		packet_id = data[0];
		data++;
		len--;

		switch (packet_id) {
		case COMM_DETECT_MOTOR_PARAM: {
			int32_t ind = 0;
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

			ind = 0;
			send_buffer[ind++] = COMM_DETECT_MOTOR_PARAM;
			buffer_append_int32(send_buffer, (int32_t)(detect_cycle_int_limit * 1000.0), &ind);
			buffer_append_int32(send_buffer, (int32_t)(detect_coupling_k * 1000.0), &ind);
			memcpy(send_buffer + ind, detect_hall_table, 8);
			ind += 8;
			send_buffer[ind++] = detect_hall_res;

			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_DETECT_MOTOR_R_L: {
			mc_configuration *mcconf = mempools_alloc_mcconf();
			*mcconf = *mc_interface_get_configuration();
			mc_configuration *mcconf_old = mempools_alloc_mcconf();
			*mcconf_old = *mcconf;

			mcconf->motor_type = MOTOR_TYPE_FOC;
			mc_interface_set_configuration(mcconf);

			float r = 0.0;
			float l = 0.0;
			bool res = mcpwm_foc_measure_res_ind(&r, &l);
			mc_interface_set_configuration(mcconf_old);

			if (!res) {
				r = 0.0;
				l = 0.0;
			}

			int32_t ind = 0;
			send_buffer[ind++] = COMM_DETECT_MOTOR_R_L;
			buffer_append_float32(send_buffer, r, 1e6, &ind);
			buffer_append_float32(send_buffer, l, 1e3, &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}

			mempools_free_mcconf(mcconf);
			mempools_free_mcconf(mcconf_old);
		} break;

		case COMM_DETECT_MOTOR_FLUX_LINKAGE: {
			int32_t ind = 0;
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
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_DETECT_ENCODER: {
			if (encoder_is_configured()) {
				mc_configuration *mcconf = mempools_alloc_mcconf();
				*mcconf = *mc_interface_get_configuration();
				mc_configuration *mcconf_old = mempools_alloc_mcconf();
				*mcconf_old = *mcconf;

				int32_t ind = 0;
				float current = buffer_get_float32(data, 1e3, &ind);

				mcconf->motor_type = MOTOR_TYPE_FOC;
				mcconf->foc_f_sw = 10000.0;
				mcconf->foc_current_kp = 0.01;
				mcconf->foc_current_ki = 10.0;
				mc_interface_set_configuration(mcconf);

				float offset = 0.0;
				float ratio = 0.0;
				bool inverted = false;
				mcpwm_foc_encoder_detect(current, false, &offset, &ratio, &inverted);
				mc_interface_set_configuration(mcconf_old);

				ind = 0;
				send_buffer[ind++] = COMM_DETECT_ENCODER;
				buffer_append_float32(send_buffer, offset, 1e6, &ind);
				buffer_append_float32(send_buffer, ratio, 1e6, &ind);
				send_buffer[ind++] = inverted;

				if (send_func_blocking) {
					send_func_blocking(send_buffer, ind);
				}

				mempools_free_mcconf(mcconf);
				mempools_free_mcconf(mcconf_old);
			} else {
				int32_t ind = 0;
				send_buffer[ind++] = COMM_DETECT_ENCODER;
				buffer_append_float32(send_buffer, 1001.0, 1e6, &ind);
				buffer_append_float32(send_buffer, 0.0, 1e6, &ind);
				send_buffer[ind++] = false;

				if (send_func_blocking) {
					send_func_blocking(send_buffer, ind);
				}
			}
		} break;

		case COMM_DETECT_HALL_FOC: {
			mc_configuration *mcconf = mempools_alloc_mcconf();
			*mcconf = *mc_interface_get_configuration();

			if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_HALL) {
				mc_configuration *mcconf_old = mempools_alloc_mcconf();
				*mcconf_old = *mcconf;

				int32_t ind = 0;
				float current = buffer_get_float32(data, 1e3, &ind);

				mcconf->motor_type = MOTOR_TYPE_FOC;
				mcconf->foc_f_sw = 10000.0;
				mcconf->foc_current_kp = 0.01;
				mcconf->foc_current_ki = 10.0;
				mc_interface_set_configuration(mcconf);

				uint8_t hall_tab[8];
				bool res = mcpwm_foc_hall_detect(current, hall_tab);
				mc_interface_set_configuration(mcconf_old);

				ind = 0;
				send_buffer[ind++] = COMM_DETECT_HALL_FOC;
				memcpy(send_buffer + ind, hall_tab, 8);
				ind += 8;
				send_buffer[ind++] = res ? 0 : 1;

				if (send_func_blocking) {
					send_func_blocking(send_buffer, ind);
				}

				mempools_free_mcconf(mcconf_old);
			} else {
				int32_t ind = 0;
				send_buffer[ind++] = COMM_DETECT_HALL_FOC;
				memset(send_buffer, 255, 8);
				ind += 8;
				send_buffer[ind++] = 0;
				if (send_func_blocking) {
					send_func_blocking(send_buffer, ind);
				}
			}

			mempools_free_mcconf(mcconf);
		} break;

		case COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP: {
			int32_t ind = 0;
			float current = buffer_get_float32(data, 1e3, &ind);
			float erpm_per_sec = buffer_get_float32(data, 1e3, &ind);
			float duty = buffer_get_float32(data, 1e3, &ind);
			float resistance = buffer_get_float32(data, 1e6, &ind);
			float inductance = 0.0;

			if (len >= (uint32_t)ind + 4) {
				inductance = buffer_get_float32(data, 1e8, &ind);
			}

			float linkage, linkage_undriven, undriven_samples;
			bool res = conf_general_measure_flux_linkage_openloop(current, duty,
					erpm_per_sec, resistance, inductance,
					&linkage, &linkage_undriven, &undriven_samples);

			if (undriven_samples > 60) {
				linkage = linkage_undriven;
			}

			if (!res) {
				linkage = 0.0;
			}

			ind = 0;
			send_buffer[ind++] = COMM_DETECT_MOTOR_FLUX_LINKAGE_OPENLOOP;
			buffer_append_float32(send_buffer, linkage, 1e7, &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_DETECT_APPLY_ALL_FOC: {
			int32_t ind = 0;
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
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_TERMINAL_CMD:
			data[len] = '\0';
			chMtxLock(&terminal_mutex);
			terminal_process_string((char*)data);
			chMtxUnlock(&terminal_mutex);
			break;

		case COMM_PING_CAN: {
			int32_t ind = 0;
			send_buffer[ind++] = COMM_PING_CAN;

			for (uint8_t i = 0;i < 255;i++) {
				HW_TYPE hw_type;
				if (comm_can_ping(i, &hw_type)) {
					send_buffer[ind++] = i;
				}
			}

			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

#if HAS_BLACKMAGIC
		case COMM_BM_CONNECT: {
			int32_t ind = 0;
			send_buffer[ind++] = packet_id;
			buffer_append_int16(send_buffer, bm_connect(), &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_BM_ERASE_FLASH_ALL: {
			int32_t ind = 0;
			send_buffer[ind++] = packet_id;
			buffer_append_int16(send_buffer, bm_erase_flash_all(), &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_BM_WRITE_FLASH_LZO:
		case COMM_BM_WRITE_FLASH: {
			if (packet_id == COMM_BM_WRITE_FLASH_LZO) {
				memcpy(send_buffer, data + 6, len - 6);
				int32_t ind = 4;
				lzo_uint decompressed_len = buffer_get_uint16(data, &ind);
				lzo1x_decompress_safe(send_buffer, len - 6, data + 4, &decompressed_len, NULL);
				len = decompressed_len + 4;
			}

			int32_t ind = 0;
			uint32_t addr = buffer_get_uint32(data, &ind);

			int res = bm_write_flash(addr, data + ind, len - ind);

			ind = 0;
			send_buffer[ind++] = packet_id;
			buffer_append_int16(send_buffer, res, &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_BM_REBOOT: {
			int32_t ind = 0;
			send_buffer[ind++] = packet_id;
			buffer_append_int16(send_buffer, bm_reboot(), &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_BM_DISCONNECT: {
			bm_disconnect();
			bm_leave_nrf_debug_mode();

			int32_t ind = 0;
			send_buffer[ind++] = packet_id;
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_BM_MAP_PINS_DEFAULT: {
			bm_default_swd_pins();
			int32_t ind = 0;
			send_buffer[ind++] = packet_id;
			buffer_append_int16(send_buffer, 1, &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_BM_MAP_PINS_NRF5X: {
			int32_t ind = 0;
			send_buffer[ind++] = packet_id;

#ifdef NRF5x_SWDIO_GPIO
			buffer_append_int16(send_buffer, 1, &ind);
			bm_change_swd_pins(NRF5x_SWDIO_GPIO, NRF5x_SWDIO_PIN,
					NRF5x_SWCLK_GPIO, NRF5x_SWCLK_PIN);
#else
			buffer_append_int16(send_buffer, 0, &ind);
#endif
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		case COMM_BM_MEM_READ: {
			int32_t ind = 0;
			uint32_t addr = buffer_get_uint32(data, &ind);
			uint16_t read_len = buffer_get_uint16(data, &ind);

			if (read_len > sizeof(send_buffer) - 3) {
				read_len = sizeof(send_buffer) - 3;
			}

			int res = bm_mem_read(addr, send_buffer + 3, read_len);

			ind = 0;
			send_buffer[ind++] = packet_id;
			buffer_append_int16(send_buffer, res, &ind);
			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind + read_len);
			}
		} break;
#endif
		case COMM_GET_IMU_CALIBRATION: {
			int32_t ind = 0;
			float yaw = buffer_get_float32(data, 1e3, &ind);
			float imu_cal[9];
			imu_get_calibration(yaw, imu_cal);

			ind = 0;
			send_buffer[ind++] = COMM_GET_IMU_CALIBRATION;
			buffer_append_float32(send_buffer, imu_cal[0], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[1], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[2], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[3], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[4], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[5], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[6], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[7], 1e6, &ind);
			buffer_append_float32(send_buffer, imu_cal[8], 1e6, &ind);

			if (send_func_blocking) {
				send_func_blocking(send_buffer, ind);
			}
		} break;

		default:
			break;
		}
	}
}
