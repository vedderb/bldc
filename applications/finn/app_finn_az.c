/*
	Copyright 2020 - 2021 Benjamin Vedder	benjamin@vedder.se

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

#include "app.h"
#include "ch.h"
#include "hal.h"

#include "mc_interface.h"
#include "utils.h"
#include "encoder.h"
#include "terminal.h"
#include "comm_can.h"
#include "hw.h"
#include "commands.h"
#include "timeout.h"
#include "buffer.h"

#include "app_finn_types.h"

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

/*
 * HW Connections:
 *
 * btn_left: UART_RX and VCC
 * btn_right: ADC_EXT and VCC
 * limit_sw: ADC_EXT2 and VCC
 *
 * Motor:
 * blue: A
 * yellow: B
 * red: C
 *
 * Motor sensor connector should be plugged in.
 *
 */

// Settings
#define FILTER_CONST			0.02 // Range: 0 - 1. Higher values make the control more aggressive.
#define BUTTON_RATE				5.0 // How fast the home adjustment buttons move the pod. Unit: Deg/s
#define START_DELAY				10.0 // Start delay in seconds

/**
 * Homing procedure
 *
 * The pod will rotate left, stay there for left_time, then rotate right
 * until the limit switch is found or until HOMING_ANGLE_MAX is reached. If
 * HOMING_ANGLE_MAX an error is thrown and the pod will rotate back to the initial
 * position. If a homing error occurs, no new commands are accepted.
 */
#define HOMING_RATE				20.0 // Deg/s
#define HOMING_ANGLE_BACK		-30.0
#define HOMING_ANGLE_MAX		270.0
#define HOMING_BACK_TIME		3.0 // Seconds

#define P_ADDR_OFFSET			0

typedef enum {
	FINN_MSG_GET_STATE = 0,
	FINN_MSG_HOME,
	FINN_MSG_SET_MOTORS_ENABLED,
} FINN_MSG;

// Threads
static THD_FUNCTION(control_thread, arg);
static THD_WORKING_AREA(control_thread_wa, 1024);

static THD_FUNCTION(status_thread, arg);
static THD_WORKING_AREA(status_thread_wa, 1024);
static thread_t *status_tp;

// Private variables
static volatile bool stop_now = true;
static volatile bool control_is_running = false;
static volatile bool status_is_running = false;
static volatile POD_STATE m_pod_state;
static volatile bool m_motors_enabled = true;
static volatile bool m_start_wait_done = false;

// Private functions
static void process_custom_app_data(unsigned char *data, unsigned int len);
static bool can_eid_callback(uint32_t id, uint8_t *data, uint8_t len);
static void terminal_mon(int argc, const char **argv);
static void terminal_home(int argc, const char **argv);

void app_custom_start(void) {
	memset((void*)&m_pod_state, 0, sizeof(m_pod_state));
	m_pod_state.homing_angle_now = HOMING_ANGLE_BACK;

	commands_set_app_data_handler(process_custom_app_data);
	comm_can_set_eid_rx_callback(can_eid_callback);

	palSetPadMode(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN, PAL_MODE_INPUT_PULLDOWN);
	palSetPadMode(HW_ADC_EXT2_GPIO, HW_ADC_EXT2_PIN, PAL_MODE_INPUT_PULLDOWN);
	palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_INPUT_PULLDOWN);

	terminal_register_command_callback(
			"sd_mon",
			"Monitor IO for 30 seconds.",
			0,
			terminal_mon);

	terminal_register_command_callback(
			"sd_home",
			"Start homing procedure.",
			0,
			terminal_home);

	eeprom_var v;
	if (conf_general_read_eeprom_var_custom(&v, P_ADDR_OFFSET)) {
		m_pod_state.angle_offset = v.as_float;
	}

	stop_now = false;
	chThdCreateStatic(control_thread_wa, sizeof(control_thread_wa),
			NORMALPRIO, control_thread, NULL);
	chThdCreateStatic(status_thread_wa, sizeof(status_thread_wa),
			NORMALPRIO, status_thread, NULL);
}

void app_custom_stop(void) {
	commands_set_app_data_handler(0);
	comm_can_set_eid_rx_callback(0);
	terminal_unregister_callback(terminal_mon);

	palSetPadMode(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN, PAL_MODE_INPUT_ANALOG);
	palSetPadMode(HW_ADC_EXT2_GPIO, HW_ADC_EXT2_PIN, PAL_MODE_INPUT_ANALOG);

	stop_now = true;
	while (control_is_running || status_is_running) {
		chThdSleepMilliseconds(1);
	}
}

void app_custom_configure(app_configuration *conf) {
	int pod_id = conf->controller_id;
	while (pod_id >= 100) {
		pod_id -= 100;
	}
	m_pod_state.pod_id = pod_id;
}

static void process_custom_app_data(unsigned char *data, unsigned int len) {
	(void)len;

	int32_t ind = 0;
	FINN_MSG msg = data[ind++];

	switch (msg) {
	case FINN_MSG_GET_STATE: {
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		buffer_append_float32_auto(dataTx, m_pod_state.req_angle, &ind);
		buffer_append_float32_auto(dataTx, m_pod_state.actual_angle, &ind);
		buffer_append_float32_auto(dataTx, m_pod_state.angle_offset, &ind);
		buffer_append_float32_auto(dataTx, m_pod_state.angle_home, &ind);
		buffer_append_float32_auto(dataTx, m_pod_state.homing_angle_now, &ind);
		buffer_append_float32_auto(dataTx, m_pod_state.homing_back_time, &ind);

		buffer_append_float32_auto(dataTx, UTILS_AGE_S(m_pod_state.last_update), &ind);

		dataTx[ind++] = m_pod_state.btn_left_pressed;
		dataTx[ind++] = m_pod_state.btn_right_pressed;
		dataTx[ind++] = m_pod_state.btn_limit_pressed;

		dataTx[ind++] = m_pod_state.homing_done;
		dataTx[ind++] = m_pod_state.homing_error;

		dataTx[ind++] = m_pod_state.wait_start;
		dataTx[ind++] = m_pod_state.wait_data;

		commands_send_app_data(dataTx, ind);
	} break;

	case FINN_MSG_HOME: {
		m_pod_state.homing_angle_now = HOMING_ANGLE_BACK + m_pod_state.req_angle + m_pod_state.angle_home + m_pod_state.angle_offset;
		m_pod_state.homing_back_time = 0.0;
		m_pod_state.homing_done = false;
		m_pod_state.homing_error = false;

		// Send ack
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		commands_send_app_data(dataTx, ind);
	} break;

	case FINN_MSG_SET_MOTORS_ENABLED: {
		m_motors_enabled = data[ind++];

		// Send ack
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		dataTx[ind++] = m_motors_enabled;
		commands_send_app_data(dataTx, ind);
	} break;

	default:
		break;
	}
}

/*
 * Here we handle CAN-frames from the joystick.
 */
static bool can_eid_callback(uint32_t id, uint8_t *data, uint8_t len) {
	bool res = false;

	uint32_t sourceType = (id >> 0) & 0xFF;
	uint32_t sourceIndex = (id >> 8) & 0xFF;
	uint32_t packetType = (id >> 16) & 0xFF;
	uint32_t prio = (id >> 24) & 0x1F;

//	commands_printf("sourceType: %02X sourceIndex: %02X, packetType: %02X, prio: %02X",
//			sourceType, sourceIndex, packetType, prio);

	(void)sourceType;
	(void)sourceIndex;
	(void)prio;

	CAN_PAYLOAD_UNION packet_union;
	memcpy(packet_union.bytes, data, len);

	switch(packetType)
	{
	case CAN_MESSAGE_POD_REQUEST:
	{
		CAN_PACKET_POD_REQUEST *pkt = &packet_union.podRequest;
		uint8_t pktpodid = pkt->pod_id;

		if (m_pod_state.pod_id == pktpodid) {// 0-7
			int16_t ang_int = pkt->req_angle;

			// Ignore angles outside of range
			if (ang_int > 5100.0 || ang_int < -5100.0) {
				res = false;
				break;
			}

			m_pod_state.req_angle = (float)ang_int / 5000.0 * 90.0;
			m_pod_state.last_update = chVTGetSystemTimeX();
		}

		res = true;
		break;
	}

	case CAN_MESSAGE_POD_REQ_FOUR:
	{
		CAN_PACKET_POD_REQ_FOUR *pkt = &packet_union.podReqFour;

		if (m_pod_state.pod_id < 4) {
			int16_t ang_int = pkt->req_angle[m_pod_state.pod_id];

			// Ignore angles outside of range
			if (ang_int > 5100.0 || ang_int < -5100.0) {
				res = false;
				break;
			}

			m_pod_state.req_angle = (float)ang_int / 5000.0 * 90.0;
			m_pod_state.last_update = chVTGetSystemTimeX();
		}

		res = true;
		break;
	}

	case CAN_MESSAGE_POD_REQ_FOUR_HI:
	{
		CAN_PACKET_POD_REQ_FOUR *pkt = &packet_union.podReqFour;
		if (m_pod_state.pod_id >= 4 && m_pod_state.pod_id < 8) {
			int16_t ang_int = pkt->req_angle[m_pod_state.pod_id-4];

			// Ignore angles outside of range
			if (ang_int > 5100.0 || ang_int < -5100.0) {
				res = false;
				break;
			}

			m_pod_state.req_angle = (float)ang_int / 5000.0 * 90.0;
			m_pod_state.last_update = chVTGetSystemTimeX();
		}

		res = true;
		break;
	}

	default:
		break;
	}

	if (res) {
		// Trigger status thread to send update
		chEvtSignal(status_tp, (eventmask_t) 1);
	}

	return res;
}

static THD_FUNCTION(control_thread, arg) {
	(void)arg;

	chRegSetThreadName("Finn AZ");

	control_is_running = true;
	float angle_target = 0.0;
	systime_t time_last = chVTGetSystemTimeX();

	int btn_left_samples = 0;
	int btn_right_samples = 0;
	int btn_lim_samples = 0;

	for(;;) {
		if (stop_now) {
			control_is_running = false;
			return;
		}

		if (UTILS_AGE_S(0) < START_DELAY && !m_start_wait_done) {
			chThdSleepMilliseconds(10);
			time_last = chVTGetSystemTimeX();
			m_pod_state.wait_start = true;
			continue;
		}

		m_pod_state.wait_start = false;
		m_start_wait_done = true;

		{
			static int samp_cnt = 0;
			samp_cnt++;
			if (samp_cnt == 5) {
				samp_cnt = 0;

				// Sample push button slower for some debouncing

				btn_left_samples += palReadPad(HW_UART_RX_PORT, HW_UART_RX_PIN) ? 1 : -1;
				utils_truncate_number_int(&btn_left_samples, -4, 5);
				m_pod_state.btn_left_pressed = btn_left_samples > 0;

				btn_right_samples += palReadPad(HW_ADC_EXT_GPIO, HW_ADC_EXT_PIN) ? 1 : -1;
				utils_truncate_number_int(&btn_right_samples, -4, 5);
				m_pod_state.btn_right_pressed = btn_right_samples > 0;
			}
		}

		// Sample limit switch faster to not miss the pulse. Also do some filtering.
		btn_lim_samples += (!palReadPad(HW_ADC_EXT2_GPIO, HW_ADC_EXT2_PIN)) ? 1 : -1;
		utils_truncate_number_int(&btn_lim_samples, -4, 5);
		m_pod_state.btn_limit_pressed = btn_lim_samples > 0;

		float dt = UTILS_AGE_S(time_last);
		time_last = chVTGetSystemTimeX();

		float angle_target_no_filter = m_pod_state.req_angle + m_pod_state.angle_home + m_pod_state.angle_offset;

		float angle_now = mc_interface_get_pid_pos_now();
		if (angle_now > 180.0) {
			angle_now -= 360.0;
		}
		angle_now *= APP_FINN_WRAP_FACTOR;

		if (!m_pod_state.homing_done) {
			m_pod_state.homing_back_time += dt;

			if (m_pod_state.homing_back_time >= HOMING_BACK_TIME) {
				m_pod_state.homing_angle_now += HOMING_RATE * dt;
			}

			angle_target_no_filter = m_pod_state.homing_angle_now;

			if (m_pod_state.btn_limit_pressed) {
				m_pod_state.homing_done = true;
				m_pod_state.angle_home = angle_now;
			}

			if (m_pod_state.homing_angle_now > HOMING_ANGLE_MAX) {
				m_pod_state.homing_error = true;
			}
		}

		if (m_pod_state.homing_error) {
			angle_target_no_filter = 0.0;
		}

		{
			// Offset update
			static bool offset_updated = false;
			systime_t offset_update_time = 0;

			if (m_pod_state.btn_left_pressed) {
				m_pod_state.angle_offset -= BUTTON_RATE * dt;
				offset_updated = true;
				offset_update_time = chVTGetSystemTimeX();
			}

			if (m_pod_state.btn_right_pressed) {
				m_pod_state.angle_offset += BUTTON_RATE * dt;
				offset_updated = true;
				offset_update_time = chVTGetSystemTimeX();
			}

			utils_truncate_number_abs((float*)&m_pod_state.angle_offset, 180.0);

			// Store offset update 2s after the last adjustment
			if (offset_updated && UTILS_AGE_S(offset_update_time) > 2.0) {
				offset_updated = false;
				eeprom_var v;
				v.as_float = m_pod_state.angle_offset;
				conf_general_store_eeprom_var_custom(&v, P_ADDR_OFFSET);
			}
		}

		UTILS_LP_FAST(angle_target, angle_target_no_filter, FILTER_CONST);

		m_pod_state.actual_angle = angle_now - m_pod_state.angle_home - m_pod_state.angle_offset;

		if (UTILS_AGE_S(m_pod_state.last_update) < 2.0 && m_motors_enabled) {
			timeout_reset();
			mc_interface_set_pid_pos(angle_target / APP_FINN_WRAP_FACTOR);
			m_pod_state.wait_data = false;
		} else {
//			m_pod_state.req_angle = angle_now - m_pod_state.angle_home - m_pod_state.angle_offset;
			m_pod_state.wait_data = true;
		}

		chThdSleepMilliseconds(5);
	}
}

static THD_FUNCTION(status_thread, arg) {
	(void)arg;

	chRegSetThreadName("Finn AZ stat");
	status_tp = chThdGetSelfX();

	status_is_running = true;

	for(;;) {
		chEvtWaitAnyTimeout((eventmask_t)1, MS2ST(100));

		if (stop_now) {
			status_is_running = false;
			return;
		}

		CAN_PACKET_POD_STATUS podstatus_actual = {0};

		podstatus_actual.pod_id_lo = m_pod_state.pod_id & 3;
		podstatus_actual.pod_id_hi = m_pod_state.pod_id & 4 ? 1 : 0;
		podstatus_actual.error = false;
		podstatus_actual.ready = m_pod_state.homing_done;
		podstatus_actual.stepdriver_alarm = false;
		podstatus_actual.calib_running = !m_pod_state.homing_done;
		podstatus_actual.calib_error = m_pod_state.homing_error;
		podstatus_actual.config_mode = 0;
		podstatus_actual.limitswitch = m_pod_state.btn_limit_pressed;
		podstatus_actual.fake_ready = false;

		podstatus_actual.accepted_angle = (int16_t)(m_pod_state.req_angle / 90.0 * 5000.0);
		podstatus_actual.actual_angle = (int16_t)(m_pod_state.actual_angle / 90.0 * 5000.0);

		uint32_t sourceType = 0x0B;
		uint32_t sourceIndex = m_pod_state.pod_id;
		uint32_t packetType = CAN_MESSAGE_POD_STATUS;
		uint32_t prio = CAN_HEADER_PRIORITY_LOW | 0x03;

		uint32_t id_ext = 0;
		id_ext |= (sourceType & 0xFF) << 0;
		id_ext |= (sourceIndex & 0xFF) << 8;
		id_ext |= (packetType & 0xFF) << 16;
		id_ext |= (prio & 0x1F) << 24;

		comm_can_transmit_eid(id_ext, (uint8_t*)&podstatus_actual, 8);
	}
}

static void terminal_home(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	m_pod_state.homing_angle_now = HOMING_ANGLE_BACK + m_pod_state.req_angle + m_pod_state.angle_home + m_pod_state.angle_offset;
	m_pod_state.homing_back_time = 0.0;
	m_pod_state.homing_done = false;
	m_pod_state.homing_error = false;

	commands_printf("OK");
}

static void terminal_mon(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	commands_printf("Monitoring IO for 30 seconds");

	float req_ang_last = m_pod_state.req_angle;
	bool btn_left_last = m_pod_state.btn_left_pressed;
	bool btn_right_last = m_pod_state.btn_right_pressed;
	bool btn_limit_last = m_pod_state.btn_limit_pressed;
	float offset_last = m_pod_state.angle_offset;
	bool homing_done_last = m_pod_state.homing_done;
	bool homing_error_last = m_pod_state.homing_error;

	commands_printf("req_ang      : %.0f", (double)req_ang_last);
	commands_printf("SW_LEFT      : %d", m_pod_state.btn_left_pressed);
	commands_printf("SW_RIGHT     : %d", m_pod_state.btn_right_pressed);
	commands_printf("SW_LIMIT     : %d", m_pod_state.btn_limit_pressed);
	commands_printf("Offset       : %.1f", (double)m_pod_state.angle_offset);
	commands_printf("Homing Done  : %d", m_pod_state.homing_done);
	commands_printf("Homing Error : %d", m_pod_state.homing_error);

	for (int i = 0;i < 3000;i++) {
		if (m_pod_state.req_angle != req_ang_last) {
			req_ang_last = m_pod_state.req_angle;
			commands_printf("req_ang      : %.0f", (double)req_ang_last);
		}

		if (m_pod_state.btn_left_pressed != btn_left_last) {
			btn_left_last = m_pod_state.btn_left_pressed;
			commands_printf("SW_LEFT      : %d", m_pod_state.btn_left_pressed);
		}

		if (m_pod_state.btn_right_pressed != btn_right_last) {
			btn_right_last = m_pod_state.btn_right_pressed;
			commands_printf("SW_RIGHT     : %d", m_pod_state.btn_right_pressed);
		}

		if (m_pod_state.btn_limit_pressed != btn_limit_last) {
			btn_limit_last = m_pod_state.btn_limit_pressed;
			commands_printf("SW_LIMIT     : %d", m_pod_state.btn_limit_pressed);
		}

		if (m_pod_state.angle_offset != offset_last) {
			offset_last = m_pod_state.angle_offset;
			commands_printf("Offset       : %.1f", (double)m_pod_state.angle_offset);
		}

		if (m_pod_state.homing_done != homing_done_last) {
			homing_done_last = m_pod_state.homing_done;
			commands_printf("Homing Done  : %d", m_pod_state.homing_done);
		}

		if (m_pod_state.homing_error != homing_error_last) {
			homing_error_last = m_pod_state.homing_error;
			commands_printf("Homing Error : %d", m_pod_state.homing_error);
		}

		chThdSleepMilliseconds(20);
	}

	commands_printf("Monitoring IO ended\n");
}
