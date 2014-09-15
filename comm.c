/*
	Copyright 2012-2014 Benjamin Vedder	benjamin@vedder.se

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
 * comm.c
 *
 *  Created on: 22 nov 2012
 *      Author: benjamin
 */

#include "ch.h"
#include "hal.h"
#include "comm.h"
#include "main.h"
#include "stm32f4xx_conf.h"
#include "servo.h"
#include "buffer.h"
#include "packet.h"
#include "myUSB.h"
#include "terminal.h"
#include "hw.h"
#include "mcpwm.h"

#include <math.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

// Internal data types
typedef enum {
	COMM_GET_VALUES = 0,
	COMM_SET_DUTY,
	COMM_SET_CURRENT,
	COMM_SET_CURRENT_BRAKE,
	COMM_SET_RPM,
	COMM_SET_DETECT,
	COMM_SET_SERVO_OFFSET,
	COMM_SET_MCCONF,
	COMM_GET_MCCONF,
	COMM_SAMPLE_PRINT,
	COMM_TERMINAL_CMD,
	COMM_PRINT,
	COMM_ROTOR_POSITION,
	COMM_EXPERIMENT_SAMPLE,
	COMM_DETECT_MOTOR_PARAM
} COMM_PACKET_ID;

// Settings
#define PACKET_BUFFER_LEN	30
#define PRINT_BUFFER_LEN	10
#define PRINT_MAXLEN		240

// Private variables
#define SERIAL_RX_BUFFER_SIZE		4096
static uint8_t serial_rx_buffer[SERIAL_RX_BUFFER_SIZE];
static int serial_rx_read_pos = 0;
static int serial_rx_write_pos = 0;
static WORKING_AREA(serial_read_thread_wa, 1024);
static WORKING_AREA(serial_process_thread_wa, 4096);
static Mutex send_mutex;
static Thread *process_tp;

// Private functions
static void process_packet(unsigned char *data, unsigned char len);
static void send_packet(unsigned char *buffer, unsigned char len);

static msg_t serial_read_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Serial read");

	uint8_t buffer[128];
	int i;
	int len;
	int had_data = 0;

	for(;;) {
		len = chSequentialStreamRead(&SDU1, (uint8_t*) buffer, 1);

		for (i = 0;i < len;i++) {
			serial_rx_buffer[serial_rx_write_pos++] = buffer[i];

			if (serial_rx_write_pos == SERIAL_RX_BUFFER_SIZE) {
				serial_rx_write_pos = 0;
			}

			had_data = 1;
		}

		if (had_data) {
			chEvtSignal(process_tp, (eventmask_t) 1);
			had_data = 0;
		}
	}

	return 0;
}

static msg_t serial_process_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Serial process");

	process_tp = chThdSelf();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		while (serial_rx_read_pos != serial_rx_write_pos) {
			packet_process_byte(serial_rx_buffer[serial_rx_read_pos++], 0);

			if (serial_rx_read_pos == SERIAL_RX_BUFFER_SIZE) {
				serial_rx_read_pos = 0;
			}
		}
	}

	return 0;
}

static void process_packet(unsigned char *data, unsigned char len) {
	if (!len) {
		return;
	}

	COMM_PACKET_ID packet_id;
	uint8_t send_buffer[256];
	int32_t ind = 0;
	uint16_t sample_len;
	uint8_t decimation;
	bool at_start;
	mc_configuration mcconf;
	float detect_cycle_int_limit;
	float detect_coupling_k;
	float detect_current;
	float detect_min_rpm;
	float detect_low_duty;

	(void)len;

	packet_id = data[0];
	data++;
	len--;

	switch (packet_id) {
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
		buffer_append_int32(send_buffer, (int32_t)(mcpwm_read_reset_avg_motor_current() * 100.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcpwm_read_reset_avg_input_current() * 100.0), &ind);
		buffer_append_int16(send_buffer, (int16_t)(mcpwm_get_duty_cycle_now() * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)mcpwm_get_rpm(), &ind);
		buffer_append_int16(send_buffer, (int16_t)(GET_INPUT_VOLTAGE() * 10.0), &ind);
		packet_send_packet(send_buffer, ind, 0);
		break;

	case COMM_SET_DUTY:
		ind = 0;
		mcpwm_set_duty((float)buffer_get_int32(data, &ind) / 100000.0);
		break;

	case COMM_SET_CURRENT:
		ind = 0;
		mcpwm_set_current((float)buffer_get_int32(data, &ind) / 1000.0);
		break;

	case COMM_SET_CURRENT_BRAKE:
		ind = 0;
		mcpwm_set_brake_current((float)buffer_get_int32(data, &ind) / 1000.0);
		break;

	case COMM_SET_RPM:
		ind = 0;
		mcpwm_set_pid_speed((float)buffer_get_int32(data, &ind));
		break;

	case COMM_SET_DETECT:
		mcpwm_set_detect();
		break;

	case COMM_SET_SERVO_OFFSET:
		servos[0].offset = data[0];
		break;

	case COMM_SET_MCCONF:
		ind = 0;
		mcconf.pwm_mode = data[ind++];
		mcconf.comm_mode = data[ind++];

		mcconf.l_current_max = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_current_min = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_in_current_max = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_in_current_min = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_abs_current_max = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_min_erpm = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_max_erpm = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_max_erpm_fbrake = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_min_vin = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_max_vin = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.l_slow_abs_current = data[ind++];
		mcconf.l_rpm_lim_neg_torque = data[ind++];

		mcconf.sl_is_sensorless = data[ind++];
		mcconf.sl_min_erpm = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_min_erpm_cycle_int_limit = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_cycle_int_limit = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_cycle_int_limit_high_fac = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_cycle_int_rpm_br = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.sl_bemf_coupling_k = (float)buffer_get_int32(data, &ind) / 1000.0;

		mcconf.hall_dir = data[ind++];
		mcconf.hall_fwd_add = data[ind++];
		mcconf.hall_rev_add = data[ind++];

		mcconf.s_pid_kp = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.s_pid_ki = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.s_pid_kd = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.s_pid_min_rpm = (float)buffer_get_int32(data, &ind) / 1000.0;

		mcconf.cc_startup_boost_duty = (float)buffer_get_int32(data, &ind) / 1000000.0;
		mcconf.cc_min_current = (float)buffer_get_int32(data, &ind) / 1000.0;
		mcconf.cc_gain = (float)buffer_get_int32(data, &ind) / 1000000.0;

		conf_general_store_mc_configuration(&mcconf);
		mcpwm_set_configuration(&mcconf);
		break;

	case COMM_GET_MCCONF:
		mcconf = *mcpwm_get_configuration();

		ind = 0;
		send_buffer[ind++] = COMM_GET_MCCONF;

		send_buffer[ind++] = mcconf.pwm_mode;
		send_buffer[ind++] = mcconf.comm_mode;

		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_current_max * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_current_min * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_in_current_max * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_in_current_min * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_abs_current_max * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_min_erpm * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_erpm * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_erpm_fbrake * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_min_vin * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.l_max_vin * 1000.0), &ind);
		send_buffer[ind++] = mcconf.l_slow_abs_current;
		send_buffer[ind++] = mcconf.l_rpm_lim_neg_torque;

		send_buffer[ind++] = mcconf.sl_is_sensorless;
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_min_erpm * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_min_erpm_cycle_int_limit * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_cycle_int_limit * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_cycle_int_limit_high_fac * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_cycle_int_rpm_br * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.sl_bemf_coupling_k * 1000.0), &ind);

		send_buffer[ind++] = mcconf.hall_dir;
		send_buffer[ind++] = mcconf.hall_fwd_add;
		send_buffer[ind++] = mcconf.hall_rev_add;

		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_kp * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_ki * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_kd * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.s_pid_min_rpm * 1000.0), &ind);

		buffer_append_int32(send_buffer, (int32_t)(mcconf.cc_startup_boost_duty * 1000000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.cc_min_current * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(mcconf.cc_gain * 1000000.0), &ind);

		packet_send_packet(send_buffer, ind, 0);
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
		detect_current = (float)buffer_get_int32(data, &ind) / 1000.0;
		detect_min_rpm = (float)buffer_get_int32(data, &ind) / 1000.0;
		detect_low_duty = (float)buffer_get_int32(data, &ind) / 1000.0;

		if (!conf_general_detect_motor_param(detect_current, detect_min_rpm,
				detect_low_duty, &detect_cycle_int_limit, &detect_coupling_k)) {
			detect_cycle_int_limit = 0.0;
			detect_coupling_k = 0.0;
		}

		ind = 0;
		send_buffer[ind++] = COMM_DETECT_MOTOR_PARAM;
		buffer_append_int32(send_buffer, (int32_t)(detect_cycle_int_limit * 1000.0), &ind);
		buffer_append_int32(send_buffer, (int32_t)(detect_coupling_k * 1000.0), &ind);
		packet_send_packet(send_buffer, ind, 0);
		break;

	default:
		break;
	}
}

static void send_packet(unsigned char *buffer, unsigned char len) {
	chMtxLock(&send_mutex);
	chSequentialStreamWrite(&SDU1, buffer, len);
	chMtxUnlock();
}

void comm_init(void) {
	myUSBinit();
	packet_init(send_packet, process_packet, 0);

	chMtxInit(&send_mutex);

	// Threads
	chThdCreateStatic(serial_read_thread_wa, sizeof(serial_read_thread_wa), NORMALPRIO, serial_read_thread, NULL);
	chThdCreateStatic(serial_process_thread_wa, sizeof(serial_process_thread_wa), NORMALPRIO, serial_process_thread, NULL);
}

void comm_printf(char* format, ...) {
	va_list arg;
	va_start (arg, format);
	int len;
	static char print_buffer[255];

	print_buffer[0] = COMM_PRINT;
	len = vsnprintf(print_buffer+1, 254, format, arg);
	va_end (arg);

	if(len>0) {
		packet_send_packet((unsigned char*)print_buffer, (len<254)? len+1: 255, 0);
	}
}

void comm_send_samples(uint8_t *data, int len) {
	uint8_t buffer[len + 1];
	int index = 0;

	buffer[index++] = COMM_SAMPLE_PRINT;

	for (int i = 0;i < len;i++) {
		buffer[index++] = data[i];
	}

	packet_send_packet(buffer, index, 0);
}

void comm_send_rotor_pos(float rotor_pos) {
	uint8_t buffer[5];
	int32_t index = 0;

	buffer[index++] = COMM_ROTOR_POSITION;
	buffer_append_int32(buffer, (int32_t)(rotor_pos * 100000.0), &index);

	packet_send_packet(buffer, index, 0);
}

void comm_print_fault_code(mc_fault_code fault_code) {
	switch (fault_code) {
	case FAULT_CODE_NONE: comm_printf("FAULT_CODE_NONE\n"); break;
	case FAULT_CODE_OVER_VOLTAGE: comm_printf("FAULT_CODE_OVER_VOLTAGE\n"); break;
	case FAULT_CODE_UNDER_VOLTAGE: comm_printf("FAULT_CODE_UNDER_VOLTAGE\n"); break;
	case FAULT_CODE_DRV8302: comm_printf("FAULT_CODE_DRV8302\n"); break;
	case FAULT_CODE_ABS_OVER_CURRENT: comm_printf("FAULT_CODE_ABS_OVER_CURRENT\n"); break;
	default: break;
	}
}

void comm_send_experiment_samples(float *samples, int len) {
	if ((len * 4 + 1) > 256) {
		return;
	}

	uint8_t buffer[len * 4 + 1];
	int32_t index = 0;

	buffer[index++] = COMM_EXPERIMENT_SAMPLE;

	for (int i = 0;i < len;i++) {
		buffer_append_int32(buffer, (int32_t)(samples[i] * 10000.0), &index);
	}

	packet_send_packet(buffer, index, 0);
}
