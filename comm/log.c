/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#include <string.h>
#include "log.h"
#include "comm_can.h"
#include "commands.h"
#include "buffer.h"
#include "datatypes.h"
#include "mempools.h"

void log_start(
		int can_id,
		int field_num,
		float rate_hz,
		bool append_time,
		bool append_gnss,
		bool append_gnss_time) {

	int32_t ind = 0;
	uint8_t buffer[20];

	buffer[ind++] = COMM_LOG_START;
	buffer_append_int16(buffer, field_num, &ind);
	buffer_append_float32_auto(buffer, rate_hz, &ind);
	buffer[ind++] = append_time;
	buffer[ind++] = append_gnss;
	buffer[ind++] = append_gnss_time;

	if (can_id >= 0 && can_id < 255) {
		comm_can_send_buffer(can_id, buffer, ind, 0);
	} else {
		commands_send_packet(buffer, ind);
	}
}

void log_stop(int can_id) {
	int32_t ind = 0;
	uint8_t buffer[10];
	buffer[ind++] = COMM_LOG_STOP;

	if (can_id >= 0 && can_id < 255) {
		comm_can_send_buffer(can_id, buffer, ind, 0);
	} else {
		commands_send_packet(buffer, ind);
	}
}

void log_config_field(
		int can_id,
		int field_ind,
		char *key,
		char *name,
		char *unit,
		int precision,
		bool is_relative,
		bool is_timestamp) {

	size_t key_len = strlen(key);
	size_t name_len = strlen(name);
	size_t unit_len = strlen(unit);

	if ((key_len + name_len + unit_len) > 400) {
		return;
	}

	int32_t ind = 0;
	uint8_t *buffer = mempools_get_packet_buffer();

	buffer[ind++] = COMM_LOG_CONFIG_FIELD;
	buffer_append_int16(buffer, field_ind, &ind);
	strcpy((char*)(buffer + ind), key); ind += key_len + 1;
	strcpy((char*)(buffer + ind), name); ind += name_len + 1;
	strcpy((char*)(buffer + ind), unit); ind += unit_len + 1;
	buffer[ind++] = precision;
	buffer[ind++] = is_relative;
	buffer[ind++] = is_timestamp;

	if (can_id >= 0 && can_id < 255) {
		comm_can_send_buffer(can_id, buffer, ind, 0);
	} else {
		commands_send_packet(buffer, ind);
	}

	mempools_free_packet_buffer(buffer);
}

void log_send_samples_f32(
		int can_id,
		int field_start,
		float *samples,
		int sample_num) {

	if (sample_num > 100) {
		return;
	}

	int32_t ind = 0;
	uint8_t *buffer = mempools_get_packet_buffer();

	buffer[ind++] = COMM_LOG_DATA_F32;
	buffer_append_int16(buffer, field_start, &ind);

	for (int i = 0;i < sample_num;i++) {
		buffer_append_float32_auto(buffer, samples[i], &ind);
	}

	if (can_id >= 0 && can_id < 255) {
		comm_can_send_buffer(can_id, buffer, ind, 0);
	} else {
		commands_send_packet(buffer, ind);
	}

	mempools_free_packet_buffer(buffer);
}

void log_send_samples_f64(
		int can_id,
		int field_start,
		double *samples,
		int sample_num) {

	if (sample_num > 50) {
		return;
	}

	int32_t ind = 0;
	uint8_t *buffer = mempools_get_packet_buffer();

	buffer[ind++] = COMM_LOG_DATA_F64;
	buffer_append_int16(buffer, field_start, &ind);

	for (int i = 0;i < sample_num;i++) {
		buffer_append_float64_auto(buffer, samples[i], &ind);
	}

	if (can_id >= 0 && can_id < 255) {
		comm_can_send_buffer(can_id, buffer, ind, 0);
	} else {
		commands_send_packet(buffer, ind);
	}

	mempools_free_packet_buffer(buffer);
}
