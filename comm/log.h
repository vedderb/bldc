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

#ifndef COMM_LOG_H_
#define COMM_LOG_H_

#include <stdint.h>
#include <stdbool.h>

// Functions
void log_start(
		int can_id,
		int field_num,
		float rate_hz,
		bool append_time,
		bool append_gnss,
		bool append_gnss_time);
void log_stop(int can_id);
void log_config_field(
		int can_id,
		int field_ind,
		char *key,
		char *name,
		char *unit,
		int precision,
		bool is_relative,
		bool is_timestamp);
void log_send_samples_f32(
		int can_id,
		int field_start,
		float *samples,
		int sample_num);

#endif /* COMM_LOG_H_ */
