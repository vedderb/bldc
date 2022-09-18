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

#ifndef DATATYPES_H_
#define DATATYPES_H_

#include <stdint.h>
#include <stdbool.h>

typedef enum {
	BALANCE_PID_MODE_ANGLE = 0,
	BALANCE_PID_MODE_ANGLE_RATE_CASCADE
} BALANCE_PID_MODE;

typedef struct {
	BALANCE_PID_MODE pid_mode;
	float kp;
	float ki;
	float kd;
	float kp2;
	float ki2;
	float kd2;
	uint16_t hertz;
	uint16_t loop_time_filter;
	float fault_pitch;
	float fault_roll;
	float fault_duty;
	float fault_adc1;
	float fault_adc2;
	uint16_t fault_delay_pitch;
	uint16_t fault_delay_roll;
	uint16_t fault_delay_duty;
	uint16_t fault_delay_switch_half;
	uint16_t fault_delay_switch_full;
	uint16_t fault_adc_half_erpm;
	bool fault_is_dual_switch;
	float tiltback_duty_angle;
	float tiltback_duty_speed;
	float tiltback_duty;
	float tiltback_hv_angle;
	float tiltback_hv_speed;
	float tiltback_hv;
	float tiltback_lv_angle;
	float tiltback_lv_speed;
	float tiltback_lv;
	float tiltback_return_speed;
	float tiltback_constant;
	uint16_t tiltback_constant_erpm;
	float tiltback_variable;
	float tiltback_variable_max;
	float noseangling_speed;
	float startup_pitch_tolerance;
	float startup_roll_tolerance;
	float startup_speed;
	float deadzone;
	bool multi_esc;
	float yaw_kp;
	float yaw_ki;
	float yaw_kd;
	float roll_steer_kp;
	float roll_steer_erpm_kp;
	float brake_current;
	uint16_t brake_timeout;
	float yaw_current_clamp;
	uint16_t kd_pt1_lowpass_frequency;
	uint16_t kd_pt1_highpass_frequency;
	float kd_biquad_lowpass;
	float kd_biquad_highpass;
	float booster_angle;
	float booster_ramp;
	float booster_current;
	float torquetilt_start_current;
	float torquetilt_angle_limit;
	float torquetilt_on_speed;
	float torquetilt_off_speed;
	float torquetilt_strength;
	float torquetilt_filter;
	float turntilt_strength;
	float turntilt_angle_limit;
	float turntilt_start_angle;
	uint16_t turntilt_start_erpm;
	float turntilt_speed;
	uint16_t turntilt_erpm_boost;
	uint16_t turntilt_erpm_boost_end;
} balance_config;

// DATATYPES_H_
#endif
