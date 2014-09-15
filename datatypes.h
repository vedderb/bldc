/*
 * datatypes.h
 *
 *  Created on: 14 sep 2014
 *      Author: benjamin
 */

#ifndef DATATYPES_H_
#define DATATYPES_H_

#include <stdint.h>
#include <stdbool.h>

// Data types
typedef enum {
   MC_STATE_OFF = 0,
   MC_STATE_DETECTING,
   MC_STATE_RUNNING,
   MC_STATE_FULL_BRAKE,
} mc_state;

typedef enum {
	PWM_MODE_NONSYNCHRONOUS_HISW = 0, // This mode is not recommended
	PWM_MODE_SYNCHRONOUS, // The recommended and most tested mode
	PWM_MODE_BIPOLAR // Some glitches occasionally, can kill MOSFETs
} mc_pwm_mode;

typedef enum {
	COMM_MODE_INTEGRATE = 0,
	COMM_MODE_DELAY
} mc_comm_mode;

typedef enum {
	FAULT_CODE_NONE = 0,
	FAULT_CODE_OVER_VOLTAGE,
	FAULT_CODE_UNDER_VOLTAGE,
	FAULT_CODE_DRV8302,
	FAULT_CODE_ABS_OVER_CURRENT
} mc_fault_code;

typedef enum {
	CONTROL_MODE_DUTY = 0,
	CONTROL_MODE_SPEED,
	CONTROL_MODE_CURRENT,
	CONTROL_MODE_CURRENT_BRAKE,
	CONTROL_MODE_NONE
} mc_control_mode;

typedef struct {
	volatile float cycle_int_limit;
	volatile float cycle_int_limit_running;
	volatile float cycle_int_limit_max;
	volatile float comm_time_sum;
	volatile float comm_time_sum_min_rpm;
	volatile uint32_t comms;
	volatile uint32_t time_at_comm;
} mc_rpm_dep_struct;

typedef struct {
	// Switching and drive
	mc_pwm_mode pwm_mode;
	mc_comm_mode comm_mode;
	// Limits
	float l_current_max;
	float l_current_min;
	float l_in_current_max;
	float l_in_current_min;
	float l_abs_current_max;
	float l_min_erpm;
	float l_max_erpm;
	float l_max_erpm_fbrake;
	float l_min_vin;
	float l_max_vin;
	bool l_slow_abs_current;
	bool l_rpm_lim_neg_torque;
	// Sensorless
	bool sl_is_sensorless;
	float sl_min_erpm;
	float sl_min_erpm_cycle_int_limit;
	float sl_cycle_int_limit;
	float sl_cycle_int_limit_high_fac;
	float sl_cycle_int_rpm_br;
	float sl_bemf_coupling_k;
	// Hall sensor
	int hall_dir;
	int hall_fwd_add;
	int hall_rev_add;
	// Speed PID
	float s_pid_kp;
	float s_pid_ki;
	float s_pid_kd;
	float s_pid_min_rpm;
	// Current controller
	float cc_startup_boost_duty;
	float cc_min_current;
	float cc_gain;
} mc_configuration;

#endif /* DATATYPES_H_ */
