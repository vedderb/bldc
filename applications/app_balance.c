/*
	Copyright 2019 Mitch Lustig

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

#include "ch.h" // ChibiOS
#include "hal.h" // ChibiOS HAL
#include "mc_interface.h" // Motor control functions
#include "hw.h" // Pin mapping on this hardware
#include "timeout.h" // To reset the timeout
#include "commands.h"
#include "imu/imu.h"
#include "imu/ahrs.h"
#include "utils.h"


#include <math.h>

// Data type
typedef enum {
	CALIBRATING = 0,
	RUNNING,
	FAULT,
	DEAD
} BalanceState;

typedef enum {
	STARTUP = 0,
	TILTBACK
} SetpointAdjustmentType;

// Example thread
static THD_FUNCTION(example_thread, arg);
static THD_WORKING_AREA(example_thread_wa, 2048); // 2kb stack for this thread

static volatile balance_config config;
static thread_t *app_thread;

// Values used in loop
static BalanceState state;
static float pitch, roll;
static float proportional, integral, derivative;
static float last_proportional;
static float pid_value;
static float setpoint, setpoint_target;
static SetpointAdjustmentType setpointAdjustmentType;
static float startup_step_size, tiltback_step_size;
static systime_t current_time, last_time, diff_time;
static systime_t cal_start_time, cal_diff_time;

// Values read to pass in app data to GUI
static float motor_current;
static float motor_position;

void app_balance_configure(balance_config *conf) {
	config = *conf;
}

void app_balance_start(void) {

	// Reset IMU
	if(config.use_peripheral){
		hw_stop_i2c();
		hw_start_i2c();
		imu_init(true);
	}

	// Reset all Values
	state = CALIBRATING;
	pitch = 0;
	roll = 0;
	proportional = 0;
	integral = 0;
	derivative = 0;
	last_proportional = 0;
	pid_value = 0;
	setpoint = 0;
	setpoint_target = 0;
	setpointAdjustmentType = STARTUP;
	startup_step_size = 0;
	tiltback_step_size = 0;
	current_time = 0;
	last_time = 0;
	diff_time = 0;
	cal_start_time = 0;
	cal_diff_time = 0;

	// Start the example thread
	app_thread = chThdCreateStatic(example_thread_wa, sizeof(example_thread_wa), NORMALPRIO, example_thread, NULL);
}

void app_balance_stop(void) {
	chThdTerminate(app_thread);
	mc_interface_set_current(0);

	// Reset IMU
	if(config.use_peripheral){
		imu_init(false);
		hw_stop_i2c();
		hw_start_i2c();
	}
}

float app_balance_get_pid_output(void) {
	return pid_value;
}
float app_balance_get_pitch(void) {
	return pitch;
}
float app_balance_get_roll(void) {
	return roll;
}
uint32_t app_balance_get_diff_time(void) {
	return ST2US(diff_time);
}
float app_balance_get_motor_current(void) {
	return motor_current;
}
float app_balance_get_motor_position(void) {
	return motor_position;
}
uint16_t app_balance_get_state(void) {
	return state;
}

float get_setpoint_adjustment_step_size(void){
	switch(setpointAdjustmentType){
		case (STARTUP):
			return startup_step_size;
		case (TILTBACK):
			return tiltback_step_size;
	}
	return 0;
}

float apply_deadzone(float error){
	if(config.deadzone == 0){
		return error;
	}

	if(error < config.deadzone && error > -config.deadzone){
		return 0;
	} else if(error > config.deadzone){
		return error - config.deadzone;
	} else {
		return error + config.deadzone;
	}
}

static THD_FUNCTION(example_thread, arg) {
	(void)arg;
	chRegSetThreadName("APP_BALANCE");

	// Do one off config
	startup_step_size = (config.startup_speed / 1000) * config.loop_delay;
	tiltback_step_size = (config.tiltback_speed / 1000) * config.loop_delay;

	state = CALIBRATING;
	setpointAdjustmentType = STARTUP;

	while (!chThdShouldTerminateX()) {
		// Update times
		current_time = chVTGetSystemTimeX();
		if(last_time == 0){
		  last_time = current_time;
		}
		diff_time = current_time - last_time;
		last_time = current_time;

		// Read values for GUI
		motor_current = mc_interface_get_tot_current_directional_filtered();
		motor_position = mc_interface_get_pid_pos_now();

		// Read gyro values
		pitch = imu_get_pitch() * 180.0f / M_PI;
		roll = imu_get_roll() * 180.0f / M_PI;

		// Apply offsets
		pitch = fmodf(((pitch + 180.0f) + config.pitch_offset), 360.0f) - 180.0f;
		roll = fmodf(((roll + 180.0f) + config.roll_offset), 360.0f) - 180.0f;

		// State based logic
		switch(state){
			case (CALIBRATING):
				if(cal_start_time == 0){
					cal_start_time = current_time;
					ahrs_set_madgwick_acc_confidence_decay(config.cal_m_acd);
					ahrs_set_madgwick_beta(config.cal_m_b);
				}
				cal_diff_time = current_time - cal_start_time;

				// Calibration is done
				if(ST2MS(cal_diff_time) > config.cal_delay){

					// Set gyro config to be running config
					ahrs_set_madgwick_acc_confidence_decay(config.m_acd);
					ahrs_set_madgwick_beta(config.m_b);

					// Set fault and wait for valid startup condition
					state = FAULT;
					cal_start_time = 0;
					cal_diff_time = 0;
				}
				break;
			case (RUNNING):
				// Check for overspeed
				if(mc_interface_get_duty_cycle_now() > config.overspeed_duty || mc_interface_get_duty_cycle_now() < -config.overspeed_duty){
					state = DEAD;
				}

				// Check for fault
				if(pitch > config.pitch_fault || pitch < -config.pitch_fault || roll > config.roll_fault || roll < -config.roll_fault){
					state = FAULT;
				}

				// Over speed tilt back safety
				if(mc_interface_get_duty_cycle_now() > config.tiltback_duty){
					setpoint_target = config.tiltback_angle;
					setpointAdjustmentType = TILTBACK;
				} else if(mc_interface_get_duty_cycle_now() < -config.tiltback_duty){
					setpoint_target = -config.tiltback_angle;
					setpointAdjustmentType = TILTBACK;
				}else{
					setpoint_target = 0;
				}

				// Adjust setpoint
				if(setpoint != setpoint_target){
					// If we are less than one step size away, go all the way
					if(fabsf(setpoint_target - setpoint) < get_setpoint_adjustment_step_size()){
						setpoint = setpoint_target;
					}else if (setpoint_target - setpoint > 0){
						setpoint += get_setpoint_adjustment_step_size();
					}else{
						setpoint -= get_setpoint_adjustment_step_size();
					}
				}

				// Do PID maths
				proportional = setpoint - pitch;
				// Apply deadzone
				proportional = apply_deadzone(proportional);
				// Resume real PID maths
				integral = integral + proportional;
				derivative = proportional - last_proportional;

				pid_value = (config.kp * proportional) + (config.ki * integral) + (config.kd * derivative);

				last_proportional = proportional;

				// Apply current boost
				if(pid_value > 0){
					pid_value += config.current_boost;
				}else if(pid_value < 0){
					pid_value -= config.current_boost;
				}

				// Reset the timeout
				timeout_reset();

				// Output to motor
				if(pid_value == 0){
					mc_interface_release_motor();
				}else {
					mc_interface_set_current(pid_value);
				}
				break;
			case (FAULT):
				// Check for valid startup position
				if(pitch < config.startup_pitch && pitch > -config.startup_pitch && roll < config.startup_roll && roll > -config.startup_roll){
					setpoint = pitch;
					setpoint_target = 0;
					setpointAdjustmentType = STARTUP;
					state = RUNNING;
					break;
				}

				// Disable output
				mc_interface_set_current(0);
				break;
			case (DEAD):
				// Disable output
				mc_interface_set_current(0);
				break;
		}

		// Delay between loops
		chThdSleepMilliseconds(config.loop_delay);
	}

	// Disable output
	mc_interface_set_current(0);
}
