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
#include "datatypes.h"


#include <math.h>

// Data type
typedef enum {
	STARTUP = 0,
	RUNNING,
	FAULT,
	DEAD
} BalanceState;

typedef enum {
	CENTERING = 0,
	TILTBACK
} SetpointAdjustmentType;

// Balance thread
static THD_FUNCTION(balance_thread, arg);
static THD_WORKING_AREA(balance_thread_wa, 2048); // 2kb stack for this thread

static volatile balance_config balance_conf;
static volatile imu_config imu_conf;
static thread_t *app_thread;

// Values used in loop
static BalanceState state;
static float pitch_angle, roll_angle;
static float proportional, integral, derivative;
static float last_proportional;
static float pid_value;
static float setpoint, setpoint_target;
static SetpointAdjustmentType setpointAdjustmentType;
static float startup_step_size, tiltback_step_size;
static systime_t current_time, last_time, diff_time;
static systime_t startup_start_time, startup_diff_time;
static uint16_t switches_value;

// Values read to pass in app data to GUI
static float motor_current;
static float motor_position;

void app_balance_configure(balance_config *conf, imu_config *conf2) {
	balance_conf = *conf;
	imu_conf = *conf2;
}

void app_balance_start(void) {

	// Reset all Values
	state = STARTUP;
	pitch_angle = 0;
	roll_angle = 0;
	switches_value = 0;
	proportional = 0;
	integral = 0;
	derivative = 0;
	last_proportional = 0;
	pid_value = 0;
	setpoint = 0;
	setpoint_target = 0;
	setpointAdjustmentType = CENTERING;
	startup_step_size = 0;
	tiltback_step_size = 0;
	current_time = 0;
	last_time = 0;
	diff_time = 0;
	startup_start_time = 0;
	startup_diff_time = 0;

#ifdef HW_SPI_PORT_SCK
	// Configure pins
	if(balance_conf.use_switches){
		palSetPadMode(HW_SPI_PORT_SCK, HW_SPI_PIN_SCK, PAL_MODE_INPUT_PULLDOWN);
		palSetPadMode(HW_SPI_PORT_MISO, HW_SPI_PIN_MISO, PAL_MODE_INPUT_PULLDOWN);
	}
#endif

	// Start the balance thread
	app_thread = chThdCreateStatic(balance_thread_wa, sizeof(balance_thread_wa), NORMALPRIO, balance_thread, NULL);
}

void app_balance_stop(void) {
	if(app_thread != NULL){
		chThdTerminate(app_thread);
		chThdWait(app_thread);
	}
	mc_interface_set_current(0);
}

float app_balance_get_pid_output(void) {
	return pid_value;
}
float app_balance_get_pitch_angle(void) {
	return pitch_angle;
}
float app_balance_get_roll_angle(void) {
	return roll_angle;
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
uint16_t app_balance_get_switch_value(void) {
	return switches_value;
}

float get_setpoint_adjustment_step_size(void){
	switch(setpointAdjustmentType){
		case (CENTERING):
			return startup_step_size;
		case (TILTBACK):
			return tiltback_step_size;
	}
	return 0;
}

float apply_deadzone(float error){
	if(balance_conf.deadzone == 0){
		return error;
	}

	if(error < balance_conf.deadzone && error > -balance_conf.deadzone){
		return 0;
	} else if(error > balance_conf.deadzone){
		return error - balance_conf.deadzone;
	} else {
		return error + balance_conf.deadzone;
	}
}

static THD_FUNCTION(balance_thread, arg) {
	(void)arg;
	chRegSetThreadName("APP_BALANCE");

	// Do one off config
	startup_step_size = balance_conf.startup_speed / balance_conf.hertz;
	tiltback_step_size = balance_conf.tiltback_speed / balance_conf.hertz;

	state = STARTUP;
	setpointAdjustmentType = CENTERING;

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

		// Get the values we want
		pitch_angle = imu_get_pitch() * 180.0f / M_PI;
		roll_angle = imu_get_roll() * 180.0f / M_PI;

		if(!balance_conf.use_switches){
			switches_value = 2;
		}else{
			switches_value = 0;
#ifdef HW_SPI_PORT_SCK
			if(palReadPad(HW_SPI_PORT_SCK, HW_SPI_PIN_SCK)){
				switches_value += 1;
			}
			if(palReadPad(HW_SPI_PORT_MISO, HW_SPI_PIN_MISO)){
				switches_value += 1;
			}
#endif
		}

		// State based logic
		switch(state){
			case (STARTUP):
				while(!imu_startup_done()){
					chThdSleepMilliseconds(50);
				}
				state = FAULT;
				startup_start_time = 0;
				startup_diff_time = 0;
				break;
			case (RUNNING):
				// Check for overspeed
				if(fabsf(mc_interface_get_duty_cycle_now()) > balance_conf.overspeed_duty){
					state = DEAD;
				}

				// Check for fault
				if(
					fabsf(pitch_angle) > balance_conf.pitch_fault || // Balnce axis tip over
					fabsf(roll_angle) > balance_conf.roll_fault || // Cross axis tip over
					app_balance_get_switch_value() == 0 || // Switch fully open
					(app_balance_get_switch_value() == 1 && fabsf(mc_interface_get_duty_cycle_now()) < 0.003) // Switch partially open and stopped
						){
					state = FAULT;
				}

				// Over speed tilt back safety
				if(fabsf(mc_interface_get_duty_cycle_now()) > balance_conf.tiltback_duty ||
						(fabsf(mc_interface_get_duty_cycle_now()) > 0.05 && GET_INPUT_VOLTAGE() > balance_conf.tiltback_high_voltage) ||
						(fabsf(mc_interface_get_duty_cycle_now()) > 0.05 && GET_INPUT_VOLTAGE() < balance_conf.tiltback_low_voltage)){
					if(mc_interface_get_duty_cycle_now() > 0){
						setpoint_target = balance_conf.tiltback_angle;
					} else {
						setpoint_target = -balance_conf.tiltback_angle;
					}
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
				proportional = setpoint - pitch_angle;
				// Apply deadzone
				proportional = apply_deadzone(proportional);
				// Resume real PID maths
				integral = integral + proportional;
				derivative = proportional - last_proportional;

				pid_value = (balance_conf.kp * proportional) + (balance_conf.ki * integral) + (balance_conf.kd * derivative);

				last_proportional = proportional;

				// Apply current boost
				if(pid_value > 0){
					pid_value += balance_conf.current_boost;
				}else if(pid_value < 0){
					pid_value -= balance_conf.current_boost;
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
				// Check for valid startup position and switch state
				if(fabsf(pitch_angle) < balance_conf.startup_pitch_tolerance && fabsf(roll_angle) < balance_conf.startup_roll_tolerance && app_balance_get_switch_value() == 2){
					setpoint = pitch_angle;
					setpoint_target = 0;
					setpointAdjustmentType = CENTERING;
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
		chThdSleepMicroseconds((int)((1000.0 / balance_conf.hertz) * 1000.0));
	}

	// Disable output
	mc_interface_set_current(0);
}
