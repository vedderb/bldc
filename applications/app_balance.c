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

#include <math.h>

// Data type
typedef enum {
	CALIBRATING = 0,
	RUNNING,
	FAULT
} BalanceState;

typedef enum {
	SLOW = 40,
	FAST = 5
} SetpointAdjustmentSpeed;

// Example thread
static THD_FUNCTION(example_thread, arg);
static THD_WORKING_AREA(example_thread_wa, 2048); // 2kb stack for this thread

static volatile balance_config config;
static thread_t *app_thread;

static bool registered_terminal = false;

// Values used in loop
static BalanceState state;
static double pitch, roll;
static double proportional, integral, derivative;
static double last_proportional;
static double pid_value;
static double setpoint, setpoint_target;
static SetpointAdjustmentSpeed setpointAdjustmentSpeed;
static systime_t current_time, last_time, diff_time;
static systime_t cal_start_time, cal_diff_time;
static systime_t last_setpoint_step_time;

// Values read to pass in app data to GUI
static double motor_current;
static double motor_position;

void app_balance_terminal_rpy(int argc, const char **argv) {
  if (argc != 4){
    commands_printf("PID values NOT set!");
    return;
  }
}

void app_balance_configure(balance_config *conf) {
  config = *conf;

  if(!registered_terminal){
    terminal_register_command_callback("euc_pid", "Sets PID values!", 0, app_balance_terminal_rpy);
    registered_terminal = true;
  }
}

void app_balance_start(void) {
	// Set the UART TX pin as an input with pulldown
	// palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_INPUT_PULLDOWN);
  commands_printf("Custom app start");
  hw_stop_i2c();
  hw_start_i2c();
  imu_init();

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
  setpointAdjustmentSpeed = SLOW;
  current_time = NULL;
  last_time = NULL;
  diff_time = NULL;
  cal_start_time = NULL;
  cal_diff_time = NULL;
  last_setpoint_step_time = NULL;

	// Start the example thread
	app_thread = chThdCreateStatic(example_thread_wa, sizeof(example_thread_wa), NORMALPRIO, example_thread, NULL);
}

void app_balance_stop(void) {
  commands_printf("Custom app stop");
	chThdTerminate(app_thread);
  mc_interface_set_current(0);
  hw_stop_i2c();
  hw_start_i2c();
  imu_init();
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
  if(diff_time != NULL){
    return ST2US(diff_time);
  }else{
    return 0;
  }
}
float app_balance_get_motor_current(void) {
  return motor_current;
}
float app_balance_get_motor_position(void) {
  return motor_position;
}

static THD_FUNCTION(example_thread, arg) {
	(void)arg;
	chRegSetThreadName("APP_EXAMPLE");
	commands_printf("Custom app thread");
//  chThdSleepSeconds(config.start_delay);

	while (!chThdShouldTerminateX()) {
		// Update times
		current_time = chVTGetSystemTimeX();
		if(last_time == NULL){
		  last_time = current_time;
		}
		diff_time = current_time - last_time;
		last_time = current_time;

		// Read values for GUI
		motor_current = mc_interface_get_tot_current_directional_filtered();
//		motor_current = mc_interface_get_duty_cycle_now();
		motor_position = mc_interface_get_pid_pos_now();

		// Read gyro values
		pitch = (double)(imu_get_pitch() * 180.0 / M_PI);
		roll = (double)(imu_get_roll() * 180.0 / M_PI);

		// Apply offsets
		pitch = fmod(((pitch + 180.0) + config.pitch_offset), 360.0) - 180.0;
		roll = fmod(((roll + 180.0) + config.roll_offset), 360.0) - 180.0;

		// State based logic
		switch(state){
			case (CALIBRATING):
				if(cal_start_time == NULL){
					cal_start_time = current_time;
					ahrs_set_madgwick_acc_confidence_decay(config.cal_m_acd);
					ahrs_set_madgwick_beta(config.cal_m_b);
				}
				cal_diff_time = current_time - cal_start_time;
				if(ST2MS(cal_diff_time) > config.cal_delay){
					state = FAULT;
					cal_start_time = NULL;
					cal_diff_time = NULL;

					ahrs_set_madgwick_acc_confidence_decay(config.m_acd);
					ahrs_set_madgwick_beta(config.m_b);
				}
				break;
			case (RUNNING):
				// Check for fault
				if(pitch > config.pitch_fault || pitch < -config.pitch_fault || roll > config.roll_fault || roll < -config.roll_fault){
					state = FAULT;
				}

				// Over speed tilt back safety
				if(mc_interface_get_duty_cycle_now() > .75){
					setpoint_target = 15;
					setpointAdjustmentSpeed = SLOW;
				} else if(mc_interface_get_duty_cycle_now() < -.75){
					setpoint_target = -15;
					setpointAdjustmentSpeed = SLOW;
				}else{
					setpoint_target = 0;
				}

				// Adjust setpoint
				if(setpoint != setpoint_target){
					// If it is time to take a step
					if(last_setpoint_step_time == NULL || ST2MS(current_time - last_setpoint_step_time) > setpointAdjustmentSpeed){
						// If we are less than one step size away, go all the way
						if(fabs(setpoint_target - setpoint) < 0.2){
							setpoint = setpoint_target;
						}else if (setpoint_target - setpoint > 0){
							setpoint += 0.2;
						}else{
							setpoint -= 0.2;
						}

						// Update step time
						last_setpoint_step_time = current_time;


					}
				}

				// Do PID maths
				proportional = setpoint - pitch;
				integral = integral + proportional;
				derivative = proportional - last_proportional;

				pid_value = (config.kp * proportional) + (config.ki * integral) + (config.kd * derivative);

				last_proportional = proportional;

				// Reset the timeout
				timeout_reset();

				// Output to motor
				mc_interface_set_current(pid_value);
				break;
			case (FAULT):
				// Check for running
				if(pitch < 30.0 && pitch > -30.0 && roll < 5.0 && roll > -5.0){
					setpoint = pitch;
					setpoint_target = 0;
					setpointAdjustmentSpeed = FAST;
					state = RUNNING;
					break;
				}

				// Disable output
				mc_interface_set_current(0);
				break;
		}



		// Delay between loops
		chThdSleepMilliseconds(config.loop_delay);
	}
  mc_interface_set_current(0);
}
