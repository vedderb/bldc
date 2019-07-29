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

#include <math.h>

// Example thread
static THD_FUNCTION(example_thread, arg);
static THD_WORKING_AREA(example_thread_wa, 2048); // 2kb stack for this thread

static volatile balance_config config;
static thread_t *app_thread;

static bool registered_terminal = false;

// Values used in loop
static double pitch;
static double error, error_average, error_change;
static double last_error;
static double pid_value;

// Values read to pass in app data to GUI
static double motor_current;

void app_balance_terminal_rpy(int argc, const char **argv) {
  if (argc != 4){
    commands_printf("PID values NOT set!");
    return;
  }
  // float _p = 0, _i = 0, _d = 0;
  // sscanf(argv[1], "%f", &_p);
  // sscanf(argv[2], "%f", &_i);
  // sscanf(argv[2], "%f", &_d);
  //
  // p = _p;
  // i = _i;
  // d = _d;
  //
	// commands_printf("PID values set :) %.2f %.2f %.2f - %.2f %.2f %.2f", _p, _i, _d, p, i, d);
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
  pitch = 0;
  error = 0;
  error_average = 0;
  error_change = 0;
  last_error = 0;
  pid_value = 0;
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
  return config.ki;
}
float app_balance_get_motor_current(void) {
  return motor_current;
}

static THD_FUNCTION(example_thread, arg) {
	(void)arg;

	chRegSetThreadName("APP_EXAMPLE");

  commands_printf("Custom app thread");

  chThdSleepSeconds(config.start_delay);


	while (!chThdShouldTerminateX()) {
    // Read values for GUI
    motor_current = mc_interface_get_tot_current_directional_filtered();

    pitch = (double)(imu_get_pitch() * 180.0 / M_PI);

    error = config.pitch_offset - pitch;
    error_average = error_average + error;
    error_change = error - last_error;

    pid_value = (config.kp*error) + (config.ki*error_average) + (config.kd*error_change);

    last_error = error;

    // commands_printf("euc_pid values  %.4f %.4f - %.2f %.2f %.2f - %.2f %.2f %.2f", pitch, pid_value, p, i, d, error, error_average, error_change);
    mc_interface_set_current(pid_value);
		// Run this loop at 1000Hz
    chThdSleepMilliseconds(5);

		// Reset the timeout
		timeout_reset();
	}
  mc_interface_set_current(0);
}
