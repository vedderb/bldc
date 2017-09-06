/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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
#include "hw.h"
#include "mc_interface.h"
#include "commands.h"
#include "utils.h"
#include "timeout.h"
#include <string.h>
#include <math.h>
#include "led_external.h"
#include "datatypes.h"
#include "comm_can.h"
#include "terminal.h"

// Settings
#define OUTPUT_ITERATION_TIME_MS		1
#define MAX_CAN_AGE						0.1
#define RPM_FILTER_SAMPLES				8
#define LOCAL_TIMEOUT					2000

// Threads
static THD_FUNCTION(chuk_thread, arg);
static THD_WORKING_AREA(chuk_thread_wa, 1024);
static THD_FUNCTION(output_thread, arg);
static THD_WORKING_AREA(output_thread_wa, 1024);

// Private variables
static volatile bool stop_now = true;
static volatile bool is_running = false;
static volatile chuck_data chuck_d;
static volatile int chuck_error = 0;
static volatile chuk_config config;
static volatile bool output_running = false;
static volatile systime_t last_update_time;

// Private functions
static void terminal_cmd_nunchuk_status(int argc, const char **argv);

void app_nunchuk_configure(chuk_config *conf) {
	config = *conf;

	terminal_register_command_callback(
			"nunchuk_status",
			"Print the status of the nunchuk app",
			0,
			terminal_cmd_nunchuk_status);
}

void app_nunchuk_start(void) {
	chuck_d.js_y = 128;
	stop_now = false;
	hw_start_i2c();
	chThdCreateStatic(chuk_thread_wa, sizeof(chuk_thread_wa), NORMALPRIO, chuk_thread, NULL);
}

void app_nunchuk_stop(void) {
	stop_now = true;

	if (is_running) {
		hw_stop_i2c();
	}

	while (is_running) {
		chThdSleepMilliseconds(1);
	}
}

float app_nunchuk_get_decoded_chuk(void) {
	return ((float)chuck_d.js_y - 128.0) / 128.0;
}

void app_nunchuk_update_output(chuck_data *data) {
	if (!output_running) {
		last_update_time = 0;
		output_running = true;
		chuck_d.js_y = 128;
		chThdCreateStatic(output_thread_wa, sizeof(output_thread_wa), NORMALPRIO, output_thread, NULL);
	}

	chuck_d = *data;
	last_update_time = chVTGetSystemTime();
	timeout_reset();
}

static THD_FUNCTION(chuk_thread, arg) {
	(void)arg;

	chRegSetThreadName("Nunchuk i2c");
	is_running = true;

	uint8_t rxbuf[10];
	uint8_t txbuf[10];
	msg_t status = MSG_OK;
	systime_t tmo = MS2ST(5);
	i2caddr_t chuck_addr = 0x52;
	chuck_data chuck_d_tmp;

	hw_start_i2c();
	chThdSleepMilliseconds(10);

	for(;;) {
		bool is_ok = true;

		if (stop_now) {
			is_running = false;
			chuck_error = 0;
			return;
		}

		txbuf[0] = 0xF0;
		txbuf[1] = 0x55;
		i2cAcquireBus(&HW_I2C_DEV);
		status = i2cMasterTransmitTimeout(&HW_I2C_DEV, chuck_addr, txbuf, 2, rxbuf, 0, tmo);
		i2cReleaseBus(&HW_I2C_DEV);
		is_ok = status == MSG_OK;

		if (is_ok) {
			txbuf[0] = 0xFB;
			txbuf[1] = 0x00;
			i2cAcquireBus(&HW_I2C_DEV);
			status = i2cMasterTransmitTimeout(&HW_I2C_DEV, chuck_addr, txbuf, 2, rxbuf, 0, tmo);
			i2cReleaseBus(&HW_I2C_DEV);
			is_ok = status == MSG_OK;
		}

		if (is_ok) {
			txbuf[0] = 0x00;
			i2cAcquireBus(&HW_I2C_DEV);
			status = i2cMasterTransmitTimeout(&HW_I2C_DEV, chuck_addr, txbuf, 1, rxbuf, 0, tmo);
			i2cReleaseBus(&HW_I2C_DEV);
			is_ok = status == MSG_OK;
		}

		if (is_ok) {
			chThdSleepMilliseconds(3);

			i2cAcquireBus(&HW_I2C_DEV);
			status = i2cMasterReceiveTimeout(&HW_I2C_DEV, chuck_addr, rxbuf, 6, tmo);
			i2cReleaseBus(&HW_I2C_DEV);
			is_ok = status == MSG_OK;
		}

		if (is_ok) {
			static uint8_t last_buffer[6];
			int same = 1;

			for (int i = 0;i < 6;i++) {
				if (last_buffer[i] != rxbuf[i]) {
					same = 0;
				}
			}

			memcpy(last_buffer, rxbuf, 6);

			if (!same) {
				chuck_error = 0;
				chuck_d_tmp.js_x = rxbuf[0];
				chuck_d_tmp.js_y = rxbuf[1];
				chuck_d_tmp.acc_x = (rxbuf[2] << 2) | ((rxbuf[5] >> 2) & 3);
				chuck_d_tmp.acc_y = (rxbuf[3] << 2) | ((rxbuf[5] >> 4) & 3);
				chuck_d_tmp.acc_z = (rxbuf[4] << 2) | ((rxbuf[5] >> 6) & 3);
				chuck_d_tmp.bt_z = !((rxbuf[5] >> 0) & 1);
				chuck_d_tmp.bt_c = !((rxbuf[5] >> 1) & 1);

				app_nunchuk_update_output(&chuck_d_tmp);
			}

			if (timeout_has_timeout()) {
				chuck_error = 1;
			}
		} else {
			chuck_error = 2;
			hw_try_restore_i2c();
			chThdSleepMilliseconds(100);
		}

		chThdSleepMilliseconds(10);
	}
}

static THD_FUNCTION(output_thread, arg) {
	(void)arg;

	chRegSetThreadName("Nunchuk output");

	for(;;) {
		chThdSleepMilliseconds(OUTPUT_ITERATION_TIME_MS);

		if (timeout_has_timeout() || chuck_error != 0 || config.ctrl_type == CHUK_CTRL_TYPE_NONE) {
			continue;
		}

		// Local timeout to prevent this thread from causing problems after not
		// being used for a while.
		if (chVTTimeElapsedSinceX(last_update_time) > MS2ST(LOCAL_TIMEOUT)) {
			continue;
		}

		const volatile mc_configuration *mcconf = mc_interface_get_configuration();
		static bool is_reverse = false;
		static bool was_z = false;
		const float current_now = mc_interface_get_tot_current_directional_filtered();
		static float prev_current = 0.0;
		const float max_current_diff = mcconf->l_current_max * 0.2;

		if (chuck_d.bt_c && chuck_d.bt_z) {
			led_external_set_state(LED_EXT_BATT);
			continue;
		}

		if (chuck_d.bt_z && !was_z && config.ctrl_type == CHUK_CTRL_TYPE_CURRENT &&
				fabsf(current_now) < max_current_diff) {
			if (is_reverse) {
				is_reverse = false;
			} else {
				is_reverse = true;
			}
		}

		was_z = chuck_d.bt_z;

		led_external_set_reversed(is_reverse);

		float out_val = app_nunchuk_get_decoded_chuk();
		utils_deadband(&out_val, config.hyst, 1.0);
		out_val = utils_throttle_curve(out_val, config.throttle_exp, config.throttle_exp_brake, config.throttle_exp_mode);

		// LEDs
		float x_axis = ((float)chuck_d.js_x - 128.0) / 128.0;
		if (out_val < -0.001) {
			if (x_axis < -0.4) {
				led_external_set_state(LED_EXT_BRAKE_TURN_LEFT);
			} else if (x_axis > 0.4) {
				led_external_set_state(LED_EXT_BRAKE_TURN_RIGHT);
			} else {
				led_external_set_state(LED_EXT_BRAKE);
			}
		} else {
			if (x_axis < -0.4) {
				led_external_set_state(LED_EXT_TURN_LEFT);
			} else if (x_axis > 0.4) {
				led_external_set_state(LED_EXT_TURN_RIGHT);
			} else {
				led_external_set_state(LED_EXT_NORMAL);
			}
		}

		// If c is pressed and no throttle is used, maintain the current speed with PID control
		static bool was_pid = false;

		// Filter RPM to avoid glitches
		static float filter_buffer[RPM_FILTER_SAMPLES];
		static int filter_ptr = 0;
		filter_buffer[filter_ptr++] = mc_interface_get_rpm();
		if (filter_ptr >= RPM_FILTER_SAMPLES) {
			filter_ptr = 0;
		}

		float rpm_filtered = 0.0;
		for (int i = 0;i < RPM_FILTER_SAMPLES;i++) {
			rpm_filtered += filter_buffer[i];
		}
		rpm_filtered /= RPM_FILTER_SAMPLES;

		if (chuck_d.bt_c) {
			static float pid_rpm = 0.0;

			if (!was_pid) {
				pid_rpm = rpm_filtered;

				if ((is_reverse && pid_rpm > 0.0) || (!is_reverse && pid_rpm < 0.0)) {
					if (fabsf(pid_rpm) > mcconf->s_pid_min_erpm) {
						// Abort if the speed is too high in the opposite direction
						continue;
					} else {
						pid_rpm = 0.0;
					}
				}

				was_pid = true;
			} else {
				if (is_reverse) {
					if (pid_rpm > 0.0) {
						pid_rpm = 0.0;
					}

					pid_rpm -= (out_val * config.stick_erpm_per_s_in_cc) / ((float)OUTPUT_ITERATION_TIME_MS * 1000.0);

					if (pid_rpm < (rpm_filtered - config.stick_erpm_per_s_in_cc)) {
						pid_rpm = rpm_filtered - config.stick_erpm_per_s_in_cc;
					}
				} else {
					if (pid_rpm < 0.0) {
						pid_rpm = 0.0;
					}

					pid_rpm += (out_val * config.stick_erpm_per_s_in_cc) / ((float)OUTPUT_ITERATION_TIME_MS * 1000.0);

					if (pid_rpm > (rpm_filtered + config.stick_erpm_per_s_in_cc)) {
						pid_rpm = rpm_filtered + config.stick_erpm_per_s_in_cc;
					}
				}
			}

			mc_interface_set_pid_speed(pid_rpm);

			// Send the same current to the other controllers
			if (config.multi_esc) {
				float current = mc_interface_get_tot_current_directional_filtered();

				for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
					can_status_msg *msg = comm_can_get_status_msg_index(i);

					if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
						comm_can_set_current(msg->id, current);
					}
				}
			}

			// Set the previous ramping current to not get a spike when releasing
			// PID control and to get a smooth transition.
			prev_current = current_now;

			continue;
		}

		was_pid = false;

		float current = 0;

		if (out_val >= 0.0) {
			current = out_val * mcconf->lo_current_motor_max_now;
		} else {
			current = out_val * fabsf(mcconf->lo_current_motor_min_now);
		}

		// Find lowest RPM and highest current
		float rpm_local = mc_interface_get_rpm();
		if (is_reverse) {
			rpm_local = -rpm_local;
		}

		float rpm_lowest = rpm_local;
		float current_highest_abs = current_now;

		if (config.multi_esc) {
			for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);

				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
					float rpm_tmp = msg->rpm;
					if (is_reverse) {
						rpm_tmp = -rpm_tmp;
					}

					if (rpm_tmp < rpm_lowest) {
						rpm_lowest = rpm_tmp;
					}

					// Make the current directional
					float msg_current = msg->current;
					if (msg->duty < 0.0) {
						msg_current = -msg_current;
					}

					if (fabsf(msg_current) > fabsf(current_highest_abs)) {
						current_highest_abs = msg_current;
					}
				}
			}
		}

		// Apply ramping
		const float current_range = mcconf->l_current_max + fabsf(mcconf->l_current_min);
		const float ramp_time = fabsf(current) > fabsf(prev_current) ? config.ramp_time_pos : config.ramp_time_neg;

		if (ramp_time > 0.01) {
			const float ramp_step = ((float)OUTPUT_ITERATION_TIME_MS * current_range) / (ramp_time * 1000.0);

			float current_goal = prev_current;
			const float goal_tmp = current_goal;
			utils_step_towards(&current_goal, current, ramp_step);
			bool is_decreasing = current_goal < goal_tmp;

			// Make sure the desired current is close to the actual current to avoid surprises
			// when changing direction
			float goal_tmp2 = current_goal;
			if (is_reverse) {
				if (fabsf(current_goal + current_highest_abs) > max_current_diff) {
					utils_step_towards(&goal_tmp2, -current_highest_abs, 2.0 * ramp_step);
				}
			} else {
				if (fabsf(current_goal - current_highest_abs) > max_current_diff) {
					utils_step_towards(&goal_tmp2, current_highest_abs, 2.0 * ramp_step);
				}
			}

			// Always allow negative ramping
			bool is_decreasing2 = goal_tmp2 < current_goal;
			if ((!is_decreasing || is_decreasing2) && fabsf(out_val) > 0.001) {
				current_goal = goal_tmp2;
			}

			current = current_goal;
		}

		prev_current = current;

		if (current < 0.0) {
			mc_interface_set_brake_current(current);

			// Send brake command to all ESCs seen recently on the CAN bus
			for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);

				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
					comm_can_set_current_brake(msg->id, current);
				}
			}
		} else {
			float current_out = current;

			// Traction control
			if (config.multi_esc) {
				for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
					can_status_msg *msg = comm_can_get_status_msg_index(i);

					if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
						if (config.tc) {
							float rpm_tmp = msg->rpm;
							if (is_reverse) {
								rpm_tmp = -rpm_tmp;
							}

							float diff = rpm_tmp - rpm_lowest;
							current_out = utils_map(diff, 0.0, config.tc_max_diff, current, 0.0);
							if (current_out < mcconf->cc_min_current) {
								current_out = 0.0;
							}
						}

						if (is_reverse) {
							comm_can_set_current(msg->id, -current_out);
						} else {
							comm_can_set_current(msg->id, current_out);
						}
					}
				}

				if (config.tc) {
					float diff = rpm_local - rpm_lowest;
					current_out = utils_map(diff, 0.0, config.tc_max_diff, current, 0.0);
					if (current_out < mcconf->cc_min_current) {
						current_out = 0.0;
					}
				}
			}

			if (is_reverse) {
				mc_interface_set_current(-current_out);
			} else {
				mc_interface_set_current(current_out);
			}
		}
	}
}

static void terminal_cmd_nunchuk_status(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	commands_printf("Nunchuk Status");
	commands_printf("Output: %s", output_running ? "On" : "Off");
	commands_printf(" ");
}
