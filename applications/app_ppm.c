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
 * app_ppm.c
 *
 *  Created on: 18 apr 2014
 *      Author: benjamin
 */

#include "app.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "servo_dec.h"
#include "mcpwm.h"
#include "timeout.h"
#include "utils.h"
#include "comm_can.h"
#include <math.h>

// Settings
#define MAX_CAN_AGE						0.1

// Threads
static msg_t ppm_thread(void *arg);
static WORKING_AREA(ppm_thread_wa, 1024);
static Thread *ppm_tp;
VirtualTimer vt;

// Private functions
static void servodec_func(void);

// Private variables
static volatile bool is_running = false;
static volatile ppm_config config;

// Private functions
void update(void *p);

void app_ppm_configure(ppm_config *conf) {
	config = *conf;

	if (is_running) {
		servodec_set_pulse_options(config.pulse_start, config.pulse_width);
	}
}

void app_ppm_start(void) {
	chThdCreateStatic(ppm_thread_wa, sizeof(ppm_thread_wa), NORMALPRIO, ppm_thread, NULL);

	chSysLock();
	chVTSetI(&vt, MS2ST(1), update, NULL);
	chSysUnlock();
}

static void servodec_func(void) {
	chSysLockFromIsr();
	timeout_reset();
	chEvtSignalI(ppm_tp, (eventmask_t) 1);
	chSysUnlockFromIsr();
}

void update(void *p) {
	chSysLockFromIsr();
	chVTSetI(&vt, MS2ST(1), update, p);
	chEvtSignalI(ppm_tp, (eventmask_t) 1);
	chSysUnlockFromIsr();
}

static msg_t ppm_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP_PPM");
	ppm_tp = chThdSelf();

	servodec_set_pulse_options(config.pulse_start, config.pulse_width);
	servodec_init(servodec_func);
	is_running = true;

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		if (timeout_has_timeout()) {
			continue;
		}

		float servo_val = servodec_get_servo(0);

		switch (config.ctrl_type) {
		case PPM_CTRL_TYPE_CURRENT_NOREV:
		case PPM_CTRL_TYPE_DUTY_NOREV:
		case PPM_CTRL_TYPE_PID_NOREV:
			servo_val += 1.0;
			servo_val /= 2.0;
			break;

		default:
			break;
		}

		utils_deadband(&servo_val, config.hyst, 1.0);

		// Find lowest RPM
		float rpm_local = mcpwm_get_rpm();
		float rpm_lowest = rpm_local;
		if (config.multi_esc) {
			for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);

				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
					float rpm_tmp = msg->rpm;

					if (fabsf(rpm_tmp) < fabsf(rpm_lowest)) {
						rpm_lowest = rpm_tmp;
					}
				}
			}
		}

		float current = 0;
		bool current_mode = false;
		bool current_mode_brake = false;
		const volatile mc_configuration *mcconf = mcpwm_get_configuration();
		bool send_duty = false;

		switch (config.ctrl_type) {
		case PPM_CTRL_TYPE_CURRENT:
		case PPM_CTRL_TYPE_CURRENT_NOREV:
			current_mode = true;
			if (servo_val >= 0.0) {
				current = servo_val * mcconf->l_current_max;
			} else {
				current = servo_val * fabsf(mcconf->l_current_min);
			}
			break;

		case PPM_CTRL_TYPE_CURRENT_NOREV_BRAKE:
			current_mode = true;
			if (servo_val >= 0.0) {
				current = servo_val * mcconf->l_current_max;
			} else {
				current = fabsf(servo_val * mcconf->l_current_min);
				current_mode_brake = true;
			}
			break;

		case PPM_CTRL_TYPE_DUTY:
		case PPM_CTRL_TYPE_DUTY_NOREV:
			mcpwm_set_duty(servo_val);
			send_duty = true;
			break;

		case PPM_CTRL_TYPE_PID:
		case PPM_CTRL_TYPE_PID_NOREV:
			mcpwm_set_pid_speed(servo_val * config.pid_max_erpm);
			send_duty = true;
			break;

		default:
			break;
		}

		if (send_duty && config.multi_esc) {
			float duty = mcpwm_get_duty_cycle_now();

			for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);

				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
					comm_can_set_duty(msg->id, duty);
				}
			}
		}

		if (current_mode) {
			if (current_mode_brake) {
				mcpwm_set_brake_current(current);

				// Send brake command to all ESCs seen recently on the CAN bus
				for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
					can_status_msg *msg = comm_can_get_status_msg_index(i);

					if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
						comm_can_set_current_brake(msg->id, current);
					}
				}
			} else {
				// Apply soft RPM limit
				if (rpm_lowest > config.rpm_lim_end && current > 0.0) {
					current = mcconf->cc_min_current;
				} else if (rpm_lowest > config.rpm_lim_start && current > 0.0) {
					current = utils_map(rpm_lowest, config.rpm_lim_start, config.rpm_lim_end, current, mcconf->cc_min_current);
				} else if (rpm_lowest < -config.rpm_lim_end && current < 0.0) {
					current = mcconf->cc_min_current;
				} else if (rpm_lowest < -config.rpm_lim_start && current < 0.0) {
					rpm_lowest = -rpm_lowest;
					current = -current;
					current = utils_map(rpm_lowest, config.rpm_lim_start, config.rpm_lim_end, current, mcconf->cc_min_current);
					current = -current;
					rpm_lowest = -rpm_lowest;
				}

				float current_out = current;
				bool is_reverse = false;
				if (current_out < 0.0) {
					is_reverse = true;
					current_out = -current_out;
					current = -current;
					rpm_local = -rpm_local;
					rpm_lowest = -rpm_lowest;
				}

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
					mcpwm_set_current(-current_out);
				} else {
					mcpwm_set_current(current_out);
				}
			}
		}

	}

	return 0;
}
