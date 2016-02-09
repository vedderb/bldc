/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

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
#include "mc_interface.h"
#include "timeout.h"
#include "utils.h"
#include "comm_can.h"
#include <math.h>

// Only available if servo output is not active
#if !SERVO_OUT_ENABLE

// Settings
#define MAX_CAN_AGE						0.1
#define MIN_PULSES_WITHOUT_POWER		50

// Threads
static THD_FUNCTION(ppm_thread, arg);
static THD_WORKING_AREA(ppm_thread_wa, 1024);
static thread_t *ppm_tp;
virtual_timer_t vt;

// Private functions
static void servodec_func(void);

// Private variables
static volatile bool is_running = false;
static volatile ppm_config config;
static volatile int pulses_without_power = 0;

// Private functions
static void update(void *p);
#endif

void app_ppm_configure(ppm_config *conf) {
#if !SERVO_OUT_ENABLE
	config = *conf;
	pulses_without_power = 0;

	if (is_running) {
		servodec_set_pulse_options(config.pulse_start, config.pulse_end, config.median_filter);
	}
#else
	(void)conf;
#endif
}

void app_ppm_start(void) {
#if !SERVO_OUT_ENABLE
	chThdCreateStatic(ppm_thread_wa, sizeof(ppm_thread_wa), NORMALPRIO, ppm_thread, NULL);

	chSysLock();
	chVTSetI(&vt, MS2ST(1), update, NULL);
	chSysUnlock();
#endif
}

#if !SERVO_OUT_ENABLE
static void servodec_func(void) {
	chSysLockFromISR();
	timeout_reset();
	chEvtSignalI(ppm_tp, (eventmask_t) 1);
	chSysUnlockFromISR();
}

static void update(void *p) {
	chSysLockFromISR();
	chVTSetI(&vt, MS2ST(2), update, p);
	chEvtSignalI(ppm_tp, (eventmask_t) 1);
	chSysUnlockFromISR();
}

static THD_FUNCTION(ppm_thread, arg) {
	(void)arg;

	chRegSetThreadName("APP_PPM");
	ppm_tp = chThdGetSelfX();

	servodec_set_pulse_options(config.pulse_start, config.pulse_end, config.median_filter);
	servodec_init(servodec_func);
	is_running = true;

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		if (timeout_has_timeout() || servodec_get_time_since_update() > timeout_get_timeout_msec() ||
				mc_interface_get_fault() != FAULT_CODE_NONE) {
			pulses_without_power = 0;
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

		float current = 0;
		bool current_mode = false;
		bool current_mode_brake = false;
		const volatile mc_configuration *mcconf = mc_interface_get_configuration();
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

			if (fabsf(servo_val) < 0.001) {
				pulses_without_power++;
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

			if (servo_val < 0.001) {
				pulses_without_power++;
			}
			break;

		case PPM_CTRL_TYPE_DUTY:
		case PPM_CTRL_TYPE_DUTY_NOREV:
			if (fabsf(servo_val) < 0.001) {
				pulses_without_power++;
			}

			if (!(pulses_without_power < MIN_PULSES_WITHOUT_POWER && config.safe_start)) {
				mc_interface_set_duty(utils_map(servo_val, -1.0, 1.0, -mcconf->l_max_duty, mcconf->l_max_duty));
				send_duty = true;
			}
			break;

		case PPM_CTRL_TYPE_PID:
		case PPM_CTRL_TYPE_PID_NOREV:
			if (fabsf(servo_val) < 0.001) {
				pulses_without_power++;
			}

			if (!(pulses_without_power < MIN_PULSES_WITHOUT_POWER && config.safe_start)) {
				mc_interface_set_pid_speed(servo_val * config.pid_max_erpm);
				send_duty = true;
			}
			break;

		default:
			continue;
		}

		if (pulses_without_power < MIN_PULSES_WITHOUT_POWER && config.safe_start) {
			static int pulses_without_power_before = 0;
			if (pulses_without_power == pulses_without_power_before) {
				pulses_without_power = 0;
			}
			pulses_without_power_before = pulses_without_power;
			mc_interface_set_brake_current(timeout_get_brake_current());
			continue;
		}

		// Find lowest RPM
		float rpm_local = mc_interface_get_rpm();
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

		if (send_duty && config.multi_esc) {
			float duty = mc_interface_get_duty_cycle_now();

			for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
				can_status_msg *msg = comm_can_get_status_msg_index(i);

				if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < MAX_CAN_AGE) {
					comm_can_set_duty(msg->id, duty);
				}
			}
		}

		if (current_mode) {
			if (current_mode_brake) {
				mc_interface_set_brake_current(current);

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
					mc_interface_set_current(-current_out);
				} else {
					mc_interface_set_current(current_out);
				}
			}
		}

	}
}
#endif
