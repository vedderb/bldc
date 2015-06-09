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
 * app_adc.c
 *
 *  Created on: 1 may 2015
 *      Author: benjamin
 */

#include "app.h"

#include "ch.h"
#include "hal.h"
#include "stm32f4xx_conf.h"
#include "mcpwm.h"
#include "timeout.h"
#include "utils.h"
#include "comm_can.h"
#include "hw.h"
#include <math.h>

// Settings
#define MAX_CAN_AGE						0.1
#define MIN_MS_WITHOUT_POWER			500
#define FILTER_SAMPLES					5

// Threads
static msg_t adc_thread(void *arg);
static WORKING_AREA(adc_thread_wa, 1024);

// Private variables
static volatile adc_config config;
static volatile float ms_without_power = 0;
static volatile float decoded_level = 0.0;
static volatile float read_voltage = 0.0;

void app_adc_configure(adc_config *conf) {
	config = *conf;
	ms_without_power = 0.0;
}

void app_adc_start(void) {
	chThdCreateStatic(adc_thread_wa, sizeof(adc_thread_wa), NORMALPRIO, adc_thread, NULL);
}

float app_adc_get_decoded_level(void) {
	return decoded_level;
}

float app_adc_get_voltage(void) {
	return read_voltage;
}

static msg_t adc_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("APP_ADC");

	// Set servo pin as an input with pullup
	palSetPadMode(HW_ICU_GPIO, HW_ICU_PIN, PAL_MODE_INPUT_PULLUP);

	for(;;) {
		// Sleep for a time according to the specified rate
		systime_t sleep_time = CH_FREQUENCY / config.update_rate_hz;

		// At least one tick should be slept to not block the other threads
		if (sleep_time == 0) {
			sleep_time = 1;
		}
		chThdSleep(sleep_time);

		// Read the external ADC pin and convert the value to a voltage.
		float pwr = (float)ADC_Value[ADC_IND_EXT];
		pwr /= 4095;
		pwr *= V_REG;

		read_voltage = pwr;

		// Optionally apply a mean value filter
		if (config.use_filter) {
			static float filter_buffer[FILTER_SAMPLES];
			static int filter_ptr = 0;

			filter_buffer[filter_ptr++] = pwr;
			if (filter_ptr >= FILTER_SAMPLES) {
				filter_ptr = 0;
			}

			pwr = 0.0;
			for (int i = 0;i < FILTER_SAMPLES;i++) {
				pwr += filter_buffer[i];
			}
			pwr /= FILTER_SAMPLES;
		}

		// Map and truncate the read voltage
		pwr = utils_map(pwr, config.voltage_start, config.voltage_end, 0.0, 1.0);
		utils_truncate_number(&pwr, 0.0, 1.0);

		// Optionally invert the read voltage
		if (config.voltage_inverted) {
			pwr = 1.0 - pwr;
		}

		decoded_level = pwr;

		// Read the servo pin and optionally invert it.
		bool button_val = !palReadPad(HW_ICU_GPIO, HW_ICU_PIN);
		if (config.button_inverted) {
			button_val = !button_val;
		}

		switch (config.ctrl_type) {
		case ADC_CTRL_TYPE_CURRENT_REV_CENTER:
		case ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_CENTER:
		case ADC_CTRL_TYPE_DUTY_REV_CENTER:
			// Scale the voltage and set 0 at the center
			pwr *= 2.0;
			pwr -= 1.0;
			break;

		case ADC_CTRL_TYPE_CURRENT_REV_BUTTON:
		case ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_BUTTON:
		case ADC_CTRL_TYPE_DUTY_REV_BUTTON:
			// Invert the voltage if the button is pressed
			if (button_val) {
				pwr = -pwr;
			}
			break;

		default:
			break;
		}

		// Apply a deadband
		utils_deadband(&pwr, config.hyst, 1.0);

		float current = 0;
		bool current_mode = false;
		bool current_mode_brake = false;
		const volatile mc_configuration *mcconf = mcpwm_get_configuration();
		bool send_duty = false;

		// Use the filtered and mapped voltage for control according to the configuration.
		switch (config.ctrl_type) {
		case ADC_CTRL_TYPE_CURRENT:
		case ADC_CTRL_TYPE_CURRENT_REV_CENTER:
		case ADC_CTRL_TYPE_CURRENT_REV_BUTTON:
			current_mode = true;
			if (pwr >= 0.0) {
				current = pwr * mcconf->l_current_max;
			} else {
				current = pwr * fabsf(mcconf->l_current_min);
			}

			if (fabsf(pwr) < 0.001) {
				ms_without_power += (1000.0 * (float)sleep_time) / (float)CH_FREQUENCY;
			}
			break;

		case ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_CENTER:
		case ADC_CTRL_TYPE_CURRENT_NOREV_BRAKE_BUTTON:
			current_mode = true;
			if (pwr >= 0.0) {
				current = pwr * mcconf->l_current_max;
			} else {
				current = fabsf(pwr * mcconf->l_current_min);
				current_mode_brake = true;
			}

			if (pwr < 0.001) {
				ms_without_power += (1000.0 * (float)sleep_time) / (float)CH_FREQUENCY;
			}
			break;

		case ADC_CTRL_TYPE_DUTY:
		case ADC_CTRL_TYPE_DUTY_REV_CENTER:
		case ADC_CTRL_TYPE_DUTY_REV_BUTTON:
			if (fabsf(pwr) < 0.001) {
				ms_without_power += (1000.0 * (float)sleep_time) / (float)CH_FREQUENCY;
			}

			if (!(ms_without_power < MIN_MS_WITHOUT_POWER && config.safe_start)) {
				mcpwm_set_duty(pwr);
				send_duty = true;
			}
			break;

		default:
			continue;
		}

		// If safe start is enabled and the output has not been zero for long enough
		if (ms_without_power < MIN_MS_WITHOUT_POWER && config.safe_start) {
			static int pulses_without_power_before = 0;
			if (ms_without_power == pulses_without_power_before) {
				ms_without_power = 0;
			}
			pulses_without_power_before = ms_without_power;
			mcpwm_set_brake_current(timeout_get_brake_current());
			continue;
		}

		// Reset timeout
		timeout_reset();

		// Find lowest RPM (for traction control)
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

		// Optionally send the duty cycles to the other ESCs seen on the CAN-bus
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
