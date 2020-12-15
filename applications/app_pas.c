/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se
	Copyright 2020 Marcos Chaparro	mchaparro@powerdesigns.ca

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
#include "stm32f4xx_conf.h"
#include "mc_interface.h"
#include "timeout.h"
#include "utils.h"
#include "comm_can.h"
#include "hw.h"
#include <math.h>

// Settings
#define PEDAL_INPUT_TIMEOUT				0.2
#define MIN_MS_WITHOUT_POWER			500
#define FILTER_SAMPLES					5
#define RPM_FILTER_SAMPLES				8

// Threads
static THD_FUNCTION(pas_thread, arg);
static THD_WORKING_AREA(pas_thread_wa, 1024);

// Private variables
static volatile pas_config config;
static volatile float output_current_rel = 0.0;
static volatile float ms_without_power = 0.0;
static volatile float max_pulse_period = 0.0;
static volatile float min_pedal_period = 0.0;
static volatile float direction_conf = 0.0;
static volatile float pedal_rpm = 0;
static volatile bool primary_output = false;
static volatile bool stop_now = true;
static volatile bool is_running = false;

void app_pas_configure(pas_config *conf) {
	config = *conf;
	ms_without_power = 0.0;
	output_current_rel = 0.0;

	// a period longer than this should immediately reduce power to zero
	max_pulse_period = 1.0 / ((config.pedal_rpm_start / 60.0) * config.magnets) * 1.2;

	// if pedal spins at x3 the end rpm, assume its beyond limits
	min_pedal_period = 1.0 / ((config.pedal_rpm_end * 3.0 / 60.0));

	if (config.invert_pedal_direction == true )
		direction_conf= -1.0;
	else
		direction_conf = 1.0;
}

/**
 * Start PAS thread
 *
 * @param is_primary_output
 * True when PAS app takes direct control of the current target,
 * false when PAS app shares control with the ADC app for current command
 */
void app_pas_start(bool is_primary_output) {
	stop_now = false;
	chThdCreateStatic(pas_thread_wa, sizeof(pas_thread_wa), NORMALPRIO, pas_thread, NULL);

	primary_output = is_primary_output;
}

bool app_pas_is_running(void) {
	return is_running;
}

void app_pas_stop(void) {
	stop_now = true;
	while (is_running) {
		chThdSleepMilliseconds(1);
	}

	if (primary_output == true) {
		mc_interface_set_current_rel(0.0);
	}
	else {
		output_current_rel = 0.0;
	}
}

float app_pas_get_current_target_rel(void) {
	return output_current_rel;
}

void pas_event_handler(void) {
#ifdef HW_PAS1_PORT
	const int8_t QEM[] = {0,-1,1,2,1,0,2,-1,-1,2,0,1,2,1,-1,0}; // Quadrature Encoder Matrix
	float direction_qem;
	uint8_t new_state;
	static uint8_t old_state = 0;
	static float old_timestamp = 0;
	static float inactivity_time = 0;
	static float period_filtered = 0;

	uint8_t PAS1_level = palReadPad(HW_PAS1_PORT, HW_PAS1_PIN);
	uint8_t PAS2_level = palReadPad(HW_PAS2_PORT, HW_PAS2_PIN);

	new_state = PAS2_level * 2 + PAS1_level;
	direction_qem = (float) QEM[old_state * 4 + new_state];
	old_state = new_state;

	const float timestamp = (float)chVTGetSystemTimeX() / (float)CH_CFG_ST_FREQUENCY;

	// sensors are poorly placed, so use only one rising edge as reference
	if(new_state == 3) {
		float period = (timestamp - old_timestamp) * (float)config.magnets;
		old_timestamp = timestamp;

		UTILS_LP_FAST(period_filtered, period, 1.0);

		if(period_filtered < min_pedal_period) { //can't be that short, abort
			return;
		}
		pedal_rpm = 60.0 / period_filtered;
		pedal_rpm *= (direction_conf * direction_qem);
		inactivity_time = 0.0;
	}
	else {
		inactivity_time += 1.0 / (float)config.update_rate_hz;

		//if no pedal activity, set RPM as zero
		if(inactivity_time > max_pulse_period) {
			pedal_rpm = 0.0;
		}
	}
#endif
}

static THD_FUNCTION(pas_thread, arg) {
	(void)arg;

	float output = 0;
	chRegSetThreadName("APP_PAS");

#ifdef HW_PAS1_PORT
	palSetPadMode(HW_PAS1_PORT, HW_PAS1_PIN, PAL_MODE_INPUT_PULLUP);
	palSetPadMode(HW_PAS2_PORT, HW_PAS2_PIN, PAL_MODE_INPUT_PULLUP);
#endif

	is_running = true;

	for(;;) {
		// Sleep for a time according to the specified rate
		systime_t sleep_time = CH_CFG_ST_FREQUENCY / config.update_rate_hz;

		// At least one tick should be slept to not block the other threads
		if (sleep_time == 0) {
			sleep_time = 1;
		}
		chThdSleep(sleep_time);

		if (stop_now) {
			is_running = false;
			return;
		}

		pas_event_handler();	// this should happen inside an ISR instead of being polled

		// For safe start when fault codes occur
		if (mc_interface_get_fault() != FAULT_CODE_NONE) {
			ms_without_power = 0;
		}

		if (app_is_output_disabled()) {
			continue;
		}

		// Map the rpm to assist level
		switch (config.ctrl_type) {
			case PAS_CTRL_TYPE_NONE:
				output = 0.0;
				break;
			case PAS_CTRL_TYPE_CADENCE:
				output = utils_map(pedal_rpm, config.pedal_rpm_start, config.pedal_rpm_end, 0.0, config.current_scaling);
				utils_truncate_number(&output, 0.0, config.current_scaling);
				break;
			default:
				break;
		}

		// Apply ramping
		static systime_t last_time = 0;
		static float output_ramp = 0.0;
		float ramp_time = fabsf(output) > fabsf(output_ramp) ? config.ramp_time_pos : config.ramp_time_neg;

		if (ramp_time > 0.01) {
			const float ramp_step = (float)ST2MS(chVTTimeElapsedSinceX(last_time)) / (ramp_time * 1000.0);
			utils_step_towards(&output_ramp, output, ramp_step);
			utils_truncate_number(&output_ramp, 0.0, config.current_scaling);

			last_time = chVTGetSystemTimeX();
			output = output_ramp;
		}

		if (output < 0.001) {
			ms_without_power += (1000.0 * (float)sleep_time) / (float)CH_CFG_ST_FREQUENCY;
		}

		// Safe start is enabled if the output has not been zero for long enough
		if (ms_without_power < MIN_MS_WITHOUT_POWER) {
			static int pulses_without_power_before = 0;
			if (ms_without_power == pulses_without_power_before) {
				ms_without_power = 0;
			}
			pulses_without_power_before = ms_without_power;
			output_current_rel = 0.0;
			continue;
		}

		// Reset timeout
		timeout_reset();

		if (primary_output == true) {
			mc_interface_set_current_rel(output);
		}
		else {
			output_current_rel = output;
		}
	}
}
