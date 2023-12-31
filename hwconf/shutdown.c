/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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

#include "shutdown.h"
#include "app.h"
#include "conf_general.h"
#include "mc_interface.h"

#ifdef USE_LISPBM
#include "lispif.h"
#endif

#ifdef HW_SHUTDOWN_HOLD_ON

// Private variables
bool volatile m_button_pressed = false;
static volatile float m_inactivity_time = 0.0;
static THD_WORKING_AREA(shutdown_thread_wa, 128);
static mutex_t m_sample_mutex;
static volatile bool m_init_done = false;
static volatile bool m_sampling_disabled = false;
static volatile bool m_shutdown_hold = false;

// Private functions
static THD_FUNCTION(shutdown_thread, arg);

void shutdown_init(void) {
	chMtxObjectInit(&m_sample_mutex);
	chThdCreateStatic(shutdown_thread_wa, sizeof(shutdown_thread_wa), NORMALPRIO, shutdown_thread, NULL);
	m_init_done = true;
}

void shutdown_reset_timer(void) {
	m_inactivity_time = 0.0;
}

bool shutdown_button_pressed(void) {
	return m_button_pressed;
}

float shutdown_get_inactivity_time(void) {
	return m_inactivity_time;
}

void shutdown_set_sampling_disabled(bool disabled) {
	if (!m_init_done) {
		return;
	}

	chMtxLock(&m_sample_mutex);
	m_sampling_disabled = disabled;
	chMtxUnlock(&m_sample_mutex);
}

void shutdown_hold(bool hold) {
	m_shutdown_hold = hold;
}

bool do_shutdown(bool resample) {
	conf_general_store_backup_data();
#ifdef USE_LISPBM
	lispif_process_shutdown();
#endif
	chThdSleepMilliseconds(100);

	while (m_shutdown_hold) {
		chThdSleepMilliseconds(5);
	}

	bool disable_gates = true;
	if (resample) {
		chMtxLock(&m_sample_mutex);
		if (!m_sampling_disabled) {
			disable_gates = HW_SAMPLE_SHUTDOWN();
		}
		chMtxUnlock(&m_sample_mutex);
	}
	if (disable_gates) {
		DISABLE_GATE();
		HW_SHUTDOWN_HOLD_OFF();
	}
	return disable_gates;
}

static THD_FUNCTION(shutdown_thread, arg) {
	(void)arg;

	chRegSetThreadName("Shutdown");

	bool gates_disabled_here = false;
	float gate_disable_time = 0.0;
	systime_t last_iteration_time = chVTGetSystemTimeX();
	uint64_t odometer_old = mc_interface_get_odometer();

	for(;;) {
		float dt = (float)chVTTimeElapsedSinceX(last_iteration_time) / (float)CH_CFG_ST_FREQUENCY;
		last_iteration_time = chVTGetSystemTimeX();

		chMtxLock(&m_sample_mutex);

		if (m_sampling_disabled) {
			chMtxUnlock(&m_sample_mutex);
			chThdSleepMilliseconds(10);
			continue;
		}

		bool sample = HW_SAMPLE_SHUTDOWN();
		chMtxUnlock(&m_sample_mutex);
		bool clicked = m_button_pressed && !sample;
		m_button_pressed = sample;

		const app_configuration *conf = app_get_configuration();

		// Note: When the gates are enabled, the push to start function
		// will prevent the regulator from shutting down. Therefore, the
		// gate driver has to be disabled.

		switch (conf->shutdown_mode) {
		case SHUTDOWN_MODE_ALWAYS_OFF:
			if (m_button_pressed) {
				gates_disabled_here = do_shutdown(true);
			}
			break;

		case SHUTDOWN_MODE_ALWAYS_ON:
			m_inactivity_time += dt;
			HW_SHUTDOWN_HOLD_ON();
			// Without a shutdown switch use inactivity timer to estimate
			// when device is stopped. Check also distance between store
			// to prevent excessive flash write cycles.
			if (m_inactivity_time >= SHUTDOWN_SAVE_BACKUPDATA_TIMEOUT) {
				shutdown_reset_timer();
				// If at least 1km was done then we can store data 
				if((mc_interface_get_odometer()-odometer_old) >= 1000) {
					conf_general_store_backup_data();
					odometer_old = mc_interface_get_odometer();
				}
			}
			break;

		default:
			if (clicked) {
				gates_disabled_here = do_shutdown(false);
			}
			break;
		}

		// If disabling the gates did not shut the VESC down within
		// 2 seconds, enable the gates again.
		if (gates_disabled_here && m_button_pressed) {
			gate_disable_time += dt;

			if (gate_disable_time > 2.0) {
				ENABLE_GATE();
				gates_disabled_here = false;
				gate_disable_time = 0.0;
			}
		}

		if (conf->shutdown_mode >= SHUTDOWN_MODE_OFF_AFTER_10S) {
			m_inactivity_time += dt;

			float shutdown_timeout = 0.0;
			switch (conf->shutdown_mode) {
			case SHUTDOWN_MODE_OFF_AFTER_10S: shutdown_timeout = 10.0; break;
			case SHUTDOWN_MODE_OFF_AFTER_1M: shutdown_timeout = 60.0; break;
			case SHUTDOWN_MODE_OFF_AFTER_5M: shutdown_timeout = 60.0 * 5.0; break;
			case SHUTDOWN_MODE_OFF_AFTER_10M: shutdown_timeout = 60.0 * 10.0; break;
			case SHUTDOWN_MODE_OFF_AFTER_30M: shutdown_timeout = 60.0 * 30.0; break;
			case SHUTDOWN_MODE_OFF_AFTER_1H: shutdown_timeout = 60.0 * 60.0; break;
			case SHUTDOWN_MODE_OFF_AFTER_5H: shutdown_timeout = 60.0 * 60.0 * 5.0; break;
			default: break;
			}

			if (m_inactivity_time >= shutdown_timeout && m_button_pressed) {
				gates_disabled_here = do_shutdown(false);
			}
		}

		chThdSleepMilliseconds(10);
	}
}

#else // HARDWARE WITHOUT POWER SWITCH 
// just saving backup data, no actual shutdown

// Private variables
static volatile float m_inactivity_time = 0.0;
static THD_WORKING_AREA(shutdown_thread_wa, 128);

// Private functions
static THD_FUNCTION(shutdown_thread, arg);

void shutdown_init(void) {
	chThdCreateStatic(shutdown_thread_wa, sizeof(shutdown_thread_wa), LOWPRIO, shutdown_thread, NULL);
}

void shutdown_reset_timer(void) {
	m_inactivity_time = 0.0;
}

float shutdown_get_inactivity_time(void) {
	return m_inactivity_time;
}

void shutdown_hold(bool hold) {
	(void)hold;
}

bool do_shutdown(bool resample) {
	(void)resample;
	return false;
}

static THD_FUNCTION(shutdown_thread, arg) {
	(void)arg;

	chRegSetThreadName("Shutdown");

	systime_t last_iteration_time = chVTGetSystemTimeX();
	uint64_t odometer_old = mc_interface_get_odometer();

	for(;;) {
		float dt = (float)chVTTimeElapsedSinceX(last_iteration_time) / (float)CH_CFG_ST_FREQUENCY;
		last_iteration_time = chVTGetSystemTimeX();

		const app_configuration *conf = app_get_configuration();

		//if set to always off don't store backup
		if(conf->shutdown_mode != SHUTDOWN_MODE_ALWAYS_OFF) {
			m_inactivity_time += dt;
			if (m_inactivity_time >= SHUTDOWN_SAVE_BACKUPDATA_TIMEOUT) {
				shutdown_reset_timer();
				// Without a shutdown switch use inactivity time to measure
				// when stopped. If timeout is passed and trip distance is
				// greater than 1km store it to prevent excessive flash write.
				// Example, i stop for 4 miutes after 3,4km and the firmware
				// stores parameters, if i stop for 20s or after 600m no.
				if((mc_interface_get_odometer()-odometer_old) >= 1000) {
					conf_general_store_backup_data();
					odometer_old = mc_interface_get_odometer();
				}
			}
		}

		chThdSleepMilliseconds(1000);
	}
}
#endif
