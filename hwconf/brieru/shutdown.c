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

static bool do_shutdown(void) {
	//save settings only if we have done at least 1km to save writing cycles
	if(mc_interface_get_distance_abs() > 1000) {
		conf_general_store_backup_data();
	}
#ifdef USE_LISPBM
	lispif_process_shutdown();
#endif
	chThdSleepMilliseconds(100);
	DISABLE_GATE();
	HW_SHUTDOWN_HOLD_OFF();
	return true;
}

static THD_FUNCTION(shutdown_thread, arg) {
	(void)arg;

	chRegSetThreadName("Shutdown");

	systime_t last_iteration_time = chVTGetSystemTimeX();

	for(;;) {
		float dt = (float)chVTTimeElapsedSinceX(last_iteration_time) / (float)CH_CFG_ST_FREQUENCY;
		last_iteration_time = chVTGetSystemTimeX();

		
		if (m_sampling_disabled) {
			chThdSleepMilliseconds(10);
			continue;
		}

		const app_configuration *conf = app_get_configuration();

		if(conf->shutdown_mode == SHUTDOWN_MODE_ALWAYS_ON) {
			m_inactivity_time += dt;
			HW_SHUTDOWN_HOLD_ON();
			if (m_inactivity_time >= SHUTDOWN_SAVE_BACKUPDATA_TIMEOUT) {
				shutdown_reset_timer();
				//if at least 1km was done then we can store data 
				if(mc_interface_get_distance_abs() > 1000) {
					conf_general_store_backup_data();
				}
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

			if (m_inactivity_time >= shutdown_timeout) {
				do_shutdown();
				chThdSleepMilliseconds(500);
				shutdown_reset_timer();
				HW_SHUTDOWN_HOLD_ON();
				//reset timer if esc didn't shut down
				//example if a switch is connected instead of a button
			}
		} else {
			m_inactivity_time = 0.0;
		}

		chThdSleepMilliseconds(10);
	}
}

#endif
