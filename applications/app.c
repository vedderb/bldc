/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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
#include "nrf_driver.h"
#include "rfhelp.h"
#include "comm_can.h"
#include "imu.h"
#include "crc.h"
#include "servo_simple.h"
#include "servo_dec.h"

// Private variables
static app_configuration appconf = {0};
static virtual_timer_t output_vt = {0};
static bool output_vt_init_done = false;
static volatile bool output_disabled_now = false;

// Private functions
static void output_vt_cb(void *arg);

const app_configuration* app_get_configuration(void) {
	return &appconf;
}

/**
 * Reconfigure and restart all apps. Some apps don't have any configuration options.
 *
 * @param conf
 * The new configuration to use.
 */
void app_set_configuration(app_configuration *conf) {
	bool app_changed = appconf.app_to_use != conf->app_to_use;

	if (!app_changed) {
		app_changed = appconf.servo_out_enable != conf->servo_out_enable;
	}

	appconf = *conf;

	if (app_changed) {
		app_ppm_stop();
		app_adc_stop();
		app_uartcomm_stop(UART_PORT_COMM_HEADER);
		app_nunchuk_stop();
		app_pas_stop();

#ifdef APP_CUSTOM_TO_USE
		app_custom_stop();
#endif
	}

	if (!conf_general_permanent_nrf_found) {
		nrf_driver_stop();
	}

#if CAN_ENABLE
	comm_can_set_baud(conf->can_baud_rate);
#endif

	imu_init(&conf->imu_conf);

	if (app_changed) {
		if (appconf.app_to_use != APP_PPM &&
				appconf.app_to_use != APP_PPM_UART &&
				appconf.servo_out_enable) {
			servodec_stop();
			servo_simple_init();
		} else {
			servo_simple_stop();
		}

		switch (appconf.app_to_use) {
		case APP_PPM:
			app_ppm_start();
			break;

		case APP_ADC:
			app_adc_start(true);
			break;

		case APP_UART:
			hw_stop_i2c();
			app_uartcomm_start(UART_PORT_COMM_HEADER);
			break;

		case APP_PPM_UART:
			hw_stop_i2c();
			app_ppm_start();
			app_uartcomm_start(UART_PORT_COMM_HEADER);
			break;

		case APP_ADC_UART:
			hw_stop_i2c();
			app_adc_start(false);
			app_uartcomm_start(UART_PORT_COMM_HEADER);
			break;

		case APP_NUNCHUK:
			app_nunchuk_start();
			break;

		case APP_PAS:
			app_pas_start(true);
			break;

		case APP_ADC_PAS:
			app_adc_start(false);
			app_pas_start(false);
			break;

		case APP_NRF:
			if (!conf_general_permanent_nrf_found) {
				nrf_driver_init();
				rfhelp_restart();
			}
			break;

		case APP_CUSTOM:
#ifdef APP_CUSTOM_TO_USE
			hw_stop_i2c();
			app_custom_start();
#endif
			break;

		default:
			break;
		}
	}

	app_ppm_configure(&appconf.app_ppm_conf);
	app_adc_configure(&appconf.app_adc_conf);
	app_pas_configure(&appconf.app_pas_conf);
	app_uartcomm_configure(appconf.app_uart_baudrate, true, UART_PORT_COMM_HEADER);
	app_uartcomm_configure(0, appconf.permanent_uart_enabled, UART_PORT_BUILTIN);
	app_nunchuk_configure(&appconf.app_chuk_conf);

#ifdef APP_CUSTOM_TO_USE
	app_custom_configure(&appconf);
#endif

	rfhelp_update_conf(&appconf.app_nrf_conf);
}

/**
 * Disable output on apps
 *
 * @param time_ms
 * The amount of time to disable output in ms
 * 0: Enable output now
 * -1: Disable forever
 * >0: Amount of milliseconds to disable output
 */
void app_disable_output(int time_ms) {
	if (!output_vt_init_done) {
		chVTObjectInit(&output_vt);
		output_vt_init_done = true;
	}

	if (time_ms == 0) {
		output_disabled_now = false;
	} else if (time_ms == -1) {
		output_disabled_now = true;
		chVTReset(&output_vt);
	} else {
		output_disabled_now = true;
		chVTSet(&output_vt, MS2ST(time_ms), output_vt_cb, 0);
	}
}

bool app_is_output_disabled(void) {
	return output_disabled_now;
}

static void output_vt_cb(void *arg) {
	(void)arg;
	output_disabled_now = false;
}

/**
 * Get app_configuration CRC
 *
 * @param conf
 * Pointer to app_configuration or NULL for current appconf
 *
 * @return
 * CRC16 (with crc field in struct temporarily set to zero).
 */
unsigned short app_calc_crc(app_configuration* conf) {
	if (NULL == conf) {
		conf = &appconf;
	}

	unsigned short crc_old = conf->crc;
	conf->crc = 0;
	unsigned short crc_new = crc16((uint8_t*)conf, sizeof(app_configuration));
	conf->crc = crc_old;
	return crc_new;
}
