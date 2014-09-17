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
 * app.c
 *
 *  Created on: 18 apr 2014
 *      Author: benjamin
 */

#include "app.h"
#include "ch.h"
#include "hal.h"

// Private variables
static app_configuration app_conf;

void app_init(app_configuration *conf) {
	app_conf = *conf;

	switch (app_conf.app_to_use) {
	case APP_PPM:
		app_ppm_configure(app_conf.app_ppm_ctrl_type, app_conf.app_ppm_pid_max_erpm, app_conf.app_ppm_use_rev);
		app_ppm_start();
		break;

	case APP_UARTCOMM:
		app_uartcomm_init();
		break;

	case APP_CUSTOM:
#ifdef USE_APP_STEN
		app_sten_init();
#endif
#ifdef USE_APP_GURGALOF
		app_gurgalof_init();
#endif
		break;

	default:
		break;
	}
}

const app_configuration* app_get_configuration(void) {
	return &app_conf;
}

/**
 * Reconfigure all apps. Note that this will not start apps that are not already running, that
 * should be done at boot. Some apps don't have any configuration options.
 *
 * @param conf
 * The new configuration to use.
 */
void app_set_configuration(app_configuration *conf) {
	app_conf = *conf;
	app_ppm_configure(app_conf.app_ppm_ctrl_type, app_conf.app_ppm_pid_max_erpm, app_conf.app_ppm_use_rev);
}
