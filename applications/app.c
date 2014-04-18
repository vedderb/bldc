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
#include "stm32f4xx_conf.h"
#include "servo.h"

void app_init(void) {
#ifdef USE_APP_RCCAR
	app_rccar_init();
#endif
#ifdef USE_APP_STEN
	app_sten_init();
#endif
#ifdef USE_APP_GURGALOF
	app_gurgalof_init();
#endif
}
