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
 * app.h
 *
 *  Created on: 18 apr 2014
 *      Author: benjamin
 */

#ifndef APP_H_
#define APP_H_

#include "conf_general.h"

// Functions
void app_init(void);

// The init functions for all applications
void app_rccar_init(void);
void app_sten_init(void);
void app_gurgalof_init(void);

#endif /* APP_H_ */
