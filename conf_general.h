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
 * conf_general.h
 *
 *  Created on: 14 apr 2014
 *      Author: benjamin
 */

#ifndef CONF_GENERAL_H_
#define CONF_GENERAL_H_

/*
 * Settings
 */
#define AUTO_PRINT_FAULTS		0

/*
 * Select only one hardware version
 */
#define HW_VERSION_40
//#define HW_VERSION_R2

/*
 * Select only one motor configuration
 */
//#define MCCONF_OUTRUNNER1
//#define MCCONF_RCCAR1
#define MCCONF_STEN

/*
 * Select which application to use
 */
//#define USE_APP_RCCAR
#define USE_APP_STEN
//#define USE_APP_GURGALOF

#endif /* CONF_GENERAL_H_ */
