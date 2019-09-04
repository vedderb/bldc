/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#ifndef APPCONF_APPCONF_EXAMPLE_PPM_H_
#define APPCONF_APPCONF_EXAMPLE_PPM_H_

// Override app configuration parameters such that the PPM app becomes default
#define APPCONF_APP_TO_USE				APP_PPM
#define APPCONF_PPM_CTRL_TYPE			PPM_CTRL_TYPE_CURRENT_NOREV_BRAKE

#endif /* APPCONF_APPCONF_EXAMPLE_PPM_H_ */
