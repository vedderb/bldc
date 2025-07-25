/*
	Copyright 2022 Benjamin Vedder	benjamin@vedder.se

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

#ifndef HW_JetFleetF6_20s_H_

#define HW_JetFleetF6_20s_H_

#define HW_NAME					"JetFleetF6_20s"
#define HW_JetFleetF6_20s

#define VIN_R2				            4700.0
#define MCCONF_L_MAX_VOLTAGE			90.0	// Maximum input voltage
#define MCCONF_L_MAX_ABS_CURRENT		260.0	// The maximum absolute current above which a fault is generated
#define HW_LIM_CURRENT			        -260.0, 260.0
#define HW_LIM_CURRENT_ABS		        0.0, 320.0
#define HW_LIM_VIN			            18.0, 90.0


#include "hw_JetFleetF6_core.h"

#endif /* HW_JetFleetF6_20s_H_ */
