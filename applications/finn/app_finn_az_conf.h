/*
	Copyright 2020 Benjamin Vedder	benjamin@vedder.se

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

#ifndef APP_FINN_AZ_CONF_H_
#define APP_FINN_AZ_CONF_H_

#define APP_CUSTOM_TO_USE				"finn/app_finn_az.c"
#define APPCONF_APP_TO_USE				APP_CUSTOM
#define APPCONF_CAN_BAUD_RATE			CAN_BAUD_75K
#define APPCONF_CONTROLLER_ID			0

#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC
#define MCCONF_FOC_SENSOR_MODE			FOC_SENSOR_MODE_HALL
#define MCCONF_M_INVERT_DIRECTION		true

// Limits
#define MCCONF_L_RPM_MIN				-7000
#define MCCONF_L_RPM_MAX				7000
#define MCCONF_L_RPM_START				0.3
#define MCCONF_L_CURRENT_MAX			20.0
#define MCCONF_L_CURRENT_MIN			-20.0
#define MCCONF_L_IN_CURRENT_MAX			30.0
#define MCCONF_L_IN_CURRENT_MIN			-10.0

// Position PID controller
#define APP_FINN_WRAP_FACTOR			4.0 // Avoid wrap-around within the control region by only using a fraction of it
#define MCCONF_P_PID_KP					(0.2 * APP_FINN_WRAP_FACTOR)
#define MCCONF_P_PID_KI					(0.2 * APP_FINN_WRAP_FACTOR)
#define MCCONF_P_PID_KD					(0.01 * APP_FINN_WRAP_FACTOR)
#define MCCONF_P_PID_ANG_DIV			(150.0 * 7.0 * APP_FINN_WRAP_FACTOR) // 1:75 gearing and 7 pole pairs
#define MCCONF_P_PID_KD_FILTER			0.2
#define MCCONF_P_PID_GAIN_DEC_ANGLE		300.0

// FOC
#define MCCONF_FOC_CURRENT_KP			0.027
#define MCCONF_FOC_CURRENT_KI			34.0
#define MCCONF_FOC_MOTOR_L				26e-6
#define MCCONF_FOC_MOTOR_R				34e-3
#define MCCONF_FOC_MOTOR_FLUX_LINKAGE	6.495e-3
#define MCCONF_FOC_OBSERVER_GAIN		20e6

// Hall sensors
#define MCCONF_FOC_HALL_TAB_0			255
#define MCCONF_FOC_HALL_TAB_1			142
#define MCCONF_FOC_HALL_TAB_2			8
#define MCCONF_FOC_HALL_TAB_3			174
#define MCCONF_FOC_HALL_TAB_4			74
#define MCCONF_FOC_HALL_TAB_5			108
#define MCCONF_FOC_HALL_TAB_6			42
#define MCCONF_FOC_HALL_TAB_7			255
#define MCCONF_FOC_SL_ERPM				2000.0
#define MCCONF_M_HALL_EXTRA_SAMPLES		3

#define QMLUI_SOURCE_APP				"finn/finn_qml.c"
#define QMLUI_HEADER_APP				"finn/finn_qml.h"

#endif /* APP_FINN_AZ_CONF_H_ */

