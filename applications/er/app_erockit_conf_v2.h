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

#ifndef APP_EROCKIT_CONF_H_
#define APP_EROCKIT_CONF_H_

#define APP_CUSTOM_TO_USE					"er/app_erockit_v2.c"
#define APPCONF_APP_TO_USE					APP_CUSTOM
#define APPCONF_SHUTDOWN_MODE				SHUTDOWN_MODE_ALWAYS_ON
#define APPCONF_CHUK_CTRL_TYPE				CHUK_CTRL_TYPE_NONE

#define MCCONF_DEFAULT_MOTOR_TYPE			MOTOR_TYPE_FOC
#define MCCONF_FOC_SENSOR_MODE				FOC_SENSOR_MODE_SENSORLESS
#define MCCONF_FOC_CC_DECOUPLING			FOC_CC_DECOUPLING_DISABLED
#define MCCONF_FOC_OPENLOOP_RPM				100.0
#define MCCONF_FOC_TEMP_COMP				true
#define MCCONF_FOC_TEMP_COMP_BASE_TEMP		21.0
#define MCCONF_FOC_MOTOR_L					12e-6
#define MCCONF_FOC_MOTOR_R					13.3e-3
#define MCCONF_FOC_MOTOR_FLUX_LINKAGE		5.61e-3
#define MCCONF_FOC_OBSERVER_GAIN			35e6
#define MCCONF_L_CURRENT_MAX				50.0
#define MCCONF_L_CURRENT_MIN				-50.0
#define MCCONF_L_IN_CURRENT_MAX				10.0
#define MCCONF_L_IN_CURRENT_MIN				-10.0
#define MCCONF_FOC_CURRENT_KP				0.025
#define MCCONF_FOC_CURRENT_KI				27.0
#define MCCONF_FOC_SL_ERPM					2000.0
#define MCCONF_M_INVERT_DIRECTION			false
#define MCCONF_FOC_PHASE_FILTER_MAX_ERPM	6000.0

//#define MCCONF_S_PID_KP						0.01
//#define MCCONF_S_PID_KI						0.1
//#define MCCONF_S_PID_KD						0.00015
//#define MCCONF_S_PID_KD_FILTER				0.5
#define MCCONF_S_PID_KP						0.002
#define MCCONF_S_PID_KI						0.03
#define MCCONF_S_PID_KD						0.00015
#define MCCONF_S_PID_KD_FILTER				0.05
#define MCCONF_S_PID_MIN_RPM				100.0

#define QMLUI_SOURCE_APP					"er/er_qml.c"
#define QMLUI_HEADER_APP					"er/er_qml.h"

#endif /* APP_EROCKIT_CONF_H_ */

