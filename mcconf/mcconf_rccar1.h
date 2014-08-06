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
 * mcconf_rccar1.h
 *
 *  Created on: 14 apr 2014
 *      Author: benjamin
 */

#ifndef MCCONF_RCCAR1_H_
#define MCCONF_RCCAR1_H_

/*
 * Parameters
 */
#define MCPWM_CURRENT_MAX				80.0	// Current limit in Amperes (Upper)
#define MCPWM_CURRENT_MIN				-80.0	// Current limit in Amperes (Lower)
#define MCPWM_IN_CURRENT_MAX			80.0	// Input current limit in Amperes (Upper)
#define MCPWM_IN_CURRENT_MIN			-25.0	// Input current limit in Amperes (Lower)
#define MCPWM_MAX_ABS_CURRENT			140.0	// The maximum absolute current above which a fault is generated
#define MCPWM_CURRENT_STARTUP_BOOST		0.08	// The lowest duty cycle to use in current control mode @ 20V.
#define MCPWM_CURRENT_CONTROL_NO_REV	0		// Do not reverse the direction in current control mode, brake only
#define MCPWM_RPM_MAX					100000.0	// The motor speed limit (Upper)
#define MCPWM_RPM_MIN					-100000.0	// The motor speed limit (Lower)

// Sensorless settings
#define MCPWM_IS_SENSORLESS				1		// Use sensorless commutation
#define MCPWM_MIN_RPM					300		// Auto-commutate below this RPM
#define MCPWM_CYCLE_INT_START_RPM_BR	5000.0	// RPM border between the START and LOW interval
#define MCPWM_CYCLE_INT_LIMIT_START		600.0	// Flux integrator limit 0 ERPM
#define MCPWM_CYCLE_INT_LIMIT_LOW		50.0	// Flux integrator limit MCPWM_CYCLE_INT_START_RPM_BR ERPM
#define MCPWM_CYCLE_INT_LIMIT_HIGH		10.0	// Flux integrator limit 80K ERPM

// Hall sensor settings
#define MCPWM_HALL_DIR					0		// Hall sensor direction [0 or 1]
#define MCPWM_HALL_FWD_ADD				2		// Hall sensor offset fwd [0 to 5]
#define MCPWM_HALL_REV_ADD				3		// Hall sensor offset rev [0 to 5]

// Speed PID parameters
#define MCPWM_PID_KP					0.0001	// Proportional gain
#define MCPWM_PID_KI					0.002	// Integral gain
#define MCPWM_PID_KD					0.0		// Derivative gain
#define MCPWM_PID_MIN_RPM				950.0	// Minimum allowed RPM

// Current control parameters
#define MCPWM_CURRENT_CONTROL_GAIN		0.0036	// Current controller error gain
#define MCPWM_CURRENT_CONTROL_MIN		1.0		// Minimum allowed current

#endif /* MCCONF_RCCAR1_H_ */
