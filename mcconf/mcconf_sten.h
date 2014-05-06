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
 * mcconf_sten.h
 *
 *  Created on: 14 apr 2014
 *      Author: benjamin
 */

#ifndef MCCONF_STEN_H_
#define MCCONF_STEN_H_

/*
 * Parameters
 */
#define MCPWM_CURRENT_MAX				40.0	// Current limit in Amperes (Upper)
#define MCPWM_CURRENT_MIN				-40.0	// Current limit in Amperes (Lower)
#define MCPWM_IN_CURRENT_MAX			25.0	// Input current limit in Amperes (Upper)
#define MCPWM_IN_CURRENT_MIN			-25.0	// Input current limit in Amperes (Lower)
#define MCPWM_RPM_MAX					29000.0	// The motor speed limit (Upper)
#define MCPWM_RPM_MIN					-10.0	// The motor speed limit (Lower)
#define MCPWM_MIN_VOLTAGE				34.0	// Minimum input voltage
#define MCPWM_MAX_VOLTAGE				45.0	// Maximum input voltage

// Sensorless settings
#define MCPWM_IS_SENSORLESS				1		// Use sensorless commutation
#define MCPWM_MIN_RPM					300		// Auto-commutate below this RPM
#define MCPWM_CYCLE_INT_LIMIT_LOW		250.0	// Flux integrator limit 0 ERPM
#define MCPWM_CYCLE_INT_LIMIT_HIGH		30.0	// Flux integrator limit 50K ERPM

// Speed PID parameters
#define MCPWM_PID_KP					0.0001	// Proportional gain
#define MCPWM_PID_KI					0.002	// Integral gain
#define MCPWM_PID_KD					0.0		// Derivative gain
#define MCPWM_PID_MIN_RPM				1200.0	// Minimum allowed RPM

// Current control parameters
#define MCPWM_CURRENT_CONTROL_GAIN		0.0016	// Current controller error gain
#define MCPWM_CURRENT_CONTROL_MIN		1.0		// Minimum allowed current

#endif /* MCCONF_STEN_H_ */
