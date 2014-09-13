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
 * mcconf_rccar2.h
 *
 * This configuration is for my crappy coreless inrunner. Since this motor is coreless,
 * the coupling constant is extremeley low compared to the other motors.
 *
 *  Created on: 14 apr 2014
 *      Author: benjamin
 */

#ifndef MCCONF_RCCAR2_H_
#define MCCONF_RCCAR2_H_

/*
 * Parameters
 */
#define MCPWM_CURRENT_MAX				80.0	// Current limit in Amperes (Upper)
#define MCPWM_CURRENT_MIN				-80.0	// Current limit in Amperes (Lower)
#define MCPWM_IN_CURRENT_MAX			80.0	// Input current limit in Amperes (Upper)
#define MCPWM_IN_CURRENT_MIN			-25.0	// Input current limit in Amperes (Lower)
#define MCPWM_MAX_ABS_CURRENT			110.0	// The maximum absolute current above which a fault is generated
#define MCPWM_SLOW_ABS_OVERCURRENT		1		// Use the filtered (and hence slower) current for the overcurrent fault detection
#define MCPWM_CURRENT_STARTUP_BOOST		0.08	// The lowest duty cycle to use in current control mode (has to be > MCPWM_MIN_DUTY_CYCLE)
#define MCPWM_RPM_MAX					100000.0	// The motor speed limit (Upper)
#define MCPWM_RPM_MIN					-100000.0	// The motor speed limit (Lower)

// Sensorless settings
#define MCPWM_IS_SENSORLESS				1		// Use sensorless commutation
#define MCPWM_MIN_RPM					500		// Auto-commutate below this RPM
#define MCPWM_CYCLE_INT_LIMIT_MIN_RPM	800.0	// Minimum RPM to calculate the BEMF coupling from
#define MCPWM_CYCLE_INT_LIMIT			40.0	// Flux integrator limit 0 ERPM
#define MCPWM_CYCLE_INT_LIMIT_HIGH_FAC	0.8		// Flux integrator limit percentage at MCPWM_CYCLE_INT_START_RPM_BR ERPM
#define MCPWM_BEMF_INPUT_COUPLING_K		60.0	// Input voltage to bemf coupling constant

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
#define MCPWM_CURRENT_CONTROL_GAIN		0.0046	// Current controller error gain
#define MCPWM_CURRENT_CONTROL_MIN		1.0		// Minimum allowed current

#endif /* MCCONF_RCCAR2_H_ */
