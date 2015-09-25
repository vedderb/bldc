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
 * mcconf_outrunner2.h
 *
 * A configuration for my scorpion 225kv outrunner.
 *
 *  Created on: 14 apr 2014
 *      Author: benjamin
 */

#ifndef MCCONF_OUTRUNNER2_H_
#define MCCONF_OUTRUNNER2_H_

/*
 * Parameters
 */
#define MCCONF_L_CURRENT_MAX				60.0	// Current limit in Amperes (Upper)
#define MCCONF_L_CURRENT_MIN				-60.0	// Current limit in Amperes (Lower)
#define MCCONF_L_IN_CURRENT_MAX			60.0	// Input current limit in Amperes (Upper)
#define MCCONF_L_IN_CURRENT_MIN			-20.0	// Input current limit in Amperes (Lower)
#define MCCONF_L_MAX_ABS_CURRENT			130.0	// The maximum absolute current above which a fault is generated
#define MCCONF_L_SLOW_ABS_OVERCURRENT		1		// Use the filtered (and hence slower) current for the overcurrent fault detection

// Sensorless settings
#define MCCONF_SENSOR_MODE				SENSOR_MODE_SENSORLESS // Sensor mode
#define MCCONF_SL_MIN_RPM					150		// Auto-commutate below this RPM
#define MCCONF_SL_MIN_ERPM_CYCLE_INT_LIMIT	1100.0	// Minimum RPM to calculate the BEMF coupling from
#define MCCONF_SL_CYCLE_INT_LIMIT			62.0	// Flux integrator limit 0 ERPM
#define MCCONF_SL_PHASE_ADVANCE_AT_BR	0.8		// Flux integrator limit percentage at MCPWM_CYCLE_INT_START_RPM_BR ERPM
#define MCCONF_SL_BEMF_COUPLING_K		600.0	// Input voltage to bemf coupling constant

// Speed PID parameters
#define MCCONF_S_PID_KP					0.0001	// Proportional gain
#define MCCONF_S_PID_KI					0.002	// Integral gain
#define MCCONF_S_PID_KD					0.0		// Derivative gain
#define MCCONF_S_PID_MIN_RPM				900.0	// Minimum allowed RPM

// Position PID parameters
#define MCCONF_P_PID_KP					0.0001	// Proportional gain
#define MCCONF_P_PID_KI					0.002	// Integral gain
#define MCCONF_P_PID_KD					0.0		// Derivative gain

// Current control parameters
#define MCCONF_CC_GAIN		0.0046	// Current controller error gain
#define MCCONF_CC_MIN_CURRENT		1.0		// Minimum allowed current

#endif /* MCCONF_OUTRUNNER2_H_ */
