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

#ifndef MCCONF_DEFAULT_H_
#define MCCONF_DEFAULT_H_

// Default settings
#ifndef MCCONF_DEFAULT_MOTOR_TYPE
#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_BLDC
#endif
#ifndef MCCONF_PWM_MODE
#define MCCONF_PWM_MODE					PWM_MODE_SYNCHRONOUS // Default PWM mode
#endif
#ifndef MCCONF_SENSOR_MODE
#define MCCONF_SENSOR_MODE				SENSOR_MODE_SENSORLESS // Sensor mode
#endif
#ifndef MCCONF_COMM_MODE
#define MCCONF_COMM_MODE				COMM_MODE_INTEGRATE	// The commutation mode to use
#endif

// Limits
#ifndef MCCONF_L_CURRENT_MAX
#define MCCONF_L_CURRENT_MAX			60.0	// Current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_CURRENT_MIN
#define MCCONF_L_CURRENT_MIN			-60.0	// Current limit in Amperes (Lower)
#endif
#ifndef MCCONF_L_IN_CURRENT_MAX
#define MCCONF_L_IN_CURRENT_MAX			60.0	// Input current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_IN_CURRENT_MIN
#define MCCONF_L_IN_CURRENT_MIN			-40.0	// Input current limit in Amperes (Lower)
#endif
#ifndef MCCONF_L_MAX_ABS_CURRENT
#define MCCONF_L_MAX_ABS_CURRENT		130.0	// The maximum absolute current above which a fault is generated
#endif
#ifndef MCCONF_L_MIN_VOLTAGE
#define MCCONF_L_MIN_VOLTAGE			8.0		// Minimum input voltage
#endif
#ifndef MCCONF_L_MAX_VOLTAGE
#define MCCONF_L_MAX_VOLTAGE			57.0	// Maximum input voltage
#endif
#ifndef MCCONF_L_BATTERY_CUT_START
#define MCCONF_L_BATTERY_CUT_START		10.0	// Start limiting the positive current at this voltage
#endif
#ifndef MCCONF_L_BATTERY_CUT_END
#define MCCONF_L_BATTERY_CUT_END		8.0		// Limit the positive current completely at this voltage
#endif
#ifndef MCCONF_L_RPM_MAX
#define MCCONF_L_RPM_MAX				100000.0	// The motor speed limit (Upper)
#endif
#ifndef MCCONF_L_RPM_MIN
#define MCCONF_L_RPM_MIN				-100000.0	// The motor speed limit (Lower)
#endif
#ifndef MCCONF_L_RPM_START
#define MCCONF_L_RPM_START				0.8		// Fraction of full speed where RPM current limiting starts
#endif
#ifndef MCCONF_L_SLOW_ABS_OVERCURRENT
#define MCCONF_L_SLOW_ABS_OVERCURRENT	true	// Use the filtered (and hence slower) current for the overcurrent fault detection
#endif
#ifndef MCCONF_L_MIN_DUTY
#define MCCONF_L_MIN_DUTY				0.005	// Minimum duty cycle
#endif
#ifndef MCCONF_L_MAX_DUTY
#define MCCONF_L_MAX_DUTY				0.95	// Maximum duty cycle
#endif
#ifndef MCCONF_L_CURR_MAX_RPM_FBRAKE
#define MCCONF_L_CURR_MAX_RPM_FBRAKE	300		// Maximum electrical RPM to use full brake at
#endif
#ifndef MCCONF_L_CURR_MAX_RPM_FBRAKE_CC
#define MCCONF_L_CURR_MAX_RPM_FBRAKE_CC	1500	// Maximum electrical RPM to use full brake at with current control
#endif
#ifndef MCCONF_L_LIM_TEMP_FET_START
#define MCCONF_L_LIM_TEMP_FET_START		85.0	// MOSFET temperature where current limiting should begin
#endif
#ifndef MCCONF_L_LIM_TEMP_FET_END
#define MCCONF_L_LIM_TEMP_FET_END		100.0	// MOSFET temperature where everything should be shut off
#endif
#ifndef MCCONF_L_LIM_TEMP_MOTOR_START
#define MCCONF_L_LIM_TEMP_MOTOR_START	85.0	// MOTOR temperature where current limiting should begin
#endif
#ifndef MCCONF_L_LIM_TEMP_MOTOR_END
#define MCCONF_L_LIM_TEMP_MOTOR_END		100.0	// MOTOR temperature where everything should be shut off
#endif
#ifndef MCCONF_L_LIM_TEMP_ACCEL_DEC
#define MCCONF_L_LIM_TEMP_ACCEL_DEC		0.15	// Decrease temperature limits this much during acceleration
#endif
#ifndef MCCONF_L_WATT_MAX
#define MCCONF_L_WATT_MAX				15000.0	// Maximum wattage output
#endif
#ifndef MCCONF_L_WATT_MIN
#define MCCONF_L_WATT_MIN				-15000.0	// Minimum wattage output (braking)
#endif

// Speed PID parameters
#ifndef MCCONF_S_PID_KP
#define MCCONF_S_PID_KP					0.004	// Proportional gain
#endif
#ifndef MCCONF_S_PID_KI
#define MCCONF_S_PID_KI					0.004	// Integral gain
#endif
#ifndef MCCONF_S_PID_KD
#define MCCONF_S_PID_KD					0.0001	// Derivative gain
#endif
#ifndef MCCONF_S_PID_MIN_RPM
#define MCCONF_S_PID_MIN_RPM			900.0	// Minimum allowed RPM
#endif
#ifndef MCCONF_S_PID_ALLOW_BRAKING
#define MCCONF_S_PID_ALLOW_BRAKING		true	// Allow braking in speed control mode
#endif

// Position PID parameters
#ifndef MCCONF_P_PID_KP
#define MCCONF_P_PID_KP					0.03	// Proportional gain
#endif
#ifndef MCCONF_P_PID_KI
#define MCCONF_P_PID_KI					0.0		// Integral gain
#endif
#ifndef MCCONF_P_PID_KD
#define MCCONF_P_PID_KD					0.0004	// Derivative gain
#endif
#ifndef MCCONF_P_PID_ANG_DIV
#define MCCONF_P_PID_ANG_DIV			1.0		// Divide angle by this value
#endif

// Current control parameters
#ifndef MCCONF_CC_GAIN
#define MCCONF_CC_GAIN					0.0046	// Current controller error gain
#endif
#ifndef MCCONF_CC_MIN_CURRENT
#define MCCONF_CC_MIN_CURRENT			0.1		// Minimum allowed current
#endif
#ifndef MCCONF_CC_STARTUP_BOOST_DUTY
#define MCCONF_CC_STARTUP_BOOST_DUTY	0.01	// The lowest duty cycle to use in current control mode (has to be > MCPWM_MIN_DUTY_CYCLE)
#endif
#ifndef MCCONF_CC_RAMP_STEP
#define MCCONF_CC_RAMP_STEP				0.04	// Maximum duty cycle ramping step in CC mode
#endif

// BLDC
#ifndef MCCONF_SL_MIN_RPM
#define MCCONF_SL_MIN_RPM				150		// Auto-commutate below this RPM
#endif
#ifndef MCCONF_SL_MIN_ERPM_CYCLE_INT_LIMIT
#define MCCONF_SL_MIN_ERPM_CYCLE_INT_LIMIT	1100.0	// Minimum RPM to calculate the BEMF coupling from
#endif
#ifndef MCCONF_SL_CYCLE_INT_LIMIT
#define MCCONF_SL_CYCLE_INT_LIMIT		62.0	// Flux integrator limit 0 ERPM
#endif
#ifndef MCCONF_SL_BEMF_COUPLING_K
#define MCCONF_SL_BEMF_COUPLING_K		600.0	// Input voltage to bemf coupling constant
#endif
#ifndef MCCONF_SL_PHASE_ADVANCE_AT_BR
#define MCCONF_SL_PHASE_ADVANCE_AT_BR	0.8		// Flux integrator limit percentage at MCPWM_CYCLE_INT_START_RPM_BR ERPM
#endif
#ifndef MCCONF_SL_CYCLE_INT_BR
#define MCCONF_SL_CYCLE_INT_BR			80000.0	// RPM border between the START and LOW interval
#endif
#ifndef MCCONF_SL_MAX_FB_CURR_DIR_CHANGE
#define MCCONF_SL_MAX_FB_CURR_DIR_CHANGE	10.0	// Maximum current during full brake during which a direction change is allowed
#endif

// BLDC hall sensor table
#ifndef MCCONF_HALL_TAB_0
#define MCCONF_HALL_TAB_0				-1
#endif
#ifndef MCCONF_HALL_TAB_1
#define MCCONF_HALL_TAB_1				1
#endif
#ifndef MCCONF_HALL_TAB_2
#define MCCONF_HALL_TAB_2				3
#endif
#ifndef MCCONF_HALL_TAB_3
#define MCCONF_HALL_TAB_3				2
#endif
#ifndef MCCONF_HALL_TAB_4
#define MCCONF_HALL_TAB_4				5
#endif
#ifndef MCCONF_HALL_TAB_5
#define MCCONF_HALL_TAB_5				6
#endif
#ifndef MCCONF_HALL_TAB_6
#define MCCONF_HALL_TAB_6				4
#endif
#ifndef MCCONF_HALL_TAB_7
#define MCCONF_HALL_TAB_7				-1
#endif
#ifndef MCCONF_HALL_ERPM
#define MCCONF_HALL_ERPM				2000.0	// ERPM above which sensorless commutation is used in hybrid mode
#endif

// FOC
#ifndef MCCONF_FOC_CURRENT_KP
#define MCCONF_FOC_CURRENT_KP			0.03
#endif
#ifndef MCCONF_FOC_CURRENT_KI
#define MCCONF_FOC_CURRENT_KI			50.0
#endif
#ifndef MCCONF_FOC_F_SW
#define MCCONF_FOC_F_SW					20000.0
#endif
#ifndef MCCONF_FOC_DT_US
#define MCCONF_FOC_DT_US				0.08 // Microseconds for dead time compensation
#endif
#ifndef MCCONF_FOC_ENCODER_INVERTED
#define MCCONF_FOC_ENCODER_INVERTED		false
#endif
#ifndef MCCONF_FOC_ENCODER_OFFSET
#define MCCONF_FOC_ENCODER_OFFSET		180.0
#endif
#ifndef MCCONF_FOC_ENCODER_RATIO
#define MCCONF_FOC_ENCODER_RATIO		7.0
#endif
#ifndef MCCONF_FOC_SENSOR_MODE
#define MCCONF_FOC_SENSOR_MODE			FOC_SENSOR_MODE_SENSORLESS
#endif
#ifndef MCCONF_FOC_PLL_KP
#define MCCONF_FOC_PLL_KP				2000.0
#endif
#ifndef MCCONF_FOC_PLL_KI
#define MCCONF_FOC_PLL_KI				40000.0
#endif
#ifndef MCCONF_FOC_MOTOR_L
#define MCCONF_FOC_MOTOR_L				0.000007
#endif
#ifndef MCCONF_FOC_MOTOR_R
#define MCCONF_FOC_MOTOR_R				0.015
#endif
#ifndef MCCONF_FOC_MOTOR_FLUX_LINKAGE
#define MCCONF_FOC_MOTOR_FLUX_LINKAGE	0.00245
#endif
#ifndef MCCONF_FOC_OBSERVER_GAIN
#define MCCONF_FOC_OBSERVER_GAIN		9e7		// Can be something like 600 / L
#endif
#ifndef MCCONF_FOC_OBSERVER_GAIN_SLOW
#define MCCONF_FOC_OBSERVER_GAIN_SLOW	0.3		// Observer gain scale at minimum duty cycle
#endif
#ifndef MCCONF_FOC_DUTY_DOWNRAMP_KP
#define MCCONF_FOC_DUTY_DOWNRAMP_KP		10.0	// PI controller for duty control when decreasing the duty
#endif
#ifndef MCCONF_FOC_DUTY_DOWNRAMP_KI
#define MCCONF_FOC_DUTY_DOWNRAMP_KI		200.0	// PI controller for duty control when decreasing the duty
#endif
#ifndef MCCONF_FOC_OPENLOOP_RPM
#define MCCONF_FOC_OPENLOOP_RPM			400.0	// Openloop RPM (sensorless low speed or when finding index pulse)
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_HYST
#define MCCONF_FOC_SL_OPENLOOP_HYST		0.1		// Time below min RPM to activate openloop (s)
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_TIME
#define MCCONF_FOC_SL_OPENLOOP_TIME		0.1		// Time to remain in openloop (s)
#endif
#ifndef MCCONF_FOC_SL_D_CURRENT_DUTY
#define MCCONF_FOC_SL_D_CURRENT_DUTY	0.0		// Inject d-axis current below this duty cycle in sensorless more
#endif
#ifndef MCCONF_FOC_SL_D_CURRENT_FACTOR
#define MCCONF_FOC_SL_D_CURRENT_FACTOR	0.0		// Maximum q-axis current factor
#endif
#ifndef MCCONF_FOC_HALL_TAB_0
#define MCCONF_FOC_HALL_TAB_0			255
#endif
#ifndef MCCONF_FOC_HALL_TAB_1
#define MCCONF_FOC_HALL_TAB_1			255
#endif
#ifndef MCCONF_FOC_HALL_TAB_2
#define MCCONF_FOC_HALL_TAB_2			255
#endif
#ifndef MCCONF_FOC_HALL_TAB_3
#define MCCONF_FOC_HALL_TAB_3			255
#endif
#ifndef MCCONF_FOC_HALL_TAB_4
#define MCCONF_FOC_HALL_TAB_4			255
#endif
#ifndef MCCONF_FOC_HALL_TAB_5
#define MCCONF_FOC_HALL_TAB_5			255
#endif
#ifndef MCCONF_FOC_HALL_TAB_6
#define MCCONF_FOC_HALL_TAB_6			255
#endif
#ifndef MCCONF_FOC_HALL_TAB_7
#define MCCONF_FOC_HALL_TAB_7			255
#endif
#ifndef MCCONF_FOC_SL_ERPM
#define MCCONF_FOC_SL_ERPM				2500.0	// ERPM above which only the observer is used
#endif
#ifndef MCCONF_FOC_SAMPLE_V0_V7
#define MCCONF_FOC_SAMPLE_V0_V7			false	// Run control loop in both v0 and v7 (requires phase shunts)
#endif
#ifndef MCCONF_FOC_SAMPLE_HIGH_CURRENT
#define MCCONF_FOC_SAMPLE_HIGH_CURRENT	false	// High current sampling mode (requires three shunts)
#endif
#ifndef MCCONF_FOC_SAT_COMP
#define MCCONF_FOC_SAT_COMP				0.0		// Stator saturation compensation
#endif
#ifndef MCCONF_FOC_TEMP_COMP
#define MCCONF_FOC_TEMP_COMP			false	// Motor temperature compensation
#endif
#ifndef MCCONF_FOC_TEMP_COMP_BASE_TEMP
#define MCCONF_FOC_TEMP_COMP_BASE_TEMP	25.0	// Motor temperature compensation base temperature
#endif

// Misc
#ifndef MCCONF_M_FAULT_STOP_TIME
#define MCCONF_M_FAULT_STOP_TIME		500	// Ignore commands for this duration in msec when faults occur
#endif
#ifndef MCCONF_M_RAMP_STEP
#define MCCONF_M_RAMP_STEP				0.02	// Duty cycle ramping step (1000 times/sec) at maximum duty cycle
#endif
#ifndef MCCONF_M_CURRENT_BACKOFF_GAIN
#define MCCONF_M_CURRENT_BACKOFF_GAIN	0.5		// The error gain of the current limiting algorithm
#endif
#ifndef MCCONF_M_ENCODER_COUNTS
#define MCCONF_M_ENCODER_COUNTS			8192	// The number of encoder counts
#endif
#ifndef MCCONF_M_SENSOR_PORT_MODE
#define MCCONF_M_SENSOR_PORT_MODE		SENSOR_PORT_MODE_HALL // The mode of the hall_encoder port
#endif
#ifndef MCCONF_M_INVERT_DIRECTION
#define MCCONF_M_INVERT_DIRECTION		false // Invert the motor direction
#endif
#ifndef MCCONF_M_DRV8301_OC_MODE
#define MCCONF_M_DRV8301_OC_MODE		DRV8301_OC_LIMIT // DRV8301 over current protection mode
#endif
#ifndef MCCONF_M_DRV8301_OC_ADJ
#define MCCONF_M_DRV8301_OC_ADJ			16 // DRV8301 over current protection threshold
#endif
#ifndef MCCONF_M_BLDC_F_SW_MIN
#define MCCONF_M_BLDC_F_SW_MIN			3000 // Minimum switching frequency in bldc mode
#endif
#ifndef MCCONF_M_BLDC_F_SW_MAX
#define MCCONF_M_BLDC_F_SW_MAX			40000 // Maximum switching frequency in bldc mode
#endif
#ifndef MCCONF_M_DC_F_SW
#define MCCONF_M_DC_F_SW				35000 // Switching frequency in dc mode
#endif
#ifndef MCCONF_M_NTC_MOTOR_BETA
#define MCCONF_M_NTC_MOTOR_BETA			3380.0 // Beta value for motor termistor
#endif

#endif /* MCCONF_DEFAULT_H_ */
