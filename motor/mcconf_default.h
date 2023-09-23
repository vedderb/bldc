/*
	Copyright 2016 - 2019 Benjamin Vedder	benjamin@vedder.se

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
#define MCCONF_DEFAULT_MOTOR_TYPE		MOTOR_TYPE_FOC
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
#define MCCONF_L_IN_CURRENT_MAX			99.0	// Input current limit in Amperes (Upper)
#endif
#ifndef MCCONF_L_IN_CURRENT_MIN
#define MCCONF_L_IN_CURRENT_MIN			-60.0	// Input current limit in Amperes (Lower)
#endif
#ifndef MCCONF_L_IN_CURRENT_MAP_START
#define MCCONF_L_IN_CURRENT_MAP_START	1.0		// Input current to Q axis current limit map start
#endif
#ifndef MCCONF_L_IN_CURRENT_MAP_FILTER
#define MCCONF_L_IN_CURRENT_MAP_FILTER	0.005	// Input current filter for the mapped limit
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
#ifndef MCCONF_L_BATTERY_REGEN_CUT_START
#define MCCONF_L_BATTERY_REGEN_CUT_START    1000.0	// Start limiting the regen current at this voltage
#endif
#ifndef MCCONF_L_BATTERY_REGEN_CUT_END
#define MCCONF_L_BATTERY_REGEN_CUT_END  1100.0		// Limit the regen current completely at this voltage
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
#define MCCONF_L_SLOW_ABS_OVERCURRENT	false	// Use the filtered (and hence slower) current for the overcurrent fault detection
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
#define MCCONF_L_WATT_MAX				1500000.0	// Maximum wattage output
#endif
#ifndef MCCONF_L_WATT_MIN
#define MCCONF_L_WATT_MIN				-1500000.0	// Minimum wattage output (braking)
#endif
#ifndef MCCONF_L_CURRENT_MAX_SCALE
#define MCCONF_L_CURRENT_MAX_SCALE		1.0	// Maximum current scale
#endif
#ifndef MCCONF_L_CURRENT_MIN_SCALE
#define MCCONF_L_CURRENT_MIN_SCALE		1.0	// Minimum current scale
#endif
#ifndef MCCONF_L_DUTY_START
#define MCCONF_L_DUTY_START				1.0 // Start limiting current at this duty cycle
#endif

// Common PID-parameters
#ifndef MCCONF_SP_PID_LOOP_RATE
#define MCCONF_SP_PID_LOOP_RATE			PID_RATE_1000_HZ // PID loop rate
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
#ifndef MCCONF_S_PID_KD_FILTER
#define MCCONF_S_PID_KD_FILTER			0.2	// Derivative filter
#endif
#ifndef MCCONF_S_PID_MIN_RPM
#define MCCONF_S_PID_MIN_RPM			900.0	// Minimum allowed RPM
#endif
#ifndef MCCONF_S_PID_ALLOW_BRAKING
#define MCCONF_S_PID_ALLOW_BRAKING		true	// Allow braking in speed control mode
#endif
#ifndef MCCONF_S_PID_RAMP_ERPMS_S
#define MCCONF_S_PID_RAMP_ERPMS_S		25000.0	// Speed input ramping, in ERPM/s
#endif
#ifndef MCCONF_S_PID_SPEED_SOURCE
#define MCCONF_S_PID_SPEED_SOURCE		S_PID_SPEED_SRC_PLL
#endif

// Position PID parameters
#ifndef MCCONF_P_PID_KP
#define MCCONF_P_PID_KP					0.025	// Proportional gain
#endif
#ifndef MCCONF_P_PID_KI
#define MCCONF_P_PID_KI					0.0		// Integral gain
#endif
#ifndef MCCONF_P_PID_KD
#define MCCONF_P_PID_KD					0.00000	// Derivative gain
#endif
#ifndef MCCONF_P_PID_KD_PROC
#define MCCONF_P_PID_KD_PROC			0.00035	// Derivative gain process
#endif
#ifndef MCCONF_P_PID_KD_FILTER
#define MCCONF_P_PID_KD_FILTER			0.2		// Derivative filter
#endif
#ifndef MCCONF_P_PID_ANG_DIV
#define MCCONF_P_PID_ANG_DIV			1.0		// Divide angle by this value
#endif
#ifndef MCCONF_P_PID_GAIN_DEC_ANGLE
#define MCCONF_P_PID_GAIN_DEC_ANGLE		0.0		// Decrease PID-gains when the error is below this value
#endif
#ifndef MCCONF_P_PID_OFFSET
#define MCCONF_P_PID_OFFSET				0.0		// Angle offset
#endif

// Current control parameters
#ifndef MCCONF_CC_GAIN
#define MCCONF_CC_GAIN					0.0046	// Current controller error gain
#endif
#ifndef MCCONF_CC_MIN_CURRENT
#define MCCONF_CC_MIN_CURRENT			0.05	// Minimum allowed current
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
#ifndef MCCONF_FOC_F_ZV
#define MCCONF_FOC_F_ZV					25000.0
#endif
#ifndef MCCONF_FOC_DT_US
#define MCCONF_FOC_DT_US				0.12 // Microseconds for dead time compensation
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
#define MCCONF_FOC_PLL_KI				30000.0
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
#ifndef MCCONF_FOC_MOTOR_LD_LQ_DIFF
#define MCCONF_FOC_MOTOR_LD_LQ_DIFF		0.0
#endif
#ifndef MCCONF_FOC_OBSERVER_GAIN
#define MCCONF_FOC_OBSERVER_GAIN		9e7		// Can be something like 600 / L
#endif
#ifndef MCCONF_FOC_OBSERVER_GAIN_SLOW
#define MCCONF_FOC_OBSERVER_GAIN_SLOW	0.05	// Observer gain scale at minimum duty cycle
#endif
#ifndef MCCONF_FOC_OBSERVER_OFFSET
#define MCCONF_FOC_OBSERVER_OFFSET		-1.0	// Observer offset in timer update cycles
#endif
#ifndef MCCONF_FOC_DUTY_DOWNRAMP_KP
#define MCCONF_FOC_DUTY_DOWNRAMP_KP		50.0	// PI controller for duty control when decreasing the duty
#endif
#ifndef MCCONF_FOC_DUTY_DOWNRAMP_KI
#define MCCONF_FOC_DUTY_DOWNRAMP_KI		1000.0	// PI controller for duty control when decreasing the duty
#endif
#ifndef MCCONF_FOC_START_CURR_DEC
#define MCCONF_FOC_START_CURR_DEC		1.0	// Decrease current to this fraction at start
#endif
#ifndef MCCONF_FOC_START_CURR_DEC_RPM
#define MCCONF_FOC_START_CURR_DEC_RPM	2500.0	// At this RPM the full current is available
#endif
#ifndef MCCONF_FOC_OPENLOOP_RPM
#define MCCONF_FOC_OPENLOOP_RPM			1500.0	// Openloop RPM (sensorless low speed or when finding index pulse)
#endif
#ifndef MCCONF_FOC_OPENLOOP_RPM_LOW
#define MCCONF_FOC_OPENLOOP_RPM_LOW		0.0		// Fraction of OPENLOOP_RPM at minimum motor current
#endif
#ifndef MCCONF_FOC_D_GAIN_SCALE_START
#define MCCONF_FOC_D_GAIN_SCALE_START	0.9		// Start reducing D axis current controller gain at this modulation
#endif
#ifndef MCCONF_FOC_D_GAIN_SCALE_MAX_MOD
#define MCCONF_FOC_D_GAIN_SCALE_MAX_MOD	0.2		// D axis currnet controller gain at maximum modulation
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_HYST
#define MCCONF_FOC_SL_OPENLOOP_HYST		0.1		// Time below min RPM to activate openloop (s)
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_TIME
#define MCCONF_FOC_SL_OPENLOOP_TIME		0.05	// Time to remain in openloop after ramping (s)
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_BOOST_Q
#define MCCONF_FOC_SL_OPENLOOP_BOOST_Q	0.0		// Q-axis current boost during the open loop procedure
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_MAX_Q
#define MCCONF_FOC_SL_OPENLOOP_MAX_Q	-1.0		// Q-axis maximum current during the open loop procedure
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_T_LOCK
#define MCCONF_FOC_SL_OPENLOOP_T_LOCK	0.0		// Time to lock motor in beginning of open loop sequence
#endif
#ifndef MCCONF_FOC_SL_OPENLOOP_T_RAMP
#define MCCONF_FOC_SL_OPENLOOP_T_RAMP	0.1		// Time to ramp up motor to openloop speed
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
#ifndef MCCONF_FOC_HALL_INTERP_ERPM
#define MCCONF_FOC_HALL_INTERP_ERPM		500		// Do not interpolate hall sensors below this ERPM
#endif
#ifndef MCCONF_FOC_SL_ERPM_START
#define MCCONF_FOC_SL_ERPM_START		2500.0	// ERPM below which only sensored commutation is used
#endif
#ifndef MCCONF_FOC_SL_ERPM
#define MCCONF_FOC_SL_ERPM				3500.0	// ERPM above which only the observer is used
#endif
#ifndef MCCONF_FOC_CONTROL_SAMPLE_MODE
#define MCCONF_FOC_CONTROL_SAMPLE_MODE	FOC_CONTROL_SAMPLE_MODE_V0
#endif
#ifndef MCCONF_FOC_CURRENT_SAMPLE_MODE
#define MCCONF_FOC_CURRENT_SAMPLE_MODE	FOC_CURRENT_SAMPLE_MODE_LONGEST_ZERO
#endif
#ifndef MCCONF_FOC_SAT_COMP_MODE
#define MCCONF_FOC_SAT_COMP_MODE		SAT_COMP_LAMBDA		// Stator saturation compensation mode
#endif
#ifndef MCCONF_FOC_SAT_COMP
#define MCCONF_FOC_SAT_COMP				0.0		// Stator saturation compensation factor
#endif
#ifndef MCCONF_FOC_TEMP_COMP
#define MCCONF_FOC_TEMP_COMP			false	// Motor temperature compensation
#endif
#ifndef MCCONF_FOC_TEMP_COMP_BASE_TEMP
#define MCCONF_FOC_TEMP_COMP_BASE_TEMP	25.0	// Motor temperature compensation base temperature
#endif
#ifndef MCCONF_FOC_CURRENT_FILTER_CONST
#define MCCONF_FOC_CURRENT_FILTER_CONST	0.1		// Filter constant for the filtered currents
#endif
#ifndef MCCONF_FOC_CC_DECOUPLING
#define MCCONF_FOC_CC_DECOUPLING		FOC_CC_DECOUPLING_DISABLED // Current controller decoupling
#endif
#ifndef MCCONF_FOC_OBSERVER_TYPE
#define MCCONF_FOC_OBSERVER_TYPE		FOC_OBSERVER_MXLEMMING_LAMBDA_COMP // Position observer type for FOC
#endif
#ifndef MCCONF_FOC_HFI_VOLTAGE_START
#define MCCONF_FOC_HFI_VOLTAGE_START	20 // HFI voltage at start when resolving ambiguity
#endif
#ifndef MCCONF_FOC_HFI_VOLTAGE_RUN
#define MCCONF_FOC_HFI_VOLTAGE_RUN		4 // HFI voltage during tracking
#endif
#ifndef MCCONF_FOC_HFI_GAIN
#define MCCONF_FOC_HFI_GAIN				0.3 // Correction gain for HFI V2
#endif
#ifndef MCCONF_FOC_HFI_HYST
#define MCCONF_FOC_HFI_HYST				0.0 // Sense vector offset hysteresis for HFI V2
#endif
#ifndef MCCONF_FOC_HFI_VOLTAGE_MAX
#define MCCONF_FOC_HFI_VOLTAGE_MAX		6 // HFI voltage during tracking at max current
#endif
#ifndef MCCONF_FOC_SL_ERPM_HFI
#define MCCONF_FOC_SL_ERPM_HFI			3000.0	// ERPM above which only the observer is used
#endif
#ifndef MCCONF_FOC_HFI_START_SAMPLES
#define MCCONF_FOC_HFI_START_SAMPLES	5 // Sample this often at start to resolve ambiguity
#endif
#ifndef MCCONF_FOC_HFI_OBS_OVR_SEC
#define MCCONF_FOC_HFI_OBS_OVR_SEC		0.001 // Continue using observer for this long when entering HFI speed
#endif
#ifndef MCCONF_FOC_HFI_SAMPLES
#define MCCONF_FOC_HFI_SAMPLES			HFI_SAMPLES_16 // Samples per motor revolution for HFI
#endif
#ifndef MCCONF_FOC_OFFSETS_CAL_ON_BOOT
#define MCCONF_FOC_OFFSETS_CAL_ON_BOOT	true // Measure offsets every boot
#endif
#ifndef MCCONF_FOC_OFFSETS_CURRENT_0
#define MCCONF_FOC_OFFSETS_CURRENT_0	2048.0 // Current 0 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_CURRENT_1
#define MCCONF_FOC_OFFSETS_CURRENT_1	2048.0 // Current 1 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_CURRENT_2
#define MCCONF_FOC_OFFSETS_CURRENT_2	2048.0 // Current 2 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_VOLTAGE_0
#define MCCONF_FOC_OFFSETS_VOLTAGE_0	0.0 // Voltage 0 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_VOLTAGE_1
#define MCCONF_FOC_OFFSETS_VOLTAGE_1	0.0 // Voltage 1 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_VOLTAGE_2
#define MCCONF_FOC_OFFSETS_VOLTAGE_2	0.0 // Voltage 2 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_VOLTAGE_UNDRIVEN_0
#define MCCONF_FOC_OFFSETS_VOLTAGE_UNDRIVEN_0	0.0 // Voltage undriven 0 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_VOLTAGE_UNDRIVEN_1
#define MCCONF_FOC_OFFSETS_VOLTAGE_UNDRIVEN_1	0.0 // Voltage undriven 1 offset
#endif
#ifndef MCCONF_FOC_OFFSETS_VOLTAGE_UNDRIVEN_2
#define MCCONF_FOC_OFFSETS_VOLTAGE_UNDRIVEN_2	0.0 // Voltage undriven 2 offset
#endif
#ifndef MCCONF_FOC_PHASE_FILTER_ENABLE
#define MCCONF_FOC_PHASE_FILTER_ENABLE	true // Use phase voltage filters when available
#endif
#ifndef MCCONF_FOC_PHASE_FILTER_DISABLE_FAULT
#define MCCONF_FOC_PHASE_FILTER_DISABLE_FAULT	true // Disable phase filter fault code
#endif
#ifndef MCCONF_FOC_PHASE_FILTER_MAX_ERPM
#define MCCONF_FOC_PHASE_FILTER_MAX_ERPM	4000.0 // Use phase filter up to this ERPM
#endif
#ifndef MCCONF_FOC_MTPA_MODE
#define MCCONF_FOC_MTPA_MODE				MTPA_MODE_OFF // Maximum torque per amp (MTPA) algorithm mode
#endif
#ifndef MCCONF_FOC_FW_CURRENT_MAX
#define MCCONF_FOC_FW_CURRENT_MAX		0.0 // Maximum field weakening current
#endif
#ifndef MCCONF_FOC_FW_DUTY_START
#define MCCONF_FOC_FW_DUTY_START		0.9 // Start field weakening at this fraction of max duty cycle
#endif
#ifndef MCCONF_FOC_FW_RAMP_TIME
#define MCCONF_FOC_FW_RAMP_TIME			0.2 // Ramp time for field weakening current
#endif
#ifndef MCCONF_FOC_FW_Q_CURRENT_FACTOR
#define MCCONF_FOC_FW_Q_CURRENT_FACTOR	0.02 // Factor of the FW-current to feed to the Q-axis to slow motor down when setting 0 current
#endif
#ifndef MCCONF_FOC_SPEED_SOURCE
#define MCCONF_FOC_SPEED_SOURCE			FOC_SPEED_SRC_CORRECTED // Position source for speed trackers
#endif

// GPD
#ifndef MCCONF_GPD_BUFFER_NOTIFY_LEFT
#define MCCONF_GPD_BUFFER_NOTIFY_LEFT	200		// Notify when the buffer space left is less than this
#endif
#ifndef MCCONF_GPD_BUFFER_INTERPOL
#define MCCONF_GPD_BUFFER_INTERPOL		0		// Buffer interpolation
#endif
#ifndef MCCONF_GPD_CURRENT_FILTER_CONST
#define MCCONF_GPD_CURRENT_FILTER_CONST	0.1		// Current filter constant
#endif
#ifndef MCCONF_GPD_CURRENT_KP
#define MCCONF_GPD_CURRENT_KP			0.03
#endif
#ifndef MCCONF_GPD_CURRENT_KI
#define MCCONF_GPD_CURRENT_KI			50.0
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
#ifndef MCCONF_M_ENCODER_SIN_AMP
#define MCCONF_M_ENCODER_SIN_AMP		1.0	// Sine amplitude
#endif
#ifndef MCCONF_M_ENCODER_SIN_OFFSET
#define MCCONF_M_ENCODER_SIN_OFFSET		1.65 // Sine offset of the sin/cos encoder
#endif
#ifndef MCCONF_M_ENCODER_COS_AMP
#define MCCONF_M_ENCODER_COS_AMP		1.0	// Cosine amplitude
#endif
#ifndef MCCONF_M_ENCODER_COS_OFFSET
#define MCCONF_M_ENCODER_COS_OFFSET		1.65 // Cosine offset of the sin/cos encoder
#endif
#ifndef MCCONF_M_ENCODER_SINCOS_FILTER
#define MCCONF_M_ENCODER_SINCOS_FILTER	0.5		// Sin/Cos Encoder signal filter constant
#endif
#ifndef MCCONF_M_ENCODER_SINCOS_PHASE
#define MCCONF_M_ENCODER_SINCOS_PHASE		0.0		// Sin/Cos Encoder signal phase correction
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
#define MCCONF_M_BLDC_F_SW_MAX			35000 // Maximum switching frequency in bldc mode
#endif
#ifndef MCCONF_M_DC_F_SW
#define MCCONF_M_DC_F_SW				25000 // Switching frequency in dc mode
#endif
#ifndef MCCONF_M_NTC_MOTOR_BETA
#define MCCONF_M_NTC_MOTOR_BETA			3380.0 // Beta value for motor termistor
#endif
#ifndef MCCONF_M_OUT_AUX_MODE
#define MCCONF_M_OUT_AUX_MODE			OUT_AUX_MODE_OFF // Auxiliary output mode
#endif
#ifndef MCCONF_M_MOTOR_TEMP_SENS_TYPE
#define MCCONF_M_MOTOR_TEMP_SENS_TYPE	TEMP_SENSOR_NTC_10K_25C // Motor temperature sensor type
#endif
#ifndef MCCONF_M_PTC_MOTOR_COEFF
#define MCCONF_M_PTC_MOTOR_COEFF		0.61 // %/K coefficient for motor PTC sensor
#endif
#ifndef MCCONF_M_NTCX_PTCX_RES
#define MCCONF_M_NTCX_PTCX_RES			10000.0 // Custom NTC/PTC resistance
#endif
#ifndef MCCONF_M_NTCX_PTCX_BASE_TEMP
#define MCCONF_M_NTCX_PTCX_BASE_TEMP	25.0 // Custom NTC/PTC base temperature
#endif
#ifndef MCCONF_M_HALL_EXTRA_SAMPLES
#define MCCONF_M_HALL_EXTRA_SAMPLES		3 // Extra samples for filtering when reading hall sensors
#endif
#ifndef MCCONF_M_BATT_FILTER_CONST
#define MCCONF_M_BATT_FILTER_CONST		45 // Battery level filter constant
#endif

// Setup Info
#ifndef MCCONF_SI_MOTOR_POLES
#define MCCONF_SI_MOTOR_POLES			14 // Motor pole count
#endif
#ifndef MCCONF_SI_GEAR_RATIO
#define MCCONF_SI_GEAR_RATIO			3 // Gear ratio
#endif
#ifndef MCCONF_SI_WHEEL_DIAMETER
#define MCCONF_SI_WHEEL_DIAMETER		0.083 // Wheel Diameter
#endif
#ifndef MCCONF_SI_BATTERY_TYPE
#define MCCONF_SI_BATTERY_TYPE			BATTERY_TYPE_LIION_3_0__4_2 // Battery Type
#endif
#ifndef MCCONF_SI_BATTERY_CELLS
#define MCCONF_SI_BATTERY_CELLS			3 // Battery Cells
#endif
#ifndef MCCONF_SI_BATTERY_AH
#define MCCONF_SI_BATTERY_AH			6.0 // Battery amp hours
#endif
#ifndef MCCONF_SI_MOTOR_NL_CURRENT
#define MCCONF_SI_MOTOR_NL_CURRENT		1.0 // Motor no load current
#endif

// BMS
#ifndef MCCONF_BMS_TYPE
#define MCCONF_BMS_TYPE					BMS_TYPE_VESC
#endif
#ifndef MCCONF_BMS_LIMIT_MODE
#define MCCONF_BMS_LIMIT_MODE			3
#endif
#ifndef MCCONF_BMS_T_LIMIT_START
#define MCCONF_BMS_T_LIMIT_START		45
#endif
#ifndef MCCONF_BMS_T_LIMIT_END
#define MCCONF_BMS_T_LIMIT_END			65
#endif
#ifndef MCCONF_BMS_SOC_LIMIT_START
#define MCCONF_BMS_SOC_LIMIT_START		0.05
#endif
#ifndef MCCONF_BMS_SOC_LIMIT_END
#define MCCONF_BMS_SOC_LIMIT_END		0
#endif
#ifndef MCCONF_BMS_FWD_CAN_MODE
#define MCCONF_BMS_FWD_CAN_MODE			BMS_FWD_CAN_MODE_DISABLED
#endif

#endif /* MCCONF_DEFAULT_H_ */
