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
 * mc_pwm.h
 *
 *  Created on: 13 okt 2012
 *      Author: benjamin
 */

#ifndef MCPWM_H_
#define MCPWM_H_

typedef enum {
   MC_STATE_OFF = 0,
   MC_STATE_DETECTING,
   MC_STATE_RUNNING,
   MC_STATE_FULL_BRAKE,
} mc_state;

typedef enum {
	PWM_MODE_NONSYNCHRONOUS_HISW, // This mode is not recommended
	PWM_MODE_SYNCHRONOUS, // The recommended and most tested mode
	PWM_MODE_BIPOLAR // Some glitches occasionally, can kill MOSFETs
} mc_pwm_mode;

typedef enum {
	FAULT_CODE_NONE = 0,
	FAULT_CODE_OVER_VOLTAGE,
	FAULT_CODE_UNDER_VOLTAGE,
	FAULT_CODE_DRV8302
} mc_fault_code;

typedef enum {
	CONTROL_MODE_DUTY = 0,
	CONTROL_MODE_SPEED,
	CONTROL_MODE_CURRENT,
	CONTROL_MODE_NONE
} mc_control_mode;

// Functions
void mcpwm_init(void);
void mcpwm_set_duty(float dutyCycle);
void mcpwm_set_pid_speed(float rpm);
void mcpwm_set_current(float current);
void mcpwm_brake_at_stop(int brake);
void mcpwm_brake_now(void);
void mcpwm_release_motor(void);
int mcpwm_get_comm_step(void);
float mcpwm_get_duty_cycle_set(void);
float mcpwm_get_duty_cycle_now(void);
float mcpwm_get_rpm(void);
mc_state mcpwm_get_state(void);
mc_fault_code mcpwm_get_fault(void);
float mcpwm_get_kv(void);
float mcpwm_get_kv_filtered(void);
int mcpwm_get_tachometer_value(int reset);
float mcpwm_get_tot_current(void);
float mcpwm_get_tot_current_filtered(void);
float mcpwm_get_tot_current_in(void);
float mcpwm_get_tot_current_in_filtered(void);
void mcpwm_set_detect(void);
float mcpwm_get_detect_pos(void);
signed int mcpwm_read_hall_phase(void);
float mcpwm_read_reset_avg_motor_current(void);
float mcpwm_read_reset_avg_input_current(void);
float mcpwm_get_last_adc_isr_duration(void);
float mcpwm_get_last_inj_adc_isr_duration(void);

// Interrupt handlers
void mcpwm_time_int_handler(void);
void mcpwm_update_int_handler(void);
void mcpwm_adc_inj_int_handler(void);
void mcpwm_adc_int_handler(void *p, uint32_t flags);

// External variables
extern volatile uint16_t ADC_Value[];
extern volatile int ADC_curr_norm_value[];
extern volatile float mcpwm_detect_currents[];
extern volatile int mcpwm_vzero;

/*
 * Parameters
 */
#define MCPWM_SWITCH_FREQUENCY_MIN		8000	// The lowest switching frequency in Hz
#define MCPWM_SWITCH_FREQUENCY_MAX		40000	// The highest switching frequency in Hz
#define MCPWM_DEAD_TIME_CYCLES			80		// Dead time
#define MCPWM_PWM_MODE					PWM_MODE_SYNCHRONOUS // Default PWM mode
#define MCPWM_MIN_DUTY_CYCLE			0.01	// Minimum duty cycle
#define MCPWM_MAX_DUTY_CYCLE			0.95	// Maximum duty cycle
#define MCPWM_AVG_COM_RPM				6		// Number of commutations to average RPM over
#define MCPWM_HALL_SENSOR_ORDER			5		// Order in which hall sensors are connected
#define MCPWM_RAMP_STEP					0.02	// Ramping step (1000 times/sec) at maximum duty cycle
#define MCPWM_CURRENT_MAX				60.0	// Current limit in Amperes (Upper)
#define MCPWM_CURRENT_MIN				-60.0	// Current limit in Amperes (Lower)
#define MCPWM_IN_CURRENT_MAX			40.0	// Input current limit in Amperes (Upper)
#define MCPWM_IN_CURRENT_MIN			-20.0	// Input current limit in Amperes (Lower)
#define MCPWM_CURRENT_LIMIT_GAIN		0.1		// The error gain of the current limiting algorithm
#define MCPWM_MIN_VOLTAGE				8.0		// Minimum input voltage
#define MCPWM_MAX_VOLTAGE				50.0	// Maximum input voltage
#define MCPWM_FAULT_STOP_TIME			3000	// Ignore commands for this duration in msec when faults occur
#define MCPWM_RPM_MAX					100000.0	// The motor speed limit (Upper)
#define MCPWM_RPM_MIN					-100000.0	// The motor speed limit (Lower)

// Sensorless settings
#define MCPWM_IS_SENSORLESS				1		// Use sensorless commutation
#define MCPWM_MIN_RPM					300		// Auto-commutate below this RPM
#define MCPWM_CYCLE_INT_LIMIT_LOW		150.0	// Flux integrator limit 0 ERPM
#define MCPWM_CYCLE_INT_LIMIT_HIGH		20.0	// Flux integrator limit 50K ERPM
#define MCPWM_VZERO_FACT				1.0		// Virtual zero adjustment

// Speed PID parameters
#define MCPWM_PID_TIME_K				0.001	// Pid controller sample time in seconds
#define MCPWM_PID_KP					0.0001	// Proportional gain
#define MCPWM_PID_KI					0.002	// Integral gain
#define MCPWM_PID_KD					0.0		// Derivative gain
#define MCPWM_PID_MIN_RPM				1200.0	// Minimum allowed RPM

// Current control parameters
#define MCPWM_CURRENT_CONTROL_GAIN		0.0002	// Current controller error gain
#define MCPWM_CURRENT_CONTROL_MIN		1.0		// Minimum allowed current

/*
 * ==== Parameter guidelines ====
 *
 * Most hobby inrunners and small motors:
 * MCPWM_CYCLE_INT_LIMIT_LOW	15
 * MCPWM_CYCLE_INT_LIMIT_HIGH	2
 *
 * Most hobby outrunners (1kw - 5kw):
 * MCPWM_CYCLE_INT_LIMIT_LOW	150
 * MCPWM_CYCLE_INT_LIMIT_HIGH	20
 *
 * Large 12V high-inductance motor
 * MCPWM_CYCLE_INT_LIMIT_LOW	250
 * MCPWM_CYCLE_INT_LIMIT_HIGH	30
 *
 *
 * ==== Some notes ====
 *
 * - Decreasing the MCPWM_MIN_RPM parameter gives a bit more
 *   startup torque, but will make the start more rough.
 *
 * - Starting at a low MCPWM_CYCLE_INT_LIMIT and then increasing
 *   it usually works well.
 */

#endif /* MC_PWM_H_ */
