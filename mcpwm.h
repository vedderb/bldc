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

#include "conf_general.h"
#include "datatypes.h"
#include <stdbool.h>

// Functions
void mcpwm_init(mc_configuration *configuration);
const volatile mc_configuration* mcpwm_get_configuration(void);
void mcpwm_set_configuration(mc_configuration *configuration);
void mcpwm_init_hall_table(int8_t *table);
void mcpwm_set_duty(float dutyCycle);
void mcpwm_set_pid_speed(float rpm);
void mcpwm_set_pid_pos(float pos);
void mcpwm_set_current(float current);
void mcpwm_set_brake_current(float current);
void mcpwm_brake_now(void);
void mcpwm_release_motor(void);
void mcpwm_lock(void);
void mcpwm_unlock(void);
void mcpwm_lock_override_once(void);
int mcpwm_get_comm_step(void);
float mcpwm_get_duty_cycle_set(void);
float mcpwm_get_duty_cycle_now(void);
float mcpwm_get_switching_frequency_now(void);
float mcpwm_get_rpm(void);
mc_state mcpwm_get_state(void);
mc_fault_code mcpwm_get_fault(void);
const char* mcpwm_fault_to_string(mc_fault_code fault);
float mcpwm_get_kv(void);
float mcpwm_get_kv_filtered(void);
int mcpwm_get_tachometer_value(bool reset);
int mcpwm_get_tachometer_abs_value(bool reset);
float mcpwm_get_amp_hours(bool reset);
float mcpwm_get_amp_hours_charged(bool reset);
float mcpwm_get_watt_hours(bool reset);
float mcpwm_get_watt_hours_charged(bool reset);
float mcpwm_get_tot_current(void);
float mcpwm_get_tot_current_filtered(void);
float mcpwm_get_tot_current_directional(void);
float mcpwm_get_tot_current_directional_filtered(void);
float mcpwm_get_tot_current_in(void);
float mcpwm_get_tot_current_in_filtered(void);
void mcpwm_set_detect(void);
float mcpwm_get_detect_pos(void);
void mcpwm_reset_hall_detect_table(void);
int mcpwm_get_hall_detect_result(int8_t *table);
int mcpwm_read_hall_phase(void);
float mcpwm_read_reset_avg_motor_current(void);
float mcpwm_read_reset_avg_input_current(void);
float mcpwm_read_reset_avg_cycle_integrator(void);
void mcpwm_set_min_rpm(float rpm);
float mcpwm_get_min_rpm(void);
void mcpwm_set_comm_mode(mc_comm_mode mode);
mc_comm_mode mcpwm_get_comm_mode(void);
float mcpwm_get_last_adc_isr_duration(void);
float mcpwm_get_last_inj_adc_isr_duration(void);
mc_rpm_dep_struct mcpwm_get_rpm_dep(void);

// Interrupt handlers
void mcpwm_adc_inj_int_handler(void);
void mcpwm_adc_int_handler(void *p, uint32_t flags);

// External variables
extern volatile uint16_t ADC_Value[];
extern volatile int ADC_curr_norm_value[];
extern volatile float mcpwm_detect_currents[];
extern volatile float mcpwm_detect_voltages[6];
extern volatile float mcpwm_detect_currents_diff[];
extern volatile int mcpwm_vzero;

/*
 * Fixed parameters
 */
#define MCPWM_SWITCH_FREQUENCY_MIN		3000	// The lowest switching frequency in Hz
#define MCPWM_SWITCH_FREQUENCY_MAX		35000	// The highest switching frequency in Hz
#define MCPWM_SWITCH_FREQUENCY_DC_MOTOR	25000	// The DC motor switching frequency
#define MCPWM_DEAD_TIME_CYCLES			80		// Dead time
#define MCPWM_RPM_TIMER_FREQ			1000000.0	// Frequency of the RPM measurement timer
#define MCPWM_RAMP_STEP					0.01	// Ramping step (1000 times/sec) at maximum duty cycle
#define MCPWM_RAMP_STEP_RPM_LIMIT		0.0005	// Ramping step when limiting the RPM
#define MCPWM_CURRENT_LIMIT_GAIN		2.0		// The error gain of the current limiting algorithm
#define MCPWM_CMD_STOP_TIME				0		// Ignore commands for this duration in msec after a stop has been sent
#define MCPWM_DETECT_STOP_TIME			500		// Ignore commands for this duration in msec after a detect command

// Speed PID parameters
#define MCPWM_PID_TIME_K				0.001	// Pid controller sample time in seconds

#endif /* MC_PWM_H_ */
