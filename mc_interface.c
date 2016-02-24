/*
	Copyright 2015 Benjamin Vedder	benjamin@vedder.se

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
 * mc_interface.c
 *
 *  Created on: 10 okt 2015
 *      Author: benjamin
 */

#include "mc_interface.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "ledpwm.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "terminal.h"
#include "utils.h"
#include "ch.h"
#include "hal.h"
#include "commands.h"
#include "encoder.h"
#include <math.h>

// Global variables
volatile uint16_t ADC_Value[HW_ADC_CHANNELS];
volatile int ADC_curr_norm_value[3];

// Private variables
static volatile mc_configuration m_conf;
static mc_fault_code m_fault_now;
static int m_ignore_iterations;
static volatile unsigned int m_cycles_running;
static volatile bool m_lock_enabled;
static volatile bool m_lock_override_once;
static volatile float m_motor_current_sum;
static volatile float m_input_current_sum;
static volatile float m_motor_current_iterations;
static volatile float m_input_current_iterations;
static volatile float m_amp_seconds;
static volatile float m_amp_seconds_charged;
static volatile float m_watt_seconds;
static volatile float m_watt_seconds_charged;
static volatile float m_position_set;

// Sampling variables
#define ADC_SAMPLE_MAX_LEN		2000
static volatile int16_t m_curr0_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t m_curr1_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t m_ph1_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t m_ph2_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t m_ph3_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t m_vzero_samples[ADC_SAMPLE_MAX_LEN];
static volatile uint8_t m_status_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t m_curr_fir_samples[ADC_SAMPLE_MAX_LEN];
static volatile int16_t m_f_sw_samples[ADC_SAMPLE_MAX_LEN];
static volatile int m_sample_len;
static volatile int m_sample_int;
static volatile int m_sample_ready;
static volatile int m_sample_now;
static volatile int m_sample_at_start;
static volatile int m_start_comm;
static volatile float m_last_adc_duration_sample;

// Private functions
static void update_override_limits(volatile mc_configuration *conf);

// Function pointers
static void(*pwn_done_func)(void) = 0;

// Threads
static THD_WORKING_AREA(timer_thread_wa, 1024);
static THD_FUNCTION(timer_thread, arg);
static THD_WORKING_AREA(sample_send_thread_wa, 1024);
static THD_FUNCTION(sample_send_thread, arg);
static thread_t *sample_send_tp;

void mc_interface_init(mc_configuration *configuration) {
	m_conf = *configuration;
	m_fault_now = FAULT_CODE_NONE;
	m_ignore_iterations = 0;
	m_cycles_running = 0;
	m_lock_enabled = false;
	m_lock_override_once = false;
	m_motor_current_sum = 0.0;
	m_input_current_sum = 0.0;
	m_motor_current_iterations = 0.0;
	m_input_current_iterations = 0.0;
	m_amp_seconds = 0.0;
	m_amp_seconds_charged = 0.0;
	m_watt_seconds = 0.0;
	m_watt_seconds_charged = 0.0;
	m_position_set = 0.0;
	m_last_adc_duration_sample = 0.0;

	m_sample_len = 1000;
	m_sample_int = 1;
	m_sample_ready = 1;
	m_sample_now = 0;
	m_sample_at_start = 0;
	m_start_comm = 0;

	// Start threads
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
	chThdCreateStatic(sample_send_thread_wa, sizeof(sample_send_thread_wa), NORMALPRIO - 1, sample_send_thread, NULL);

	// Initialize selected implementation
	switch (m_conf.motor_type) {
		case MOTOR_TYPE_BLDC:
		case MOTOR_TYPE_DC:
			mcpwm_init(&m_conf);
			break;

		case MOTOR_TYPE_FOC:
			mcpwm_foc_init(&m_conf);
			break;

		default:
			break;
	}
}

const volatile mc_configuration* mc_interface_get_configuration(void) {
	return &m_conf;
}

void mc_interface_set_configuration(mc_configuration *configuration) {
	if (m_conf.motor_type == MOTOR_TYPE_FOC
			&& configuration->motor_type != MOTOR_TYPE_FOC) {
		mcpwm_foc_deinit();
		m_conf = *configuration;
		mcpwm_init(&m_conf);
	} else if (m_conf.motor_type != MOTOR_TYPE_FOC
			&& configuration->motor_type == MOTOR_TYPE_FOC) {
		mcpwm_deinit();
		m_conf = *configuration;
		mcpwm_foc_init(&m_conf);
	} else {
		m_conf = *configuration;
	}

	update_override_limits(&m_conf);

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_configuration(&m_conf);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_configuration(&m_conf);
		break;

	default:
		break;
	}
}

bool mc_interface_dccal_done(void) {
	bool ret = false;
	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_is_dccal_done();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_is_dccal_done();
		break;

	default:
		break;
	}

	return ret;
}

/**
 * Set a function that should be called after each PWM cycle.
 *
 * @param p_func
 * The function to be called. 0 will not call any function.
 */
void mc_interface_set_pwm_callback(void (*p_func)(void)) {
	pwn_done_func = p_func;
}

/**
 * Lock the control by disabling all control commands.
 */
void mc_interface_lock(void) {
	m_lock_enabled = true;
}

/**
 * Unlock all control commands.
 */
void mc_interface_unlock(void) {
	m_lock_enabled = false;
}

/**
 * Allow just one motor control command in the locked state.
 */
void mc_interface_lock_override_once(void) {
	m_lock_override_once = true;
}

mc_fault_code mc_interface_get_fault(void) {
	return m_fault_now;
}

const char* mc_interface_fault_to_string(mc_fault_code fault) {
	switch (fault) {
	case FAULT_CODE_NONE: return "FAULT_CODE_NONE"; break;
	case FAULT_CODE_OVER_VOLTAGE: return "FAULT_CODE_OVER_VOLTAGE"; break;
	case FAULT_CODE_UNDER_VOLTAGE: return "FAULT_CODE_UNDER_VOLTAGE"; break;
	case FAULT_CODE_DRV8302: return "FAULT_CODE_DRV8302"; break;
	case FAULT_CODE_ABS_OVER_CURRENT: return "FAULT_CODE_ABS_OVER_CURRENT"; break;
	case FAULT_CODE_OVER_TEMP_FET: return "FAULT_CODE_OVER_TEMP_FET"; break;
	case FAULT_CODE_OVER_TEMP_MOTOR: return "FAULT_CODE_OVER_TEMP_MOTOR"; break;
	default: return "FAULT_UNKNOWN"; break;
	}
}

mc_state mc_interface_get_state(void) {
	mc_state ret = MC_STATE_OFF;
	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_state();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_state();
		break;

	default:
		break;
	}

	return ret;
}

void mc_interface_set_duty(float dutyCycle) {
	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_duty(dutyCycle);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_duty(dutyCycle);
		break;

	default:
		break;
	}
}

void mc_interface_set_duty_noramp(float dutyCycle) {
	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_duty_noramp(dutyCycle);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_duty_noramp(dutyCycle);
		break;

	default:
		break;
	}
}

void mc_interface_set_pid_speed(float rpm) {
	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_pid_speed(rpm);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_pid_speed(rpm);
		break;

	default:
		break;
	}
}

void mc_interface_set_pid_pos(float pos) {
	if (mc_interface_try_input()) {
		return;
	}

	m_position_set = pos;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_pid_pos(pos);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_pid_pos(pos);
		break;

	default:
		break;
	}
}

void mc_interface_set_current(float current) {
	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_current(current);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_current(current);
		break;

	default:
		break;
	}
}

void mc_interface_set_brake_current(float current) {
	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_brake_current(current);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_brake_current(current);
		break;

	default:
		break;
	}
}

void mc_interface_brake_now(void) {
	mc_interface_set_duty(0.0);
}

/**
 * Disconnect the motor and let it turn freely.
 */
void mc_interface_release_motor(void) {
	mc_interface_set_current(0.0);
}

/**
 * Stop the motor and use braking.
 */
float mc_interface_get_duty_cycle_set(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_duty_cycle_set();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_duty_cycle_set();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_duty_cycle_now(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_duty_cycle_now();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_duty_cycle_now();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_switching_frequency_now(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_switching_frequency_now();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_switching_frequency_now();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_rpm(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_rpm();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_rpm();
		break;

	default:
		break;
	}

	return ret;
}

/**
 * Get the amount of amp hours drawn from the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of amp hours drawn.
 */
float mc_interface_get_amp_hours(bool reset) {
	float val = m_amp_seconds / 3600;

	if (reset) {
		m_amp_seconds = 0.0;
	}

	return val;
}

/**
 * Get the amount of amp hours fed back into the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of amp hours fed back.
 */
float mc_interface_get_amp_hours_charged(bool reset) {
	float val = m_amp_seconds_charged / 3600;

	if (reset) {
		m_amp_seconds_charged = 0.0;
	}

	return val;
}

/**
 * Get the amount of watt hours drawn from the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of watt hours drawn.
 */
float mc_interface_get_watt_hours(bool reset) {
	float val = m_watt_seconds / 3600;

	if (reset) {
		m_amp_seconds = 0.0;
	}

	return val;
}

/**
 * Get the amount of watt hours fed back into the input source.
 *
 * @param reset
 * If true, the counter will be reset after this call.
 *
 * @return
 * The amount of watt hours fed back.
 */
float mc_interface_get_watt_hours_charged(bool reset) {
	float val = m_watt_seconds_charged / 3600;

	if (reset) {
		m_watt_seconds_charged = 0.0;
	}

	return val;
}

float mc_interface_get_tot_current(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tot_current();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tot_current();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_tot_current_filtered(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tot_current_filtered();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tot_current_filtered();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_tot_current_directional(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tot_current_directional();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tot_current_directional();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_tot_current_directional_filtered(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tot_current_directional_filtered();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tot_current_directional_filtered();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_tot_current_in(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tot_current_in();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tot_current_in();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_tot_current_in_filtered(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tot_current_in_filtered();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tot_current_in_filtered();
		break;

	default:
		break;
	}

	return ret;
}

int mc_interface_get_tachometer_value(bool reset) {
	int ret = 0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tachometer_value(reset);
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tachometer_value(reset);
		break;

	default:
		break;
	}

	return ret;
}

int mc_interface_get_tachometer_abs_value(bool reset) {
	int ret = 0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_tachometer_abs_value(reset);
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_tachometer_abs_value(reset);
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_last_inj_adc_isr_duration(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_last_inj_adc_isr_duration();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_last_inj_adc_isr_duration();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_read_reset_avg_motor_current(void) {
	float res = m_motor_current_sum / m_motor_current_iterations;
	m_motor_current_sum = 0;
	m_motor_current_iterations = 0;
	return res;
}

float mc_interface_read_reset_avg_input_current(void) {
	float res = m_input_current_sum / m_input_current_iterations;
	m_input_current_sum = 0;
	m_input_current_iterations = 0;
	return res;
}

float mc_interface_get_pid_pos_set(void) {
	return m_position_set;
}

float mc_interface_get_pid_pos_now(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = encoder_read_deg();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_pid_pos_now();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_get_last_sample_adc_isr_duration(void) {
	return m_last_adc_duration_sample;
}

void mc_interface_sample_print_data(bool at_start, uint16_t len, uint8_t decimation) {
	if (len > ADC_SAMPLE_MAX_LEN) {
		len = ADC_SAMPLE_MAX_LEN;
	}

	m_sample_len = len;
	m_sample_int = decimation;

	if (at_start) {
		m_sample_at_start = 1;
		m_start_comm = mcpwm_get_comm_step();
	} else {
		m_sample_now = 0;
		m_sample_ready = 0;
	}
}

// MC implementation functions
/**
 * A helper function that should be called before sending commands to control
 * the motor. If the state is detecting, the detection will be stopped.
 *
 * @return
 * The amount if milliseconds left until user commands are allowed again.
 *
 */
int mc_interface_try_input(void) {
	// TODO: Remove this later
	if (mc_interface_get_state() == MC_STATE_DETECTING) {
		mcpwm_stop_pwm();
		m_ignore_iterations = MCPWM_DETECT_STOP_TIME;
	}

	int retval = m_ignore_iterations;

	if (!m_ignore_iterations && m_lock_enabled) {
		if (!m_lock_override_once) {
			retval = 1;
		} else {
			m_lock_override_once = false;
		}
	}

	return retval;
}

void mc_interface_fault_stop(mc_fault_code fault) {
	if (mc_interface_dccal_done() && m_fault_now == FAULT_CODE_NONE) {
		// Sent to terminal fault logger so that all faults and their conditions
		// can be printed for debugging.
		chSysLock();
		volatile int val_samp = TIM8->CCR1;
		volatile int current_samp = TIM1->CCR4;
		volatile int tim_top = TIM1->ARR;
		chSysUnlock();

		fault_data fdata;
		fdata.fault = fault;
		fdata.current = mc_interface_get_tot_current();
		fdata.current_filtered = mc_interface_get_tot_current_filtered();
		fdata.voltage = GET_INPUT_VOLTAGE();
		fdata.duty = mc_interface_get_duty_cycle_now();
		fdata.rpm = mc_interface_get_rpm();
		fdata.tacho = mc_interface_get_tachometer_value(false);
		fdata.cycles_running = m_cycles_running;
		fdata.tim_val_samp = val_samp;
		fdata.tim_current_samp = current_samp;
		fdata.tim_top = tim_top;
		fdata.comm_step = mcpwm_get_comm_step();
		fdata.temperature = NTC_TEMP(ADC_IND_TEMP_MOS1);
		terminal_add_fault_data(&fdata);
	}

	m_ignore_iterations = m_conf.m_fault_stop_time_ms;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_stop_pwm();
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_stop_pwm();
		break;

	default:
		break;
	}

	m_fault_now = fault;
}

void mc_interface_mc_timer_isr(void) {
	ledpwm_update_pwm(); // LED PWM Driver update

	const float input_voltage = GET_INPUT_VOLTAGE();

	// Check for faults that should stop the motor
	static int wrong_voltage_iterations = 0;
	if (input_voltage < m_conf.l_min_vin ||
			input_voltage > m_conf.l_max_vin) {
		wrong_voltage_iterations++;

		if ((wrong_voltage_iterations >= 8)) {
			mc_interface_fault_stop(input_voltage < m_conf.l_min_vin ?
					FAULT_CODE_UNDER_VOLTAGE : FAULT_CODE_OVER_VOLTAGE);
		}
	} else {
		wrong_voltage_iterations = 0;
	}

	if (mc_interface_get_state() == MC_STATE_RUNNING) {
		m_cycles_running++;
	} else {
		m_cycles_running = 0;
	}

	if (pwn_done_func) {
		pwn_done_func();
	}

	const float current = mc_interface_get_tot_current_filtered();
	const float current_in = mc_interface_get_tot_current_in_filtered();
	m_motor_current_sum += current;
	m_input_current_sum += current_in;
	m_motor_current_iterations++;
	m_input_current_iterations++;

	float abs_current = mc_interface_get_tot_current();
	float abs_current_filtered = current;
	if (m_conf.motor_type == MOTOR_TYPE_FOC) {
		// TODO: Make this more general
		abs_current = mcpwm_foc_get_abs_motor_current();
		abs_current_filtered = mcpwm_foc_get_abs_motor_current_filtered();
	}

	// Current fault code
	if (m_conf.l_slow_abs_current) {
		if (fabsf(abs_current_filtered) > m_conf.l_abs_current_max) {
			mc_interface_fault_stop(FAULT_CODE_ABS_OVER_CURRENT);
		}
	} else {
		if (fabsf(abs_current) > m_conf.l_abs_current_max) {
			mc_interface_fault_stop(FAULT_CODE_ABS_OVER_CURRENT);
		}
	}

	// Watt and ah counters
	const float f_sw = mc_interface_get_switching_frequency_now();
	if (fabsf(current) > 1.0) {
		// Some extra filtering
		static float curr_diff_sum = 0.0;
		static float curr_diff_samples = 0;

		curr_diff_sum += current_in / f_sw;
		curr_diff_samples += 1.0 / f_sw;

		if (curr_diff_samples >= 0.01) {
			if (curr_diff_sum > 0.0) {
				m_amp_seconds += curr_diff_sum;
				m_watt_seconds += curr_diff_sum * input_voltage;
			} else {
				m_amp_seconds_charged -= curr_diff_sum;
				m_watt_seconds_charged -= curr_diff_sum * input_voltage;
			}

			curr_diff_samples = 0.0;
			curr_diff_sum = 0.0;
		}
	}

	// Sample collection
	if (m_sample_at_start && (mc_interface_get_state() == MC_STATE_RUNNING ||
			m_start_comm != mcpwm_get_comm_step())) {
		m_sample_now = 0;
		m_sample_ready = 0;
		m_sample_at_start = 0;
	}

	static int a = 0;
	if (!m_sample_ready) {
		a++;
		if (a >= m_sample_int) {
			a = 0;

			if (mc_interface_get_state() == MC_STATE_DETECTING) {
				m_curr0_samples[m_sample_now] = (int16_t)mcpwm_detect_currents[mcpwm_get_comm_step() - 1];
				m_curr1_samples[m_sample_now] = (int16_t)mcpwm_detect_currents_diff[mcpwm_get_comm_step() - 1];

				m_ph1_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[0];
				m_ph2_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[1];
				m_ph3_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[2];
			} else {
				m_curr0_samples[m_sample_now] = ADC_curr_norm_value[0];
				m_curr1_samples[m_sample_now] = ADC_curr_norm_value[1];

				m_ph1_samples[m_sample_now] = ADC_V_L1 - mcpwm_vzero;
				m_ph2_samples[m_sample_now] = ADC_V_L2 - mcpwm_vzero;
				m_ph3_samples[m_sample_now] = ADC_V_L3 - mcpwm_vzero;
			}

			m_vzero_samples[m_sample_now] = mcpwm_vzero;

			m_curr_fir_samples[m_sample_now] = (int16_t)(mc_interface_get_tot_current() * 100.0);
			m_f_sw_samples[m_sample_now] = (int16_t)(mc_interface_get_switching_frequency_now() / 10.0);

			m_status_samples[m_sample_now] = mcpwm_get_comm_step() | (mcpwm_read_hall_phase() << 3);

			m_sample_now++;

			if (m_sample_now == m_sample_len) {
				m_sample_ready = 1;
				m_sample_now = 0;
				chSysLockFromISR();
				chEvtSignalI(sample_send_tp, (eventmask_t) 1);
				chSysUnlockFromISR();
			}

			m_last_adc_duration_sample = mcpwm_get_last_adc_isr_duration();
		}
	}
}

void mc_interface_adc_inj_int_handler(void) {
	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_adc_inj_int_handler();
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_adc_inj_int_handler();
		break;

	default:
		break;
	}
}

/**
 * Update the override limits for a configuration based on MOSFET temperature etc.
 *
 * @param conf
 * The configaration to update.
 */
static void update_override_limits(volatile mc_configuration *conf) {
	const float temp = NTC_TEMP(ADC_IND_TEMP_MOS1);
	const float v_in = GET_INPUT_VOLTAGE();

	// Temperature
	if (temp < conf->l_temp_fet_start) {
		conf->lo_current_min = conf->l_current_min;
		conf->lo_current_max = conf->l_current_max;
	} else if (temp > conf->l_temp_fet_end) {
		conf->lo_current_min = 0.0;
		conf->lo_current_max = 0.0;
		mc_interface_fault_stop(FAULT_CODE_OVER_TEMP_FET);
	} else {
		float maxc = fabsf(conf->l_current_max);
		if (fabsf(conf->l_current_min) > maxc) {
			maxc = fabsf(conf->l_current_min);
		}

		maxc = utils_map(temp, conf->l_temp_fet_start, conf->l_temp_fet_end, maxc, 0.0);

		if (fabsf(conf->l_current_max) > maxc) {
			conf->lo_current_max = SIGN(conf->l_current_max) * maxc;
		}

		if (fabsf(conf->l_current_min) > maxc) {
			conf->lo_current_min = SIGN(conf->l_current_min) * maxc;
		}
	}

	// Battery cutoff
	if (v_in > conf->l_battery_cut_start) {
		conf->lo_in_current_max = conf->l_in_current_max;
	} else if (v_in < conf->l_battery_cut_end) {
		conf->lo_in_current_max = 0.0;
	} else {
		conf->lo_in_current_max = utils_map(v_in, conf->l_battery_cut_start,
				conf->l_battery_cut_end, conf->l_in_current_max, 0.0);
	}

	conf->lo_in_current_min = conf->l_in_current_min;
}

static THD_FUNCTION(timer_thread, arg) {
	(void)arg;

	chRegSetThreadName("mcif timer");

	for(;;) {
		// Check if the DRV8302 indicates any fault
		if (IS_DRV_FAULT()) {
			mc_interface_fault_stop(FAULT_CODE_DRV8302);
		}

		// Decrease fault iterations
		if (m_ignore_iterations > 0) {
			m_ignore_iterations--;
		} else {
			if (!IS_DRV_FAULT()) {
				m_fault_now = FAULT_CODE_NONE;
			}
		}

		update_override_limits(&m_conf);

		chThdSleepMilliseconds(1);
	}
}

static THD_FUNCTION(sample_send_thread, arg) {
	(void)arg;

	chRegSetThreadName("SampleSender");

	sample_send_tp = chThdGetSelfX();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		for (int i = 0;i < m_sample_len;i++) {
			uint8_t buffer[20];
			int index = 0;

			buffer[index++] = m_curr0_samples[i] >> 8;
			buffer[index++] = m_curr0_samples[i];
			buffer[index++] = m_curr1_samples[i] >> 8;
			buffer[index++] = m_curr1_samples[i];
			buffer[index++] = m_ph1_samples[i] >> 8;
			buffer[index++] = m_ph1_samples[i];
			buffer[index++] = m_ph2_samples[i] >> 8;
			buffer[index++] = m_ph2_samples[i];
			buffer[index++] = m_ph3_samples[i] >> 8;
			buffer[index++] = m_ph3_samples[i];
			buffer[index++] = m_vzero_samples[i] >> 8;
			buffer[index++] = m_vzero_samples[i];
			buffer[index++] = m_status_samples[i];
			buffer[index++] = m_curr_fir_samples[i] >> 8;
			buffer[index++] = m_curr_fir_samples[i];
			buffer[index++] = m_f_sw_samples[i] >> 8;
			buffer[index++] = m_f_sw_samples[i];

			commands_send_samples(buffer, index);
		}
	}
}
