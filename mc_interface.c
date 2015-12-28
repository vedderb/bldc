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
#include "main.h"
#include "stm32f4xx_conf.h"
#include "hw.h"
#include "terminal.h"
#include "utils.h"
#include "ch.h"
#include "hal.h"
#include <math.h>

// Global variables
volatile uint16_t ADC_Value[HW_ADC_CHANNELS];
volatile int ADC_curr_norm_value[3];

// Private variables
static volatile mc_configuration conf;
static mc_fault_code fault_now;
static int ignore_iterations;
static volatile unsigned int cycles_running;
static volatile bool lock_enabled;
static volatile bool lock_override_once;
static volatile float motor_current_sum;
static volatile float input_current_sum;
static volatile float motor_current_iterations;
static volatile float input_current_iterations;
static volatile float amp_seconds;
static volatile float amp_seconds_charged;
static volatile float watt_seconds;
static volatile float watt_seconds_charged;
static volatile float position_set;

// Private functions
static void update_override_limits(volatile mc_configuration *conf);

// Function pointers
static void(*pwn_done_func)(void) = 0;

// Threads
static THD_WORKING_AREA(timer_thread_wa, 1024);
static THD_FUNCTION(timer_thread, arg);

void mc_interface_init(mc_configuration *configuration) {
	conf = *configuration;
	fault_now = FAULT_CODE_NONE;
	ignore_iterations = 0;
	cycles_running = 0;
	lock_enabled = false;
	lock_override_once = false;
	motor_current_sum = 0.0;
	input_current_sum = 0.0;
	motor_current_iterations = 0.0;
	input_current_iterations = 0.0;
	amp_seconds = 0.0;
	amp_seconds_charged = 0.0;
	watt_seconds = 0.0;
	watt_seconds_charged = 0.0;
	position_set = 0.0;

	// Start threads
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);

	// Initialize selected implementation
	switch (conf.motor_type) {
		case MOTOR_TYPE_BLDC:
		case MOTOR_TYPE_DC:
			mcpwm_init(&conf);
			break;

		case MOTOR_TYPE_FOC:
			mcpwm_foc_init(&conf);
			break;

		default:
			break;
	}
}

const volatile mc_configuration* mc_interface_get_configuration(void) {
	return &conf;
}

void mc_interface_set_configuration(mc_configuration *configuration) {
	if (conf.motor_type == MOTOR_TYPE_FOC
			&& configuration->motor_type != MOTOR_TYPE_FOC) {
		mcpwm_foc_deinit();
		conf = *configuration;
		mcpwm_init(&conf);
	} else if (conf.motor_type != MOTOR_TYPE_FOC
			&& configuration->motor_type == MOTOR_TYPE_FOC) {
		mcpwm_deinit();
		conf = *configuration;
		mcpwm_foc_init(&conf);
	} else {
		conf = *configuration;
	}

	update_override_limits(&conf);

	switch (conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_configuration(&conf);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_configuration(&conf);
		break;

	default:
		break;
	}
}

bool mc_interface_dccal_done(void) {
	bool ret = false;
	switch (conf.motor_type) {
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
	lock_enabled = true;
}

/**
 * Unlock all control commands.
 */
void mc_interface_unlock(void) {
	lock_enabled = false;
}

/**
 * Allow just one motor control command in the locked state.
 */
void mc_interface_lock_override_once(void) {
	lock_override_once = true;
}

mc_fault_code mc_interface_get_fault(void) {
	return fault_now;
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
	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	position_set = pos;

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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
	float val = amp_seconds / 3600;

	if (reset) {
		amp_seconds = 0.0;
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
	float val = amp_seconds_charged / 3600;

	if (reset) {
		amp_seconds_charged = 0.0;
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
	float val = watt_seconds / 3600;

	if (reset) {
		amp_seconds = 0.0;
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
	float val = watt_seconds_charged / 3600;

	if (reset) {
		watt_seconds_charged = 0.0;
	}

	return val;
}

float mc_interface_get_tot_current(void) {
	float ret = 0.0;

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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

	switch (conf.motor_type) {
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
	float res = motor_current_sum / motor_current_iterations;
	motor_current_sum = 0;
	motor_current_iterations = 0;
	return res;
}

float mc_interface_read_reset_avg_input_current(void) {
	float res = input_current_sum / input_current_iterations;
	input_current_sum = 0;
	input_current_iterations = 0;
	return res;
}

float mc_interface_get_pos_set(void) {
	return position_set;
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
		ignore_iterations = MCPWM_DETECT_STOP_TIME;
	}

	int retval = ignore_iterations;

	if (!ignore_iterations && lock_enabled) {
		if (!lock_override_once) {
			retval = 1;
		} else {
			lock_override_once = false;
		}
	}

	return retval;
}

void mc_interface_fault_stop(mc_fault_code fault) {
	if (mc_interface_dccal_done() && fault_now == FAULT_CODE_NONE) {
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
		fdata.cycles_running = cycles_running;
		fdata.tim_val_samp = val_samp;
		fdata.tim_current_samp = current_samp;
		fdata.tim_top = tim_top;
		fdata.comm_step = mcpwm_get_comm_step();
		fdata.temperature = NTC_TEMP(ADC_IND_TEMP_MOS1);
		terminal_add_fault_data(&fdata);
	}

	ignore_iterations = conf.m_fault_stop_time_ms;

	switch (conf.motor_type) {
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

	fault_now = fault;
}

void mc_interface_mc_timer_isr(void) {
	const float input_voltage = GET_INPUT_VOLTAGE();

	// Check for faults that should stop the motor
	static int wrong_voltage_iterations = 0;
	if (input_voltage < conf.l_min_vin ||
			input_voltage > conf.l_max_vin) {
		wrong_voltage_iterations++;

		if ((wrong_voltage_iterations >= 8)) {
			mc_interface_fault_stop(input_voltage < conf.l_min_vin ?
					FAULT_CODE_UNDER_VOLTAGE : FAULT_CODE_OVER_VOLTAGE);
		}
	} else {
		wrong_voltage_iterations = 0;
	}

	if (mc_interface_get_state() == MC_STATE_RUNNING) {
		cycles_running++;
	} else {
		cycles_running = 0;
	}

	main_dma_adc_handler();

	if (pwn_done_func) {
		pwn_done_func();
	}

	const float current = mc_interface_get_tot_current_filtered();
	const float current_in = mc_interface_get_tot_current_in_filtered();
	motor_current_sum += current;
	input_current_sum += current_in;
	motor_current_iterations++;
	input_current_iterations++;

	float abs_current = mc_interface_get_tot_current();
	float abs_current_filtered = current;
	if (conf.motor_type == MOTOR_TYPE_FOC) {
		// TODO: Make this more general
		abs_current = mcpwm_foc_get_abs_motor_current();
		abs_current_filtered = mcpwm_foc_get_abs_motor_current_filtered();
	}

	// Current fault code
	if (conf.l_slow_abs_current) {
		if (fabsf(abs_current_filtered) > conf.l_abs_current_max) {
			mc_interface_fault_stop(FAULT_CODE_ABS_OVER_CURRENT);
		}
	} else {
		if (fabsf(abs_current) > conf.l_abs_current_max) {
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
				amp_seconds += curr_diff_sum;
				watt_seconds += curr_diff_sum * input_voltage;
			} else {
				amp_seconds_charged -= curr_diff_sum;
				watt_seconds_charged -= curr_diff_sum * input_voltage;
			}

			curr_diff_samples = 0.0;
			curr_diff_sum = 0.0;
		}
	}
}

void mc_interface_adc_inj_int_handler(void) {
	switch (conf.motor_type) {
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
		if (ignore_iterations > 0) {
			ignore_iterations--;
		} else {
			if (!IS_DRV_FAULT()) {
				fault_now = FAULT_CODE_NONE;
			}
		}

		update_override_limits(&conf);

		chThdSleepMilliseconds(1);
	}
}
