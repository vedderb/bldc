/*
	Copyright 2016 - 2018 Benjamin Vedder	benjamin@vedder.se

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
#include "drv8301.h"
#include "drv8320s.h"
#include "drv8323s.h"
#include "buffer.h"
#include "gpdrive.h"
#include "comm_can.h"
#include "shutdown.h"
#include "app.h"
#include "utils.h"

#include <math.h>
#include <stdlib.h>

// Macros
#define DIR_MULT		(m_conf.m_invert_direction ? -1.0 : 1.0)

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
static volatile float m_motor_id_sum;
static volatile float m_motor_iq_sum;
static volatile float m_motor_id_iterations;
static volatile float m_motor_iq_iterations;
static volatile float m_motor_vd_sum;
static volatile float m_motor_vq_sum;
static volatile float m_motor_vd_iterations;
static volatile float m_motor_vq_iterations;
static volatile float m_amp_seconds;
static volatile float m_amp_seconds_charged;
static volatile float m_watt_seconds;
static volatile float m_watt_seconds_charged;
static volatile float m_position_set;
static volatile float m_temp_fet;
static volatile float m_temp_motor;
static volatile float m_gate_driver_voltage;
static volatile float m_motor_current_unbalance;
static volatile float m_motor_current_unbalance_error_rate;

// Sampling variables
#define ADC_SAMPLE_MAX_LEN		2000
__attribute__((section(".ram4"))) static volatile int16_t m_curr0_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_curr1_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_ph1_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_ph2_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_ph3_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_vzero_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile uint8_t m_status_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_curr_fir_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_f_sw_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int8_t m_phase_samples[ADC_SAMPLE_MAX_LEN];

static volatile int m_sample_len;
static volatile int m_sample_int;
static volatile debug_sampling_mode m_sample_mode;
static volatile debug_sampling_mode m_sample_mode_last;
static volatile int m_sample_now;
static volatile int m_sample_trigger;
static volatile float m_last_adc_duration_sample;

#if !WS2811_ENABLE
static app_configuration m_tmp_appconf;
#endif

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
	m_motor_id_sum = 0.0;
	m_motor_iq_sum = 0.0;
	m_motor_id_iterations = 0.0;
	m_motor_iq_iterations = 0.0;
	m_motor_vd_sum = 0.0;
	m_motor_vq_sum = 0.0;
	m_motor_vd_iterations = 0.0;
	m_motor_vq_iterations = 0.0;
	m_amp_seconds = 0.0;
	m_amp_seconds_charged = 0.0;
	m_watt_seconds = 0.0;
	m_watt_seconds_charged = 0.0;
	m_position_set = 0.0;
	m_last_adc_duration_sample = 0.0;
	m_temp_fet = 0.0;
	m_temp_motor = 0.0;
	m_gate_driver_voltage = 0.0;
	m_motor_current_unbalance = 0.0;
	m_motor_current_unbalance_error_rate = 0.0;

	m_sample_len = 1000;
	m_sample_int = 1;
	m_sample_now = 0;
	m_sample_trigger = 0;
	m_sample_mode = DEBUG_SAMPLING_OFF;
	m_sample_mode_last = DEBUG_SAMPLING_OFF;

	// Start threads
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
	chThdCreateStatic(sample_send_thread_wa, sizeof(sample_send_thread_wa), NORMALPRIO - 1, sample_send_thread, NULL);

#ifdef HW_HAS_DRV8301
	drv8301_set_oc_mode(configuration->m_drv8301_oc_mode);
	drv8301_set_oc_adj(configuration->m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8320S)
	drv8320s_set_oc_mode(configuration->m_drv8301_oc_mode);
	drv8320s_set_oc_adj(configuration->m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8323S)
	drv8323s_set_oc_mode(configuration->m_drv8301_oc_mode);
	drv8323s_set_oc_adj(configuration->m_drv8301_oc_adj);
#endif

	// Initialize encoder
#if !WS2811_ENABLE
	switch (m_conf.m_sensor_port_mode) {
	case SENSOR_PORT_MODE_ABI:
		encoder_init_abi(m_conf.m_encoder_counts);
		break;

	case SENSOR_PORT_MODE_AS5047_SPI:
		encoder_init_as5047p_spi();
		break;

	case SENSOR_PORT_MODE_AD2S1205:
		encoder_init_ad2s1205_spi();
		break;

	case SENSOR_PORT_MODE_SINCOS:
		encoder_init_sincos(m_conf.foc_encoder_sin_gain, m_conf.foc_encoder_sin_offset,
							m_conf.foc_encoder_cos_gain, m_conf.foc_encoder_cos_offset,
							m_conf.foc_encoder_sincos_filter_constant);
		break;

	case SENSOR_PORT_MODE_TS5700N8501:
		conf_general_read_app_configuration(&m_tmp_appconf);
		if (m_tmp_appconf.app_to_use == APP_ADC ||
				m_tmp_appconf.app_to_use == APP_UART ||
				m_tmp_appconf.app_to_use == APP_PPM_UART ||
				m_tmp_appconf.app_to_use == APP_ADC_UART) {
			m_tmp_appconf.app_to_use = APP_NONE;
			conf_general_store_app_configuration(&m_tmp_appconf);
		}
		encoder_init_ts5700n8501();
		break;

	default:
		break;
	}
#endif

	// Initialize selected implementation
	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_init(&m_conf);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_init(&m_conf);
		break;

	case MOTOR_TYPE_GPD:
		gpdrive_init(&m_conf);
		break;

	default:
		break;
	}
}

const volatile mc_configuration* mc_interface_get_configuration(void) {
	return &m_conf;
}

void mc_interface_set_configuration(mc_configuration *configuration) {
#if !WS2811_ENABLE
	if (m_conf.m_sensor_port_mode != configuration->m_sensor_port_mode) {
		encoder_deinit();
		switch (configuration->m_sensor_port_mode) {
		case SENSOR_PORT_MODE_ABI:
			encoder_init_abi(configuration->m_encoder_counts);
			break;

		case SENSOR_PORT_MODE_AS5047_SPI:
			encoder_init_as5047p_spi();
			break;

		case SENSOR_PORT_MODE_AD2S1205:
			encoder_init_ad2s1205_spi();
			break;

		case SENSOR_PORT_MODE_SINCOS:
			encoder_init_sincos(m_conf.foc_encoder_sin_gain, m_conf.foc_encoder_sin_offset,
								m_conf.foc_encoder_cos_gain, m_conf.foc_encoder_cos_offset,
								m_conf.foc_encoder_sincos_filter_constant);
			break;

		case SENSOR_PORT_MODE_TS5700N8501: {
			m_tmp_appconf = *app_get_configuration();
			if (m_tmp_appconf.app_to_use == APP_ADC ||
					m_tmp_appconf.app_to_use == APP_UART ||
					m_tmp_appconf.app_to_use == APP_PPM_UART ||
					m_tmp_appconf.app_to_use == APP_ADC_UART) {
				m_tmp_appconf.app_to_use = APP_NONE;
				conf_general_store_app_configuration(&m_tmp_appconf);
				app_set_configuration(&m_tmp_appconf);
			}
			encoder_init_ts5700n8501();
		} break;

		default:
			break;
		}
	}

	if (configuration->m_sensor_port_mode == SENSOR_PORT_MODE_ABI) {
		encoder_set_counts(configuration->m_encoder_counts);
	}
#endif

#ifdef HW_HAS_DRV8301
	drv8301_set_oc_mode(configuration->m_drv8301_oc_mode);
	drv8301_set_oc_adj(configuration->m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8320S)
	drv8320s_set_oc_mode(configuration->m_drv8301_oc_mode);
	drv8320s_set_oc_adj(configuration->m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8323S)
	drv8323s_set_oc_mode(configuration->m_drv8301_oc_mode);
	drv8323s_set_oc_adj(configuration->m_drv8301_oc_adj);
#endif

	if (m_conf.motor_type != configuration->motor_type) {
		mcpwm_deinit();
		mcpwm_foc_deinit();
		gpdrive_deinit();

		m_conf = *configuration;

		switch (m_conf.motor_type) {
		case MOTOR_TYPE_BLDC:
		case MOTOR_TYPE_DC:
			mcpwm_init(&m_conf);
			break;

		case MOTOR_TYPE_FOC:
			mcpwm_foc_init(&m_conf);
			break;

		case MOTOR_TYPE_GPD:
			gpdrive_init(&m_conf);
			break;

		default:
			break;
		}
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

	case MOTOR_TYPE_GPD:
		gpdrive_set_configuration(&m_conf);
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

	case MOTOR_TYPE_GPD:
		ret = gpdrive_is_dccal_done();
		break;

	default:
		break;
	}

	return ret;
}

/**
 * Set a function that should be called after each PWM cycle.
 *
 * Note: this function is called from an interrupt.
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
	case FAULT_CODE_DRV: return "FAULT_CODE_DRV"; break;
	case FAULT_CODE_ABS_OVER_CURRENT: return "FAULT_CODE_ABS_OVER_CURRENT"; break;
	case FAULT_CODE_OVER_TEMP_FET: return "FAULT_CODE_OVER_TEMP_FET"; break;
	case FAULT_CODE_OVER_TEMP_MOTOR: return "FAULT_CODE_OVER_TEMP_MOTOR"; break;
	case FAULT_CODE_GATE_DRIVER_OVER_VOLTAGE: return "FAULT_CODE_GATE_DRIVER_OVER_VOLTAGE"; break;
	case FAULT_CODE_GATE_DRIVER_UNDER_VOLTAGE: return "FAULT_CODE_GATE_DRIVER_UNDER_VOLTAGE"; break;
	case FAULT_CODE_MCU_UNDER_VOLTAGE: return "FAULT_CODE_MCU_UNDER_VOLTAGE"; break;
	case FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET: return "FAULT_CODE_BOOTING_FROM_WATCHDOG_RESET"; break;
	case FAULT_CODE_ENCODER_SPI: return "FAULT_CODE_ENCODER_SPI"; break;
	case FAULT_CODE_ENCODER_SINCOS_BELOW_MIN_AMPLITUDE: return "FAULT_CODE_ENCODER_SINCOS_BELOW_MIN_AMPLITUDE"; break;
	case FAULT_CODE_ENCODER_SINCOS_ABOVE_MAX_AMPLITUDE: return "FAULT_CODE_ENCODER_SINCOS_ABOVE_MAX_AMPLITUDE"; break;
    case FAULT_CODE_FLASH_CORRUPTION: return "FAULT_CODE_FLASH_CORRUPTION";
    case FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_1: return "FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_1";
    case FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_2: return "FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_2";
    case FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_3: return "FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_3";
    case FAULT_CODE_UNBALANCED_CURRENTS: return "FAULT_CODE_UNBALANCED_CURRENTS";
    case FAULT_CODE_BRK: return "FAULT_CODE_BRK";
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
	if (fabsf(dutyCycle) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_duty(DIR_MULT * dutyCycle);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_duty(DIR_MULT * dutyCycle);
		break;

	default:
		break;
	}
}

void mc_interface_set_duty_noramp(float dutyCycle) {
	if (fabsf(dutyCycle) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_duty_noramp(DIR_MULT * dutyCycle);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_duty_noramp(DIR_MULT * dutyCycle);
		break;

	default:
		break;
	}
}

void mc_interface_set_pid_speed(float rpm) {
	if (fabsf(rpm) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_pid_speed(DIR_MULT * rpm);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_pid_speed(DIR_MULT * rpm);
		break;

	default:
		break;
	}
}

void mc_interface_set_pid_pos(float pos) {
	SHUTDOWN_RESET();

	if (mc_interface_try_input()) {
		return;
	}

	m_position_set = pos;

	pos *= DIR_MULT;
	utils_norm_angle(&pos);

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
	if (fabsf(current) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_current(DIR_MULT * current);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_current(DIR_MULT * current);
		break;

	default:
		break;
	}
}

void mc_interface_set_brake_current(float current) {
	if (fabsf(current) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_brake_current(DIR_MULT * current);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_brake_current(DIR_MULT * current);
		break;

	case MOTOR_TYPE_GPD:
		// For timeout to stop the output
		gpdrive_set_mode(GPD_OUTPUT_MODE_NONE);
		break;

	default:
		break;
	}
}

/**
 * Set current relative to the minimum and maximum current limits.
 *
 * @param current
 * The relative current value, range [-1.0 1.0]
 */
void mc_interface_set_current_rel(float val) {
	if (fabsf(val) > 0.001) {
		SHUTDOWN_RESET();
	}

	mc_interface_set_current(val * m_conf.lo_current_motor_max_now);
}

/**
 * Set brake current relative to the minimum current limit.
 *
 * @param current
 * The relative current value, range [0.0 1.0]
 */
void mc_interface_set_brake_current_rel(float val) {
	if (fabsf(val) > 0.001) {
		SHUTDOWN_RESET();
	}

	mc_interface_set_brake_current(val * fabsf(m_conf.lo_current_motor_min_now));
}

/**
 * Set open loop current vector to brake motor.
 *
 * @param current
 * The current value.
 */
void mc_interface_set_handbrake(float current) {
	if (fabsf(current) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		// TODO: Not implemented yet, use brake mode for now.
		mcpwm_set_brake_current(current);
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_handbrake(current);
		break;

	default:
		break;
	}
}

/**
 * Set handbrake brake current relative to the minimum current limit.
 *
 * @param current
 * The relative current value, range [0.0 1.0]
 */
void mc_interface_set_handbrake_rel(float val) {
	if (fabsf(val) > 0.001) {
		SHUTDOWN_RESET();
	}

	mc_interface_set_handbrake(val * fabsf(m_conf.lo_current_motor_min_now));
}

void mc_interface_brake_now(void) {
	SHUTDOWN_RESET();

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

	return DIR_MULT * ret;
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

	return DIR_MULT * ret;
}

float mc_interface_get_sampling_frequency_now(void) {
	float ret = 0.0;

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_get_switching_frequency_now();
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_sampling_frequency_now();
		break;

	case MOTOR_TYPE_GPD:
		ret = gpdrive_get_switching_frequency_now();
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

	return DIR_MULT * ret;
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
		m_watt_seconds = 0.0;
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

	return DIR_MULT * ret;
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

	return DIR_MULT * ret;
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

float mc_interface_get_abs_motor_current_unbalance(void) {
	float ret = 0.0;

#ifdef HW_HAS_3_SHUNTS
	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_get_abs_motor_current_unbalance();
		break;

	default:
		break;
	}
#endif
	return ret;
}

int mc_interface_set_tachometer_value(int steps)
{
	int ret = 0;
	switch (m_conf.motor_type)
	{
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		ret = mcpwm_set_tachometer_value(DIR_MULT * steps);
		break;

	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_set_tachometer_value(DIR_MULT * steps);
		break;

	default:
		break;
	}

	return DIR_MULT * ret;
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

	return DIR_MULT * ret;
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
		ret = mcpwm_foc_get_last_adc_isr_duration();
		break;

	case MOTOR_TYPE_GPD:
		ret = gpdrive_get_last_adc_isr_duration();
		break;

	default:
		break;
	}

	return ret;
}

float mc_interface_read_reset_avg_motor_current(void) {
	if (m_conf.motor_type == MOTOR_TYPE_GPD) {
		return gpdrive_get_current_filtered();
	}

	float res = m_motor_current_sum / m_motor_current_iterations;
	m_motor_current_sum = 0.0;
	m_motor_current_iterations = 0.0;
	return res;
}

float mc_interface_read_reset_avg_input_current(void) {
	if (m_conf.motor_type == MOTOR_TYPE_GPD) {
		return gpdrive_get_current_filtered() * gpdrive_get_modulation();
	}

	float res = m_input_current_sum / m_input_current_iterations;
	m_input_current_sum = 0.0;
	m_input_current_iterations = 0.0;
	return res;
}

/**
 * Read and reset the average direct axis motor current. (FOC only)
 *
 * @return
 * The average D axis current.
 */
float mc_interface_read_reset_avg_id(void) {
	float res = m_motor_id_sum / m_motor_id_iterations;
	m_motor_id_sum = 0.0;
	m_motor_id_iterations = 0.0;
	return DIR_MULT * res; // TODO: DIR_MULT?
}

/**
 * Read and reset the average quadrature axis motor current. (FOC only)
 *
 * @return
 * The average Q axis current.
 */
float mc_interface_read_reset_avg_iq(void) {
	float res = m_motor_iq_sum / m_motor_iq_iterations;
	m_motor_iq_sum = 0.0;
	m_motor_iq_iterations = 0.0;
	return DIR_MULT * res;
}

/**
 * Read and reset the average direct axis motor voltage. (FOC only)
 *
 * @return
 * The average D axis voltage.
 */
float mc_interface_read_reset_avg_vd(void) {
	float res = m_motor_vd_sum / m_motor_vd_iterations;
	m_motor_vd_sum = 0.0;
	m_motor_vd_iterations = 0.0;
	return DIR_MULT * res; // TODO: DIR_MULT?
}

/**
 * Read and reset the average quadrature axis motor voltage. (FOC only)
 *
 * @return
 * The average Q axis voltage.
 */
float mc_interface_read_reset_avg_vq(void) {
	float res = m_motor_vq_sum / m_motor_vq_iterations;
	m_motor_vq_sum = 0.0;
	m_motor_vq_iterations = 0.0;
	return DIR_MULT * res;
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

	ret *= DIR_MULT;
	utils_norm_angle(&ret);

	return ret;
}

float mc_interface_get_last_sample_adc_isr_duration(void) {
	return m_last_adc_duration_sample;
}

void mc_interface_sample_print_data(debug_sampling_mode mode, uint16_t len, uint8_t decimation) {
	if (len > ADC_SAMPLE_MAX_LEN) {
		len = ADC_SAMPLE_MAX_LEN;
	}

	if (mode == DEBUG_SAMPLING_SEND_LAST_SAMPLES) {
		chEvtSignal(sample_send_tp, (eventmask_t) 1);
	} else {
		m_sample_trigger = -1;
		m_sample_now = 0;
		m_sample_len = len;
		m_sample_int = decimation;
		m_sample_mode = mode;
	}
}

/**
 * Get filtered MOSFET temperature. The temperature is pre-calculated, so this
 * functions is fast.
 *
 * @return
 * The filtered MOSFET temperature.
 */
float mc_interface_temp_fet_filtered(void) {
	return m_temp_fet;
}

/**
 * Get filtered motor temperature. The temperature is pre-calculated, so this
 * functions is fast.
 *
 * @return
 * The filtered motor temperature.
 */
float mc_interface_temp_motor_filtered(void) {
	return m_temp_motor;
}

/**
 * Get the battery level, based on battery settings in configuration. Notice that
 * this function is based on remaining watt hours, and not amp hours.
 *
 * @param wh_left
 * Pointer to where to store the remaining watt hours, can be null.
 *
 * @return
 * Battery level, range 0 to 1
 */
float mc_interface_get_battery_level(float *wh_left) {
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const float v_in = GET_INPUT_VOLTAGE();
	float battery_avg_voltage = 0.0;
	float battery_avg_voltage_left = 0.0;
	float ah_left = 0;

	switch (conf->si_battery_type) {
	case BATTERY_TYPE_LIION_3_0__4_2:
		battery_avg_voltage = ((3.2 + 4.2) / 2.0) * (float)(conf->si_battery_cells);
		battery_avg_voltage_left = ((3.2 * (float)(conf->si_battery_cells) + v_in) / 2.0);
		ah_left = utils_map(v_in / (float)(conf->si_battery_cells),
				3.2, 4.2, 0.0, conf->si_battery_ah);
		break;

	case BATTERY_TYPE_LIIRON_2_6__3_6:
		battery_avg_voltage = ((2.8 + 3.6) / 2.0) * (float)(conf->si_battery_cells);
		battery_avg_voltage_left = ((2.8 * (float)(conf->si_battery_cells) + v_in) / 2.0);
		ah_left = utils_map(v_in / (float)(conf->si_battery_cells),
				2.6, 3.6, 0.0, conf->si_battery_ah);
		break;

	case BATTERY_TYPE_LEAD_ACID:
		// TODO: This does not really work for lead-acid batteries
		battery_avg_voltage = ((2.1 + 2.36) / 2.0) * (float)(conf->si_battery_cells);
		battery_avg_voltage_left = ((2.1 * (float)(conf->si_battery_cells) + v_in) / 2.0);
		ah_left = utils_map(v_in / (float)(conf->si_battery_cells),
				2.1, 2.36, 0.0, conf->si_battery_ah);
		break;

	default:
		break;
	}

	const float wh_batt_tot = conf->si_battery_ah * battery_avg_voltage;
	const float wh_batt_left = ah_left * battery_avg_voltage_left;

	if (wh_left) {
		*wh_left = wh_batt_left;
	}

	return wh_batt_left / wh_batt_tot;
}

/**
 * Get the speed based on wheel diameter, gearing and motor pole settings.
 *
 * @return
 * Speed, in m/s
 */
float mc_interface_get_speed(void) {
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const float rpm = mc_interface_get_rpm() / (conf->si_motor_poles / 2.0);
	return (rpm / 60.0) * conf->si_wheel_diameter * M_PI / conf->si_gear_ratio;
}

/**
 * Get the distance traveled based on wheel diameter, gearing and motor pole settings.
 *
 * @return
 * Distance traveled since boot, in meters
 */
float mc_interface_get_distance(void) {
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const float tacho_scale = (conf->si_wheel_diameter * M_PI) / (3.0 * conf->si_motor_poles * conf->si_gear_ratio);
	return mc_interface_get_tachometer_value(false) * tacho_scale;
}

/**
 * Get the absolute distance traveled based on wheel diameter, gearing and motor pole settings.
 *
 * @return
 * Absolute distance traveled since boot, in meters
 */
float mc_interface_get_distance_abs(void) {
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const float tacho_scale = (conf->si_wheel_diameter * M_PI) / (3.0 * conf->si_motor_poles * conf->si_gear_ratio);
	return mc_interface_get_tachometer_abs_value(false) * tacho_scale;
}

setup_values mc_interface_get_setup_values(void) {
	setup_values val = {0};
	val.num_vescs = 1;

	val.ah_tot += mc_interface_get_amp_hours(false);
	val.ah_charge_tot += mc_interface_get_amp_hours_charged(false);
	val.wh_tot += mc_interface_get_watt_hours(false);
	val.wh_charge_tot += mc_interface_get_watt_hours_charged(false);
	val.current_tot += mc_interface_get_tot_current_filtered();
	val.current_in_tot += mc_interface_get_tot_current_in_filtered();

	for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
		can_status_msg *msg = comm_can_get_status_msg_index(i);
		if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < 0.1) {
			val.current_tot += msg->current;
			val.num_vescs++;
		}

		can_status_msg_2 *msg2 = comm_can_get_status_msg_2_index(i);
		if (msg2->id >= 0 && UTILS_AGE_S(msg2->rx_time) < 0.1) {
			val.ah_tot += msg2->amp_hours;
			val.ah_charge_tot += msg2->amp_hours_charged;
		}

		can_status_msg_3 *msg3 = comm_can_get_status_msg_3_index(i);
		if (msg3->id >= 0 && UTILS_AGE_S(msg3->rx_time) < 0.1) {
			val.wh_tot += msg3->watt_hours;
			val.wh_charge_tot += msg3->watt_hours_charged;
		}

		can_status_msg_4 *msg4 = comm_can_get_status_msg_4_index(i);
		if (msg4->id >= 0 && UTILS_AGE_S(msg4->rx_time) < 0.1) {
			val.current_in_tot += msg4->current_in;
		}
	}

	return val;
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

	switch (m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		if (!mcpwm_init_done()) {
			retval = 1;
		}
		break;

	case MOTOR_TYPE_FOC:
		if (!mcpwm_foc_init_done()) {
			retval = 1;
		}
		break;

	default:
		break;
	}

	return retval;
}

void mc_interface_fault_stop(mc_fault_code fault) {
	if (m_fault_now == fault) {
		m_ignore_iterations = m_conf.m_fault_stop_time_ms;
		return;
	}

	if (mc_interface_dccal_done() && m_fault_now == FAULT_CODE_NONE) {
		// Sent to terminal fault logger so that all faults and their conditions
		// can be printed for debugging.
		utils_sys_lock_cnt();
		volatile int val_samp = TIM8->CCR1;
		volatile int current_samp = TIM1->CCR4;
		volatile int tim_top = TIM1->ARR;
		utils_sys_unlock_cnt();

		fault_data fdata;
		fdata.fault = fault;
		fdata.current = mc_interface_get_tot_current();
		fdata.current_filtered = mc_interface_get_tot_current_filtered();
		fdata.voltage = GET_INPUT_VOLTAGE();
		fdata.gate_driver_voltage = m_gate_driver_voltage;
		fdata.duty = mc_interface_get_duty_cycle_now();
		fdata.rpm = mc_interface_get_rpm();
		fdata.tacho = mc_interface_get_tachometer_value(false);
		fdata.cycles_running = m_cycles_running;
		fdata.tim_val_samp = val_samp;
		fdata.tim_current_samp = current_samp;
		fdata.tim_top = tim_top;
		fdata.comm_step = mcpwm_get_comm_step();
		fdata.temperature = NTC_TEMP(ADC_IND_TEMP_MOS);
#ifdef HW_HAS_DRV8301
		if (fault == FAULT_CODE_DRV) {
			fdata.drv8301_faults = drv8301_read_faults();
		}
#elif defined(HW_HAS_DRV8320S)
 		if (fault == FAULT_CODE_DRV) {
			fdata.drv8301_faults = drv8320s_read_faults();
		}
#elif defined(HW_HAS_DRV8323S)
		if (fault == FAULT_CODE_DRV) {
			fdata.drv8301_faults = drv8323s_read_faults();
 		}
#endif
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

	case MOTOR_TYPE_GPD:
		gpdrive_set_mode(GPD_OUTPUT_MODE_NONE);
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

	// Monitor currents balance. The sum of the 3 currents should be zero
#ifdef HW_HAS_3_SHUNTS
	if (!m_conf.foc_sample_high_current) { // This won't work when high current sampling is used
		m_motor_current_unbalance = mc_interface_get_abs_motor_current_unbalance();

		if (m_motor_current_unbalance > MCCONF_MAX_CURRENT_UNBALANCE) {
			UTILS_LP_FAST(m_motor_current_unbalance_error_rate, 1.0, (1 / 20000.0));
		} else {
			UTILS_LP_FAST(m_motor_current_unbalance_error_rate, 0.0, (1 / 20000.0));
		}

		if (m_motor_current_unbalance_error_rate > MCCONF_MAX_CURRENT_UNBALANCE_RATE) {
			mc_interface_fault_stop(FAULT_CODE_UNBALANCED_CURRENTS);
		}
	}
#endif

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

	m_motor_id_sum += mcpwm_foc_get_id();
	m_motor_iq_sum += mcpwm_foc_get_iq();
	m_motor_id_iterations++;
	m_motor_iq_iterations++;

	m_motor_vd_sum += mcpwm_foc_get_vd();
	m_motor_vq_sum += mcpwm_foc_get_vq();
	m_motor_vd_iterations++;
	m_motor_vq_iterations++;

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

	// DRV fault code
	if (IS_DRV_FAULT()) {
		mc_interface_fault_stop(FAULT_CODE_DRV);
	}

#ifdef HW_USE_BRK
	// BRK fault code
	if (TIM_GetFlagStatus(TIM1, TIM_FLAG_Break) != RESET) {
		mc_interface_fault_stop(FAULT_CODE_BRK);
		// latch the BRK/FAULT pin to low until next MCU reset
		palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palClearPad(BRK_GPIO, BRK_PIN);
	}
#endif

#ifdef HW_VERSION_AXIOM
	if( m_gate_driver_voltage > HW_GATE_DRIVER_SUPPLY_MAX_VOLTAGE) {
		mc_interface_fault_stop(FAULT_CODE_GATE_DRIVER_OVER_VOLTAGE);
	}

	if( m_gate_driver_voltage < HW_GATE_DRIVER_SUPPLY_MIN_VOLTAGE) {
		mc_interface_fault_stop(FAULT_CODE_GATE_DRIVER_UNDER_VOLTAGE);
	}
#endif

	// Watt and ah counters
	const float f_samp = mc_interface_get_sampling_frequency_now();
	if (fabsf(current) > 1.0) {
		// Some extra filtering
		static float curr_diff_sum = 0.0;
		static float curr_diff_samples = 0;

		curr_diff_sum += current_in / f_samp;
		curr_diff_samples += 1.0 / f_samp;

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

	bool sample = false;

	switch (m_sample_mode) {
	case DEBUG_SAMPLING_NOW:
		if (m_sample_now == m_sample_len) {
			m_sample_mode = DEBUG_SAMPLING_OFF;
			m_sample_mode_last = DEBUG_SAMPLING_NOW;
			chSysLockFromISR();
			chEvtSignalI(sample_send_tp, (eventmask_t) 1);
			chSysUnlockFromISR();
		} else {
			sample = true;
		}
		break;

	case DEBUG_SAMPLING_START:
		if (mc_interface_get_state() == MC_STATE_RUNNING || m_sample_now > 0) {
			sample = true;
		}

		if (m_sample_now == m_sample_len) {
			m_sample_mode_last = m_sample_mode;
			m_sample_mode = DEBUG_SAMPLING_OFF;
			chSysLockFromISR();
			chEvtSignalI(sample_send_tp, (eventmask_t) 1);
			chSysUnlockFromISR();
		}
		break;

	case DEBUG_SAMPLING_TRIGGER_START:
	case DEBUG_SAMPLING_TRIGGER_START_NOSEND: {
		sample = true;

		int sample_last = -1;
		if (m_sample_trigger >= 0) {
			sample_last = m_sample_trigger - m_sample_len;
			if (sample_last < 0) {
				sample_last += ADC_SAMPLE_MAX_LEN;
			}
		}

		if (m_sample_now == sample_last) {
			m_sample_mode_last = m_sample_mode;
			sample = false;

			if (m_sample_mode == DEBUG_SAMPLING_TRIGGER_START) {
				chSysLockFromISR();
				chEvtSignalI(sample_send_tp, (eventmask_t) 1);
				chSysUnlockFromISR();
			}

			m_sample_mode = DEBUG_SAMPLING_OFF;
		}

		if (mc_interface_get_state() == MC_STATE_RUNNING && m_sample_trigger < 0) {
			m_sample_trigger = m_sample_now;
		}
	} break;

	case DEBUG_SAMPLING_TRIGGER_FAULT:
	case DEBUG_SAMPLING_TRIGGER_FAULT_NOSEND: {
		sample = true;

		int sample_last = -1;
		if (m_sample_trigger >= 0) {
			sample_last = m_sample_trigger - m_sample_len;
			if (sample_last < 0) {
				sample_last += ADC_SAMPLE_MAX_LEN;
			}
		}

		if (m_sample_now == sample_last) {
			m_sample_mode_last = m_sample_mode;
			sample = false;

			if (m_sample_mode == DEBUG_SAMPLING_TRIGGER_FAULT) {
				chSysLockFromISR();
				chEvtSignalI(sample_send_tp, (eventmask_t) 1);
				chSysUnlockFromISR();
			}

			m_sample_mode = DEBUG_SAMPLING_OFF;
		}

		if (m_fault_now != FAULT_CODE_NONE && m_sample_trigger < 0) {
			m_sample_trigger = m_sample_now;
		}
	} break;

	default:
		break;
	}

	if (sample) {
		static int a = 0;
		a++;

		if (a >= m_sample_int) {
			a = 0;

			if (m_sample_now >= ADC_SAMPLE_MAX_LEN) {
				m_sample_now = 0;
			}

			int16_t zero;
			if (m_conf.motor_type == MOTOR_TYPE_FOC) {
				zero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;
				m_phase_samples[m_sample_now] = (uint8_t)(mcpwm_foc_get_phase() / 360.0 * 250.0);
//				m_phase_samples[m_sample_now] = (uint8_t)(mcpwm_foc_get_phase_observer() / 360.0 * 250.0);
//				float ang = utils_angle_difference(mcpwm_foc_get_phase_observer(), mcpwm_foc_get_phase_encoder()) + 180.0;
//				m_phase_samples[m_sample_now] = (uint8_t)(ang / 360.0 * 250.0);
			} else {
				zero = mcpwm_vzero;
				m_phase_samples[m_sample_now] = 0;
			}

			if (mc_interface_get_state() == MC_STATE_DETECTING) {
				m_curr0_samples[m_sample_now] = (int16_t)mcpwm_detect_currents[mcpwm_get_comm_step() - 1];
				m_curr1_samples[m_sample_now] = (int16_t)mcpwm_detect_currents_diff[mcpwm_get_comm_step() - 1];

				m_ph1_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[0];
				m_ph2_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[1];
				m_ph3_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[2];
			} else {
				m_curr0_samples[m_sample_now] = ADC_curr_norm_value[0];
				m_curr1_samples[m_sample_now] = ADC_curr_norm_value[1];

				m_ph1_samples[m_sample_now] = ADC_V_L1 - zero;
				m_ph2_samples[m_sample_now] = ADC_V_L2 - zero;
				m_ph3_samples[m_sample_now] = ADC_V_L3 - zero;
			}

			m_vzero_samples[m_sample_now] = zero;
			m_curr_fir_samples[m_sample_now] = (int16_t)(mc_interface_get_tot_current() * (8.0 / FAC_CURRENT));
			m_f_sw_samples[m_sample_now] = (int16_t)(f_samp / 10.0);
			m_status_samples[m_sample_now] = mcpwm_get_comm_step() | (mcpwm_read_hall_phase() << 3);

			m_sample_now++;

			m_last_adc_duration_sample = mc_interface_get_last_inj_adc_isr_duration();
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
	const float v_in = GET_INPUT_VOLTAGE();
	const float rpm_now = mc_interface_get_rpm();

	UTILS_LP_FAST(m_temp_fet, NTC_TEMP(ADC_IND_TEMP_MOS), 0.1);
	float temp_motor = 0.0;

	switch(conf->m_motor_temp_sens_type) {
	case TEMP_SENSOR_NTC_10K_25C:
		m_temp_motor = NTC_TEMP_MOTOR(conf->m_ntc_motor_beta);
		break;

	case TEMP_SENSOR_PTC_1K_100C:
		temp_motor = PTC_TEMP_MOTOR(1000.0, conf->m_ptc_motor_coeff, 100);
		break;

	case TEMP_SENSOR_KTY83_122: {
		// KTY83_122 datasheet used to approximate resistance at given temperature to cubic polynom
		// https://docs.google.com/spreadsheets/d/1iJA66biczfaXRNClSsrVF9RJuSAKoDG-bnRZFMOcuwU/edit?usp=sharing
		// Thanks to: https://vasilisks.wordpress.com/2017/12/14/getting-temperature-from-ntc-kty83-kty84-on-mcu/#more-645
		// You can change pull up resistor and update NTC_RES_MOTOR for your hardware without changing polynom
		float res = NTC_RES_MOTOR(ADC_Value[ADC_IND_TEMP_MOTOR]);
		float pow2 = res * res;
		temp_motor = 0.0000000102114874947423 * pow2 * res - 0.000069967997703501 * pow2 +
				0.243402040973194 * res - 160.145048329356;
	}
	break;
	}

	// If the reading is messed up (by e.g. reading 0 on the ADC and dividing by 0) we avoid putting an
	// invalid value in the filter, as it will never recover. It is probably safest to keep running the
	// motor even if the temperature reading fails. A config option to reduce power on invalid temperature
	// readings might be useful.
	if (UTILS_IS_NAN(temp_motor) || UTILS_IS_INF(temp_motor) || temp_motor > 600.0 || temp_motor < -200.0) {
		temp_motor = -100.0;
	}

	UTILS_LP_FAST(m_temp_motor, temp_motor, 0.1);

#ifdef HW_VERSION_AXIOM
	UTILS_LP_FAST(m_gate_driver_voltage, GET_GATE_DRIVER_SUPPLY_VOLTAGE(), 0.01);
#endif

	const float l_current_min_tmp = conf->l_current_min * conf->l_current_min_scale;
	const float l_current_max_tmp = conf->l_current_max * conf->l_current_max_scale;

	// Temperature MOSFET
	float lo_min_mos = l_current_min_tmp;
	float lo_max_mos = l_current_max_tmp;
	if (m_temp_fet < conf->l_temp_fet_start) {
		// Keep values
	} else if (m_temp_fet > conf->l_temp_fet_end) {
		lo_min_mos = 0.0;
		lo_max_mos = 0.0;
		mc_interface_fault_stop(FAULT_CODE_OVER_TEMP_FET);
	} else {
		float maxc = fabsf(l_current_max_tmp);
		if (fabsf(l_current_min_tmp) > maxc) {
			maxc = fabsf(l_current_min_tmp);
		}

		maxc = utils_map(m_temp_fet, conf->l_temp_fet_start, conf->l_temp_fet_end, maxc, 0.0);

		if (fabsf(l_current_min_tmp) > maxc) {
			lo_min_mos = SIGN(l_current_min_tmp) * maxc;
		}

		if (fabsf(l_current_max_tmp) > maxc) {
			lo_max_mos = SIGN(l_current_max_tmp) * maxc;
		}
	}

	// Temperature MOTOR
	float lo_min_mot = l_current_min_tmp;
	float lo_max_mot = l_current_max_tmp;
	if (m_temp_motor < conf->l_temp_motor_start) {
		// Keep values
	} else if (m_temp_motor > conf->l_temp_motor_end) {
		lo_min_mot = 0.0;
		lo_max_mot = 0.0;
		mc_interface_fault_stop(FAULT_CODE_OVER_TEMP_MOTOR);
	} else {
		float maxc = fabsf(l_current_max_tmp);
		if (fabsf(l_current_min_tmp) > maxc) {
			maxc = fabsf(l_current_min_tmp);
		}

		maxc = utils_map(m_temp_motor, conf->l_temp_motor_start, conf->l_temp_motor_end, maxc, 0.0);

		if (fabsf(l_current_min_tmp) > maxc) {
			lo_min_mot = SIGN(l_current_min_tmp) * maxc;
		}

		if (fabsf(l_current_max_tmp) > maxc) {
			lo_max_mot = SIGN(l_current_max_tmp) * maxc;
		}
	}

	// Decreased temperatures during acceleration
	// in order to still have braking torque available
	const float temp_fet_accel_start = utils_map(conf->l_temp_accel_dec, 0.0, 1.0, conf->l_temp_fet_start, 25.0);
	const float temp_fet_accel_end = utils_map(conf->l_temp_accel_dec, 0.0, 1.0, conf->l_temp_fet_end, 25.0);
	const float temp_motor_accel_start = utils_map(conf->l_temp_accel_dec, 0.0, 1.0, conf->l_temp_motor_start, 25.0);
	const float temp_motor_accel_end = utils_map(conf->l_temp_accel_dec, 0.0, 1.0, conf->l_temp_motor_end, 25.0);

	float lo_fet_temp_accel = 0.0;
	if (m_temp_fet < temp_fet_accel_start) {
		lo_fet_temp_accel = l_current_max_tmp;
	} else if (m_temp_fet > temp_fet_accel_end) {
		lo_fet_temp_accel = 0.0;
	} else {
		lo_fet_temp_accel = utils_map(m_temp_fet, temp_fet_accel_start,
				temp_fet_accel_end, l_current_max_tmp, 0.0);
	}

	float lo_motor_temp_accel = 0.0;
	if (m_temp_motor < temp_motor_accel_start) {
		lo_motor_temp_accel = l_current_max_tmp;
	} else if (m_temp_motor > temp_motor_accel_end) {
		lo_motor_temp_accel = 0.0;
	} else {
		lo_motor_temp_accel = utils_map(m_temp_motor, temp_motor_accel_start,
				temp_motor_accel_end, l_current_max_tmp, 0.0);
	}

	// RPM max
	float lo_max_rpm = 0.0;
	const float rpm_pos_cut_start = conf->l_max_erpm * conf->l_erpm_start;
	const float rpm_pos_cut_end = conf->l_max_erpm;
	if (rpm_now < rpm_pos_cut_start) {
		lo_max_rpm = l_current_max_tmp;
	} else if (rpm_now > rpm_pos_cut_end) {
		lo_max_rpm = 0.0;
	} else {
		lo_max_rpm = utils_map(rpm_now, rpm_pos_cut_start, rpm_pos_cut_end, l_current_max_tmp, 0.0);
	}

	// RPM min
	float lo_min_rpm = 0.0;
	const float rpm_neg_cut_start = conf->l_min_erpm * conf->l_erpm_start;
	const float rpm_neg_cut_end = conf->l_min_erpm;
	if (rpm_now > rpm_neg_cut_start) {
		lo_min_rpm = l_current_max_tmp;
	} else if (rpm_now < rpm_neg_cut_end) {
		lo_min_rpm = 0.0;
	} else {
		lo_min_rpm = utils_map(rpm_now, rpm_neg_cut_start, rpm_neg_cut_end, l_current_max_tmp, 0.0);
	}

	float lo_max = utils_min_abs(lo_max_mos, lo_max_mot);
	float lo_min = utils_min_abs(lo_min_mos, lo_min_mot);

	lo_max = utils_min_abs(lo_max, lo_max_rpm);
	lo_max = utils_min_abs(lo_max, lo_min_rpm);
	lo_max = utils_min_abs(lo_max, lo_fet_temp_accel);
	lo_max = utils_min_abs(lo_max, lo_motor_temp_accel);

	if (lo_max < conf->cc_min_current) {
		lo_max = conf->cc_min_current;
	}

	if (lo_min > -conf->cc_min_current) {
		lo_min = -conf->cc_min_current;
	}

	conf->lo_current_max = lo_max;
	conf->lo_current_min = lo_min;

	// Battery cutoff
	float lo_in_max_batt = 0.0;
	if (v_in > conf->l_battery_cut_start) {
		lo_in_max_batt = conf->l_in_current_max;
	} else if (v_in < conf->l_battery_cut_end) {
		lo_in_max_batt = 0.0;
	} else {
		lo_in_max_batt = utils_map(v_in, conf->l_battery_cut_start,
				conf->l_battery_cut_end, conf->l_in_current_max, 0.0);
	}

	// Wattage limits
	const float lo_in_max_watt = conf->l_watt_max / v_in;
	const float lo_in_min_watt = conf->l_watt_min / v_in;

	const float lo_in_max = utils_min_abs(lo_in_max_watt, lo_in_max_batt);
	const float lo_in_min = lo_in_min_watt;

	conf->lo_in_current_max = utils_min_abs(conf->l_in_current_max, lo_in_max);
	conf->lo_in_current_min = utils_min_abs(conf->l_in_current_min, lo_in_min);

	// Maximum current right now
//	float duty_abs = fabsf(mc_interface_get_duty_cycle_now());
//
//	// TODO: This is not an elegant solution.
//	if (m_conf.motor_type == MOTOR_TYPE_FOC) {
//		duty_abs *= SQRT3_BY_2;
//	}
//
//	if (duty_abs > 0.001) {
//		conf->lo_current_motor_max_now = utils_min_abs(conf->lo_current_max, conf->lo_in_current_max / duty_abs);
//		conf->lo_current_motor_min_now = utils_min_abs(conf->lo_current_min, conf->lo_in_current_min / duty_abs);
//	} else {
//		conf->lo_current_motor_max_now = conf->lo_current_max;
//		conf->lo_current_motor_min_now = conf->lo_current_min;
//	}

	// Note: The above code should work, but many people have reported issues with it. Leaving it
	// disabled for now until I have done more investigation.
	conf->lo_current_motor_max_now = conf->lo_current_max;
	conf->lo_current_motor_min_now = conf->lo_current_min;
}

static THD_FUNCTION(timer_thread, arg) {
	(void)arg;

	chRegSetThreadName("mcif timer");

	for(;;) {
		// Decrease fault iterations
		if (m_ignore_iterations > 0) {
			m_ignore_iterations--;
		} else {
			if (!IS_DRV_FAULT()) {
				m_fault_now = FAULT_CODE_NONE;
			}
		}

		update_override_limits(&m_conf);

		// Update auxiliary output
		switch (m_conf.m_out_aux_mode) {
			case OUT_AUX_MODE_OFF:
				AUX_OFF();
				break;

			case OUT_AUX_MODE_ON_AFTER_2S:
				if (chVTGetSystemTimeX() >= MS2ST(2000)) {
					AUX_ON();
				}
				break;

			case OUT_AUX_MODE_ON_AFTER_5S:
				if (chVTGetSystemTimeX() >= MS2ST(5000)) {
					AUX_ON();
				}
				break;

			case OUT_AUX_MODE_ON_AFTER_10S:
				if (chVTGetSystemTimeX() >= MS2ST(10000)) {
					AUX_ON();
				}
				break;

			default:
				break;
		}


		// Trigger encoder error rate fault, using 1% errors as threshold.
		// Relevant only in FOC mode with encoder enabled
		if(m_conf.motor_type == MOTOR_TYPE_FOC &&
			m_conf.foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			mcpwm_foc_is_using_encoder() &&
			encoder_spi_get_error_rate() > 0.05) {
			mc_interface_fault_stop(FAULT_CODE_ENCODER_SPI);
		}

		if(m_conf.motor_type == MOTOR_TYPE_FOC &&
			m_conf.foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			m_conf.m_sensor_port_mode == SENSOR_PORT_MODE_SINCOS) {

			if (encoder_sincos_get_signal_below_min_error_rate() > 0.05)
				mc_interface_fault_stop(FAULT_CODE_ENCODER_SINCOS_BELOW_MIN_AMPLITUDE);
			if (encoder_sincos_get_signal_above_max_error_rate() > 0.05)
				mc_interface_fault_stop(FAULT_CODE_ENCODER_SINCOS_ABOVE_MAX_AMPLITUDE);
		}

		if(m_conf.motor_type == MOTOR_TYPE_FOC &&
			m_conf.foc_sensor_mode == FOC_SENSOR_MODE_ENCODER &&
			m_conf.m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205) {
			if (encoder_resolver_loss_of_tracking_error_rate() > 0.05)
				mc_interface_fault_stop(FAULT_CODE_RESOLVER_LOT);
			if (encoder_resolver_degradation_of_signal_error_rate() > 0.05)
				mc_interface_fault_stop(FAULT_CODE_RESOLVER_DOS);
			if (encoder_resolver_loss_of_signal_error_rate() > 0.04)
				mc_interface_fault_stop(FAULT_CODE_RESOLVER_LOS);
		}
		// TODO: Implement for BLDC and GPDRIVE
		if(m_conf.motor_type == MOTOR_TYPE_FOC) {
			int curr0_offset;
			int curr1_offset;
			int curr2_offset;

			mcpwm_foc_get_current_offsets(&curr0_offset, &curr1_offset, &curr2_offset);

			if (abs(curr0_offset - 2048) > HW_MAX_CURRENT_OFFSET) {
				mc_interface_fault_stop(FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_1);
			}
			if (abs(curr1_offset - 2048) > HW_MAX_CURRENT_OFFSET) {
				mc_interface_fault_stop(FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_2);
			}
#ifdef HW_HAS_3_SHUNTS
			if (abs(curr2_offset - 2048) > HW_MAX_CURRENT_OFFSET) {
				mc_interface_fault_stop(FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_3);
			}
#endif
		}

		chThdSleepMilliseconds(1);
	}
}

static THD_FUNCTION(sample_send_thread, arg) {
	(void)arg;

	chRegSetThreadName("SampleSender");

	sample_send_tp = chThdGetSelfX();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		int len = 0;
		int offset = 0;

		switch (m_sample_mode_last) {
		case DEBUG_SAMPLING_NOW:
		case DEBUG_SAMPLING_START:
			len = m_sample_len;
			break;

		case DEBUG_SAMPLING_TRIGGER_START:
		case DEBUG_SAMPLING_TRIGGER_FAULT:
		case DEBUG_SAMPLING_TRIGGER_START_NOSEND:
		case DEBUG_SAMPLING_TRIGGER_FAULT_NOSEND:
			len = ADC_SAMPLE_MAX_LEN;
			offset = m_sample_trigger - m_sample_len;
			break;

		default:
			break;
		}

		for (int i = 0;i < len;i++) {
			uint8_t buffer[40];
			int32_t index = 0;
			int ind_samp = i + offset;

			while (ind_samp >= ADC_SAMPLE_MAX_LEN) {
				ind_samp -= ADC_SAMPLE_MAX_LEN;
			}

			while (ind_samp < 0) {
				ind_samp += ADC_SAMPLE_MAX_LEN;
			}

			buffer[index++] = COMM_SAMPLE_PRINT;
			buffer_append_float32_auto(buffer, (float)m_curr0_samples[ind_samp] * FAC_CURRENT, &index);
			buffer_append_float32_auto(buffer, (float)m_curr1_samples[ind_samp] * FAC_CURRENT, &index);
			buffer_append_float32_auto(buffer, ((float)m_ph1_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2), &index);
			buffer_append_float32_auto(buffer, ((float)m_ph2_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2), &index);
			buffer_append_float32_auto(buffer, ((float)m_ph3_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2), &index);
			buffer_append_float32_auto(buffer, ((float)m_vzero_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2), &index);
			buffer_append_float32_auto(buffer, (float)m_curr_fir_samples[ind_samp] / (8.0 / FAC_CURRENT), &index);
			buffer_append_float32_auto(buffer, (float)m_f_sw_samples[ind_samp] * 10.0, &index);
			buffer[index++] = m_status_samples[ind_samp];
			buffer[index++] = m_phase_samples[ind_samp];

			commands_send_packet(buffer, index);
		}
	}
}
