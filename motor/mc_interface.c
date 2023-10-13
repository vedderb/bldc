/*
	Copyright 2016 - 2022 Benjamin Vedder	benjamin@vedder.se

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
#include "utils_math.h"
#include "utils_sys.h"
#include "ch.h"
#include "hal.h"
#include "commands.h"
#include "encoder/encoder.h"
#include "buffer.h"
#include "gpdrive.h"
#include "comm_can.h"
#include "shutdown.h"
#include "app.h"
#include "mempools.h"
#include "crc.h"
#include "bms.h"
#include "events.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

// Macros
#define DIR_MULT		(motor_now()->m_conf.m_invert_direction ? -1.0 : 1.0)

// Global variables
volatile uint16_t ADC_Value[HW_ADC_CHANNELS + HW_ADC_CHANNELS_EXTRA];
volatile float ADC_curr_norm_value[6];

typedef struct {
	mc_configuration m_conf;
	mc_fault_code m_fault_now;
	setup_stats m_stats;
	int m_ignore_iterations;
	int m_drv_fault_iterations;
	unsigned int m_cycles_running;
	bool m_lock_enabled;
	bool m_lock_override_once;
	float m_motor_current_sum;
	float m_input_current_sum;
	float m_motor_current_iterations;
	float m_input_current_iterations;
	float m_motor_id_sum;
	float m_motor_iq_sum;
	float m_motor_id_iterations;
	float m_motor_iq_iterations;
	float m_motor_vd_sum;
	float m_motor_vq_sum;
	float m_motor_vd_iterations;
	float m_motor_vq_iterations;
	float m_amp_seconds;
	float m_amp_seconds_charged;
	float m_watt_seconds;
	float m_watt_seconds_charged;
	float m_position_set;
	float m_temp_fet;
	float m_temp_motor;
	float m_gate_driver_voltage;
	float m_motor_current_unbalance;
	float m_motor_current_unbalance_error_rate;
	float m_f_samp_now;
	float m_input_voltage_filtered;
	float m_input_voltage_filtered_slower;
	float m_temp_override;
	float m_i_in_filter;

	// Backup data counters
	uint64_t m_odometer_last;
	uint64_t m_runtime_last;
} motor_if_state_t;

// Private variables
static volatile motor_if_state_t m_motor_1;
#ifdef HW_HAS_DUAL_MOTORS
static volatile motor_if_state_t m_motor_2;
#endif

// Sampling variables
#define ADC_SAMPLE_MAX_LEN		1600
__attribute__((section(".ram4"))) static volatile int16_t m_curr0_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_curr1_samples[ADC_SAMPLE_MAX_LEN];
__attribute__((section(".ram4"))) static volatile int16_t m_curr2_samples[ADC_SAMPLE_MAX_LEN];
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
static volatile bool m_sample_raw;
static volatile debug_sampling_mode m_sample_mode;
static volatile debug_sampling_mode m_sample_mode_last;
static volatile int m_sample_now;
static volatile int m_sample_trigger;
static volatile float m_last_adc_duration_sample;
static volatile bool m_sample_is_second_motor;
static volatile gnss_data m_gnss = {0};

typedef struct {
	bool is_second_motor;
	mc_fault_code fault_code;
	const char *info_str;
	int info_argn;
	float info_args[2];
} fault_data_local;

static volatile fault_data_local m_fault_data = {0, FAULT_CODE_NONE, 0, 0, {0, 0}};

// Private functions
static void update_override_limits(volatile motor_if_state_t *motor, volatile mc_configuration *conf);
static void run_timer_tasks(volatile motor_if_state_t *motor);
static void update_stats(volatile motor_if_state_t *motor);
static volatile motor_if_state_t *motor_now(void);

// Function pointers
static void(*pwn_done_func)(void) = 0;
static void(* volatile send_func_sample)(unsigned char *data, unsigned int len) = 0;

// Threads
static THD_WORKING_AREA(timer_thread_wa, 512);
static THD_FUNCTION(timer_thread, arg);
static THD_WORKING_AREA(sample_send_thread_wa, 512);
static THD_FUNCTION(sample_send_thread, arg);
static thread_t *sample_send_tp;
static THD_WORKING_AREA(fault_stop_thread_wa, 512);
static THD_FUNCTION(fault_stop_thread, arg);
static thread_t *fault_stop_tp;
static THD_WORKING_AREA(stat_thread_wa, 512);
static THD_FUNCTION(stat_thread, arg);

void mc_interface_init(void) {
	memset((void*)&m_motor_1, 0, sizeof(motor_if_state_t));
#ifdef HW_HAS_DUAL_MOTORS
	memset((void*)&m_motor_2, 0, sizeof(motor_if_state_t));
#endif

	conf_general_read_mc_configuration((mc_configuration*)&m_motor_1.m_conf, false);
#ifdef HW_HAS_DUAL_MOTORS
	conf_general_read_mc_configuration((mc_configuration*)&m_motor_2.m_conf, true);
#endif

#ifdef HW_HAS_DUAL_MOTORS
	m_motor_1.m_conf.motor_type = MOTOR_TYPE_FOC;
	m_motor_2.m_conf.motor_type = MOTOR_TYPE_FOC;
#endif

	m_last_adc_duration_sample = 0.0;
	m_sample_len = 1000;
	m_sample_int = 1;
	m_sample_now = 0;
	m_sample_raw = false;
	m_sample_trigger = 0;
	m_sample_mode = DEBUG_SAMPLING_OFF;
	m_sample_mode_last = DEBUG_SAMPLING_OFF;
	m_sample_is_second_motor = false;

	mc_interface_stat_reset();

	// Start threads
	chThdCreateStatic(timer_thread_wa, sizeof(timer_thread_wa), NORMALPRIO, timer_thread, NULL);
	chThdCreateStatic(sample_send_thread_wa, sizeof(sample_send_thread_wa), NORMALPRIO - 1, sample_send_thread, NULL);
	chThdCreateStatic(fault_stop_thread_wa, sizeof(fault_stop_thread_wa), HIGHPRIO - 3, fault_stop_thread, NULL);
	chThdCreateStatic(stat_thread_wa, sizeof(stat_thread_wa), NORMALPRIO, stat_thread, NULL);

	int motor_old = mc_interface_get_motor_thread();
	mc_interface_select_motor_thread(1);
#ifdef HW_HAS_DRV8301
	drv8301_set_oc_mode(motor_now()->m_conf.m_drv8301_oc_mode);
	drv8301_set_oc_adj(motor_now()->m_conf.m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8320S)
	drv8320s_set_oc_mode(motor_now()->m_conf.m_drv8301_oc_mode);
	drv8320s_set_oc_adj(motor_now()->m_conf.m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8323S)
	drv8323s_set_oc_mode(motor_now()->m_conf.m_drv8301_oc_mode);
	drv8323s_set_oc_adj(motor_now()->m_conf.m_drv8301_oc_adj);
	DRV8323S_CUSTOM_SETTINGS();
#endif

#if defined HW_HAS_DUAL_MOTORS || defined HW_HAS_DUAL_PARALLEL
	mc_interface_select_motor_thread(2);
#ifdef HW_HAS_DRV8301
	drv8301_set_oc_mode(motor_now()->m_conf.m_drv8301_oc_mode);
	drv8301_set_oc_adj(motor_now()->m_conf.m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8320S)
	drv8320s_set_oc_mode(motor_now()->m_conf.m_drv8301_oc_mode);
	drv8320s_set_oc_adj(motor_now()->m_conf.m_drv8301_oc_adj);
#elif defined(HW_HAS_DRV8323S)
	drv8323s_set_oc_mode(motor_now()->m_conf.m_drv8301_oc_mode);
	drv8323s_set_oc_adj(motor_now()->m_conf.m_drv8301_oc_adj);
	DRV8323S_CUSTOM_SETTINGS();
#endif
#endif
	mc_interface_select_motor_thread(motor_old);

	encoder_init(&motor_now()->m_conf);

	// Initialize selected implementation
	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_init(&motor_now()->m_conf);
		break;

	case MOTOR_TYPE_FOC:
#ifdef HW_HAS_DUAL_MOTORS
		mcpwm_foc_init((mc_configuration*)&m_motor_1.m_conf, (mc_configuration*)&m_motor_2.m_conf);
#else
		mcpwm_foc_init((mc_configuration*)&m_motor_1.m_conf, (mc_configuration*)&m_motor_1.m_conf);
#endif
		break;

	case MOTOR_TYPE_GPD:
		gpdrive_init(&motor_now()->m_conf);
		break;

	default:
		break;
	}

	bms_init((bms_config*)&m_motor_1.m_conf.bms);
}

int mc_interface_motor_now(void) {
#if defined HW_HAS_DUAL_MOTORS || defined HW_HAS_DUAL_PARALLEL
	int isr_motor = mcpwm_foc_isr_motor();
	int thd_motor = chThdGetSelfX()->motor_selected;

	if (isr_motor > 0) {
		return isr_motor;
	} else if (thd_motor > 0) {
		return thd_motor;
	} else {
		return 1;
	}
#else
	return 1;
#endif
}

/**
 * Select motor for current thread. When a thread has a motor selected,
 * the mc_interface functions will use that motor for that thread. This
 * is only relevant for dual motor hardware.
 *
 * @param motor
 * 0: no specific motor selected, the last motor will be used.
 * 1: motor 1 selected (default).
 * 2: motor 2 selected.
 */
void mc_interface_select_motor_thread(int motor) {
#if defined HW_HAS_DUAL_MOTORS || defined HW_HAS_DUAL_PARALLEL
	if (motor == 0 || motor == 1 || motor == 2) {
		chThdGetSelfX()->motor_selected = motor;
	}
#else
	(void)motor;
#endif
}

/**
 * Get the motor selected for the current thread.
 *
 * @return
 * 0: no specific motor selected, the last motor will be used.
 * 1: motor 1 selected (default).
 * 2: motor 2 selected.
 */
int mc_interface_get_motor_thread(void) {
	return chThdGetSelfX()->motor_selected;
}

const volatile mc_configuration* mc_interface_get_configuration(void) {
	return &motor_now()->m_conf;
}

void mc_interface_set_configuration(mc_configuration *configuration) {
	volatile motor_if_state_t *motor = motor_now();

#if defined HW_HAS_DUAL_PARALLEL
	configuration->motor_type = MOTOR_TYPE_FOC;
#else
#ifdef HW_HAS_DUAL_MOTORS
#ifndef HW_SET_SINGLE_MOTOR
	configuration->motor_type = MOTOR_TYPE_FOC;
#endif
#endif
#endif

	if (motor->m_conf.m_sensor_port_mode != configuration->m_sensor_port_mode) {
		encoder_deinit();
		encoder_init(configuration);
	} else {
		encoder_update_config(configuration);
	}

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

#ifdef HW_HAS_DUAL_PARALLEL
	mc_interface_select_motor_thread(2);
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
	mc_interface_select_motor_thread(1);
#endif

	if (motor->m_conf.motor_type != configuration->motor_type) {
		mcpwm_deinit();
		mcpwm_foc_deinit();
		gpdrive_deinit();

#ifdef HW_SET_SINGLE_MOTOR
		if (configuration->motor_type == MOTOR_TYPE_FOC) {
			hw_init_gpio();
		} else {
			HW_SET_SINGLE_MOTOR();
		}
#endif

		motor->m_conf = *configuration;

		switch (motor->m_conf.motor_type) {
		case MOTOR_TYPE_BLDC:
		case MOTOR_TYPE_DC:
			mcpwm_init(&motor->m_conf);
			break;

		case MOTOR_TYPE_FOC:
#ifdef HW_HAS_DUAL_MOTORS
			mcpwm_foc_init((mc_configuration*)&m_motor_1.m_conf, (mc_configuration*)&m_motor_2.m_conf);
#else
			mcpwm_foc_init((mc_configuration*)&m_motor_1.m_conf, (mc_configuration*)&m_motor_1.m_conf);
#endif
			break;

		case MOTOR_TYPE_GPD:
			gpdrive_init(&motor->m_conf);
			break;

		default:
			break;
		}
	} else {
		motor->m_conf = *configuration;
	}

	update_override_limits(motor, &motor->m_conf);

	switch (motor->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_set_configuration(&motor->m_conf);
		break;

	case MOTOR_TYPE_FOC:
#ifdef HW_HAS_DUAL_MOTORS
		if (motor == &m_motor_1) {
			m_motor_2.m_conf.foc_f_zv = motor->m_conf.foc_f_zv;
			m_motor_2.m_conf.motor_type = motor->m_conf.motor_type;
		} else {
			m_motor_1.m_conf.foc_f_zv = motor->m_conf.foc_f_zv;
			m_motor_1.m_conf.motor_type = motor->m_conf.motor_type;
		}
#endif
		mcpwm_foc_set_configuration((mc_configuration*)&motor->m_conf);
		break;

	case MOTOR_TYPE_GPD:
		gpdrive_set_configuration(&motor->m_conf);
		break;

	default:
		break;
	}

	bms_init(&configuration->bms);
}

bool mc_interface_dccal_done(void) {
	bool ret = false;
	switch (motor_now()->m_conf.motor_type) {
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
	motor_now()->m_lock_enabled = true;
}

/**
 * Unlock all control commands.
 */
void mc_interface_unlock(void) {
	motor_now()->m_lock_enabled = false;
}

/**
 * Allow just one motor control command in the locked state.
 */
void mc_interface_lock_override_once(void) {
	motor_now()->m_lock_override_once = true;
}

mc_fault_code mc_interface_get_fault(void) {
	return motor_now()->m_fault_now;
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
    case FAULT_CODE_FLASH_CORRUPTION_APP_CFG: return "FAULT_CODE_FLASH_CORRUPTION_APP_CFG";
    case FAULT_CODE_FLASH_CORRUPTION_MC_CFG: return "FAULT_CODE_FLASH_CORRUPTION_MC_CFG";
    case FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_1: return "FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_1";
    case FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_2: return "FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_2";
    case FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_3: return "FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_3";
    case FAULT_CODE_UNBALANCED_CURRENTS: return "FAULT_CODE_UNBALANCED_CURRENTS";
    case FAULT_CODE_BRK: return "FAULT_CODE_BRK";
    case FAULT_CODE_RESOLVER_LOT: return "FAULT_CODE_RESOLVER_LOT";
    case FAULT_CODE_RESOLVER_DOS: return "FAULT_CODE_RESOLVER_DOS";
    case FAULT_CODE_RESOLVER_LOS: return "FAULT_CODE_RESOLVER_LOS";
    case FAULT_CODE_ENCODER_NO_MAGNET: return "FAULT_CODE_ENCODER_NO_MAGNET";
    case FAULT_CODE_ENCODER_MAGNET_TOO_STRONG: return "FAULT_CODE_ENCODER_MAGNET_TOO_STRONG";
    case FAULT_CODE_PHASE_FILTER: return "FAULT_CODE_PHASE_FILTER";
    case FAULT_CODE_ENCODER_FAULT: return "FAULT_CODE_ENCODER_FAULT";
	case FAULT_CODE_LV_OUTPUT_FAULT: return "FAULT_CODE_LV_OUTPUT_FAULT";
	}

	return "Unknown fault";
}

mc_state mc_interface_get_state(void) {
	mc_state ret = MC_STATE_OFF;
	switch (motor_now()->m_conf.motor_type) {
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

mc_control_mode mc_interface_get_control_mode(void) {
	mc_control_mode ret = CONTROL_MODE_NONE;
	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_FOC:
		ret = mcpwm_foc_control_mode();
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

	switch (motor_now()->m_conf.motor_type) {
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

	events_add("set_duty", dutyCycle);
}

void mc_interface_set_duty_noramp(float dutyCycle) {
	if (fabsf(dutyCycle) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
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

	events_add("set_duty_noramp", dutyCycle);
}

void mc_interface_set_pid_speed(float rpm) {
	if (fabsf(rpm) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
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

	events_add("set_pid_speed", rpm);
}

void mc_interface_set_pid_pos(float pos) {
	SHUTDOWN_RESET();

	if (mc_interface_try_input()) {
		return;
	}

	volatile mc_configuration *conf = &motor_now()->m_conf;

	motor_now()->m_position_set = pos;

	pos += motor_now()->m_conf.p_pid_offset;
	pos *= DIR_MULT;

	if (encoder_is_configured()) {
		if (conf->foc_encoder_inverted) {
			pos *= -1.0;
		}
	}

	utils_norm_angle(&pos);

	switch (conf->motor_type) {
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

	events_add("set_pid_pos", pos);
}

void mc_interface_set_current(float current) {
	if (fabsf(current) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
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

	events_add("set_current", current);
}

void mc_interface_set_brake_current(float current) {
	if (fabsf(current) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
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

	events_add("set_current_brake", current);
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

	volatile mc_configuration *cfg = &motor_now()->m_conf;
	float duty = mc_interface_get_duty_cycle_now();

	if (fabsf(duty) < 0.02 || SIGN(val) == SIGN(duty)) {
		mc_interface_set_current(val * cfg->lo_current_max);
	} else {
		mc_interface_set_current(val * fabsf(cfg->lo_current_min));
	}

	if (fabsf(val * cfg->l_abs_current_max) > cfg->cc_min_current) {
		mc_interface_set_current_off_delay(0.1);
	}
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

	volatile mc_configuration *cfg = &motor_now()->m_conf;

	mc_interface_set_brake_current(val * fabsf(cfg->lo_current_min));
	if (fabsf(val * cfg->lo_current_min) > cfg->cc_min_current) {
		mc_interface_set_current_off_delay(0.1);
	}
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

	switch (motor_now()->m_conf.motor_type) {
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

	events_add("set_handbrake", current);
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

	mc_interface_set_handbrake(val * fabsf(motor_now()->m_conf.lo_current_min));
}

void mc_interface_set_openloop_current(float current, float rpm) {
	if (fabsf(current) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_openloop_current(current, DIR_MULT * rpm);
		break;

	default:
		break;
	}

	events_add("set_openloop_current", current);
}
void mc_interface_set_openloop_phase(float current, float phase){
	if (fabsf(current) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_openloop_phase(current, DIR_MULT * phase);
		break;

	default:
		break;
	}

	events_add("set_openloop_phase", phase);
}
void mc_interface_set_openloop_duty(float dutyCycle, float rpm){
	if (fabsf(dutyCycle) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_openloop_duty(dutyCycle, DIR_MULT * rpm);
		break;

	default:
		break;
	}

	events_add("set_openloop_duty", dutyCycle);
}
void mc_interface_set_openloop_duty_phase(float dutyCycle, float phase){
	if (fabsf(dutyCycle) > 0.001) {
		SHUTDOWN_RESET();
	}

	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_openloop_duty_phase(dutyCycle, phase); // Should this use DIR_MULT?
		break;

	default:
		break;
	}

	events_add("set_openloop_duty_phase", phase);
}

void mc_interface_brake_now(void) {
	SHUTDOWN_RESET();

	mc_interface_set_duty(0.0);
}

/**
 * Disconnect the motor and let it turn freely.
 */
void mc_interface_release_motor(void) {
	if (mc_interface_try_input()) {
		return;
	}

	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_release_motor();
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_release_motor();
		break;

	default:
		break;
	}

	events_add("release_motor", 0.0);
}

void mc_interface_release_motor_override(void) {
	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		mcpwm_release_motor();
		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_release_motor();
		break;

	default:
		break;
	}

	events_add("release_motor_override", 0.0);
}

bool mc_interface_wait_for_motor_release(float timeout) {
	systime_t time_start = chVTGetSystemTimeX();
	bool res = false;

	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:
		while (UTILS_AGE_S(time_start) < timeout) {
			if (mcpwm_get_state() == MC_STATE_OFF) {
				res = true;
				break;
			}

			chThdSleepMilliseconds(1);
		}
		break;

	case MOTOR_TYPE_FOC:
		while (UTILS_AGE_S(time_start) < timeout) {
			if (mcpwm_foc_get_state() == MC_STATE_OFF) {
				res = true;
				break;
			}

			chThdSleepMilliseconds(1);
		}
		break;

	default:
		break;
	}

	return res;
}

/**
 * Stop the motor and use braking.
 */
float mc_interface_get_duty_cycle_set(void) {
	float ret = 0.0;

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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
	float val = motor_now()->m_amp_seconds / 3600;

	if (reset) {
		motor_now()->m_amp_seconds = 0.0;
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
	float val = motor_now()->m_amp_seconds_charged / 3600;

	if (reset) {
		motor_now()->m_amp_seconds_charged = 0.0;
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
	float val = motor_now()->m_watt_seconds / 3600;

	if (reset) {
		motor_now()->m_watt_seconds = 0.0;
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
	float val = motor_now()->m_watt_seconds_charged / 3600;

	if (reset) {
		motor_now()->m_watt_seconds_charged = 0.0;
	}

	return val;
}

float mc_interface_get_tot_current(void) {
	float ret = 0.0;

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

float mc_interface_get_input_voltage_filtered(void) {
	return motor_now()->m_input_voltage_filtered;
}

float mc_interface_get_abs_motor_current_unbalance(void) {
	float ret = 0.0;

#ifdef HW_HAS_3_SHUNTS
	switch (motor_now()->m_conf.motor_type) {
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

int mc_interface_set_tachometer_value(int steps) {
	int ret = 0;
	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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

	switch (motor_now()->m_conf.motor_type) {
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
	if (motor_now()->m_conf.motor_type == MOTOR_TYPE_GPD) {
		return gpdrive_get_current_filtered();
	}

	float res = motor_now()->m_motor_current_sum / motor_now()->m_motor_current_iterations;
	motor_now()->m_motor_current_sum = 0.0;
	motor_now()->m_motor_current_iterations = 0.0;
	return res;
}

float mc_interface_read_reset_avg_input_current(void) {
	if (motor_now()->m_conf.motor_type == MOTOR_TYPE_GPD) {
		return gpdrive_get_current_filtered() * gpdrive_get_modulation();
	}

	float res = motor_now()->m_input_current_sum / motor_now()->m_input_current_iterations;
	motor_now()->m_input_current_sum = 0.0;
	motor_now()->m_input_current_iterations = 0.0;
	return res;
}

/**
 * Read and reset the average direct axis motor current. (FOC only)
 *
 * @return
 * The average D axis current.
 */
float mc_interface_read_reset_avg_id(void) {
	float res = motor_now()->m_motor_id_sum / motor_now()->m_motor_id_iterations;
	motor_now()->m_motor_id_sum = 0.0;
	motor_now()->m_motor_id_iterations = 0.0;
	return res;
}

/**
 * Read and reset the average quadrature axis motor current. (FOC only)
 *
 * @return
 * The average Q axis current.
 */
float mc_interface_read_reset_avg_iq(void) {
	float res = motor_now()->m_motor_iq_sum / motor_now()->m_motor_iq_iterations;
	motor_now()->m_motor_iq_sum = 0.0;
	motor_now()->m_motor_iq_iterations = 0.0;
	return DIR_MULT * res;
}

/**
 * Read and reset the average direct axis motor voltage. (FOC only)
 *
 * @return
 * The average D axis voltage.
 */
float mc_interface_read_reset_avg_vd(void) {
	float res = motor_now()->m_motor_vd_sum / motor_now()->m_motor_vd_iterations;
	motor_now()->m_motor_vd_sum = 0.0;
	motor_now()->m_motor_vd_iterations = 0.0;
	return res;
}

/**
 * Read and reset the average quadrature axis motor voltage. (FOC only)
 *
 * @return
 * The average Q axis voltage.
 */
float mc_interface_read_reset_avg_vq(void) {
	float res = motor_now()->m_motor_vq_sum / motor_now()->m_motor_vq_iterations;
	motor_now()->m_motor_vq_sum = 0.0;
	motor_now()->m_motor_vq_iterations = 0.0;
	return DIR_MULT * res;
}

float mc_interface_get_pid_pos_set(void) {
	return motor_now()->m_position_set;
}

float mc_interface_get_pid_pos_now(void) {
	float ret = 0.0;

	volatile mc_configuration *conf = &motor_now()->m_conf;

	switch (conf->motor_type) {
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

	if (encoder_is_configured()) {
		if (conf->foc_encoder_inverted) {
			ret *= -1.0;
		}
	}

	ret *= DIR_MULT;
	ret -= motor_now()->m_conf.p_pid_offset;
	utils_norm_angle(&ret);

	return ret;
}

/**
 * Update the offset such that the current angle becomes angle_now
 */
void mc_interface_update_pid_pos_offset(float angle_now, bool store) {
	mc_configuration *mcconf = mempools_alloc_mcconf();
	*mcconf = *mc_interface_get_configuration();

	mcconf->p_pid_offset += mc_interface_get_pid_pos_now() - angle_now;
	utils_norm_angle(&mcconf->p_pid_offset);

	if (store) {
		conf_general_store_mc_configuration(mcconf, mc_interface_get_motor_thread() == 2);
	}

	mc_interface_set_configuration(mcconf);

	mempools_free_mcconf(mcconf);
}

float mc_interface_get_last_sample_adc_isr_duration(void) {
	return m_last_adc_duration_sample;
}

void mc_interface_sample_print_data(debug_sampling_mode mode, uint16_t len, uint8_t decimation, bool raw, 
		void(*reply_func)(unsigned char *data, unsigned int len)) {

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
		m_sample_raw = raw;
		send_func_sample = reply_func;
#ifdef HW_HAS_DUAL_MOTORS
		m_sample_is_second_motor = motor_now() == &m_motor_2;
#endif
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
	return motor_now()->m_temp_fet;
}

/**
 * Get filtered motor temperature. The temperature is pre-calculated, so this
 * functions is fast.
 *
 * @return
 * The filtered motor temperature.
 */
float mc_interface_temp_motor_filtered(void) {
	return motor_now()->m_temp_motor;
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
	const float v_in = motor_now()->m_input_voltage_filtered_slower;
	float battery_avg_voltage = 0.0;
	float battery_avg_voltage_left = 0.0;
	float ah_left = 0;
	float ah_tot = conf->si_battery_ah;

	switch (conf->si_battery_type) {
	case BATTERY_TYPE_LIION_3_0__4_2:
		battery_avg_voltage = ((3.2 + 4.2) / 2.0) * (float)(conf->si_battery_cells);
		battery_avg_voltage_left = ((3.2 * (float)(conf->si_battery_cells) + v_in) / 2.0);
		float batt_left = utils_map(v_in / (float)(conf->si_battery_cells),
									3.2, 4.2, 0.0, 1.0);
		batt_left = utils_batt_liion_norm_v_to_capacity(batt_left);
		ah_tot *= 0.85; // 0.85 because the battery is not fully depleted at 3.2V / cell
		ah_left = batt_left * ah_tot;
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

	const float wh_batt_tot = ah_tot * battery_avg_voltage;
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
#ifdef HW_HAS_WHEEL_SPEED_SENSOR
	return hw_get_speed();
#else
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const float rpm = mc_interface_get_rpm() / (conf->si_motor_poles / 2.0);
	return (rpm / 60.0) * conf->si_wheel_diameter * M_PI / conf->si_gear_ratio;
#endif
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
#ifdef HW_HAS_WHEEL_SPEED_SENSOR
	return hw_get_distance_abs();
#else
	const volatile mc_configuration *conf = mc_interface_get_configuration();
	const float tacho_scale = (conf->si_wheel_diameter * M_PI) / (3.0 * conf->si_motor_poles * conf->si_gear_ratio);
	return mc_interface_get_tachometer_abs_value(false) * tacho_scale;
#endif
}

setup_values mc_interface_get_setup_values(void) {
	setup_values val = {0, 0, 0, 0, 0, 0, 0};
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

volatile gnss_data *mc_interface_gnss(void) {
	return &m_gnss;
}

/**
 * Set odometer value in meters.
 *
 * @param new_odometer_meters
 * new odometer value in meters
 */
void mc_interface_set_odometer(uint64_t new_odometer_meters) {
	g_backup.odometer = new_odometer_meters;
}

/**
 * Return current odometer value in meters.
 *
 * @return
 * Odometer value in meters, including current trip
 */
uint64_t mc_interface_get_odometer(void) {
	return g_backup.odometer;
}

/**
 * Ignore motor control commands for this amount of time.
 */
void mc_interface_ignore_input(int time_ms) {
	volatile motor_if_state_t *motor = motor_now();
	motor->m_ignore_iterations = time_ms;
}

/**
 * Ignore motor control commands for this amount of time on both motors.
 */
void mc_interface_ignore_input_both(int time_ms) {
	m_motor_1.m_ignore_iterations = time_ms;

#ifdef HW_HAS_DUAL_MOTORS
	m_motor_2.m_ignore_iterations = time_ms;
#endif
}

void mc_interface_release_motor_override_both(void) {
	int motor_last = mc_interface_get_motor_thread();
	mc_interface_select_motor_thread(1);
	mc_interface_release_motor_override();
	mc_interface_select_motor_thread(2);
	mc_interface_release_motor_override();
	mc_interface_select_motor_thread(motor_last);
}

bool mc_interface_wait_for_motor_release_both(float timeout) {
	int motor_last = mc_interface_get_motor_thread();

	mc_interface_select_motor_thread(1);
	if (!mc_interface_wait_for_motor_release(timeout)) {
		mc_interface_select_motor_thread(motor_last);
		return false;
	}

	mc_interface_select_motor_thread(2);
	if (!mc_interface_wait_for_motor_release(timeout)) {
		mc_interface_select_motor_thread(motor_last);
		return false;
	}

	return true;
}

void mc_interface_set_current_off_delay(float delay_sec) {
	if (mc_interface_try_input()) {
		return;
	}

	UTILS_NAN_ZERO(delay_sec);
	if (delay_sec > 5.0) {
		delay_sec = 5.0;
	}

	switch (motor_now()->m_conf.motor_type) {
	case MOTOR_TYPE_BLDC:
	case MOTOR_TYPE_DC:

		break;

	case MOTOR_TYPE_FOC:
		mcpwm_foc_set_current_off_delay(delay_sec);
		break;

	default:
		break;
	}
}

void mc_interface_override_temp_motor(float temp) {
	motor_now()->m_temp_override = temp;
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
		motor_now()->m_ignore_iterations = MCPWM_DETECT_STOP_TIME;
	}

	int retval = motor_now()->m_ignore_iterations;

	if (!motor_now()->m_ignore_iterations && motor_now()->m_lock_enabled) {
		if (!motor_now()->m_lock_override_once) {
			retval = 1;
		} else {
			motor_now()->m_lock_override_once = false;
		}
	}

	switch (motor_now()->m_conf.motor_type) {
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

void mc_interface_set_fault_info(const char *str, int argn, float arg0, float arg1) {
	m_fault_data.info_str = str;
	m_fault_data.info_argn = argn;
	m_fault_data.info_args[0] = arg0;
	m_fault_data.info_args[1] = arg1;
}

void mc_interface_fault_stop(mc_fault_code fault, bool is_second_motor, bool is_isr) {
	m_fault_data.fault_code = fault;
	m_fault_data.is_second_motor = is_second_motor;

	if (is_isr) {
		chSysLockFromISR();
		chEvtSignalI(fault_stop_tp, (eventmask_t) 1);
		chSysUnlockFromISR();
	} else {
		chEvtSignal(fault_stop_tp, (eventmask_t) 1);
	}
}

void mc_interface_mc_timer_isr(bool is_second_motor) {
	ledpwm_update_pwm();

#ifdef HW_HAS_DUAL_MOTORS
	motor_if_state_t *motor = is_second_motor ? (motor_if_state_t*)&m_motor_2 : (motor_if_state_t*)&m_motor_1;
#else
	motor_if_state_t *motor = (motor_if_state_t*)&m_motor_1;
	(void)is_second_motor;
#endif

	mc_configuration *conf_now = (mc_configuration*)&motor->m_conf;
	const float input_voltage = GET_INPUT_VOLTAGE();
	UTILS_LP_FAST(motor->m_input_voltage_filtered, input_voltage, 0.02);

	// Check for faults that should stop the motor

	static float wrong_voltage_integrator = 0.0;
	float voltage_diff_now = 0.0;

	if (input_voltage < conf_now->l_min_vin) {
		voltage_diff_now = conf_now->l_min_vin - input_voltage;
	} else if (input_voltage > conf_now->l_max_vin) {
		voltage_diff_now = input_voltage - conf_now->l_max_vin;
	}

	if (voltage_diff_now > 1.0e-3) {
		wrong_voltage_integrator += voltage_diff_now;

		const float max_voltage = (conf_now->l_max_vin * 0.05);
		if (wrong_voltage_integrator > max_voltage) {
			mc_interface_fault_stop(input_voltage < conf_now->l_min_vin ?
					FAULT_CODE_UNDER_VOLTAGE : FAULT_CODE_OVER_VOLTAGE, is_second_motor, true);

			// Windup protection
			wrong_voltage_integrator = max_voltage * 2.0;
		}
	} else {
		if (wrong_voltage_integrator > 1.0) {
			wrong_voltage_integrator -= 1.0;
		} else {
			wrong_voltage_integrator = 0.0;
		}
	}

	// Fetch these values in a config-specific way to avoid some overhead of the general
	// functions. That will make this interrupt run a bit faster.
	mc_state state;
	float current;
	float current_filtered;
	float current_in_filtered;
	float abs_current;
	float abs_current_filtered;
	if (conf_now->motor_type == MOTOR_TYPE_FOC) {
		state = mcpwm_foc_get_state_motor(is_second_motor);
		current = mcpwm_foc_get_tot_current_motor(is_second_motor);
		current_filtered = mcpwm_foc_get_tot_current_filtered_motor(is_second_motor);
		current_in_filtered = mcpwm_foc_get_tot_current_in_filtered_motor(is_second_motor);
		abs_current = mcpwm_foc_get_abs_motor_current_motor(is_second_motor);
		abs_current_filtered = mcpwm_foc_get_abs_motor_current_filtered_motor(is_second_motor);
	} else {
		state = mcpwm_get_state();
		current = mcpwm_get_tot_current();
		current_filtered = mcpwm_get_tot_current_filtered();
		current_in_filtered = mcpwm_get_tot_current_in_filtered();
		abs_current = mcpwm_get_tot_current();
		abs_current_filtered = current_filtered;
	}

	// Additional input current filter for the mapped current limit
	UTILS_LP_FAST(motor->m_i_in_filter, current_in_filtered, motor->m_conf.l_in_current_map_filter);

	if (state == MC_STATE_RUNNING) {
		motor->m_cycles_running++;
	} else {
		motor->m_cycles_running = 0;
	}

	if (pwn_done_func) {
		pwn_done_func();
	}

	motor->m_motor_current_sum += current_filtered;
	motor->m_input_current_sum += current_in_filtered;
	motor->m_motor_current_iterations++;
	motor->m_input_current_iterations++;

	motor->m_motor_id_sum += mcpwm_foc_get_id();
	motor->m_motor_iq_sum += mcpwm_foc_get_iq();
	motor->m_motor_id_iterations++;
	motor->m_motor_iq_iterations++;

	motor->m_motor_vd_sum += mcpwm_foc_get_vd();
	motor->m_motor_vq_sum += mcpwm_foc_get_vq();
	motor->m_motor_vd_iterations++;
	motor->m_motor_vq_iterations++;

	// Current fault code
	if (conf_now->l_slow_abs_current) {
		if (fabsf(abs_current_filtered) > conf_now->l_abs_current_max) {
			mc_interface_fault_stop(FAULT_CODE_ABS_OVER_CURRENT, is_second_motor, true);
		}
	} else {
		if (fabsf(abs_current) > conf_now->l_abs_current_max) {
			mc_interface_fault_stop(FAULT_CODE_ABS_OVER_CURRENT, is_second_motor, true);
		}
	}

	// DRV fault code
#ifdef HW_HAS_DUAL_PARALLEL
	if (IS_DRV_FAULT() || IS_DRV_FAULT_2()) {
		is_second_motor = IS_DRV_FAULT_2();
#else
	if (is_second_motor ? IS_DRV_FAULT_2() : IS_DRV_FAULT()) {
#endif
		mc_interface_fault_stop(FAULT_CODE_DRV, is_second_motor, true);
	}

#ifdef HW_USE_BRK
	// BRK fault code
	if (TIM_GetFlagStatus(TIM1, TIM_FLAG_Break) != RESET) {
		mc_interface_fault_stop(FAULT_CODE_BRK, is_second_motor, true);
		// latch the BRK/FAULT pin to low until next MCU reset
		palSetPadMode(BRK_GPIO, BRK_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palClearPad(BRK_GPIO, BRK_PIN);
	}
#endif

#ifdef HW_HAS_GATE_DRIVER_SUPPLY_MONITOR
	if(motor->m_gate_driver_voltage > HW_GATE_DRIVER_SUPPLY_MAX_VOLTAGE) {
		mc_interface_fault_stop(FAULT_CODE_GATE_DRIVER_OVER_VOLTAGE, is_second_motor, true);
	}

	if(motor->m_gate_driver_voltage < HW_GATE_DRIVER_SUPPLY_MIN_VOLTAGE) {
		mc_interface_fault_stop(FAULT_CODE_GATE_DRIVER_UNDER_VOLTAGE, is_second_motor, true);
	}
#endif

#ifdef HW_HAS_LV_OUTPUT_PROTECTION
	if(IS_LV_OUTPUT_FAULT()) {
		mc_interface_fault_stop(FAULT_CODE_LV_OUTPUT_FAULT, is_second_motor, true);
	}
#endif

	float t_samp = 1.0 / motor->m_f_samp_now;

	// Watt and ah counters
	if (fabsf(current_filtered) > 1.0) {
		// Some extra filtering
		static float curr_diff_sum = 0.0;
		static float curr_diff_samples = 0;

		curr_diff_sum += current_in_filtered * t_samp;
		curr_diff_samples += t_samp;

		if (curr_diff_samples >= 0.01) {
			if (curr_diff_sum > 0.0) {
				motor->m_amp_seconds += curr_diff_sum;
				motor->m_watt_seconds += curr_diff_sum * input_voltage;
			} else {
				motor->m_amp_seconds_charged -= curr_diff_sum;
				motor->m_watt_seconds_charged -= curr_diff_sum * input_voltage;
			}

			curr_diff_samples = 0.0;
			curr_diff_sum = 0.0;
		}
	}

	bool sample = false;
	debug_sampling_mode sample_mode =
			m_sample_is_second_motor == is_second_motor ?
					m_sample_mode : DEBUG_SAMPLING_OFF;

	switch (sample_mode) {
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
		if (state == MC_STATE_RUNNING || m_sample_now > 0) {
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

		if (state == MC_STATE_RUNNING && m_sample_trigger < 0) {
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

		if (motor->m_fault_now != FAULT_CODE_NONE && m_sample_trigger < 0) {
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
			if (conf_now->motor_type == MOTOR_TYPE_FOC) {
				if (is_second_motor) {
					zero = (ADC_V_L4 + ADC_V_L5 + ADC_V_L6) / 3;
				} else {
					zero = (ADC_V_L1 + ADC_V_L2 + ADC_V_L3) / 3;
				}
				m_phase_samples[m_sample_now] = (uint8_t)(mcpwm_foc_get_phase() / 360.0 * 250.0);
//				m_phase_samples[m_sample_now] = (uint8_t)(mcpwm_foc_get_phase_observer() / 360.0 * 250.0);
//				float ang = utils_angle_difference(mcpwm_foc_get_phase_observer(), mcpwm_foc_get_phase_encoder()) + 180.0;
//				m_phase_samples[m_sample_now] = (uint8_t)(ang / 360.0 * 250.0);
			} else {
				zero = mcpwm_vzero;
				m_phase_samples[m_sample_now] = 0;
			}

			if (state == MC_STATE_DETECTING) {
				m_curr0_samples[m_sample_now] = (int16_t)mcpwm_detect_currents[mcpwm_get_comm_step() - 1];
				m_curr1_samples[m_sample_now] = (int16_t)mcpwm_detect_currents_diff[mcpwm_get_comm_step() - 1];
				m_curr2_samples[m_sample_now] = 0;

				m_ph1_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[0];
				m_ph2_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[1];
				m_ph3_samples[m_sample_now] = (int16_t)mcpwm_detect_voltages[2];
			} else {
				if (is_second_motor) {
					m_curr0_samples[m_sample_now] = ADC_curr_norm_value[3];
					m_curr1_samples[m_sample_now] = ADC_curr_norm_value[4];
					m_curr2_samples[m_sample_now] = ADC_curr_norm_value[5];

					m_ph1_samples[m_sample_now] = ADC_V_L4 - zero;
					m_ph2_samples[m_sample_now] = ADC_V_L5 - zero;
					m_ph3_samples[m_sample_now] = ADC_V_L6 - zero;
				} else {
					m_curr0_samples[m_sample_now] = ADC_curr_norm_value[0];
					m_curr1_samples[m_sample_now] = ADC_curr_norm_value[1];
					m_curr2_samples[m_sample_now] = ADC_curr_norm_value[2];

					m_ph1_samples[m_sample_now] = ADC_V_L1 - zero;
					m_ph2_samples[m_sample_now] = ADC_V_L2 - zero;
					m_ph3_samples[m_sample_now] = ADC_V_L3 - zero;
				}
			}

			m_vzero_samples[m_sample_now] = zero;
			m_curr_fir_samples[m_sample_now] = (int16_t)(current * (8.0 / FAC_CURRENT));
			m_f_sw_samples[m_sample_now] = (int16_t)(0.1 / t_samp);
			m_status_samples[m_sample_now] = mcpwm_get_comm_step() | (mcpwm_read_hall_phase() << 3);

			m_sample_now++;

			m_last_adc_duration_sample = mc_interface_get_last_inj_adc_isr_duration();
		}
	}
}

void mc_interface_adc_inj_int_handler(void) {
	switch (m_motor_1.m_conf.motor_type) {
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
static void update_override_limits(volatile motor_if_state_t *motor, volatile mc_configuration *conf) {
	bool is_motor_1 = motor == &m_motor_1;

	const float v_in = motor->m_input_voltage_filtered;
	float rpm_now = 0.0;

	if (motor->m_conf.motor_type == MOTOR_TYPE_FOC) {
		// Low latency is important for avoiding oscillations
		rpm_now = DIR_MULT * mcpwm_foc_get_rpm_fast();
	} else {
		rpm_now = mc_interface_get_rpm();
	}

	float rpm_abs = fabsf(rpm_now);

	const float duty_now_abs = fabsf(mc_interface_get_duty_cycle_now());

#ifdef HW_HAS_DUAL_PARALLEL
	UTILS_LP_FAST(motor->m_temp_fet, fmaxf(NTC_TEMP(ADC_IND_TEMP_MOS), NTC_TEMP(ADC_IND_TEMP_MOS_M2)), 0.1);
#else
	UTILS_LP_FAST(motor->m_temp_fet, NTC_TEMP(is_motor_1 ? ADC_IND_TEMP_MOS : ADC_IND_TEMP_MOS_M2), 0.1);
#endif

	float temp_motor = 0.0;

	switch(conf->m_motor_temp_sens_type) {
	case TEMP_SENSOR_NTC_10K_25C:
		temp_motor = is_motor_1 ? NTC_TEMP_MOTOR(conf->m_ntc_motor_beta) : NTC_TEMP_MOTOR_2(conf->m_ntc_motor_beta);
		break;

	case TEMP_SENSOR_NTC_100K_25C:
		temp_motor = is_motor_1 ? NTC100K_TEMP_MOTOR(conf->m_ntc_motor_beta) : NTC100K_TEMP_MOTOR_2(conf->m_ntc_motor_beta);
		break;

	case TEMP_SENSOR_PTC_1K_100C:
		temp_motor = is_motor_1 ? PTC_TEMP_MOTOR(1000.0, conf->m_ptc_motor_coeff, 100) : PTC_TEMP_MOTOR_2(1000.0, conf->m_ptc_motor_coeff, 100);
		break;

	case TEMP_SENSOR_KTY83_122: {
		// KTY83_122 datasheet used to approximate resistance at given temperature to cubic polynom
		// https://docs.google.com/spreadsheets/d/1iJA66biczfaXRNClSsrVF9RJuSAKoDG-bnRZFMOcuwU/edit?usp=sharing
		// Thanks to: https://vasilisks.wordpress.com/2017/12/14/getting-temperature-from-ntc-kty83-kty84-on-mcu/#more-645
		// You can change pull up resistor and update NTC_RES_MOTOR for your hardware without changing polynom
		float res = NTC_RES_MOTOR(ADC_Value[is_motor_1 ? ADC_IND_TEMP_MOTOR : ADC_IND_TEMP_MOTOR_2]);
		float pow2 = res * res;
		temp_motor = 0.0000000102114874947423 * pow2 * res - 0.000069967997703501 * pow2 +
				0.243402040973194 * res - 160.145048329356;
	} break;

	case TEMP_SENSOR_KTY84_130: {
		float res = NTC_RES_MOTOR(ADC_Value[is_motor_1 ? ADC_IND_TEMP_MOTOR : ADC_IND_TEMP_MOTOR_2]);
		temp_motor = -7.82531699e-12 * res * res * res * res + 6.34445902e-8 * res * res * res -
				0.00020119157  * res * res + 0.407683016 * res - 161.357536;
	} break;

	case TEMP_SENSOR_NTCX:
		temp_motor = is_motor_1 ? NTCX_TEMP_MOTOR(conf->m_ntcx_ptcx_res, conf->m_ntc_motor_beta, conf->m_ntcx_ptcx_temp_base) :
				NTCX_TEMP_MOTOR_2(conf->m_ntcx_ptcx_res, conf->m_ntc_motor_beta, conf->m_ntcx_ptcx_temp_base);
		break;

	case TEMP_SENSOR_PTCX:
		temp_motor = is_motor_1 ? PTC_TEMP_MOTOR(conf->m_ntcx_ptcx_res, conf->m_ptc_motor_coeff, conf->m_ntcx_ptcx_temp_base) :
				PTC_TEMP_MOTOR_2(conf->m_ntcx_ptcx_res, conf->m_ptc_motor_coeff, conf->m_ntcx_ptcx_temp_base);
		break;

	case TEMP_SENSOR_PT1000: {
		float res = NTC_RES_MOTOR(ADC_Value[is_motor_1 ? ADC_IND_TEMP_MOTOR : ADC_IND_TEMP_MOTOR_2]);
		temp_motor = -(sqrtf(-0.00232 * res + 17.59246) - 3.908) / 0.00116;
	} break;

	case TEMP_SENSOR_DISABLED:
		temp_motor = motor->m_temp_override;
		break;
	}

	// If the reading is messed up (by e.g. reading 0 on the ADC and dividing by 0) we avoid putting an
	// invalid value in the filter, as it will never recover. It is probably safest to keep running the
	// motor even if the temperature reading fails. A config option to reduce power on invalid temperature
	// readings might be useful.
	if (UTILS_IS_NAN(temp_motor) || UTILS_IS_INF(temp_motor) || temp_motor > 600.0 || temp_motor < -200.0) {
		temp_motor = -100.0;
	}

	UTILS_LP_FAST(motor->m_temp_motor, temp_motor, MOTOR_TEMP_LPF);

#ifdef HW_HAS_GATE_DRIVER_SUPPLY_MONITOR
	UTILS_LP_FAST(motor->m_gate_driver_voltage, GET_GATE_DRIVER_SUPPLY_VOLTAGE(), 0.01);
#endif

	const float l_current_min_tmp = conf->l_current_min * conf->l_current_min_scale;
	const float l_current_max_tmp = conf->l_current_max * conf->l_current_max_scale;

	// Temperature MOSFET
	float lo_min_mos = l_current_min_tmp;
	float lo_max_mos = l_current_max_tmp;
	if (motor->m_temp_fet < (conf->l_temp_fet_start + 0.1)) {
		// Keep values
	} else if (motor->m_temp_fet > (conf->l_temp_fet_end - 0.1)) {
		lo_min_mos = 0.0;
		lo_max_mos = 0.0;
		mc_interface_fault_stop(FAULT_CODE_OVER_TEMP_FET, !is_motor_1, false);
	} else {
		float maxc = fabsf(l_current_max_tmp);
		if (fabsf(l_current_min_tmp) > maxc) {
			maxc = fabsf(l_current_min_tmp);
		}

		maxc = utils_map(motor->m_temp_fet, conf->l_temp_fet_start, conf->l_temp_fet_end, maxc, 0.0);

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
	if (motor->m_temp_motor < (conf->l_temp_motor_start + 0.1)) {
		// Keep values
	} else if (motor->m_temp_motor > (conf->l_temp_motor_end - 0.1)) {
		lo_min_mot = 0.0;
		lo_max_mot = 0.0;
		mc_interface_fault_stop(FAULT_CODE_OVER_TEMP_MOTOR, !is_motor_1, false);
	} else {
		float maxc = fabsf(l_current_max_tmp);
		if (fabsf(l_current_min_tmp) > maxc) {
			maxc = fabsf(l_current_min_tmp);
		}

		maxc = utils_map(motor->m_temp_motor, conf->l_temp_motor_start, conf->l_temp_motor_end, maxc, 0.0);

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
	if (motor->m_temp_fet < (temp_fet_accel_start + 0.1)) {
		lo_fet_temp_accel = l_current_max_tmp;
	} else if (motor->m_temp_fet > (temp_fet_accel_end - 0.1)) {
		lo_fet_temp_accel = 0.0;
	} else {
		lo_fet_temp_accel = utils_map(motor->m_temp_fet, temp_fet_accel_start,
				temp_fet_accel_end, l_current_max_tmp, 0.0);
	}

	float lo_motor_temp_accel = 0.0;
	if (motor->m_temp_motor < (temp_motor_accel_start + 0.1)) {
		lo_motor_temp_accel = l_current_max_tmp;
	} else if (motor->m_temp_motor > (temp_motor_accel_end - 0.1)) {
		lo_motor_temp_accel = 0.0;
	} else {
		lo_motor_temp_accel = utils_map(motor->m_temp_motor, temp_motor_accel_start,
				temp_motor_accel_end, l_current_max_tmp, 0.0);
	}

	// RPM max
	float lo_max_rpm = 0.0;
	const float rpm_pos_cut_start = conf->l_max_erpm * conf->l_erpm_start;
	const float rpm_pos_cut_end = conf->l_max_erpm;
	if (rpm_now < (rpm_pos_cut_start + 0.1)) {
		lo_max_rpm = l_current_max_tmp;
	} else if (rpm_now > (rpm_pos_cut_end - 0.1)) {
		lo_max_rpm = 0.0;
	} else {
		lo_max_rpm = utils_map(rpm_now, rpm_pos_cut_start, rpm_pos_cut_end, l_current_max_tmp, 0.0);
	}

	// RPM min
	float lo_min_rpm = 0.0;
	const float rpm_neg_cut_start = conf->l_min_erpm * conf->l_erpm_start;
	const float rpm_neg_cut_end = conf->l_min_erpm;
	if (rpm_now > (rpm_neg_cut_start - 0.1)) {
		lo_min_rpm = l_current_max_tmp;
	} else if (rpm_now < (rpm_neg_cut_end + 0.1)) {
		lo_min_rpm = 0.0;
	} else {
		lo_min_rpm = utils_map(rpm_now, rpm_neg_cut_start, rpm_neg_cut_end, l_current_max_tmp, 0.0);
	}

	// Start Current Decrease
	float lo_max_curr_dec = l_current_max_tmp;
	if (rpm_abs < conf->foc_start_curr_dec_rpm) {
		lo_max_curr_dec = utils_map(rpm_abs, 0, conf->foc_start_curr_dec_rpm,
				conf->foc_start_curr_dec * l_current_max_tmp, l_current_max_tmp);
	}

	// Duty max
	float lo_max_duty = 0.0;
	if (duty_now_abs < (conf->l_duty_start * conf->l_max_duty) || conf->l_duty_start > 0.99) {
		lo_max_duty = l_current_max_tmp;
	} else {
		lo_max_duty = utils_map(duty_now_abs, (conf->l_duty_start * conf->l_max_duty),
				conf->l_max_duty, l_current_max_tmp, conf->cc_min_current * 5.0);
	}

	// Input current limits

	// Battery cutoff
	float lo_in_max_batt = 0.0;
	if (v_in > (conf->l_battery_cut_start - 0.1)) {
		lo_in_max_batt = conf->l_in_current_max;
	} else if (v_in < (conf->l_battery_cut_end + 0.1)) {
		lo_in_max_batt = 0.0;
	} else {
		lo_in_max_batt = utils_map(v_in, conf->l_battery_cut_start,
				conf->l_battery_cut_end, conf->l_in_current_max, 0.0);
	}

	// Regen overvoltage cutoff
	float lo_in_min_batt = 0.0;
	if (v_in < (conf->l_battery_regen_cut_start + 0.1)) {
		lo_in_min_batt = conf->l_in_current_min;
	} else if (v_in > (conf->l_battery_regen_cut_end - 0.1)) {
		lo_in_min_batt = 0.0;
	} else {
		lo_in_min_batt = utils_map(v_in, conf->l_battery_regen_cut_start,
				conf->l_battery_regen_cut_end, conf->l_in_current_min, 0.0);
	}

	// Wattage limits
	const float lo_in_max_watt = conf->l_watt_max / v_in;
	const float lo_in_min_watt = conf->l_watt_min / v_in;

	float lo_in_max = utils_min_abs(lo_in_max_watt, lo_in_max_batt);
	float lo_in_min = utils_min_abs(lo_in_min_watt, lo_in_min_batt);

	// BMS limits
	bms_update_limits(&lo_in_min,  &lo_in_max, conf->l_in_current_min, conf->l_in_current_max);

	conf->lo_in_current_max = utils_min_abs(conf->l_in_current_max, lo_in_max);
	conf->lo_in_current_min = utils_min_abs(conf->l_in_current_min, lo_in_min);

	// Limit iq based on the input current. The input current depends on id and iq combined, but id is determined
	// from iq based on MTPA and field weakening, which makes it tricky to limit them together in the fast
	// current loop. For now that is done here. Note that iq is updated recursively depending on the resulting
	// input current from id and iq.

	float lo_max_i_in = l_current_max_tmp;
	if (motor->m_i_in_filter > 0.0 && conf->l_in_current_map_start < 0.98) {
		float frac = motor->m_i_in_filter / conf->lo_in_current_max;
		if (frac > conf->l_in_current_map_start) {
			lo_max_i_in = utils_map(frac, conf->l_in_current_map_start,
					1.0, l_current_max_tmp, 0.0);
		}

		if (lo_max_i_in < 0.0) {
			lo_max_i_in = 0.0;
		}
	}

	float lo_max = utils_min_abs(lo_max_mos, lo_max_mot);
	float lo_min = utils_min_abs(lo_min_mos, lo_min_mot);

	lo_max = utils_min_abs(lo_max, lo_max_rpm);
	lo_max = utils_min_abs(lo_max, lo_min_rpm);
	lo_max = utils_min_abs(lo_max, lo_max_curr_dec);
	lo_max = utils_min_abs(lo_max, lo_fet_temp_accel);
	lo_max = utils_min_abs(lo_max, lo_motor_temp_accel);
	lo_max = utils_min_abs(lo_max, lo_max_duty);
	lo_max = utils_min_abs(lo_max, lo_max_i_in);

	if (lo_max < conf->cc_min_current) {
		lo_max = conf->cc_min_current;
	}

	if (lo_min > -conf->cc_min_current) {
		lo_min = -conf->cc_min_current;
	}

	conf->lo_current_max = lo_max;
	conf->lo_current_min = lo_min;
}

static volatile motor_if_state_t *motor_now(void) {
#ifdef HW_HAS_DUAL_MOTORS
	return mc_interface_motor_now() == 1 ? &m_motor_1 : &m_motor_2;
#else
	return &m_motor_1;
#endif
}

static void run_timer_tasks(volatile motor_if_state_t *motor) {
	bool is_motor_1 = motor == &m_motor_1;
	mc_interface_select_motor_thread(is_motor_1 ? 1 : 2);

	float voltage_fc = powf(2.0, -(float)motor->m_conf.m_batt_filter_const * 0.25);
	if (UTILS_AGE_S(0) < 10) {
		// Run the filter faster in the beginning to avoid convergance latency at boot
		voltage_fc = 0.01;
	}
	UTILS_LP_FAST(motor->m_input_voltage_filtered_slower, motor->m_input_voltage_filtered, voltage_fc);

	// Update backup data (for motor 1 only)
	if (is_motor_1) {
		uint64_t odometer = mc_interface_get_distance_abs();
		g_backup.odometer += odometer - m_motor_1.m_odometer_last;
		m_motor_1.m_odometer_last = odometer;

		uint64_t runtime = chVTGetSystemTimeX() / CH_CFG_ST_FREQUENCY;

		// Handle wrap around
		if (runtime < m_motor_1.m_runtime_last) {
			m_motor_1.m_runtime_last = 0;
		}

		g_backup.runtime += runtime - m_motor_1.m_runtime_last;
		m_motor_1.m_runtime_last = runtime;
	}

	motor->m_f_samp_now = mc_interface_get_sampling_frequency_now();

	// Decrease fault iterations
	if (motor->m_ignore_iterations > 0) {
		motor->m_ignore_iterations--;
	} else {
		if (!(is_motor_1 ? IS_DRV_FAULT() : IS_DRV_FAULT_2())) {
			motor->m_fault_now = FAULT_CODE_NONE;
		}
	}

	if (is_motor_1 ? IS_DRV_FAULT() : IS_DRV_FAULT_2()) {
		motor->m_drv_fault_iterations++;
		if (motor->m_drv_fault_iterations >= motor->m_conf.m_fault_stop_time_ms) {
			HW_RESET_DRV_FAULTS();
			motor->m_drv_fault_iterations = 0;
		}
	} else {
		motor->m_drv_fault_iterations = 0;
	}

	update_override_limits(motor, &motor->m_conf);

	// Update auxiliary output
	switch (motor->m_conf.m_out_aux_mode) {
	case OUT_AUX_MODE_UNUSED:
		break;

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

	case OUT_AUX_MODE_ON_WHEN_RUNNING:
		if (mc_interface_get_state() == MC_STATE_RUNNING) {
			AUX_ON();
		} else {
			AUX_OFF();
		}
		break;

	case OUT_AUX_MODE_ON_WHEN_NOT_RUNNING:
		if (mc_interface_get_state() == MC_STATE_RUNNING) {
			AUX_OFF();
		} else {
			AUX_ON();
		}
		break;

	case OUT_AUX_MODE_MOTOR_50:
		if (mc_interface_temp_motor_filtered() > 50.0) {AUX_ON();} else {AUX_OFF();}
		break;

	case OUT_AUX_MODE_MOSFET_50:
		if (mc_interface_temp_fet_filtered() > 50.0) {AUX_ON();} else {AUX_OFF();}
		break;

	case OUT_AUX_MODE_MOTOR_70:
		if (mc_interface_temp_motor_filtered() > 70.0) {AUX_ON();} else {AUX_OFF();}
		break;

	case OUT_AUX_MODE_MOSFET_70:
		if (mc_interface_temp_fet_filtered() > 70.0) {AUX_ON();} else {AUX_OFF();}
		break;

	case OUT_AUX_MODE_MOTOR_MOSFET_50:
		if (mc_interface_temp_motor_filtered() > 50.0 ||
				mc_interface_temp_fet_filtered() > 50.0) {AUX_ON();} else {AUX_OFF();}
		break;

	case OUT_AUX_MODE_MOTOR_MOSFET_70:
		if (mc_interface_temp_motor_filtered() > 70.0 ||
				mc_interface_temp_fet_filtered() > 70.0) {AUX_ON();} else {AUX_OFF();}
		break;
	}

	encoder_check_faults(&motor->m_conf, !is_motor_1);

	bool dc_cal_done = mc_interface_dccal_done();
	// TODO: Implement for BLDC and GPDRIVE
	if(motor->m_conf.motor_type == MOTOR_TYPE_FOC && dc_cal_done) {
		float curr0_offset;
		float curr1_offset;
		float curr2_offset;

#ifdef HW_HAS_DUAL_MOTORS
		mcpwm_foc_get_current_offsets(&curr0_offset, &curr1_offset, &curr2_offset, motor == &m_motor_2);
#else
		mcpwm_foc_get_current_offsets(&curr0_offset, &curr1_offset, &curr2_offset, false);
#endif

#ifdef HW_HAS_DUAL_PARALLEL
#define MIDDLE_ADC 4096
#else
#define MIDDLE_ADC 2048
#endif

		if (fabsf(curr0_offset - MIDDLE_ADC) > HW_MAX_CURRENT_OFFSET) {
			mc_interface_fault_stop(FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_1, !is_motor_1, false);
		}
		if (fabsf(curr1_offset - MIDDLE_ADC) > HW_MAX_CURRENT_OFFSET) {
			mc_interface_fault_stop(FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_2, !is_motor_1, false);
		}
#ifdef HW_HAS_3_SHUNTS
		if (fabsf(curr2_offset - MIDDLE_ADC) > HW_MAX_CURRENT_OFFSET) {
			mc_interface_fault_stop(FAULT_CODE_HIGH_OFFSET_CURRENT_SENSOR_3, !is_motor_1, false);
		}
#endif
	}

	// Monitor currents balance. The sum of the 3 currents should be zero
#ifdef HW_HAS_3_SHUNTS
	if (motor->m_conf.foc_current_sample_mode != FOC_CURRENT_SAMPLE_MODE_HIGH_CURRENT  && dc_cal_done) { // This won't work when high current sampling is used
		motor->m_motor_current_unbalance = mc_interface_get_abs_motor_current_unbalance();

		if (fabsf(motor->m_motor_current_unbalance) > fabsf(MCCONF_MAX_CURRENT_UNBALANCE)) {
			UTILS_LP_FAST(motor->m_motor_current_unbalance_error_rate, 1.0, (1 / 1000.0));
		} else {
			UTILS_LP_FAST(motor->m_motor_current_unbalance_error_rate, 0.0, (1 / 1000.0));
		}

		if (motor->m_motor_current_unbalance_error_rate > MCCONF_MAX_CURRENT_UNBALANCE_RATE) {
			mc_interface_fault_stop(FAULT_CODE_UNBALANCED_CURRENTS, !is_motor_1, false);
		}
	}
#endif

#ifdef HW_HAS_WHEEL_SPEED_SENSOR
	hw_update_speed_sensor();
#endif
}

static THD_FUNCTION(timer_thread, arg) {
	(void)arg;

	chRegSetThreadName("mcif timer");

	for(;;) {
		run_timer_tasks(&m_motor_1);
#ifdef HW_HAS_DUAL_MOTORS
		run_timer_tasks(&m_motor_2);
#endif

		chThdSleepMilliseconds(1);
	}
}

static void update_stats(volatile motor_if_state_t *motor) {
	mc_interface_select_motor_thread(motor == (&m_motor_1) ? 1 : 2);

	setup_values val = mc_interface_get_setup_values();

	const double power = mc_interface_get_input_voltage_filtered() * fabsf(val.current_in_tot);
	const double speed = mc_interface_get_speed();
	const double temp_mos = mc_interface_temp_fet_filtered();
	const double temp_mot = mc_interface_temp_motor_filtered();

	motor->m_stats.power_sum += power;
	motor->m_stats.speed_sum += fabs(speed);
	motor->m_stats.temp_mos_sum += temp_mos;
	motor->m_stats.temp_motor_sum += temp_mot;
	motor->m_stats.current_sum += fabs((double)(val.current_tot));
	motor->m_stats.samples += (double)1.0;

	if (power > (double)motor->m_stats.max_power) {
		motor->m_stats.max_power = power;
	}

	if (fabs(speed) > (double)motor->m_stats.max_speed) {
		motor->m_stats.max_speed = fabsf(speed);
	}

	if (temp_mos > (double)motor->m_stats.max_temp_mos) {
		motor->m_stats.max_temp_mos = temp_mos;
	}

	if (temp_mot > (double)motor->m_stats.max_temp_motor) {
		motor->m_stats.max_temp_motor = temp_mot;
	}

	if (fabsf(val.current_tot) > motor->m_stats.max_current) {
		motor->m_stats.max_current = fabsf(val.current_tot);
	}
}

float mc_interface_stat_speed_avg(void) {
	volatile setup_stats *s = &motor_now()->m_stats;
	double res = s->speed_sum / s->samples;
	return res;
}

float mc_interface_stat_speed_max(void) {
	return motor_now()->m_stats.max_speed;
}

float mc_interface_stat_power_avg(void) {
	volatile setup_stats *s = &motor_now()->m_stats;
	double res = s->power_sum / s->samples;
	return res;
}

float mc_interface_stat_power_max(void) {
	return motor_now()->m_stats.max_power;
}

float mc_interface_stat_current_avg(void) {
	volatile setup_stats *s = &motor_now()->m_stats;
	double res = s->current_sum / s->samples;
	return res;
}

float mc_interface_stat_current_max(void) {
	return motor_now()->m_stats.max_current;
}

float mc_interface_stat_temp_mosfet_avg(void) {
	volatile setup_stats *s = &motor_now()->m_stats;
	double res = s->temp_mos_sum / s->samples;
	return res;
}

float mc_interface_stat_temp_mosfet_max(void) {
	return motor_now()->m_stats.max_temp_mos;
}

float mc_interface_stat_temp_motor_avg(void) {
	volatile setup_stats *s = &motor_now()->m_stats;
	double res = s->temp_motor_sum / s->samples;
	return res;
}

float mc_interface_stat_temp_motor_max(void) {
	return motor_now()->m_stats.max_temp_motor;
}

float mc_interface_stat_count_time(void) {
	return UTILS_AGE_S(motor_now()->m_stats.time_start);
}

void mc_interface_stat_reset(void) {
	volatile setup_stats *s = &motor_now()->m_stats;
	memset((void*)s, 0, sizeof(setup_stats));
	s->time_start = chVTGetSystemTimeX();
	s->max_temp_mos = -300.0;
	s->max_temp_motor = -300.0;
}

static THD_FUNCTION(stat_thread, arg) {
	(void)arg;

	chRegSetThreadName("StatCounter");

	for(;;) {
		update_stats(&m_motor_1);
#ifdef HW_HAS_DUAL_MOTORS
		update_stats(&m_motor_2);
#endif

		chThdSleepMilliseconds(10);
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

			if (m_sample_raw) {
				buffer_append_float32_auto(buffer, (float)m_curr0_samples[ind_samp], &index);
				buffer_append_float32_auto(buffer, (float)m_curr1_samples[ind_samp], &index);
				buffer_append_float32_auto(buffer, (float)m_curr2_samples[ind_samp], &index);
				buffer_append_float32_auto(buffer, (float)m_ph1_samples[ind_samp], &index);
				buffer_append_float32_auto(buffer, (float)m_ph2_samples[ind_samp], &index);
				buffer_append_float32_auto(buffer, (float)m_ph3_samples[ind_samp], &index);
				buffer_append_float32_auto(buffer, (float)m_vzero_samples[ind_samp], &index);
				buffer_append_float32_auto(buffer, (float)m_curr_fir_samples[ind_samp], &index);
			} else {
				buffer_append_float32_auto(buffer, (float)m_curr0_samples[ind_samp] * FAC_CURRENT, &index);
				buffer_append_float32_auto(buffer, (float)m_curr1_samples[ind_samp] * FAC_CURRENT, &index);
				buffer_append_float32_auto(buffer, (float)m_curr2_samples[ind_samp] * FAC_CURRENT, &index);
				buffer_append_float32_auto(buffer, ((float)m_ph1_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2) * ADC_VOLTS_PH_FACTOR, &index);
				buffer_append_float32_auto(buffer, ((float)m_ph2_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2) * ADC_VOLTS_PH_FACTOR, &index);
				buffer_append_float32_auto(buffer, ((float)m_ph3_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2) * ADC_VOLTS_PH_FACTOR, &index);
				buffer_append_float32_auto(buffer, ((float)m_vzero_samples[ind_samp] / 4096.0 * V_REG) * ((VIN_R1 + VIN_R2) / VIN_R2) * ADC_VOLTS_INPUT_FACTOR, &index);
				buffer_append_float32_auto(buffer, (float)m_curr_fir_samples[ind_samp] / (8.0 / FAC_CURRENT), &index);
			}

			buffer_append_float32_auto(buffer, (float)m_f_sw_samples[ind_samp] * 10.0, &index);
			buffer[index++] = m_status_samples[ind_samp];
			buffer[index++] = m_phase_samples[ind_samp];

			send_func_sample(buffer, index);
		}
	}
}

static THD_FUNCTION(fault_stop_thread, arg) {
	(void)arg;

	chRegSetThreadName("Fault Stop");
	fault_stop_tp = chThdGetSelfX();

	for(;;) {
		chEvtWaitAny((eventmask_t) 1);

		fault_data_local fault_data_copy = m_fault_data;

#ifdef HW_HAS_DUAL_MOTORS
		volatile motor_if_state_t *motor = fault_data_copy.is_second_motor ? &m_motor_2 : &m_motor_1;
#else
		volatile motor_if_state_t *motor = &m_motor_1;
#endif

		mc_interface_select_motor_thread(fault_data_copy.is_second_motor ? 2 : 1);

		if (motor->m_fault_now == fault_data_copy.fault_code) {
			motor->m_ignore_iterations = motor->m_conf.m_fault_stop_time_ms;
			continue;
		}

		// Some hardwares always have a DRV-fault at boot. Therefore we do not log it in the
		// beginning to avoid confusing the user. After dccal all faults should be gone if
		// everything is ok.
		bool is_log_ok = (mc_interface_dccal_done() || motor->m_fault_now != FAULT_CODE_DRV);

		if (is_log_ok && motor->m_fault_now == FAULT_CODE_NONE) {
			// Send to terminal fault logger so that all faults and their conditions
			// can be printed for debugging.
			utils_sys_lock_cnt();
			volatile int val_samp = TIM8->CCR1;
			volatile int current_samp = TIM1->CCR4;
			volatile int tim_top = TIM1->ARR;
			utils_sys_unlock_cnt();

			fault_data fdata;
			fdata.motor = fault_data_copy.is_second_motor ? 2 : 1;
			fdata.fault = fault_data_copy.fault_code;
			fdata.current = mc_interface_get_tot_current();
			fdata.current_filtered = mc_interface_get_tot_current_filtered();
			fdata.voltage = GET_INPUT_VOLTAGE();
			fdata.gate_driver_voltage = motor->m_gate_driver_voltage;
			fdata.duty = mc_interface_get_duty_cycle_now();
			fdata.rpm = mc_interface_get_rpm();
			fdata.tacho = mc_interface_get_tachometer_value(false);
			fdata.cycles_running = motor->m_cycles_running;
			fdata.tim_val_samp = val_samp;
			fdata.tim_current_samp = current_samp;
			fdata.tim_top = tim_top;
			fdata.comm_step = mcpwm_get_comm_step();
			fdata.temperature = NTC_TEMP(ADC_IND_TEMP_MOS);
#ifdef HW_HAS_DRV8301
			if (fault_data_copy.fault_code == FAULT_CODE_DRV) {
				fdata.drv8301_faults = drv8301_read_faults();
			}
#elif defined(HW_HAS_DRV8320S)
			if (fault_data_copy.fault_code == FAULT_CODE_DRV) {
				fdata.drv8301_faults = drv8320s_read_faults();
			}
#elif defined(HW_HAS_DRV8323S)
			if (fault_data_copy.fault_code == FAULT_CODE_DRV) {
				fdata.drv8301_faults = drv8323s_read_faults();
			}
#endif
			fdata.info_str = fault_data_copy.info_str;
			fdata.info_argn = fault_data_copy.info_argn;
			fdata.info_args[0] = fault_data_copy.info_args[0];
			fdata.info_args[1] = fault_data_copy.info_args[1];
			fault_data_copy.info_str = 0;
			fault_data_copy.info_argn = 0;
			terminal_add_fault_data(&fdata);
		}

		motor->m_ignore_iterations = motor->m_conf.m_fault_stop_time_ms;

		switch (motor->m_conf.motor_type) {
		case MOTOR_TYPE_BLDC:
		case MOTOR_TYPE_DC:
			mcpwm_stop_pwm();
			break;

		case MOTOR_TYPE_FOC:
			mcpwm_foc_stop_pwm(fault_data_copy.is_second_motor);
			break;

		case MOTOR_TYPE_GPD:
			gpdrive_set_mode(GPD_OUTPUT_MODE_NONE);
			break;

		default:
			break;
		}

		motor->m_fault_now = fault_data_copy.fault_code;
	}
}

/**
 * Get mc_configuration CRC (motor 1 or 2)
 *
 * @param conf
 * Pointer to mc_configuration or NULL for current config
 *
 * @param is_motor_2
 * true if motor2, false if motor1
 * 
 * @return
 * CRC16 (with crc field in struct temporarily set to zero).
 */
unsigned mc_interface_calc_crc(mc_configuration* conf_in, bool is_motor_2) {
	volatile mc_configuration* conf = conf_in;

	if (conf == NULL) {
		if(is_motor_2) {
#ifdef HW_HAS_DUAL_MOTORS
			conf = &(m_motor_2.m_conf);
#else
			return 0; //shouldn't be here
#endif
		} else {
			conf = &(m_motor_1.m_conf);
		}
	}

	unsigned crc_old = conf->crc;
	conf->crc = 0;
	unsigned crc_new = crc16((uint8_t*)conf, sizeof(mc_configuration));
	conf->crc = crc_old;
	return crc_new;
}
