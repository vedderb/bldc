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

#include "conf_general.h"
#include "ch.h"
#include "eeprom.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "utils.h"
#include "stm32f4xx_conf.h"
#include "timeout.h"
#include "commands.h"
#include "encoder.h"
#include "comm_can.h"
#include "app.h"
#include "confgenerator.h"

#include <string.h>
#include <math.h>

// EEPROM settings
#define EEPROM_BASE_MCCONF		1000
#define EEPROM_BASE_APPCONF		2000
#define EEPROM_BASE_HW			3000
#define EEPROM_BASE_CUSTOM		4000

// Global variables
uint16_t VirtAddVarTab[NB_OF_VAR];
bool conf_general_permanent_nrf_found = false;

// Private variables
static mc_configuration mcconf, mcconf_old, mcconf_old_second;

// Private functions
static bool read_eeprom_var(eeprom_var *v, int address, uint16_t base);
static bool store_eeprom_var(eeprom_var *v, int address, uint16_t base);

void conf_general_init(void) {
	// First, make sure that all relevant virtual addresses are assigned for page swapping.
	memset(VirtAddVarTab, 0, sizeof(VirtAddVarTab));

	int ind = 0;
	for (unsigned int i = 0;i < (sizeof(mc_configuration) / 2);i++) {
		VirtAddVarTab[ind++] = EEPROM_BASE_MCCONF + i;
	}

	for (unsigned int i = 0;i < (sizeof(app_configuration) / 2);i++) {
		VirtAddVarTab[ind++] = EEPROM_BASE_APPCONF + i;
	}

	for (unsigned int i = 0;i < (EEPROM_VARS_HW * 2);i++) {
		VirtAddVarTab[ind++] = EEPROM_BASE_HW + i;
	}

	for (unsigned int i = 0;i < (EEPROM_VARS_CUSTOM * 2);i++) {
		VirtAddVarTab[ind++] = EEPROM_BASE_CUSTOM + i;
	}

	FLASH_Unlock();
	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);
	EE_Init();
	FLASH_Lock();
}

/**
 * Read hw-specific variable from emulated EEPROM.
 *
 * @param v
 * The variable to read the result from.
 *
 * @param address
 * Mapped address in EEPROM. Range 0 to 63.
 *
 * @return
 * true for success, false if variable was not found.
 */
bool conf_general_read_eeprom_var_hw(eeprom_var *v, int address) {
	return read_eeprom_var(v, address, EEPROM_BASE_HW);
}

/**
 * Read custom variable from emulated EEPROM.
 *
 * @param v
 * The variable to read the result from.
 *
 * @param address
 * Mapped address in EEPROM. Range 0 to 63.
 *
 * @return
 * true for success, false if variable was not found.
 */
bool conf_general_read_eeprom_var_custom(eeprom_var *v, int address) {
	return read_eeprom_var(v, address, EEPROM_BASE_CUSTOM);
}

/**
 * Store hw-specific variable to emulated EEPROM.
 *
 * @param v
 * The variable to store the result in.
 *
 * @param address
 * Mapped address in EEPROM. Range 0 to 63.
 *
 * @return
 * true for success, false if something went wrong.
 */
bool conf_general_store_eeprom_var_hw(eeprom_var *v, int address) {
	return store_eeprom_var(v, address, EEPROM_BASE_HW);
}

/**
 * Store custom variable to emulated EEPROM.
 *
 * @param v
 * The variable to store the result in.
 *
 * @param address
 * Mapped address in EEPROM. Range 0 to 63.
 *
 * @return
 * true for success, false if something went wrong.
 */
bool conf_general_store_eeprom_var_custom(eeprom_var *v, int address) {
	return store_eeprom_var(v, address, EEPROM_BASE_CUSTOM);
}

static bool read_eeprom_var(eeprom_var *v, int address, uint16_t base) {
	bool is_ok = true;
	uint16_t var0, var1;

	if (EE_ReadVariable(base + 2 * address, &var0) == 0 &&
			EE_ReadVariable(base + 2 * address + 1, &var1) == 0) {
		uint32_t res = ((uint32_t)var0) << 16 | var1;
		v->as_u32 = res;
	} else {
		is_ok = false;
	}

	return is_ok;
}

static bool store_eeprom_var(eeprom_var *v, int address, uint16_t base) {
	bool is_ok = true;
	uint16_t var0, var1;

	var0 = v->as_u32 >> 16;
	var1 = v->as_u32 & 0xFFFF;

	timeout_configure_IWDT_slowest();

	FLASH_Unlock();
	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

	if (EE_WriteVariable(base + address * 2, var0) != FLASH_COMPLETE) {
		is_ok = false;
	}

	if (is_ok) {
		if (EE_WriteVariable(base + address * 2 + 1, var1) != FLASH_COMPLETE) {
			is_ok = false;
		}
	}

	FLASH_Lock();

	timeout_configure_IWDT();

	return is_ok;
}

/**
 * Read app_configuration from EEPROM. If this fails, default values will be used.
 *
 * @param conf
 * A pointer to a app_configuration struct to write the read configuration to.
 */
void conf_general_read_app_configuration(app_configuration *conf) {
	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;
	uint16_t var;

	for (unsigned int i = 0;i < (sizeof(app_configuration) / 2);i++) {
		if (EE_ReadVariable(EEPROM_BASE_APPCONF + i, &var) == 0) {
			conf_addr[2 * i] = (var >> 8) & 0xFF;
			conf_addr[2 * i + 1] = var & 0xFF;
		} else {
			is_ok = false;
			break;
		}
	}

	// Set the default configuration
	if (!is_ok) {
		confgenerator_set_defaults_appconf(conf);
	}
}

/**
 * Write app_configuration to EEPROM.
 *
 * @param conf
 * A pointer to the configuration that should be stored.
 */
bool conf_general_store_app_configuration(app_configuration *conf) {
	mc_interface_unlock();
	mc_interface_release_motor();

	utils_sys_lock_cnt();
	mc_interface_lock();

	timeout_configure_IWDT_slowest();

	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;
	uint16_t var;

	FLASH_Unlock();
	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

	for (unsigned int i = 0;i < (sizeof(app_configuration) / 2);i++) {
		var = (conf_addr[2 * i] << 8) & 0xFF00;
		var |= conf_addr[2 * i + 1] & 0xFF;

		if (EE_WriteVariable(EEPROM_BASE_APPCONF + i, var) != FLASH_COMPLETE) {
			is_ok = false;
			break;
		}
	}
	FLASH_Lock();

	timeout_configure_IWDT();

	chThdSleepMilliseconds(100);
	mc_interface_unlock();
	utils_sys_unlock_cnt();

	return is_ok;
}

/**
 * Read mc_configuration from EEPROM. If this fails, default values will be used.
 *
 * @param conf
 * A pointer to a mc_configuration struct to write the read configuration to.
 */
void conf_general_read_mc_configuration(mc_configuration *conf) {
	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;
	uint16_t var;

	for (unsigned int i = 0;i < (sizeof(mc_configuration) / 2);i++) {
		if (EE_ReadVariable(EEPROM_BASE_MCCONF + i, &var) == 0) {
			conf_addr[2 * i] = (var >> 8) & 0xFF;
			conf_addr[2 * i + 1] = var & 0xFF;
		} else {
			is_ok = false;
			break;
		}
	}

	if (!is_ok) {
		confgenerator_set_defaults_mcconf(conf);
	}
}

/**
 * Write mc_configuration to EEPROM.
 *
 * @param conf
 * A pointer to the configuration that should be stored.
 */
bool conf_general_store_mc_configuration(mc_configuration *conf) {
	mc_interface_unlock();
	mc_interface_release_motor();

	utils_sys_lock_cnt();
	mc_interface_lock();

	timeout_configure_IWDT_slowest();

	bool is_ok = true;
	uint8_t *conf_addr = (uint8_t*)conf;

	FLASH_Unlock();
	FLASH_ClearFlag(FLASH_FLAG_OPERR | FLASH_FLAG_WRPERR | FLASH_FLAG_PGAERR |
			FLASH_FLAG_PGPERR | FLASH_FLAG_PGSERR);

	for (unsigned int i = 0;i < (sizeof(mc_configuration) / 2);i++) {
		uint16_t var = (conf_addr[2 * i] << 8) & 0xFF00;
		var |= conf_addr[2 * i + 1] & 0xFF;

		if (EE_WriteVariable(EEPROM_BASE_MCCONF + i, var) != FLASH_COMPLETE) {
			is_ok = false;
			break;
		}
	}
	FLASH_Lock();

	timeout_configure_IWDT();

	chThdSleepMilliseconds(100);
	mc_interface_unlock();
	utils_sys_unlock_cnt();

	return is_ok;
}

bool conf_general_detect_motor_param(float current, float min_rpm, float low_duty,
		float *int_limit, float *bemf_coupling_k, int8_t *hall_table, int *hall_res) {

	int ok_steps = 0;
	const float spinup_to_duty = 0.5;

	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_BLDC;
	mcconf.sensor_mode = SENSOR_MODE_SENSORLESS;
	mcconf.comm_mode = COMM_MODE_INTEGRATE;
	mcconf.sl_phase_advance_at_br = 1.0;
	mcconf.sl_min_erpm = min_rpm;
	mcconf.sl_bemf_coupling_k = 300;
	mcconf.sl_cycle_int_limit = 50;
	mcconf.sl_min_erpm_cycle_int_limit = 1100;
	mcconf.m_invert_direction = false;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	// Wait one second for things to get ready after
	// the fault disappears. (will fry things otherwise...)
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	mc_interface_lock_override_once();
	mc_interface_set_current(current);

	// Try to spin up the motor. Up to three attempts with different settings are made.
	bool started = false;
	for (int i = 0;i < 3;i++) {
		if (i == 1) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 2 * min_rpm;
			mcconf.sl_cycle_int_limit = 20;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		} else if (i == 2) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 4 * min_rpm;
			mcconf.comm_mode = COMM_MODE_DELAY;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		}

		int cnt = 0;
		bool switch_done = false;
		started = true;

		while (mc_interface_get_duty_cycle_now() < spinup_to_duty) {
			chThdSleepMilliseconds(1);
			cnt++;

			if (mc_interface_get_duty_cycle_now() >= (spinup_to_duty / 2.0) && !switch_done) {
				mcpwm_switch_comm_mode(COMM_MODE_DELAY);
				switch_done = true;
			}

			if (cnt > 2000 && !switch_done) {
				started = false;
				break;
			}

			if (cnt >= 5000) {
				started = false;
				break;
			}
		}

		if (switch_done) {
			break;
		}
	}

	if (!started) {
		mc_interface_set_current(0.0);
		timeout_configure(tout, tout_c);
		mc_interface_set_configuration(&mcconf_old);
		mc_interface_unlock();
		return false;
	}

	ok_steps++;

	// Reset hall sensor samples
	mcpwm_reset_hall_detect_table();

	// Run for a while to get hall sensor samples
	mc_interface_lock_override_once();
	mc_interface_set_duty(spinup_to_duty);
	chThdSleepMilliseconds(400);

	// Release the motor and wait a few commutations
	mc_interface_lock_override_once();
	mc_interface_set_current(0.0);
	int tacho = mc_interface_get_tachometer_value(0);
	for (int i = 0;i < 2000;i++) {
		if ((mc_interface_get_tachometer_value(0) - tacho) < 3) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	// Average the cycle integrator for 50 commutations
	mcpwm_read_reset_avg_cycle_integrator();
	tacho = mc_interface_get_tachometer_value(false);
	for (int i = 0;i < 3000;i++) {
		if ((mc_interface_get_tachometer_value(false) - tacho) < 50) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	// Get hall detect result
	*hall_res = mcpwm_get_hall_detect_result(hall_table);

	*int_limit = mcpwm_read_reset_avg_cycle_integrator();

	// Wait for the motor to slow down
	for (int i = 0;i < 5000;i++) {
		if (mc_interface_get_duty_cycle_now() > low_duty) {
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	mc_interface_lock_override_once();
	mc_interface_set_duty(low_duty);

	// Average the cycle integrator for 100 commutations
	mcpwm_read_reset_avg_cycle_integrator();
	tacho = mc_interface_get_tachometer_value(0);
	float rpm_sum = 0.0;
	float rpm_iterations = 0.0;
	for (int i = 0;i < 3000;i++) {
		if ((mc_interface_get_tachometer_value(0) - tacho) < 100) {
			rpm_sum += mc_interface_get_rpm();
			rpm_iterations += 1;
			chThdSleepMilliseconds(1);
		} else {
			ok_steps++;
			break;
		}
	}

	float avg_cycle_integrator_running = mcpwm_read_reset_avg_cycle_integrator();
	float rpm = rpm_sum / rpm_iterations;

	mc_interface_lock_override_once();
	mc_interface_release_motor();

	// Try to figure out the coupling factor
	avg_cycle_integrator_running -= *int_limit;
	avg_cycle_integrator_running /= (float)ADC_Value[ADC_IND_VIN_SENS];
	avg_cycle_integrator_running *= rpm;
	*bemf_coupling_k = avg_cycle_integrator_running;

	// Restore settings
	mc_interface_set_configuration(&mcconf_old);
	timeout_configure(tout, tout_c);

	mc_interface_unlock();

	return ok_steps == 5 ? true : false;
}

/**
 * Try to measure the motor flux linkage.
 *
 * @param current
 * The current so spin up the motor with.
 *
 * @param duty
 * The duty cycle to maintain.
 *
 * @param min_erpm
 * The minimum ERPM for the delay commutation mode.
 *
 * @param res
 * The motor phase resistance.
 *
 * @param linkage
 * The calculated flux linkage.
 *
 * @return
 * True for success, false otherwise.
 */
bool conf_general_measure_flux_linkage(float current, float duty,
		float min_erpm, float res, float *linkage) {
	mcconf = *mc_interface_get_configuration();
	mcconf_old_second = mcconf;

	mcconf.motor_type = MOTOR_TYPE_BLDC;
	mcconf.sensor_mode = SENSOR_MODE_SENSORLESS;
	mcconf.comm_mode = COMM_MODE_INTEGRATE;
	mcconf.sl_phase_advance_at_br = 1.0;
	mcconf.sl_min_erpm = min_erpm;
	mcconf.m_bldc_f_sw_min = 10000.0;
	mcconf.sl_bemf_coupling_k = 300;
	mcconf.sl_cycle_int_limit = 50;
	mcconf.sl_min_erpm_cycle_int_limit = 1100;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old_second);
		return false;
	}

	// Wait one second for things to get ready after
	// the fault disapears.
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	mc_interface_lock_override_once();
	mc_interface_set_current(current);

	// Try to spin up the motor. Up to three attempts with different settings are made.
	bool started = false;
	for (int i = 0;i < 4;i++) {
		if (i == 1) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_cycle_int_limit = 250;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		} else if (i == 2) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 2 * min_erpm;
			mcconf.sl_cycle_int_limit = 20;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		} else if (i == 3) {
			mc_interface_lock_override_once();
			mc_interface_release_motor();
			mcconf.sl_min_erpm = 4 * min_erpm;
			mcconf.comm_mode = COMM_MODE_DELAY;
			mc_interface_lock_override_once();
			mc_interface_set_configuration(&mcconf);
			chThdSleepMilliseconds(1000);
			mc_interface_lock_override_once();
			mc_interface_set_current(current);
		}

		int cnt = 0;
		bool switch_done = false;
		started = true;

		while (mc_interface_get_duty_cycle_now() < duty) {
			chThdSleepMilliseconds(1);
			cnt++;

			if (mc_interface_get_duty_cycle_now() >= (duty / 2.0) && !switch_done) {
				mcpwm_switch_comm_mode(COMM_MODE_DELAY);
				switch_done = true;
			}

			if (cnt > 2000 && !switch_done) {
				started = false;
				break;
			}

			if (cnt >= 5000) {
				started = false;
				break;
			}
		}

		if (switch_done) {
			break;
		}
	}

	if (!started) {
		mc_interface_set_current(0.0);
		timeout_configure(tout, tout_c);
		mc_interface_set_configuration(&mcconf_old_second);
		mc_interface_unlock();
		return false;
	}

	mc_interface_lock_override_once();
	mc_interface_set_duty(duty);

	float avg_voltage = 0.0;
	float avg_rpm = 0.0;
	float avg_current = 0.0;
	float samples = 0.0;
	for (int i = 0;i < 2000;i++) {
		avg_voltage += GET_INPUT_VOLTAGE() * mc_interface_get_duty_cycle_now();
		avg_rpm += mc_interface_get_rpm();
		avg_current += mc_interface_get_tot_current();
		samples += 1.0;
		chThdSleepMilliseconds(1.0);
	}

	timeout_configure(tout, tout_c);
	mc_interface_set_configuration(&mcconf_old_second);
	mc_interface_unlock();
	mc_interface_set_current(0.0);

	avg_voltage /= samples;
	avg_rpm /= samples;
	avg_current /= samples;
	avg_voltage -= avg_current * res * 2.0;

	*linkage = avg_voltage * 60.0 / (sqrtf(3.0) * 2.0 * M_PI * avg_rpm);

	return true;
}

/* Calculate DTG register */
uint8_t conf_general_calculate_deadtime(float deadtime_ns, float core_clock_freq) {
	uint8_t DTG = 0;
	float timebase = 1.0 / (core_clock_freq / 1000000.0) * 1000.0;

	if (deadtime_ns <= (timebase * 127.0)) {
		DTG = deadtime_ns / timebase;
	} else {
		if (deadtime_ns <= ((63.0 + 64.0) * 2.0 * timebase)) {
			DTG = deadtime_ns / (2.0 * timebase) - 64.0;
			DTG |= 0x80;
		} else {
			if (deadtime_ns <= ((31.0 + 32.0) * 8.0 * timebase)) {
				DTG = deadtime_ns / (8.0 * timebase) - 32.0;
				DTG |= 0xC0;
			} else {
				if (deadtime_ns <= ((31.0 + 32) * 16 * timebase)) {
					DTG = deadtime_ns / (16.0 * timebase) - 32.0;
					DTG |= 0xE0;
				} else {
					// Deadtime requested is longer than max achievable. Set deadtime at
					// longest possible value
					DTG = 0xFF;
					assert_param(1); //catch this
				}
			}
		}
	}

	return DTG;
}

/**
 * Try to measure the motor flux linkage using open loop FOC control.
 *
 * @param current
 * The Q-axis current to spin up the motor.
 *
 * @param duty
 * Duty cycle % to measure at
 *
 * @param erpm_per_sec
 * Acceleration rate
 *
 * @param res
 * The motor phase resistance.
 *
 * @param linkage
 * The calculated flux linkage.
 *
 * @return
 * True for success, false otherwise.
 */
bool conf_general_measure_flux_linkage_openloop(float current, float duty,
		float erpm_per_sec, float res, float *linkage) {
	bool result = false;

	mcconf = *mc_interface_get_configuration();
	mcconf_old_second = mcconf;

	mcconf.motor_type = MOTOR_TYPE_FOC;
	mcconf.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
	mcconf.foc_current_kp = 0.0005;
	mcconf.foc_current_ki = 1.0;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old_second);
		return false;
	}

	// Wait one second for things to get ready after
	// the fault disapears.
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	int cnt = 0;
	float rpm_now = 0;

	// Start by locking the motor
	mcpwm_foc_set_openloop(current, rpm_now);

	float duty_still = 0;
	float samples = 0;
	for (int i = 0;i < 1000;i++) {
		duty_still += fabsf(mc_interface_get_duty_cycle_now());
		samples += 1.0;
		chThdSleepMilliseconds(1);
	}

	duty_still /= samples;
	float duty_max = 0.0;
	const int max_time = 15000;

	while (fabsf(mc_interface_get_duty_cycle_now()) < duty) {
		rpm_now += erpm_per_sec / 1000.0;
		mcpwm_foc_set_openloop(current, mcconf.m_invert_direction ? -rpm_now : rpm_now);

		chThdSleepMilliseconds(1);
		cnt++;

		float duty_now = fabsf(mc_interface_get_duty_cycle_now());

		if (duty_now > duty_max) {
			duty_max = duty_now;
		}

		if (cnt >= max_time) {
			*linkage = -1.0;
			break;
		}

		if (cnt > 4000 && duty_now < (duty_max * 0.7)) {
			cnt = max_time;
			*linkage = -2.0;
			break;
		}

		if (cnt > 4000 && duty < duty_still * 1.1) {
			cnt = max_time;
			*linkage = -3.0;
			break;
		}

		if (rpm_now >= 12000) {
			break;
		}
	}

	chThdSleepMilliseconds(1000);

	if (cnt < max_time) {
		float vq_avg = 0.0;
		float vd_avg = 0.0;
		float iq_avg = 0.0;
		float id_avg = 0.0;
		float samples2 = 0.0;

		for (int i = 0;i < 30000;i++) {
			vq_avg += mcpwm_foc_get_vq();
			vd_avg += mcpwm_foc_get_vd();
			iq_avg += mcpwm_foc_get_iq();
			id_avg += mcpwm_foc_get_id();
			samples2 += 1.0;
			chThdSleep(1);
		}

		vq_avg /= samples2;
		vd_avg /= samples2;
		iq_avg /= samples2;
		id_avg /= samples2;

		*linkage = (sqrtf(SQ(vq_avg) + SQ(vd_avg)) - res *
				sqrtf(SQ(iq_avg) + SQ(id_avg))) / (rpm_now * ((2.0 * M_PI) / 60.0));

		result = true;
	}

	timeout_configure(tout, tout_c);
	mc_interface_unlock();
	mc_interface_release_motor();
	mc_interface_set_configuration(&mcconf_old_second);
	return result;
}

/**
 * Automatically detect sensors and apply settings in FOC mode.
 *
 * @param current
 * Current to use for detection.
 *
 * @param store_mcconf_on_success
 * Store motor configuration in emulated EEPROM if the detection succeeds.
 *
 * @param send_mcconf_on_success
 * Send motor configuration if the detection succeeds.
 *
 * @return
 * 2: AS5147 detected successfully
 * 1: Hall sensors detected successfully
 * 0: No sensors detected and sensorless mode applied successfully
 * -1: Detection failed
 */
int conf_general_autodetect_apply_sensors_foc(float current,
		bool store_mcconf_on_success, bool send_mcconf_on_success) {
	int result = -1;

	mcconf = *mc_interface_get_configuration();
	mcconf_old_second = mcconf;

	mcconf.motor_type = MOTOR_TYPE_FOC;
	mcconf.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
	mcconf.foc_current_kp = 0.0005;
	mcconf.foc_current_ki = 1.0;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old_second);
		return -1;
	}

	// Wait one second for things to get ready after
	// the fault disapears.
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	// Hall sensors
	mcconf.m_sensor_port_mode = SENSOR_PORT_MODE_HALL;
	mc_interface_set_configuration(&mcconf);

	uint8_t hall_table[8];
	bool res = mcpwm_foc_hall_detect(current, hall_table);

	// Disable timeout and lock, as hall detection will undo the lock
	timeout_reset();
	timeout_configure(60000, 0.0);
	mc_interface_lock();

	if (res) {
		mcconf_old_second.m_sensor_port_mode = SENSOR_PORT_MODE_HALL;
		mcconf_old_second.foc_sensor_mode = FOC_SENSOR_MODE_HALL;
		for (int i = 0;i < 8;i++) {
			mcconf_old_second.foc_hall_table[i] = hall_table[i];
		}

		result = 1;
	}

	// AS5047 encoder
	if (!res) {
		mcconf.m_sensor_port_mode = SENSOR_PORT_MODE_AS5047_SPI;
		mc_interface_set_configuration(&mcconf);

		mcpwm_foc_set_openloop_phase(current, 0.0);
		chThdSleepMilliseconds(1000);

		float phase_start = encoder_read_deg();
		float phase_mid = 0.0;
		float phase_end = 0.0;

		for (int i = 0;i < 180.0;i++) {
			mcpwm_foc_set_openloop_phase(current, i);
			chThdSleepMilliseconds(5);

			if (i == 90) {
				phase_mid = encoder_read_deg();
			}
		}

		phase_end = encoder_read_deg();
		float diff = fabsf(utils_angle_difference(phase_start, phase_end));
		float diff_mid = fabsf(utils_angle_difference(phase_mid, phase_end));

		if (diff > 2.0 && (diff_mid - diff / 2.0) < (diff / 4)) {
			float offset, ratio;
			bool inverted;
			mcpwm_foc_encoder_detect(current, false, &offset, &ratio, &inverted);
			mcconf_old_second.m_sensor_port_mode = SENSOR_PORT_MODE_AS5047_SPI;
			mcconf_old_second.foc_sensor_mode = FOC_SENSOR_MODE_ENCODER;
			mcconf_old_second.foc_encoder_offset = offset;
			mcconf_old_second.foc_encoder_ratio = ratio;
			mcconf_old_second.foc_encoder_inverted = inverted;

			res = true;
			result = 2;
		}
	}

	// Sensorless
	if (!res) {
		mcconf_old_second.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
		result = 0;
		res = true;
	}

	timeout_configure(tout, tout_c);
	mc_interface_unlock();
	mc_interface_release_motor();
	mc_interface_set_configuration(&mcconf_old_second);

	// On success store the mc configuration, also send it to VESC Tool.
	if (res) {
		if (store_mcconf_on_success) {
			conf_general_store_mc_configuration(&mcconf_old_second);
		}

		if (send_mcconf_on_success) {
			commands_send_mcconf(COMM_GET_MCCONF, &mcconf_old_second);
		}
	}

	return result;
}

/**
 * Detect and apply all parameters, current limits and sensors.
 *
 * @param max_power_loss
 * The maximum power loss to derive current limits, as well as detection currents, from.
 *
 * @param store_mcconf_on_success
 * Store motor configuration in emulated EEPROM if the detection succeeds.
 *
 * @param send_mcconf_on_success
 * Send motor configuration if the detection succeeds.
 *
 * @return
 * >=0: Success, see conf_general_autodetect_apply_sensors_foc codes
 * -10: Flux linkage detection failed
 *  -x: see conf_general_autodetect_apply_sensors_foc faults
 */
int conf_general_detect_apply_all_foc(float max_power_loss,
		bool store_mcconf_on_success, bool send_mcconf_on_success) {
	int result = -1;

	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	mcconf.motor_type = MOTOR_TYPE_FOC;
	mcconf.foc_sensor_mode = FOC_SENSOR_MODE_SENSORLESS;
	mcconf.foc_f_sw = 10000.0; // Lower f_sw => less dead-time distortion
	mcconf.foc_current_kp = 0.0005;
	mcconf.foc_current_ki = 1.0;
	mcconf.l_current_max = MCCONF_L_CURRENT_MAX;
	mcconf.l_current_min = MCCONF_L_CURRENT_MIN;
	mcconf.l_current_max_scale = MCCONF_L_CURRENT_MAX_SCALE;
	mcconf.l_current_min_scale = MCCONF_L_CURRENT_MIN_SCALE;
	mcconf.l_watt_max = MCCONF_L_WATT_MAX;
	mcconf.l_watt_min = MCCONF_L_WATT_MIN;
	mcconf.l_max_erpm = MCCONF_L_RPM_MAX;
	mcconf.l_min_erpm = MCCONF_L_RPM_MIN;
	mc_interface_set_configuration(&mcconf);

	// Wait maximum 5s for fault code to disappear
	for (int i = 0;i < 500;i++) {
		if (mc_interface_get_fault() == FAULT_CODE_NONE) {
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (mc_interface_get_fault() != FAULT_CODE_NONE) {
		mc_interface_set_configuration(&mcconf_old);
		return -1;
	}

	// Wait one second for things to get ready after
	// the fault disappears.
	chThdSleepMilliseconds(1000);

	// Disable timeout
	systime_t tout = timeout_get_timeout_msec();
	float tout_c = timeout_get_brake_current();
	timeout_reset();
	timeout_configure(60000, 0.0);

	mc_interface_lock();

	float current_start = mcconf.l_current_max / 50;
	if (current_start < (mcconf.cc_min_current * 1.1)) {
		current_start = mcconf.cc_min_current * 1.1;
	}

	float i_last = 0.0;
	for (float i = current_start;i < mcconf.l_current_max;i *= 1.5) {
		float res_tmp = mcpwm_foc_measure_resistance(i, 5);
		i_last = i;

		if (mc_interface_get_fault() != FAULT_CODE_NONE) {
			timeout_configure(tout, tout_c);
			mc_interface_unlock();
			mc_interface_release_motor();
			mc_interface_set_configuration(&mcconf_old);
			return -11;
		}

		if ((i * i * res_tmp) >= (max_power_loss / 3.0)) {
			break;
		}
	}

	float r = mcpwm_foc_measure_resistance(i_last, 100);
	float l = mcpwm_foc_measure_inductance_current(i_last, 100, 0, 0) * 1e-6;
	float i_max = sqrtf(max_power_loss / r);
	utils_truncate_number(&i_max, HW_LIM_CURRENT);

	// Increase switching frequency for flux linkage measurement
	// as dead-time distortion has less effect at higher modulation.
	// Having a smooth rotation is more important.
	mcconf.foc_f_sw = 20000.0;
	mc_interface_set_configuration(&mcconf);

	float lambda = 0.0;
	int res = conf_general_measure_flux_linkage_openloop(i_max / 2.5, 0.3, 1800, r, &lambda);

	mc_motor_type old_type = mcconf_old.motor_type;
	float old_r = mcconf_old.foc_motor_r;
	float old_l = mcconf_old.foc_motor_l;
	float old_flux_linkage = mcconf_old.foc_motor_flux_linkage;
	float old_kp = mcconf_old.foc_current_kp;
	float old_ki = mcconf_old.foc_current_ki;
	float old_observer_gain = mcconf_old.foc_observer_gain;
	bool old_temp_comp = mcconf_old.foc_temp_comp;
	float old_temp_comp_base_temp = mcconf_old.foc_temp_comp_base_temp;

	if (res) {
		mcconf_old.l_current_max = i_max;
		mcconf_old.l_current_min = -i_max;

		float tc = 1000.0;
		float bw = 1.0 / (tc * 1e-6);
		float kp = l * bw;
		float ki = r * bw;
		float gain = (0.00001 / r) / (lambda * lambda);

		mcconf_old.motor_type = MOTOR_TYPE_FOC;
		mcconf_old.foc_motor_r = r;
		mcconf_old.foc_motor_l = l;
		mcconf_old.foc_motor_flux_linkage = lambda;
		mcconf_old.foc_current_kp = kp;
		mcconf_old.foc_current_ki = ki;
		mcconf_old.foc_observer_gain = gain * 1e6;

		// Temperature compensation
		// Skip temperature compensation for now, as it seems to make
		// things worse on some setups.
//		if (mc_interface_temp_motor_filtered() > 0.0) {
//			mcconf_old.foc_temp_comp = true;
//			mcconf_old.foc_temp_comp_base_temp = mc_interface_temp_motor_filtered();
//		} else {
//			mcconf_old.foc_temp_comp = false;
//		}
	} else {
		result = -10;
	}

	timeout_configure(tout, tout_c);
	mc_interface_unlock();
	mc_interface_release_motor();
	mc_interface_set_configuration(&mcconf_old);

	// Restore initial settings on sensor detection failure
	if (res) {
		// Wait for motor to stop
		chThdSleepMilliseconds(100);
		for (int i = 0;i < 1000;i++) {
			if (fabsf(mc_interface_get_rpm()) > 100.0) {
				chThdSleepMilliseconds(10);
			} else {
				break;
			}
		}

		// This will also store the settings to emulated eeprom and send them to vesc tool
		result = conf_general_autodetect_apply_sensors_foc(i_max / 3.0,
				store_mcconf_on_success, send_mcconf_on_success);
	} else {
		mcconf_old.motor_type = old_type;
		mcconf_old.foc_motor_r = old_r;
		mcconf_old.foc_motor_l = old_l;
		mcconf_old.foc_motor_flux_linkage = old_flux_linkage;
		mcconf_old.foc_current_kp = old_kp;
		mcconf_old.foc_current_ki = old_ki;
		mcconf_old.foc_observer_gain = old_observer_gain;
		mcconf_old.foc_temp_comp = old_temp_comp;
		mcconf_old.foc_temp_comp_base_temp = old_temp_comp_base_temp;
		mc_interface_set_configuration(&mcconf_old);
	}

	return result;
}

/**
 * Same as conf_general_detect_apply_all_foc, but also start detection in VESCs found on the CAN-bus.
 *
 * @param detect_can
 * Run detection on VESCs found on the CAN-bus as well. Setting this to false makes
 * this function behave like conf_general_detect_apply_all_foc, with the convenience
 * of also applying the settings.
 *
 * @param max_power_loss
 * The maximum power loss to derive current limits, as well as detection currents, from.
 *
 * @param min_current_in
 * Minimum input current (negative value). 0 means leave it unchanged.
 *
 * @param max_current_in
 * MAximum input current. 0 means leave it unchanged.
 *
 * @param openloop_rpm
 * FOC openloop ERPM in sensorless mode. 0 means leave it unchanged.
 *
 * @param sl_erpm
 * FOC ERPM above which sensorless should be used in sensored modes. 0 means leave it unchanged.
 *
 * @return
 * Same as conf_general_detect_apply_all_foc, and
 * -50: CAN detection timed out
 * -51: CAN detection failed
 */
int conf_general_detect_apply_all_foc_can(bool detect_can, float max_power_loss,
		float min_current_in, float max_current_in, float openloop_rpm, float sl_erpm) {
	app_configuration appconf = *app_get_configuration();
	uint8_t id_new = appconf.controller_id;

	mcconf = *mc_interface_get_configuration();

	if (fabsf(min_current_in) > 0.001) {
		mcconf.l_in_current_min = min_current_in;
	} else {
		mcconf.l_in_current_min = MCCONF_L_IN_CURRENT_MIN;
	}

	if (fabsf(max_current_in) > 0.001) {
		mcconf.l_in_current_max = max_current_in;
	} else {
		mcconf.l_in_current_max = MCCONF_L_IN_CURRENT_MAX;
	}

	if (fabsf(openloop_rpm) > 0.001) {
		mcconf.foc_openloop_rpm = openloop_rpm;
	} else {
		mcconf.foc_openloop_rpm = MCCONF_FOC_OPENLOOP_RPM;
	}

	if (fabsf(sl_erpm) > 0.001) {
		mcconf.foc_sl_erpm = sl_erpm;
	} else {
		mcconf.foc_sl_erpm = MCCONF_FOC_SL_ERPM;
	}

	mc_interface_set_configuration(&mcconf);
	int can_devs = 0;
	comm_can_detect_all_foc_res_clear();

	if (detect_can) {
		for (int i = 0;i < 255;i++) {
			if (comm_can_ping(i)) {
				comm_can_conf_current_limits_in(i, false, mcconf.l_in_current_min, mcconf.l_in_current_max);
				comm_can_conf_foc_erpms(i, false, mcconf.foc_openloop_rpm, mcconf.foc_sl_erpm);
				comm_can_detect_apply_all_foc(i, true, max_power_loss);
				can_devs++;

				if (i == id_new) {
					id_new++;
				}

				if (id_new == 255) {
					id_new = 0;
				}
			}
		}
	}

	int res = conf_general_detect_apply_all_foc(max_power_loss, false, false);

	// Wait for all VESCs on the CAN-bus to finish detection
	int timeout = true;
	for (int i = 0;i < 4000;i++) {
		if (comm_can_detect_all_foc_res_size() >= can_devs) {
			timeout = false;
			break;
		}
		chThdSleepMilliseconds(10);
	}

	if (timeout) {
		res = -50;
	} else {
		for (int i = 0;i < can_devs;i++) {
			if (comm_can_detect_all_foc_res(i) < 0) {
				res = -51;
			}
		}
	}

	// Store and send settings
	if (res >= 0) {
		if (appconf.controller_id != id_new || appconf.send_can_status != CAN_STATUS_1_2_3_4) {
			appconf.controller_id = id_new;
			appconf.send_can_status = CAN_STATUS_1_2_3_4;
			conf_general_store_app_configuration(&appconf);
			app_set_configuration(&appconf);
			commands_send_appconf(COMM_GET_APPCONF, &appconf);
			chThdSleepMilliseconds(1000);
		}

		mcconf = *mc_interface_get_configuration();
		conf_general_store_mc_configuration(&mcconf);
		commands_send_mcconf(COMM_GET_MCCONF, &mcconf);
		chThdSleepMilliseconds(1000);
	}

	return res;
}
