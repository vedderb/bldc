/*
	Copyright 2019 - 2020 Benjamin Vedder	benjamin@vedder.se

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

#include "app.h"
#include "ch.h"
#include "hal.h"

#include "conf_general.h"
#include "mc_interface.h"
#include "utils.h"
#include "encoder/encoder.h"
#include "terminal.h"
#include "comm_can.h"
#include "hw.h"
#include "commands.h"
#include "timeout.h"
#include "comm_can.h"
#include "buffer.h"
#include "lispif.h"

#include <math.h>
#include <string.h>
#include <stdio.h>

#define IS_STANDALONE				0

// Macros
#define IS_SPORT_MODE()				(m_set_now == &m_set_sport)
#define IS_ECO_MODE()				(m_set_now == &m_set_eco)
#define LED_ECO_ON()				comm_can_io_board_set_output_digital(255, 1, 1); m_led_eco_on = true
#define LED_ECO_OFF()				comm_can_io_board_set_output_digital(255, 1, 0); m_led_eco_on = false
#define LED_SPORT_ON()				comm_can_io_board_set_output_digital(255, 2, 1); m_led_sport_on = true
#define LED_SPORT_OFF()				comm_can_io_board_set_output_digital(255, 2, 0); m_led_sport_on = false
#define LED_LOW_BATT_ON()			comm_can_io_board_set_output_digital(255, 3, 1); m_led_low_batt_on = true
#define LED_LOW_BATT_OFF()			comm_can_io_board_set_output_digital(255, 3, 0); m_led_low_batt_on = false
#define LED_FAULT_ON()				comm_can_io_board_set_output_digital(255, 4, 1); m_led_fault_on = true
#define LED_FAULT_OFF()				comm_can_io_board_set_output_digital(255, 4, 0); m_led_fault_on = false

// Threads
static THD_FUNCTION(my_thread, arg);
static THD_WORKING_AREA(my_thread_wa, 2048);

// Private variables
static volatile bool stop_now = true;
static volatile bool is_running = false;
static volatile bool plot_state = false;
static volatile float plot_sample = 0.0;
static volatile adc_config app_adc_config;

static volatile bool m_brake_rear = false;
static volatile bool m_brake_front = false;
static volatile bool m_mode_btn_down = false;
static volatile bool m_kill_sw = false;
static volatile bool m_stand = false;

static volatile bool m_led_eco_on = false;
static volatile bool m_led_sport_on = false;
static volatile bool m_led_low_batt_on = false;
static volatile bool m_led_fault_on = false;

// Parameters
#define P_OFFSET_ECO								10
#define P_OFFSET_SPORT								20
#define P_THROTTLE_HYST_ADDR						0
#define P_PEDAL_CURRENT_ADDR						1
#define P_START_GAIN_ADDR							2
#define P_START_GAIN_END_SPEED_ADDR					3
#define P_OUTPUT_POWER_ADDR							4
#define P_TOP_SPEED_ERPM_ADDR						5
#define P_BRAKE_CURRENT_FRONT_ADDR					6
#define P_BRAKE_CURRENT_REAR_ADDR					7
#define P_BRAKE_CURRENT_BOTH_ADDR					8

typedef enum {
	ER_MSG_SET_MODE_PARAMS = 0,
	ER_MSG_GET_MODE_PARAMS,
	ER_MSG_GET_IO,
	ER_MSG_RESTORE_SETTINGS,
	ER_MSG_SET_MOTORS_ENABLED,
	ER_MSG_SET_PEDAL_TEST_MODE
} ER_MSG;

typedef struct {
	volatile float p_throttle_hyst;
	volatile float p_pedal_current;
	volatile float p_start_gain;
	volatile float p_start_gain_end_speed;
	volatile float p_output_power;
	volatile float p_top_speed_erpm;
	volatile float p_brake_current_front;
	volatile float p_brake_current_rear;
	volatile float p_brake_current_both;
} SETTINGS_T;

static volatile SETTINGS_T m_set_normal;
static volatile SETTINGS_T m_set_eco;
static volatile SETTINGS_T m_set_sport;
static volatile SETTINGS_T *m_set_now = &m_set_normal;
static volatile int m_set_now_offset = 0;
static volatile bool m_motors_enabled = true;
static volatile bool m_pedal_test_mode = false;

// Private functions
static void process_custom_app_data(unsigned char *data, unsigned int len);
static void terminal_plot(int argc, const char **argv);
static void terminal_info(int argc, const char **argv);
static void terminal_mon(int argc, const char **argv);
static void terminal_restore_settings(int argc, const char **argv);

static lbm_value ext_pedal_rpm(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(mc_interface_get_speed());
}

static lbm_value ext_pedal_get_current(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	return lbm_enc_float(m_set_now->p_pedal_current);
}

static lbm_value ext_pedal_set_current(lbm_value *args, lbm_uint argn) {
	LBM_CHECK_ARGN_NUMBER(1);
	m_set_now->p_pedal_current = lbm_dec_as_float(args[0]);
	return ENC_SYM_TRUE;
}

void load_lisp_extensions(void) {
	lbm_add_extension("pedal-get-rpm", ext_pedal_rpm);
	lbm_add_extension("pedal-set-current", ext_pedal_set_current);
	lbm_add_extension("pedal-get-current", ext_pedal_get_current);
}

static void restore_settings(void) {
	m_set_eco.p_throttle_hyst = 0.04;
	m_set_eco.p_pedal_current = 25.0;
	m_set_eco.p_start_gain = 2.9;
	m_set_eco.p_start_gain_end_speed = 7.8;
	m_set_eco.p_output_power = 0.7;
	m_set_eco.p_top_speed_erpm = 2000;
	m_set_eco.p_brake_current_front = 0.5;
	m_set_eco.p_brake_current_rear = 0.5;
	m_set_eco.p_brake_current_both = 1.0;

	m_set_normal.p_throttle_hyst = 0.04;
	m_set_normal.p_pedal_current = 20.0;
	m_set_normal.p_start_gain = 3.4;
	m_set_normal.p_start_gain_end_speed = 9.0;
	m_set_normal.p_output_power = 0.85;
	m_set_normal.p_top_speed_erpm = 2000;
	m_set_normal.p_brake_current_front = 0.5;
	m_set_normal.p_brake_current_rear = 0.5;
	m_set_normal.p_brake_current_both = 1.0;

	m_set_sport.p_throttle_hyst = 0.04;
	m_set_sport.p_pedal_current = 15.0;
	m_set_sport.p_start_gain = 4.0;
	m_set_sport.p_start_gain_end_speed = 15.0;
	m_set_sport.p_output_power = 1.0;
	m_set_sport.p_top_speed_erpm = 2000;
	m_set_sport.p_brake_current_front = 0.5;
	m_set_sport.p_brake_current_rear = 0.5;
	m_set_sport.p_brake_current_both = 1.0;
}

static void load_settings(volatile SETTINGS_T *set, int offset) {
	eeprom_var v;
	if (conf_general_read_eeprom_var_custom(&v, P_THROTTLE_HYST_ADDR + offset)) {
		set->p_throttle_hyst = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_PEDAL_CURRENT_ADDR + offset)) {
		set->p_pedal_current = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_START_GAIN_ADDR + offset)) {
		set->p_start_gain = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_START_GAIN_END_SPEED_ADDR + offset)) {
		set->p_start_gain_end_speed = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_OUTPUT_POWER_ADDR + offset)) {
		set->p_output_power = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_TOP_SPEED_ERPM_ADDR + offset)) {
		set->p_top_speed_erpm = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_BRAKE_CURRENT_FRONT_ADDR + offset)) {
		set->p_brake_current_front = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_BRAKE_CURRENT_REAR_ADDR + offset)) {
		set->p_brake_current_rear = v.as_float;
	}
	if (conf_general_read_eeprom_var_custom(&v, P_BRAKE_CURRENT_BOTH_ADDR + offset)) {
		set->p_brake_current_both = v.as_float;
	}
}

static void store_settings(volatile SETTINGS_T *set, int offset) {
	eeprom_var v;
	v.as_float = set->p_throttle_hyst;
	conf_general_store_eeprom_var_custom(&v, P_THROTTLE_HYST_ADDR + offset);

	v.as_float = set->p_pedal_current;
	conf_general_store_eeprom_var_custom(&v, P_PEDAL_CURRENT_ADDR + offset);

	v.as_float = set->p_start_gain;
	conf_general_store_eeprom_var_custom(&v, P_START_GAIN_ADDR + offset);

	v.as_float = set->p_start_gain_end_speed;
	conf_general_store_eeprom_var_custom(&v, P_START_GAIN_END_SPEED_ADDR + offset);

	v.as_float = set->p_output_power;
	conf_general_store_eeprom_var_custom(&v, P_OUTPUT_POWER_ADDR + offset);

	v.as_float = set->p_top_speed_erpm;
	conf_general_store_eeprom_var_custom(&v, P_TOP_SPEED_ERPM_ADDR + offset);

	v.as_float = set->p_brake_current_front;
	conf_general_store_eeprom_var_custom(&v, P_BRAKE_CURRENT_FRONT_ADDR + offset);

	v.as_float = set->p_brake_current_rear;
	conf_general_store_eeprom_var_custom(&v, P_BRAKE_CURRENT_REAR_ADDR + offset);

	v.as_float = set->p_brake_current_both;
	conf_general_store_eeprom_var_custom(&v, P_BRAKE_CURRENT_BOTH_ADDR + offset);
}

void app_custom_start(void) {
	restore_settings();
	load_settings(&m_set_normal, 0);
	load_settings(&m_set_eco, P_OFFSET_ECO);
	load_settings(&m_set_sport, P_OFFSET_SPORT);

	app_adc_config = app_get_configuration()->app_adc_conf;

	stop_now = false;
	chThdCreateStatic(my_thread_wa, sizeof(my_thread_wa),
			NORMALPRIO, my_thread, NULL);

	terminal_register_command_callback(
			"er_plot_en",
			"Enable state plotting",
			"[en]",
			terminal_plot);

	terminal_register_command_callback(
			"er_info",
			"Print current settings",
			0,
			terminal_info);

	terminal_register_command_callback(
			"er_mon",
			"Monitor IO for 30 seconds.",
			0,
			terminal_mon);

	terminal_register_command_callback(
			"er_restore_settings",
			"Restore all settings to their default values",
			0,
			terminal_restore_settings);

	commands_set_app_data_handler(process_custom_app_data);

	app_adc_start(false);

	lispif_add_ext_load_callback(load_lisp_extensions);
}

void app_custom_stop(void) {
	terminal_unregister_callback(terminal_plot);
	terminal_unregister_callback(terminal_info);
	terminal_unregister_callback(terminal_mon);
	terminal_unregister_callback(terminal_restore_settings);

	commands_set_app_data_handler(0);

	stop_now = true;
	while (is_running) {
		chThdSleepMilliseconds(1);
	}
}

void app_custom_configure(app_configuration *conf) {
	app_adc_config = conf->app_adc_conf;
}

static THD_FUNCTION(my_thread, arg) {
	(void)arg;

	chRegSetThreadName("Erockit");

	is_running = true;

#if !IS_STANDALONE

	app_disable_output(5000);

	LED_ECO_ON();
	chThdSleepMilliseconds(500);
	LED_ECO_OFF();
	chThdSleepMilliseconds(500);
	LED_SPORT_ON();
	chThdSleepMilliseconds(500);
	LED_SPORT_OFF();
	chThdSleepMilliseconds(500);
	LED_LOW_BATT_ON();
	chThdSleepMilliseconds(500);
	LED_LOW_BATT_OFF();
	chThdSleepMilliseconds(500);
	LED_FAULT_ON();
	chThdSleepMilliseconds(500);
	LED_FAULT_OFF();
	chThdSleepMilliseconds(500);

	const float poles = 8;
	const float gearing = 112.0 / 18.0;
	const float wheel_d = 0.585;
	const float output_filter = 0.3;
	bool was_mode_button_pressed = false;
#else
	m_pedal_test_mode = true;
#endif

	const float stop_timer_max = 50.0;
	const float S_min = 200.0;
	const float S_ramp_step_up = 10.0;
	const float S_ramp_step_down = 0.1;

	float erpm_now = 0.0;
	bool running = false;
	float stop_timer = stop_timer_max;
	float S_f = 0.0;

	for(;;) {

#if !IS_STANDALONE
		io_board_adc_values *io_v1 = comm_can_get_io_board_adc_1_4_index(0);
		io_board_adc_values *io_v2 = comm_can_get_io_board_adc_5_8_index(0);

		if (io_v1 && io_v2 && UTILS_AGE_S(io_v1->rx_time) < 1.0 && UTILS_AGE_S(io_v2->rx_time) < 1.0) {
			m_brake_rear = io_v2->adc_voltages[3] > 6.0;
			m_brake_front = io_v2->adc_voltages[2] > 6.0;
			m_kill_sw = io_v1->adc_voltages[0] > 6.0;
			m_mode_btn_down = io_v1->adc_voltages[1] > 6.0;
			m_stand = io_v1->adc_voltages[2] > 6.0;
		} else {
			m_brake_rear = false;
			m_brake_front = false;
			m_kill_sw = false;
			m_mode_btn_down = false;
			m_stand = false;
		}

		float kmh_now = -1.0;
		can_status_msg *msg = comm_can_get_status_msg_index(0);
		if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < 0.5) {
			float rpm = fabsf(msg->rpm / (poles / 2.0));
			kmh_now = ((rpm / 60.0) * wheel_d * M_PI / gearing) * 3.6;
		}
#endif

		// Hook hand throttle up to ADC app
		float app_adc_pwr = 0.0;
		{
			app_disable_output(100);
			app_adc_pwr = app_adc_get_decoded_level();
			utils_deadband(&app_adc_pwr, app_adc_config.hyst, 1.0);
			app_adc_pwr = utils_throttle_curve(
					app_adc_pwr,
					app_adc_config.throttle_exp,
					app_adc_config.throttle_exp_brake,
					app_adc_config.throttle_exp_mode);
			static systime_t last_time = 0;
			static float pwr_ramp = 0.0;
			float ramp_time = fabsf(app_adc_pwr) > fabsf(pwr_ramp) ? app_adc_config.ramp_time_pos : app_adc_config.ramp_time_neg;

			if (ramp_time > 0.01) {
				const float ramp_step = (float)ST2MS(chVTTimeElapsedSinceX(last_time)) / (ramp_time * 1000.0);
				utils_step_towards(&pwr_ramp, app_adc_pwr, ramp_step);
				last_time = chVTGetSystemTimeX();
				app_adc_pwr = pwr_ramp;
			}

			if (app_adc_config.ctrl_type == ADC_CTRL_TYPE_NONE) {
				app_adc_pwr = 0.0;
			}
		}

		if (!m_motors_enabled) {
			chThdSleepMilliseconds(100);
			continue;
		}

		timeout_reset();

		// Check if it is time to stop.
		if (stop_now) {
			is_running = false;
			return;
		}

		UTILS_LP_FAST(S_f, mc_interface_get_rpm(), 0.2);
		float I = mc_interface_get_tot_current_directional_filtered();

		// Ramp up target current on low pedal speeds
		float erpm_ramp_in = 1500.0;
		float erpm_motor = fabsf(mc_interface_get_rpm());
		float pedal_current_target = m_set_now->p_pedal_current;
		if (erpm_motor < erpm_ramp_in) {
			pedal_current_target = utils_map(erpm_motor, 0.0, erpm_ramp_in, 0.0, pedal_current_target);
		}

		if ((!m_kill_sw || m_brake_front || m_brake_rear || m_stand) && !m_pedal_test_mode) {
			running = false;
		}

		if (running) {
			mc_interface_set_pid_speed(erpm_now);

			float error = (-pedal_current_target - I);
			erpm_now += error * (error > 0.0 ? S_ramp_step_up : S_ramp_step_down);
			utils_truncate_number(&erpm_now, S_min, 4000.0);

			if (I < -1.0) {
				if (stop_timer < stop_timer_max) {
					stop_timer++;
				}
			} else {
				stop_timer -= (I + 1.0) * 0.5;
				if (stop_timer <= 0) {
					running = false;
				}
			}
		} else {
			mc_interface_set_current(0);

			if (S_f > S_min) {
				stop_timer += 1.0;
				if (stop_timer >= (stop_timer_max / 2)) {
					running = true;
					erpm_now = S_f;
				}
			} else {
				if (stop_timer > 0) {
					stop_timer--;
				}
			}

			utils_step_towards(&erpm_now, S_min, S_ramp_step_down);
		}

#if !IS_STANDALONE

		static float plot_pwr = 0.0;
		static float plot_pwr_nofilter = 0.0;

		// Logic and output at 200 Hz
		static int can_div = 0;
		can_div++;
		if (can_div > 5) {
			can_div = 0;

			bool mode_clicked = m_mode_btn_down && !was_mode_button_pressed;
			was_mode_button_pressed = m_mode_btn_down;

			if (mode_clicked) {
				if (IS_ECO_MODE()) {
					m_set_now = &m_set_sport;
					m_set_now_offset = P_OFFSET_SPORT;
				} else if (IS_SPORT_MODE()) {
					m_set_now = &m_set_normal;
					m_set_now_offset = 0;
				} else {
					m_set_now = &m_set_eco;
					m_set_now_offset = P_OFFSET_ECO;
				}
			}

			if (IS_SPORT_MODE()) {
				LED_SPORT_ON();
			} else {
				LED_SPORT_OFF();
			}

			if (IS_ECO_MODE()) {
				LED_ECO_ON();
			} else {
				LED_ECO_OFF();
			}

			can_status_msg_4 *msg4 = comm_can_get_status_msg_4_index(0);
			if (msg4->id >= 0 && UTILS_AGE_S(msg4->rx_time) < 0.5) {
				if (msg4->temp_fet > 88.0 || msg4->temp_motor > 100.0) {
					LED_FAULT_ON();
				} else {
					LED_FAULT_OFF();
				}
			}

			can_status_msg_5 *msg5 = comm_can_get_status_msg_5_index(0);
			if (msg5->id >= 0 && UTILS_AGE_S(msg5->rx_time) < 0.5) {
				static float v_batt_filter = 0.0;
				UTILS_LP_FAST(v_batt_filter, msg5->v_in, 0.01);

				if ((v_batt_filter / 14) < 3.3) {
					LED_LOW_BATT_ON();
				} else if ((v_batt_filter / 14) > 3.6) {
					LED_LOW_BATT_OFF();
				}
			}

			static float pwr = 0.0;
			static float pwr_hyst = 0.0;
			static bool hyst_stepping = false;

			float brake = 0.0;
			if (m_brake_front && !m_brake_rear) {
				brake = fabsf(m_set_now->p_brake_current_front);
			} else if (!m_brake_front && m_brake_rear) {
				brake = fabsf(m_set_now->p_brake_current_rear);
			} else if (m_brake_front && m_brake_rear) {
				brake = fabsf(m_set_now->p_brake_current_both);
			}

			if (running) {
				UTILS_LP_FAST(pwr, utils_map(fabsf(mc_interface_get_rpm()), S_min,
						m_set_now->p_top_speed_erpm, 0.0, 1.0), output_filter);

				utils_truncate_number(&pwr, 0.0, 1.0);
			} else {
				utils_step_towards(&pwr, 0.0, 0.01);
			}

			float diff = fabsf(pwr - pwr_hyst);

			if (diff < (m_set_now->p_throttle_hyst / 2.0)) {
				hyst_stepping = false;
			}

			if (diff > m_set_now->p_throttle_hyst ||
					pwr < (m_set_now->p_throttle_hyst * 1.2) ||
					pwr > (1.0 - m_set_now->p_throttle_hyst * 1.2)) {
				hyst_stepping = true;
			}

			if (hyst_stepping) {
				utils_step_towards(&pwr_hyst, pwr, 0.02);
			}

			// Apply low speed boost
			float pwr_out = pwr_hyst;
			if (kmh_now >= 0.0 && kmh_now < m_set_now->p_start_gain_end_speed) {
				pwr_out *= utils_map(kmh_now, 0.0, m_set_now->p_start_gain_end_speed,
						m_set_now->p_start_gain, 1.0);
				if (pwr_out > 1.0) {
					pwr_out = 1.0;
				}
			}

			if (!m_kill_sw || m_pedal_test_mode || m_stand) {
				comm_can_set_current_brake_rel(255, 0.0);
			} else if (brake > 0.0001) {
				comm_can_set_current_brake_rel(255, brake);
			} else {
				if (app_adc_pwr > 0.001) {
					comm_can_set_current_rel(255, app_adc_pwr * m_set_now->p_output_power);
				} else {
					comm_can_set_current_rel(255, pwr_out * m_set_now->p_output_power);
				}
			}

			plot_pwr = pwr_out;
			plot_pwr_nofilter = pwr;
		}

		if (plot_state) {
			static int plot_div = 0;
			plot_div++;
			if (plot_div > 50) {
				plot_div = 0;
				commands_plot_set_graph(0);
				commands_send_plot_points(plot_sample, (float)stop_timer / (float)stop_timer_max);
				commands_plot_set_graph(1);
				commands_send_plot_points(plot_sample, plot_pwr * m_set_now->p_output_power);
				commands_plot_set_graph(2);
				commands_send_plot_points(plot_sample, plot_pwr_nofilter);
				plot_sample++;
			}
		}
#endif

		chThdSleepMilliseconds(1);
	}
}

static void process_custom_app_data(unsigned char *data, unsigned int len) {
	(void)len;

	int32_t ind = 0;
	ER_MSG msg = data[ind++];

	switch (msg) {
	case ER_MSG_SET_MODE_PARAMS: {
		m_set_now->p_throttle_hyst = buffer_get_float32_auto(data, &ind);
		m_set_now->p_pedal_current = buffer_get_float32_auto(data, &ind);
		m_set_now->p_start_gain = buffer_get_float32_auto(data, &ind);
		m_set_now->p_start_gain_end_speed = buffer_get_float32_auto(data, &ind);
		m_set_now->p_output_power = buffer_get_float32_auto(data, &ind);
		m_set_now->p_top_speed_erpm = buffer_get_float32_auto(data, &ind);
		m_set_now->p_brake_current_front = buffer_get_float32_auto(data, &ind);
		m_set_now->p_brake_current_rear = buffer_get_float32_auto(data, &ind);
		m_set_now->p_brake_current_both = buffer_get_float32_auto(data, &ind);

		store_settings(m_set_now, m_set_now_offset);

		// Send ack
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		commands_send_app_data(dataTx, ind);
	} break;

	case ER_MSG_GET_MODE_PARAMS: {
		uint8_t dataTx[50];
		ind = 0;

		dataTx[ind++] = msg;
		buffer_append_float32_auto(dataTx, m_set_now->p_throttle_hyst, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_pedal_current, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_start_gain, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_start_gain_end_speed, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_output_power, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_top_speed_erpm, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_brake_current_front, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_brake_current_rear, &ind);
		buffer_append_float32_auto(dataTx, m_set_now->p_brake_current_both, &ind);

		commands_send_app_data(dataTx, ind);
	} break;

	case ER_MSG_GET_IO: {
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		dataTx[ind++] = m_mode_btn_down;
		dataTx[ind++] = m_brake_front;
		dataTx[ind++] = m_brake_rear;
		dataTx[ind++] = m_kill_sw;

		dataTx[ind++] = m_led_eco_on;
		dataTx[ind++] = m_led_sport_on;
		dataTx[ind++] = m_led_low_batt_on;
		dataTx[ind++] = m_led_fault_on;

		dataTx[ind++] = m_stand;

		commands_send_app_data(dataTx, ind);
	} break;

	case ER_MSG_RESTORE_SETTINGS: {
		restore_settings();
		store_settings(&m_set_normal, 0);
		store_settings(&m_set_eco, P_OFFSET_ECO);
		store_settings(&m_set_sport, P_OFFSET_SPORT);

		// Send ack
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		commands_send_app_data(dataTx, ind);
	} break;

	case ER_MSG_SET_MOTORS_ENABLED: {
		m_motors_enabled = data[ind++];

		// Send ack
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		dataTx[ind++] = m_motors_enabled;
		commands_send_app_data(dataTx, ind);
	} break;

	case ER_MSG_SET_PEDAL_TEST_MODE: {
		m_pedal_test_mode = data[ind++];

		// Send ack
		uint8_t dataTx[50];
		ind = 0;
		dataTx[ind++] = msg;
		dataTx[ind++] = m_pedal_test_mode;
		commands_send_app_data(dataTx, ind);
	} break;

	default:
		break;
	}
}

static void terminal_plot(int argc, const char **argv) {
	if (argc == 2) {
		int d = -1;
		sscanf(argv[1], "%d", &d);

		if (d == 0 || d == 1) {
			plot_state = d;
			if (plot_state) {
				plot_sample = 0.0;
				commands_init_plot("Sample", "Value");
				commands_plot_add_graph("Timer");
				commands_plot_add_graph("I out");
				commands_plot_add_graph("I nofilter");
			}

			commands_printf(plot_state ?
						"Experiment plot enabled" :
						"Experiment plot disabled");
		} else {
			commands_printf("Invalid Argument. en has to be 0 or 1.\n");
		}
	} else {
		commands_printf("This command requires one argument.\n");
	}
}

static void print_settings_info(volatile SETTINGS_T *set) {
	commands_printf("p_throttle_hyst        : %.2f", (double)(set->p_throttle_hyst));
	commands_printf("p_pedal_current        : %.2f", (double)(set->p_pedal_current));
	commands_printf("p_start_gain           : %.2f", (double)(set->p_start_gain));
	commands_printf("p_start_gain_end_speed : %.2f", (double)(set->p_start_gain_end_speed));
	commands_printf("p_output_power         : %.2f", (double)(set->p_output_power));
	commands_printf("p_top_speed_erpm       : %.3f", (double)(set->p_top_speed_erpm));
	commands_printf("p_brake_current_front  : %.2f", (double)(set->p_brake_current_front));
	commands_printf("p_brake_current_rear   : %.2f", (double)(set->p_brake_current_rear));
	commands_printf("p_brake_current_both   : %.2f", (double)(set->p_brake_current_both));
	commands_printf(" ");
}

static void terminal_info(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	if (IS_SPORT_MODE()) {
		commands_printf("Mode now: Sport\n");
	} else if (IS_ECO_MODE()) {
		commands_printf("Mode now: Eco\n");
	} else {
		commands_printf("Mode now: Normal\n");
	}

	commands_printf("Normal Mode");
	print_settings_info(&m_set_normal);

	commands_printf("Eco Mode");
	print_settings_info(&m_set_eco);

	commands_printf("Sport Mode");
	print_settings_info(&m_set_sport);
}

static void terminal_mon(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	commands_printf("Monitoring IO");

	bool mode_last = m_mode_btn_down;
	bool brake_front_last = m_brake_front;
	bool brake_rear_last = m_brake_rear;
	bool kill_last = m_kill_sw;

	commands_printf("Mode Button  : %d", m_mode_btn_down);
	commands_printf("Brake Front  : %d", m_brake_front);
	commands_printf("Brake Rear   : %d", m_brake_rear);
	commands_printf("Kill SW      : %d", m_kill_sw);

	for (int i = 0;i < 3000;i++) {
		if (m_mode_btn_down != mode_last) {
			mode_last = m_mode_btn_down;
			commands_printf("Mode Button  : %d", m_mode_btn_down);
		}

		if (m_brake_front != brake_front_last) {
			brake_front_last = m_brake_front;
			commands_printf("Brake Front  : %d", m_brake_front);
		}

		if (m_brake_rear != brake_rear_last) {
			brake_rear_last = m_brake_rear;
			commands_printf("Brake Rear   : %d", m_brake_rear);
		}

		if (m_kill_sw != kill_last) {
			kill_last = m_kill_sw;
			commands_printf("Kill SW      : %d", m_kill_sw);
		}

		chThdSleepMilliseconds(10);
	}

	commands_printf("Monitoring IO ended\n");
}

static void terminal_restore_settings(int argc, const char **argv) {
	(void)argc;
	(void)argv;

	restore_settings();
	store_settings(&m_set_normal, 0);
	store_settings(&m_set_eco, P_OFFSET_ECO);
	store_settings(&m_set_sport, P_OFFSET_SPORT);

	commands_printf("Settings have been restored to their default values.");
}
