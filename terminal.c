/*
	Copyright 2016 - 2020 Benjamin Vedder	benjamin@vedder.se

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

#include "ch.h"
#include "hal.h"
#include "terminal.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "commands.h"
#include "hw.h"
#include "comm_can.h"
#include "utils.h"
#include "timeout.h"
#include "encoder.h"
#include "app.h"
#include "comm_usb.h"
#include "comm_usb_serial.h"
#include "mempools.h"
#include "crc.h"

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "terminal_cmnds/cmnd_dictionary.h"

// Settings
#define FAULT_VEC_LEN						25
#define CALLBACK_LEN						40



// Private variables
static volatile fault_data fault_vec[FAULT_VEC_LEN];
static volatile int fault_vec_write = 0;
static terminal_callback_struct callbacks[CALLBACK_LEN];
static int callback_write = 0;

void terminal_process_string(char *str) {
	enum { kMaxArgs = 64 };
	int argc = 0;
	char *argv[kMaxArgs];

	char *p2 = strtok(str, " ");
	while (p2 && argc < kMaxArgs) {
		argv[argc++] = p2;
		p2 = strtok(0, " ");
	}

	if (argc == 0) {
		commands_printf("No command received\n");
		return;
	}

	for (int i = 0;i < callback_write;i++) {
		if (callbacks[i].cbf != 0 && strcmp(argv[0], callbacks[i].command) == 0) {
			callbacks[i].cbf(argc, (const char**)argv);
			return;
		}
	}

	if (strcmp(argv[0], "ping") == 0) {
		commands_printf("pong\n");
	} else if (strcmp(argv[0], "stop") == 0) {
      cmnd_processs_stop();
	} else if (strcmp(argv[0], "last_adc_duration") == 0) {
      cmnd_processs_last_adc_duration();
	} else if (strcmp(argv[0], "kv") == 0) {
      cmnd_processs_kv();
	} else if (strcmp(argv[0], "mem") == 0) {
      cmnd_processs_mem();
	} else if (strcmp(argv[0], "threads") == 0) {
      cmnd_processs_threads();
	} else if (strcmp(argv[0], "fault") == 0) {
		commands_printf("%s\n", mc_interface_fault_to_string(mc_interface_get_fault()));
	} else if (strcmp(argv[0], "faults") == 0) {
      cmnd_processs_faults(fault_vec_write, (fault_data *)fault_vec);
	} else if (strcmp(argv[0], "rpm") == 0) {
		commands_printf("Electrical RPM: %.2f rpm\n", (double)mc_interface_get_rpm());
	} else if (strcmp(argv[0], "tacho") == 0) {
		commands_printf("Tachometer counts: %i\n", mc_interface_get_tachometer_value(0));
	} else if (strcmp(argv[0], "dist") == 0) {
		commands_printf("Trip dist.      : %.2f m", (double)mc_interface_get_distance());
		commands_printf("Trip dist. (ABS): %.2f m", (double)mc_interface_get_distance_abs());
		commands_printf("Odometer        : %u   m\n", mc_interface_get_odometer());
	} else if (strcmp(argv[0], "tim") == 0) {
      cmnd_processs_tim();
	} else if (strcmp(argv[0], "volt") == 0) {
		commands_printf("Input voltage: %.2f\n", (double)GET_INPUT_VOLTAGE());
#ifdef HW_HAS_GATE_DRIVER_SUPPLY_MONITOR
		commands_printf("Gate driver power supply output voltage: %.2f\n", (double)GET_GATE_DRIVER_SUPPLY_VOLTAGE());
#endif
	} else if (strcmp(argv[0], "param_detect") == 0) {
		// Use COMM_MODE_DELAY and try to figure out the motor parameters.
		if (argc == 4) {
			float current = -1.0;
			float min_rpm = -1.0;
			float low_duty = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &min_rpm);
			sscanf(argv[3], "%f", &low_duty);

         cmnd_processs_param_detect(current, min_rpm, low_duty);
		} else {
			commands_printf("This command requires three arguments.\n");
		}
	} else if (strcmp(argv[0], "rpm_dep") == 0) {
		mc_rpm_dep_struct rpm_dep = mcpwm_get_rpm_dep();
		commands_printf("Cycle int limit: %.2f", (double)rpm_dep.cycle_int_limit);
		commands_printf("Cycle int limit running: %.2f", (double)rpm_dep.cycle_int_limit_running);
		commands_printf("Cycle int limit max: %.2f\n", (double)rpm_dep.cycle_int_limit_max);
	} else if (strcmp(argv[0], "can_devs") == 0) {
      cmnd_processs_can_devs();
	} else if (strcmp(argv[0], "foc_encoder_detect") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);

         cmnd_processs_foc_encoder_detect(current);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "measure_res") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);

         cmnd_processs_measure_res(current);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "measure_ind") == 0) {
		if (argc == 2) {
         float duty = -1.0;
         sscanf(argv[1], "%f", &duty);

         cmnd_processs_measure_ind(duty);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "measure_linkage") == 0) {
		if (argc == 5) {
			float current = -1.0;
			float duty = -1.0;
			float min_erpm = -1.0;
			float res = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &duty);
			sscanf(argv[3], "%f", &min_erpm);
			sscanf(argv[4], "%f", &res);

         cmnd_processs_measure_linkage(current, duty, min_erpm, res);
		} else {
			commands_printf("This command requires four arguments.\n");
		}
	} else if (strcmp(argv[0], "measure_res_ind") == 0) {
      cmnd_processs_measure_res_ind();
	} else if (strcmp(argv[0], "measure_linkage_foc") == 0) {
		if (argc == 2) {
			float duty = -1.0;
			sscanf(argv[1], "%f", &duty);

         cmnd_processs_measure_linkage_foc(duty);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "measure_linkage_openloop") == 0) {
		if (argc == 6) {
			float current = -1.0;
			float duty = -1.0;
			float erpm_per_sec = -1.0;
			float res = -1.0;
			float ind = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &duty);
			sscanf(argv[3], "%f", &erpm_per_sec);
			sscanf(argv[4], "%f", &res);
			sscanf(argv[5], "%f", &ind);

         cmnd_processs_measure_linkage_openloop(current, duty, erpm_per_sec, res, ind);
		} else {
			commands_printf("This command requires five arguments.\n");
		}
	} else if (strcmp(argv[0], "foc_state") == 0) {
      cmnd_processs_foc_state();
	} else if (strcmp(argv[0], "hw_status") == 0) {
		commands_printf("Firmware: %d.%d", FW_VERSION_MAJOR, FW_VERSION_MINOR);
#ifdef HW_NAME
		commands_printf("Hardware: %s", HW_NAME);
#endif
		commands_printf("UUID: %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X",
				STM32_UUID_8[0], STM32_UUID_8[1], STM32_UUID_8[2], STM32_UUID_8[3],
				STM32_UUID_8[4], STM32_UUID_8[5], STM32_UUID_8[6], STM32_UUID_8[7],
				STM32_UUID_8[8], STM32_UUID_8[9], STM32_UUID_8[10], STM32_UUID_8[11]);
		commands_printf("Permanent NRF found: %s", conf_general_permanent_nrf_found ? "Yes" : "No");

		int curr0_offset;
		int curr1_offset;
		int curr2_offset;

		mcpwm_foc_get_current_offsets(&curr0_offset, &curr1_offset, &curr2_offset,
				mc_interface_get_motor_thread() == 2);

		commands_printf("FOC Current Offsets: %d %d %d",
				curr0_offset, curr1_offset, curr2_offset);

#ifdef COMM_USE_USB
		commands_printf("USB config events: %d", comm_usb_serial_configured_cnt());
		commands_printf("USB write timeouts: %u", comm_usb_get_write_timeout_cnt());
#else
		commands_printf("USB not enabled on hardware.");
#endif

		commands_printf("Mempool mcconf now: %d highest: %d (max %d)",
				mempools_mcconf_allocated_num(), mempools_mcconf_highest(), MEMPOOLS_MCCONF_NUM - 1);
		commands_printf("Mempool appconf now: %d highest: %d (max %d)",
				mempools_appconf_allocated_num(), mempools_appconf_highest(), MEMPOOLS_APPCONF_NUM - 1);

		commands_printf(" ");
	} else if (strcmp(argv[0], "foc_openloop") == 0) {
		if (argc == 3) {
			float current = -1.0;
			float erpm = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &erpm);

         cmnd_processs_foc_openloop(current, erpm);

		} else {
			commands_printf("This command requires two arguments.\n");
		}
	} else if (strcmp(argv[0], "foc_openloop_duty") == 0) {
		if (argc == 3) {
			float duty = -1.0;
			float erpm = -1.0;
			sscanf(argv[1], "%f", &duty);
			sscanf(argv[2], "%f", &erpm);

         cmnd_processs_foc_openloop_duty(duty, erpm);
		} else {
			commands_printf("This command requires two arguments.\n");
		}
	} else if (strcmp(argv[0], "nrf_ext_set_enabled") == 0) {
		if (argc == 2) {
			int enabled = -1;
			sscanf(argv[1], "%d", &enabled);

			if (enabled >= 0) {
				uint8_t buffer[2];
				buffer[0] = COMM_EXT_NRF_SET_ENABLED;
				buffer[1] = enabled;
				commands_send_packet_nrf(buffer, 2);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "foc_sensors_detect_apply") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);

         cmnd_processs_foc_sensors_detect_apply(current);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "rotor_lock_openloop") == 0) {
		if (argc == 4) {
			float current = -1.0;
			float time = -1.0;
			float angle = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &time);
			sscanf(argv[3], "%f", &angle);

			if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max &&
					angle >= 0.0 && angle <= 360.0) {
				if (time <= 1e-6) {
					timeout_reset();
					mcpwm_foc_set_openloop_phase(current, angle);
					commands_printf("OK\n");
				} else {
					int print_div = 0;
					for (float t = 0.0;t < time;t += 0.002) {
						timeout_reset();
						mcpwm_foc_set_openloop_phase(current, angle);
						chThdSleepMilliseconds(2);

						print_div++;
						if (print_div >= 200) {
							print_div = 0;
							commands_printf("T left: %.2f s", (double)(time - t));
						}
					}

					commands_printf("Done\n");
				}
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires three arguments.\n");
		}
	} else if (strcmp(argv[0], "foc_detect_apply_all") == 0) {
		if (argc == 2) {
			float max_power_loss = -1.0;
			sscanf(argv[1], "%f", &max_power_loss);

         cmnd_processs_foc_detect_apply_all(max_power_loss);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "can_scan") == 0) {
      cmnd_processs_can_scan();
	} else if (strcmp(argv[0], "foc_detect_apply_all_can") == 0) {
		if (argc == 2) {
			float max_power_loss = -1.0;
			sscanf(argv[1], "%f", &max_power_loss);

         cmnd_processs_foc_detect_apply_all_can(max_power_loss);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "encoder") == 0) {
		const volatile mc_configuration *mcconf = mc_interface_get_configuration();
		if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_AS5047_SPI ||
				mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_MT6816_SPI ||
				mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205 ||
				mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501 ||
				mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501_MULTITURN) {
			commands_printf("SPI encoder value: %d, errors: %d, error rate: %.3f %%",
					(unsigned int)encoder_spi_get_val(),
					encoder_spi_get_error_cnt(),
					(double)encoder_spi_get_error_rate() * (double)100.0);

			if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501 ||
					mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501_MULTITURN) {
				char sf[9];
				char almc[9];
				utils_byte_to_binary(encoder_ts5700n8501_get_raw_status()[0], sf);
				utils_byte_to_binary(encoder_ts5700n8501_get_raw_status()[7], almc);
				commands_printf("TS5700N8501 ABM: %d, SF: %s, ALMC: %s\n", encoder_ts57n8501_get_abm(), sf, almc);
			}

			if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_MT6816_SPI) {
				commands_printf("Low flux error (no magnet): errors: %d, error rate: %.3f %%",
						encoder_get_no_magnet_error_cnt(),
						(double)encoder_get_no_magnet_error_rate() * (double)100.0);
			}
		}

		if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_SINCOS) {
			commands_printf("Sin/Cos encoder signal below minimum amplitude: errors: %d, error rate: %.3f %%",
					encoder_sincos_get_signal_below_min_error_cnt(),
					(double)encoder_sincos_get_signal_below_min_error_rate() * (double)100.0);

			commands_printf("Sin/Cos encoder signal above maximum amplitude: errors: %d, error rate: %.3f %%",
					encoder_sincos_get_signal_above_max_error_cnt(),
					(double)encoder_sincos_get_signal_above_max_error_rate() * (double)100.0);
		}

		if (mcconf->m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205) {
			commands_printf("Resolver Loss Of Tracking (>5%c error): errors: %d, error rate: %.3f %%", 0xB0,
					encoder_resolver_loss_of_tracking_error_cnt(),
					(double)encoder_resolver_loss_of_tracking_error_rate() * (double)100.0);
			commands_printf("Resolver Degradation Of Signal (>33%c error): errors: %d, error rate: %.3f %%", 0xB0,
					encoder_resolver_degradation_of_signal_error_cnt(),
					(double)encoder_resolver_degradation_of_signal_error_rate() * (double)100.0);
			commands_printf("Resolver Loss Of Signal (>57%c error): errors: %d, error rate: %.3f %%", 0xB0,
					encoder_resolver_loss_of_signal_error_cnt(),
					(double)encoder_resolver_loss_of_signal_error_rate() * (double)100.0);
		}
	} else if (strcmp(argv[0], "encoder_clear_errors") == 0) {
		encoder_ts57n8501_reset_errors();
		commands_printf("Done!\n");
	} else if (strcmp(argv[0], "encoder_clear_multiturn") == 0) {
		encoder_ts57n8501_reset_multiturn();
		commands_printf("Done!\n");
	} else if (strcmp(argv[0], "uptime") == 0) {
		commands_printf("Uptime: %.2f s\n", (double)chVTGetSystemTimeX() / (double)CH_CFG_ST_FREQUENCY);
	} else if (strcmp(argv[0], "hall_analyze") == 0) {
		if (argc == 2) {
         float current = -1.0;
         sscanf(argv[1], "%f", &current);

			cmnd_hall_analyze(current);
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "io_board_set_output") == 0) {
		if (argc == 4) {
			int id = -1;
			int channel = -1;
			int state = -1;

			sscanf(argv[1], "%d", &id);
			sscanf(argv[2], "%d", &channel);
			sscanf(argv[3], "%d", &state);

         cmnd_processs_io_board_set_output(id, channel, state);
		}
	} else if (strcmp(argv[0], "io_board_set_output_pwm") == 0) {
		if (argc == 4) {
			int id = -1;
			int channel = -1;
			float duty = -1.0;

			sscanf(argv[1], "%d", &id);
			sscanf(argv[2], "%d", &channel);
			sscanf(argv[3], "%f", &duty);

         cmnd_processs_io_board_set_output_pwm(id, channel, duty);
		}
	} else if (strcmp(argv[0], "crc") == 0) {
      cmnd_processs_crc();
	}

	// The help command
	else if (strcmp(argv[0], "help") == 0) {
		cmnd_processs_help(callback_write, callbacks);
	} else {
		commands_printf("Invalid command: %s\n"
				"type help to list all available commands\n", argv[0]);
	}
}

void terminal_add_fault_data(fault_data *data) {
	fault_vec[fault_vec_write++] = *data;
	if (fault_vec_write >= FAULT_VEC_LEN) {
		fault_vec_write = 0;
	}
}

/**
 * Register a custom command  callback to the terminal. If the command
 * is already registered the old command callback will be replaced.
 *
 * @param command
 * The command name.
 *
 * @param help
 * A help text for the command. Can be NULL.
 *
 * @param arg_names
 * The argument names for the command, e.g. [arg_a] [arg_b]
 * Can be NULL.
 *
 * @param cbf
 * The callback function for the command.
 */
void terminal_register_command_callback(
		const char* command,
		const char *help,
		const char *arg_names,
		void(*cbf)(int argc, const char **argv)) {

	int callback_num = callback_write;

	for (int i = 0;i < callback_write;i++) {
		// First check the address in case the same callback is registered more than once.
		if (callbacks[i].command == command) {
			callback_num = i;
			break;
		}

		// Check by string comparison.
		if (strcmp(callbacks[i].command, command) == 0) {
			callback_num = i;
			break;
		}

		// Check if the callback is empty (unregistered)
		if (callbacks[i].cbf == 0) {
			callback_num = i;
			break;
		}
	}

	callbacks[callback_num].command = command;
	callbacks[callback_num].help = help;
	callbacks[callback_num].arg_names = arg_names;
	callbacks[callback_num].cbf = cbf;

	if (callback_num == callback_write) {
		callback_write++;
		if (callback_write >= CALLBACK_LEN) {
			callback_write = 0;
		}
	}
}

void terminal_unregister_callback(void(*cbf)(int argc, const char **argv)) {
	for (int i = 0;i < callback_write;i++) {
		if (callbacks[i].cbf == cbf) {
			callbacks[i].cbf = 0;
		}
	}
}

