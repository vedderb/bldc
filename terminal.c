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
#include "drv8301.h"
#include "drv8305.h"
#include "drv8320s.h"
#include "drv8323s.h"
#include "app.h"
#include "comm_usb.h"
#include "comm_usb_serial.h"

#include <string.h>
#include <stdio.h>
#include <math.h>

// Settings
#define FAULT_VEC_LEN						25
#define CALLBACK_LEN						40

// Private types
typedef struct _terminal_callback_struct {
	const char *command;
	const char *help;
	const char *arg_names;
	void(*cbf)(int argc, const char **argv);
} terminal_callback_struct;

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

	static mc_configuration mcconf; // static to save some stack
	static mc_configuration mcconf_old; // static to save some stack
	mcconf = *mc_interface_get_configuration();
	mcconf_old = mcconf;

	if (strcmp(argv[0], "ping") == 0) {
		commands_printf("pong\n");
	} else if (strcmp(argv[0], "stop") == 0) {
		mc_interface_set_duty(0);
		commands_printf("Motor stopped\n");
	} else if (strcmp(argv[0], "last_adc_duration") == 0) {
		commands_printf("Latest ADC duration: %.4f ms", (double)(mcpwm_get_last_adc_isr_duration() * 1000.0));
		commands_printf("Latest injected ADC duration: %.4f ms", (double)(mc_interface_get_last_inj_adc_isr_duration() * 1000.0));
		commands_printf("Latest sample ADC duration: %.4f ms\n", (double)(mc_interface_get_last_sample_adc_isr_duration() * 1000.0));
	} else if (strcmp(argv[0], "kv") == 0) {
		commands_printf("Calculated KV: %.2f rpm/volt\n", (double)mcpwm_get_kv_filtered());
	} else if (strcmp(argv[0], "mem") == 0) {
		size_t n, size;
		n = chHeapStatus(NULL, &size);
		commands_printf("core free memory : %u bytes", chCoreGetStatusX());
		commands_printf("heap fragments   : %u", n);
		commands_printf("heap free total  : %u bytes\n", size);
	} else if (strcmp(argv[0], "threads") == 0) {
		thread_t *tp;
		static const char *states[] = {CH_STATE_NAMES};
		commands_printf("    addr    stack prio refs     state           name time    ");
		commands_printf("-------------------------------------------------------------");
		tp = chRegFirstThread();
		do {
			commands_printf("%.8lx %.8lx %4lu %4lu %9s %14s %lu (%.1f %%)",
					(uint32_t)tp, (uint32_t)tp->p_ctx.r13,
					(uint32_t)tp->p_prio, (uint32_t)(tp->p_refs - 1),
					states[tp->p_state], tp->p_name, (uint32_t)tp->p_time,
					(double)(100.0 * (float)tp->p_time / (float)chVTGetSystemTimeX()));
			tp = chRegNextThread(tp);
		} while (tp != NULL);
		commands_printf(" ");
	} else if (strcmp(argv[0], "fault") == 0) {
		commands_printf("%s\n", mc_interface_fault_to_string(mc_interface_get_fault()));
	} else if (strcmp(argv[0], "faults") == 0) {
		if (fault_vec_write == 0) {
			commands_printf("No faults registered since startup\n");
		} else {
			commands_printf("The following faults were registered since start:\n");
			for (int i = 0;i < fault_vec_write;i++) {
				commands_printf("Fault            : %s", mc_interface_fault_to_string(fault_vec[i].fault));
				commands_printf("Current          : %.1f", (double)fault_vec[i].current);
				commands_printf("Current filtered : %.1f", (double)fault_vec[i].current_filtered);
				commands_printf("Voltage          : %.2f", (double)fault_vec[i].voltage);
#ifdef HW_VERSION_AXIOM
				commands_printf("Gate drv voltage : %.2f", (double)fault_vec[i].gate_driver_voltage);
#endif
				commands_printf("Duty             : %.3f", (double)fault_vec[i].duty);
				commands_printf("RPM              : %.1f", (double)fault_vec[i].rpm);
				commands_printf("Tacho            : %d", fault_vec[i].tacho);
				commands_printf("Cycles running   : %d", fault_vec[i].cycles_running);
				commands_printf("TIM duty         : %d", (int)((float)fault_vec[i].tim_top * fault_vec[i].duty));
				commands_printf("TIM val samp     : %d", fault_vec[i].tim_val_samp);
				commands_printf("TIM current samp : %d", fault_vec[i].tim_current_samp);
				commands_printf("TIM top          : %d", fault_vec[i].tim_top);
				commands_printf("Comm step        : %d", fault_vec[i].comm_step);
				commands_printf("Temperature      : %.2f", (double)fault_vec[i].temperature);
#ifdef HW_HAS_DRV8301
				if (fault_vec[i].fault == FAULT_CODE_DRV) {
					commands_printf("DRV8301_FAULTS   : %s", drv8301_faults_to_string(fault_vec[i].drv8301_faults));
				}
#elif defined(HW_HAS_DRV8320S)
 				if (fault_vec[i].fault == FAULT_CODE_DRV) {
					commands_printf("DRV8320S_FAULTS  : %s", drv8320s_faults_to_string(fault_vec[i].drv8301_faults));
				}
#elif defined(HW_HAS_DRV8323S)
				if (fault_vec[i].fault == FAULT_CODE_DRV) {
					commands_printf("DRV8323S_FAULTS  : %s", drv8323s_faults_to_string(fault_vec[i].drv8301_faults));
				}
#endif
				commands_printf(" ");
			}
		}
	} else if (strcmp(argv[0], "rpm") == 0) {
		commands_printf("Electrical RPM: %.2f rpm\n", (double)mc_interface_get_rpm());
	} else if (strcmp(argv[0], "tacho") == 0) {
		commands_printf("Tachometer counts: %i\n", mc_interface_get_tachometer_value(0));
	} else if (strcmp(argv[0], "tim") == 0) {
		chSysLock();
		volatile int t1_cnt = TIM1->CNT;
		volatile int t8_cnt = TIM8->CNT;
		volatile int dir1 = !!(TIM1->CR1 & (1 << 4));
		volatile int dir8 = !!(TIM8->CR1 & (1 << 4));
		chSysUnlock();
		int duty1 = TIM1->CCR1;
		int duty2 = TIM1->CCR2;
		int duty3 = TIM1->CCR3;
		int top = TIM1->ARR;
		int voltage_samp = TIM8->CCR1;
		int current1_samp = TIM1->CCR4;
		int current2_samp = TIM8->CCR2;
		commands_printf("Tim1 CNT: %i", t1_cnt);
		commands_printf("Tim8 CNT: %u", t8_cnt);
		commands_printf("Duty cycle1: %u", duty1);
		commands_printf("Duty cycle2: %u", duty2);
		commands_printf("Duty cycle3: %u", duty3);
		commands_printf("Top: %u", top);
		commands_printf("Dir1: %u", dir1);
		commands_printf("Dir8: %u", dir8);
		commands_printf("Voltage sample: %u", voltage_samp);
		commands_printf("Current 1 sample: %u", current1_samp);
		commands_printf("Current 2 sample: %u\n", current2_samp);
	} else if (strcmp(argv[0], "volt") == 0) {
		commands_printf("Input voltage: %.2f\n", (double)GET_INPUT_VOLTAGE());
#ifdef HW_VERSION_AXIOM
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

			if (current > 0.0 && current < mcconf.l_current_max &&
					min_rpm > 10.0 && min_rpm < 3000.0 &&
					low_duty > 0.02 && low_duty < 0.8) {

				float cycle_integrator;
				float coupling_k;
				int8_t hall_table[8];
				int hall_res;
				if (conf_general_detect_motor_param(current, min_rpm, low_duty, &cycle_integrator, &coupling_k, hall_table, &hall_res)) {
					commands_printf("Cycle integrator limit: %.2f", (double)cycle_integrator);
					commands_printf("Coupling factor: %.2f", (double)coupling_k);

					if (hall_res == 0) {
						commands_printf("Detected hall sensor table:");
						commands_printf("%i, %i, %i, %i, %i, %i, %i, %i\n",
								hall_table[0], hall_table[1], hall_table[2], hall_table[3],
								hall_table[4], hall_table[5], hall_table[6], hall_table[7]);
					} else if (hall_res == -1) {
						commands_printf("Hall sensor detection failed:");
						commands_printf("%i, %i, %i, %i, %i, %i, %i, %i\n",
								hall_table[0], hall_table[1], hall_table[2], hall_table[3],
								hall_table[4], hall_table[5], hall_table[6], hall_table[7]);
					} else if (hall_res == -2) {
						commands_printf("WS2811 enabled. Hall sensors cannot be used.\n");
					} else if (hall_res == -3) {
						commands_printf("Encoder enabled. Hall sensors cannot be used.\n");
					}
				} else {
					commands_printf("Detection failed. Try again with different parameters.\n");
				}
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires three arguments.\n");
		}
	} else if (strcmp(argv[0], "rpm_dep") == 0) {
		mc_rpm_dep_struct rpm_dep = mcpwm_get_rpm_dep();
		commands_printf("Cycle int limit: %.2f", (double)rpm_dep.cycle_int_limit);
		commands_printf("Cycle int limit running: %.2f", (double)rpm_dep.cycle_int_limit_running);
		commands_printf("Cycle int limit max: %.2f\n", (double)rpm_dep.cycle_int_limit_max);
	} else if (strcmp(argv[0], "can_devs") == 0) {
		commands_printf("CAN devices seen on the bus the past second:\n");
		for (int i = 0;i < CAN_STATUS_MSGS_TO_STORE;i++) {
			can_status_msg *msg = comm_can_get_status_msg_index(i);

			if (msg->id >= 0 && UTILS_AGE_S(msg->rx_time) < 1.0) {
				commands_printf("ID                 : %i", msg->id);
				commands_printf("RX Time            : %i", msg->rx_time);
				commands_printf("Age (milliseconds) : %.2f", (double)(UTILS_AGE_S(msg->rx_time) * 1000.0));
				commands_printf("RPM                : %.2f", (double)msg->rpm);
				commands_printf("Current            : %.2f", (double)msg->current);
				commands_printf("Duty               : %.2f\n", (double)msg->duty);
			}
		}
	} else if (strcmp(argv[0], "foc_encoder_detect") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);

			if (current > 0.0 && current <= mcconf.l_current_max) {
				if (encoder_is_configured()) {
					mc_motor_type type_old = mcconf.motor_type;
					mcconf.motor_type = MOTOR_TYPE_FOC;
					mc_interface_set_configuration(&mcconf);

					float offset = 0.0;
					float ratio = 0.0;
					bool inverted = false;
					mcpwm_foc_encoder_detect(current, true, &offset, &ratio, &inverted);

					mcconf.motor_type = type_old;
					mc_interface_set_configuration(&mcconf);

					commands_printf("Offset   : %.2f", (double)offset);
					commands_printf("Ratio    : %.2f", (double)ratio);
					commands_printf("Inverted : %s\n", inverted ? "true" : "false");
				} else {
					commands_printf("Encoder not enabled.\n");
				}
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "measure_res") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);

			if (current > 0.0 && current <= mcconf.l_current_max) {
				mcconf.motor_type = MOTOR_TYPE_FOC;
				mc_interface_set_configuration(&mcconf);

				commands_printf("Resistance: %.6f ohm\n", (double)mcpwm_foc_measure_resistance(current, 2000));

				mc_interface_set_configuration(&mcconf_old);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "measure_ind") == 0) {
		if (argc == 2) {
			float duty = -1.0;
			sscanf(argv[1], "%f", &duty);

			if (duty > 0.0 && duty < 0.9) {
				mcconf.motor_type = MOTOR_TYPE_FOC;
				mc_interface_set_configuration(&mcconf);

				float curr, ld_lq_diff;
				float ind = mcpwm_foc_measure_inductance(duty, 400, &curr, &ld_lq_diff);
				commands_printf("Inductance: %.2f uH, ld_lq_diff: %.2f uH (%.2f A)\n",
						(double)ind, (double)ld_lq_diff, (double)curr);

				mc_interface_set_configuration(&mcconf_old);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
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

			if (current > 0.0 && current <= mcconf.l_current_max && min_erpm > 0.0 && duty > 0.02 && res >= 0.0) {
				float linkage;
				conf_general_measure_flux_linkage(current, duty, min_erpm, res, &linkage);
				commands_printf("Flux linkage: %.7f\n", (double)linkage);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires four arguments.\n");
		}
	} else if (strcmp(argv[0], "measure_res_ind") == 0) {
		mcconf.motor_type = MOTOR_TYPE_FOC;
		mc_interface_set_configuration(&mcconf);

		float res = 0.0;
		float ind = 0.0;
		mcpwm_foc_measure_res_ind(&res, &ind);
		commands_printf("Resistance: %.6f ohm", (double)res);
		commands_printf("Inductance: %.2f microhenry\n", (double)ind);

		mc_interface_set_configuration(&mcconf_old);
	} else if (strcmp(argv[0], "measure_linkage_foc") == 0) {
		if (argc == 2) {
			float duty = -1.0;
			sscanf(argv[1], "%f", &duty);

			if (duty > 0.0) {
				mcconf.motor_type = MOTOR_TYPE_FOC;
				mc_interface_set_configuration(&mcconf);
				const float res = (3.0 / 2.0) * mcconf.foc_motor_r;

				// Disable timeout
				systime_t tout = timeout_get_timeout_msec();
				float tout_c = timeout_get_brake_current();
				timeout_reset();
				timeout_configure(60000, 0.0);

				for (int i = 0;i < 100;i++) {
					mc_interface_set_duty(((float)i / 100.0) * duty);
					chThdSleepMilliseconds(20);
				}

				float vq_avg = 0.0;
				float rpm_avg = 0.0;
				float samples = 0.0;
				float iq_avg = 0.0;
				for (int i = 0;i < 1000;i++) {
					vq_avg += mcpwm_foc_get_vq();
					rpm_avg += mc_interface_get_rpm();
					iq_avg += mc_interface_get_tot_current_directional();
					samples += 1.0;
					chThdSleepMilliseconds(1);
				}

				mc_interface_release_motor();
				mc_interface_set_configuration(&mcconf_old);

				// Enable timeout
				timeout_configure(tout, tout_c);

				vq_avg /= samples;
				rpm_avg /= samples;
				iq_avg /= samples;

				float linkage = (vq_avg - res * iq_avg) / (rpm_avg * ((2.0 * M_PI) / 60.0));

				commands_printf("Flux linkage: %.7f\n", (double)linkage);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "measure_linkage_openloop") == 0) {
		if (argc == 5) {
			float current = -1.0;
			float duty = -1.0;
			float erpm_per_sec = -1.0;
			float res = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &duty);
			sscanf(argv[3], "%f", &erpm_per_sec);
			sscanf(argv[4], "%f", &res);

			if (current > 0.0 && current <= mcconf.l_current_max && erpm_per_sec > 0.0 && duty > 0.02 && res >= 0.0) {
				float linkage;
				conf_general_measure_flux_linkage_openloop(current, duty, erpm_per_sec, res, &linkage);
				commands_printf("Flux linkage: %.7f\n", (double)linkage);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires four arguments.\n");
		}
	} else if (strcmp(argv[0], "foc_state") == 0) {
		mcpwm_foc_print_state();
		commands_printf(" ");
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

		mcpwm_foc_get_current_offsets(&curr0_offset, &curr1_offset, &curr2_offset);

		commands_printf("FOC Current Offsets: %d %d %d",
				curr0_offset, curr1_offset, curr2_offset);

#ifdef COMM_USE_USB
		commands_printf("USB config events: %d", comm_usb_serial_configured_cnt());
		commands_printf("USB write timeouts: %u", comm_usb_get_write_timeout_cnt());
#else
		commands_printf("USB not enabled on hardware.");
#endif

		commands_printf(" ");
	} else if (strcmp(argv[0], "foc_openloop") == 0) {
		if (argc == 3) {
			float current = -1.0;
			float erpm = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &erpm);

			if (current >= 0.0 && erpm >= 0.0) {
				timeout_reset();
				mcpwm_foc_set_openloop(current, erpm);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires two arguments.\n");
		}
	} else if (strcmp(argv[0], "foc_openloop_duty") == 0) {
		if (argc == 3) {
			float duty = -1.0;
			float erpm = -1.0;
			sscanf(argv[1], "%f", &duty);
			sscanf(argv[2], "%f", &erpm);

			if (duty >= 0.0 && erpm >= 0.0) {
				timeout_reset();
				mcpwm_foc_set_openloop_duty(duty, erpm);
			} else {
				commands_printf("Invalid argument(s).\n");
			}
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

			if (current > 0.0 && current <= mcconf.l_current_max) {
				int res = conf_general_autodetect_apply_sensors_foc(current, true, true);

				if (res == 0) {
					commands_printf("No sensors found, using sensorless mode.\n");
				} else if (res == 1) {
					commands_printf("Found hall sensors, using them.\n");
				} else if (res == 2) {
					commands_printf("Found AS5047 encoder, using it.\n");
				} else {
					commands_printf("Detection error: %d\n", res);
				}
			} else {
				commands_printf("Invalid argument(s).\n");
			}
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

			if (current > 0.0 && current <= mcconf.l_current_max &&
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

			if (max_power_loss > 0.0) {
				commands_printf("Running detection...");
				int res = conf_general_detect_apply_all_foc(max_power_loss, true, true);

				commands_printf("Res: %d", res);

				if (res >= 0) {
					commands_printf("Detection finished and applied. Results:");
					mcconf = *mc_interface_get_configuration();
					commands_printf("Motor Current       : %.1f A", (double)(mcconf.l_current_max));
					commands_printf("Motor R             : %.2f mOhm", (double)(mcconf.foc_motor_r * 1e3));
					commands_printf("Motor L             : %.2f microH", (double)(mcconf.foc_motor_l * 1e6));
					commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf.foc_motor_flux_linkage * 1e3));
					commands_printf("Temp Comp           : %s", mcconf.foc_temp_comp ? "true" : "false");
					if (mcconf.foc_temp_comp) {
						commands_printf("Temp Comp Base Temp : %.1f degC", (double)mcconf.foc_temp_comp_base_temp);
					}

					if (res == 0) {
						commands_printf("No sensors found, using sensorless mode.\n");
					} else if (res == 1) {
						commands_printf("Found hall sensors, using them.\n");
					} else if (res == 2) {
						commands_printf("Found AS5047 encoder, using it.\n");
					} else {
						commands_printf("Detection error: %d\n", res);
					}
				} else {
					if (res == -10) {
						commands_printf("Could not measure flux linkage.");
					} else if (res == -11) {
						commands_printf("Fault code occured during detection.");
					}

					commands_printf("Detection failed.\n");
				}
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "can_scan") == 0) {
		bool found = false;
		for (int i = 0;i < 254;i++) {
			if (comm_can_ping(i)) {
				commands_printf("Found VESC with ID: %d", i);
				found = true;
			}
		}

		if (found) {
			commands_printf("Done\n");
		} else {
			commands_printf("No CAN devices found\n");
		}
	} else if (strcmp(argv[0], "foc_detect_apply_all_can") == 0) {
		if (argc == 2) {
			float max_power_loss = -1.0;
			sscanf(argv[1], "%f", &max_power_loss);

			if (max_power_loss > 0.0) {
				commands_printf("Running detection...");
				int res = conf_general_detect_apply_all_foc_can(true, max_power_loss, 0.0, 0.0, 0.0, 0.0);

				commands_printf("Res: %d", res);

				if (res >= 0) {
					commands_printf("Detection finished and applied. Results:");
					mcconf = *mc_interface_get_configuration();
					commands_printf("Motor Current       : %.1f A", (double)(mcconf.l_current_max));
					commands_printf("Motor R             : %.2f mOhm", (double)(mcconf.foc_motor_r * 1e3));
					commands_printf("Motor L             : %.2f microH", (double)(mcconf.foc_motor_l * 1e6));
					commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf.foc_motor_flux_linkage * 1e3));
					commands_printf("Temp Comp           : %s", mcconf.foc_temp_comp ? "true" : "false");
					if (mcconf.foc_temp_comp) {
						commands_printf("Temp Comp Base Temp : %.1f degC", (double)mcconf.foc_temp_comp_base_temp);
					}

					if (res == 0) {
						commands_printf("No sensors found, using sensorless mode.\n");
					} else if (res == 1) {
						commands_printf("Found hall sensors, using them.\n");
					} else if (res == 2) {
						commands_printf("Found AS5047 encoder, using it.\n");
					} else {
						commands_printf("Detection error: %d\n", res);
					}
				} else {
					if (res == -10) {
						commands_printf("Could not measure flux linkage.");
					} else if (res == -11) {
						commands_printf("Fault code occured during detection.");
					}

					commands_printf("Detection failed.\n");
				}
			} else {
				commands_printf("Invalid argument(s).\n");
			}
		} else {
			commands_printf("This command requires one argument.\n");
		}
	} else if (strcmp(argv[0], "encoder") == 0) {
		if (mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_AS5047_SPI ||
			mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205 ||
			mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501 ||
			mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501_MULTITURN) {
			commands_printf("SPI encoder value: %d, errors: %d, error rate: %.3f %%",
				(unsigned int)encoder_spi_get_val(),
				encoder_spi_get_error_cnt(),
				(double)encoder_spi_get_error_rate() * (double)100.0);

			if (mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501 ||
					mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_TS5700N8501_MULTITURN) {
				char sf[9];
				char almc[9];
				utils_byte_to_binary(encoder_ts5700n8501_get_raw_status()[0], sf);
				utils_byte_to_binary(encoder_ts5700n8501_get_raw_status()[7], almc);
				commands_printf("TS5700N8501 ABM: %d, SF: %s, ALMC: %s\n", encoder_ts57n8501_get_abm(), sf, almc);
			}
		}

		if (mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_SINCOS) {
			commands_printf("Sin/Cos encoder signal below minimum amplitude: errors: %d, error rate: %.3f %%",
					encoder_sincos_get_signal_below_min_error_cnt(),
					(double)encoder_sincos_get_signal_below_min_error_rate() * (double)100.0);

			commands_printf("Sin/Cos encoder signal above maximum amplitude: errors: %d, error rate: %.3f %%",
					encoder_sincos_get_signal_above_max_error_cnt(),
					(double)encoder_sincos_get_signal_above_max_error_rate() * (double)100.0);
		}

		if (mcconf.m_sensor_port_mode == SENSOR_PORT_MODE_AD2S1205) {
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
	}

	// The help command
	else if (strcmp(argv[0], "help") == 0) {
		commands_printf("Valid commands are:");
		commands_printf("help");
		commands_printf("  Show this help");

		commands_printf("ping");
		commands_printf("  Print pong here to see if the reply works");

		commands_printf("stop");
		commands_printf("  Stop the motor");

		commands_printf("last_adc_duration");
		commands_printf("  The time the latest ADC interrupt consumed");

		commands_printf("kv");
		commands_printf("  The calculated kv of the motor");

		commands_printf("mem");
		commands_printf("  Show memory usage");

		commands_printf("threads");
		commands_printf("  List all threads");

		commands_printf("fault");
		commands_printf("  Prints the current fault code");

		commands_printf("faults");
		commands_printf("  Prints all stored fault codes and conditions when they arrived");

		commands_printf("rpm");
		commands_printf("  Prints the current electrical RPM");

		commands_printf("tacho");
		commands_printf("  Prints tachometer value");

		commands_printf("tim");
		commands_printf("  Prints tim1 and tim8 settings");

		commands_printf("volt");
		commands_printf("  Prints different voltages");

		commands_printf("param_detect [current] [min_rpm] [low_duty]");
		commands_printf("  Spin up the motor in COMM_MODE_DELAY and compute its parameters.");
		commands_printf("  This test should be performed without load on the motor.");
		commands_printf("  Example: param_detect 5.0 600 0.06");

		commands_printf("rpm_dep");
		commands_printf("  Prints some rpm-dep values");

		commands_printf("can_devs");
		commands_printf("  Prints all CAN devices seen on the bus the past second");

		commands_printf("foc_encoder_detect [current]");
		commands_printf("  Run the motor at 1Hz on open loop and compute encoder settings");

		commands_printf("measure_res [current]");
		commands_printf("  Lock the motor with a current and calculate its resistance");

		commands_printf("measure_ind [duty]");
		commands_printf("  Send short voltage pulses, measure the current and calculate the motor inductance");

		commands_printf("measure_linkage [current] [duty] [min_erpm] [motor_res]");
		commands_printf("  Run the motor in BLDC delay mode and measure the flux linkage");
		commands_printf("  example measure_linkage 5 0.5 700 0.076");
		commands_printf("  tip: measure the resistance with measure_res first");

		commands_printf("measure_res_ind");
		commands_printf("  Measure the motor resistance and inductance with an incremental adaptive algorithm.");

		commands_printf("measure_linkage_foc [duty]");
		commands_printf("  Run the motor with FOC and measure the flux linkage.");

		commands_printf("measure_linkage_openloop [current] [duty] [erpm_per_sec] [motor_res]");
		commands_printf("  Run the motor in openloop FOC and measure the flux linkage");
		commands_printf("  example measure_linkage 5 0.5 1000 0.076");
		commands_printf("  tip: measure the resistance with measure_res first");

		commands_printf("foc_state");
		commands_printf("  Print some FOC state variables.");

		commands_printf("hw_status");
		commands_printf("  Print some hardware status information.");

		commands_printf("foc_openloop [current] [erpm]");
		commands_printf("  Create an open loop rotating current vector.");

		commands_printf("foc_openloop_duty [duty] [erpm]");
		commands_printf("  Create an open loop rotating voltage vector.");

		commands_printf("nrf_ext_set_enabled [enabled]");
		commands_printf("  Enable or disable external NRF51822.");

		commands_printf("foc_sensors_detect_apply [current]");
		commands_printf("  Automatically detect FOC sensors, and apply settings on success.");

		commands_printf("rotor_lock_openloop [current_A] [time_S] [angle_DEG]");
		commands_printf("  Lock the motor with a current for a given time. Time 0 means forever, or");
		commands_printf("  or until the heartbeat packets stop.");

		commands_printf("foc_detect_apply_all [max_power_loss_W]");
		commands_printf("  Detect and apply all motor settings, based on maximum resistive motor power losses.");

		commands_printf("can_scan");
		commands_printf("  Scan CAN-bus using ping commands, and print all devices that are found.");

		commands_printf("foc_detect_apply_all_can [max_power_loss_W]");
		commands_printf("  Detect and apply all motor settings, based on maximum resistive motor power losses. Also");
		commands_printf("  initiates detection in all VESCs found on the CAN-bus.");
		
		commands_printf("encoder");
		commands_printf("  Prints the status of the AS5047, AD2S1205, or TS5700N8501 encoder.");

		commands_printf("encoder_clear_errors");
		commands_printf("  Clear error of the TS5700N8501 encoder.)");

		commands_printf("encoder_clear_multiturn");
		commands_printf("  Clear multiturn counter of the TS5700N8501 encoder.)");

		commands_printf("uptime");
		commands_printf("  Prints how many seconds have passed since boot.");

		for (int i = 0;i < callback_write;i++) {
			if (callbacks[i].cbf == 0) {
				continue;
			}

			if (callbacks[i].arg_names) {
				commands_printf("%s %s", callbacks[i].command, callbacks[i].arg_names);
			} else {
				commands_printf(callbacks[i].command);
			}

			if (callbacks[i].help) {
				commands_printf("  %s", callbacks[i].help);
			} else {
				commands_printf("  There is no help available for this command.");
			}
		}

		commands_printf(" ");
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

