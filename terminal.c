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

#include "ch.h"
#include "hal.h"
#include "terminal.h"
#include "mcpwm.h"
#include "mcpwm_foc.h"
#include "mc_interface.h"
#include "commands.h"
#include "hw.h"
#include "comm_can.h"
#include "utils_math.h"
#include "utils_sys.h"
#include "timeout.h"
#include "encoder/encoder.h"
#include "app.h"
#include "comm_usb.h"
#include "comm_usb_serial.h"
#include "mempools.h"
#include "crc.h"
#include "firmware_metadata.h"

#include <string.h>
#include <ctype.h>
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
	// Echo command so user can see what they previously ran
	commands_printf("-> %s \n", str);

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

	// force command argument to be lowercase
	for(int i = 0; argv[0][i] != '\0'; i++){
  		argv[0][i] = tolower(argv[0][i]);
	}

	for (int i = 0;i < callback_write;i++) {
		if (callbacks[i].cbf != 0 && strcmp(argv[0], callbacks[i].command) == 0) {
			callbacks[i].cbf(argc, (const char**)argv);
			return;
		}
	}

	if (strcmp(argv[0], "last_adc_duration") == 0) {
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
		static systime_t last_check_time = 0;
		commands_printf("    addr    stack prio refs     state           name motor stackmin  time    ");
		commands_printf("-----------------------------------------------------------------------------");
		tp = chRegFirstThread();
		do {
			int stack_left = utils_check_min_stack_left(tp);
			commands_printf("%.8lx %.8lx %4lu %4lu %9s %14s %5lu %8d  %lu (%.1f %%)",
					(uint32_t)tp, (uint32_t)tp->p_ctx.r13,
					(uint32_t)tp->p_prio, (uint32_t)(tp->p_refs - 1),
					states[tp->p_state], tp->p_name, tp->motor_selected, stack_left, (uint32_t)tp->p_time,
					(double)(100.0 * (float)tp->p_time / (float)(chVTGetSystemTimeX() - last_check_time)));
			tp->p_time = 0;
			tp = chRegNextThread(tp);
		} while (tp != NULL);
		last_check_time = chVTGetSystemTimeX();
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
				commands_printf("Motor            : %d", fault_vec[i].motor);
				commands_printf("Current          : %.1f", (double)fault_vec[i].current);
				commands_printf("Current filtered : %.1f", (double)fault_vec[i].current_filtered);
				commands_printf("Voltage          : %.2f", (double)fault_vec[i].voltage);
#ifdef HW_HAS_GATE_DRIVER_SUPPLY_MONITOR
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
				if (fault_vec[i].info_str != 0) {
					char f_str[100];
					strcpy(f_str, "Info             : ");
					strcpy(f_str + 19, fault_vec[i].info_str);
					if (fault_vec[i].info_argn == 0) {
						commands_printf(f_str);
					} else if (fault_vec[i].info_argn == 1) {
						commands_printf(f_str, (double)fault_vec[i].info_args[0]);
					} else if (fault_vec[i].info_argn == 2) {
						commands_printf(f_str, (double)fault_vec[i].info_args[0], (double)fault_vec[i].info_args[1]);
					}
				}
				commands_printf(" ");
			}
		}
	} else if (strcmp(argv[0], "tim") == 0) {
		chSysLock();
		volatile int t1_cnt = TIM1->CNT;
		volatile int t8_cnt = TIM8->CNT;
		volatile int t1_cnt2 = TIM1->CNT;
		volatile int t2_cnt = TIM2->CNT;
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
		commands_printf("Tim8 CNT: %i", t8_cnt);
		commands_printf("Tim2 CNT: %i", t2_cnt);
		commands_printf("Amount off CNT: %i",top - (2*t8_cnt + t1_cnt + t1_cnt2)/2);
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
		commands_printf("Input voltage: %.2f\n", (double)mc_interface_get_input_voltage_filtered());
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
			commands_printf("Detecting parameters for BLDC...");
			if (current > 0.0 && current < mc_interface_get_configuration()->l_current_max &&
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
				commands_printf("Invalid argument(s)");
				if (!(current > 0.0 && current < mc_interface_get_configuration()->l_current_max)) {
					commands_printf("Current must be between 0.0 and %.2f", (double)mc_interface_get_configuration()->l_current_max);
				}
				if (!(min_rpm > 10.0 && min_rpm < 3000.0)) {
					commands_printf("ERPM must be between 10 and 3000");
				}
				if (!(low_duty > 0.02 && low_duty < 0.8)) {
					commands_printf("Duty must be between 0.02 and 0.8");
				}
				commands_printf(" ");
			}
		} else {
			commands_printf("This command requires three arguments. [current erpm duty]\n");
		}
	} else if (strcmp(argv[0], "rpm_dep") == 0) {
		mc_rpm_dep_struct rpm_dep = mcpwm_get_rpm_dep();
		commands_printf("Cycle int limit: %.2f", (double)rpm_dep.cycle_int_limit);
		commands_printf("Cycle int limit running: %.2f", (double)rpm_dep.cycle_int_limit_running);
		commands_printf("Cycle int limit max: %.2f\n", (double)rpm_dep.cycle_int_limit_max);
	} else if (strcmp(argv[0], "foc_encoder_detect") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);

			mc_configuration *mcconf = mempools_alloc_mcconf();
			*mcconf = *mc_interface_get_configuration();
			commands_printf("Detecting encoder...");
			if (current > 0.0 && current <= mcconf->l_current_max) {
				if (encoder_is_configured()) {
					mc_motor_type type_old = mcconf->motor_type;
					mcconf->motor_type = MOTOR_TYPE_FOC;
					mc_interface_set_configuration(mcconf);

					float offset = 0.0;
					float ratio = 0.0;
					bool inverted = false;
					mcpwm_foc_encoder_detect(current, true, &offset, &ratio, &inverted);

					mcconf->motor_type = type_old;
					mc_interface_set_configuration(mcconf);

					commands_printf("Offset   : %.2f", (double)offset);
					commands_printf("Ratio    : %.2f", (double)ratio);
					commands_printf("Inverted : %s\n", inverted ? "true" : "false");
				} else {
					commands_printf("Encoder not enabled.\n");
				}
			} else {
				commands_printf("Invalid argument(s). Current must be between 0.0 and %.2f\n", (double)mcconf->l_current_max);
			}

			mempools_free_mcconf(mcconf);
		} else {
			commands_printf("This command requires one argument. [current]\n");
		}
	} else if (strcmp(argv[0], "measure_res") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);

			mc_configuration *mcconf = mempools_alloc_mcconf();
			*mcconf = *mc_interface_get_configuration();
			mc_configuration *mcconf_old = mempools_alloc_mcconf();
			*mcconf_old = *mc_interface_get_configuration();
			commands_printf("Measuring resistance...");
			if (current > 0.0 && current <= mcconf->l_current_max) {
				mcconf->motor_type = MOTOR_TYPE_FOC;
				mc_interface_set_configuration(mcconf);
				float tmp_r = 0.0;
				int fault = mcpwm_foc_measure_resistance(current, 2000, true, &tmp_r, true);
				if(fault == FAULT_CODE_NONE) {
					commands_printf("Resistance: %.6f ohm\n", (double)tmp_r);
				} else {
					commands_printf("Resistance measurement failed due to fault: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");
				}
				mc_interface_set_configuration(mcconf_old);
			} else {
				commands_printf("Invalid argument(s). Current must be between 0.0 and %.2f\n", (double)mcconf->l_current_max);
			}

			mempools_free_mcconf(mcconf);
			mempools_free_mcconf(mcconf_old);
		} else {
			commands_printf("This command requires one argument. [current]\n");
		}
	} else if (strcmp(argv[0], "measure_ind") == 0) {
		if (argc == 2) {
			float duty = -1.0;
			sscanf(argv[1], "%f", &duty);
			commands_printf("Measuring inductance...");
			if (duty > 0.0 && duty <= 0.9) {
				mc_configuration *mcconf = mempools_alloc_mcconf();
				*mcconf = *mc_interface_get_configuration();
				mc_configuration *mcconf_old = mempools_alloc_mcconf();
				*mcconf_old = *mc_interface_get_configuration();

				mcconf->motor_type = MOTOR_TYPE_FOC;
				mc_interface_set_configuration(mcconf);

				float curr, ld_lq_diff, ind;
				int fault = mcpwm_foc_measure_inductance(duty, 400, &curr, &ld_lq_diff, &ind);
				if(fault == FAULT_CODE_NONE) {
					commands_printf("Inductance: %.2f uH, ld_lq_diff: %.2f uH (%.2f A)\n",
									(double)ind, (double)ld_lq_diff, (double)curr);
				} else {
					commands_printf("Inductance measurement failed with fault: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");
				}
				mc_interface_set_configuration(mcconf_old);

				mempools_free_mcconf(mcconf);
				mempools_free_mcconf(mcconf_old);
			} else {
				commands_printf("Invalid argument. Duty must be between 0.0 and 0.9 \n");
			}
		} else {
			commands_printf("This command requires one argument. [duty]\n");
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
			commands_printf("Measuring flux linkage...");
			if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max &&
					min_erpm > 0.0 && duty > 0.02 && duty <= 0.9 && res >= 0.0) {
				float linkage;
				conf_general_measure_flux_linkage(current, duty, min_erpm, res, &linkage);
				commands_printf("Flux linkage: %.7f\n", (double)linkage);
			} else {
				commands_printf("Invalid argument(s).");
				if (!(current > 0.0 && current <= mc_interface_get_configuration()->l_current_max)) {
					commands_printf("Current must be between 0.0 and %.2f", (double)mc_interface_get_configuration()->l_current_max);
				}
				if (!(duty > 0.02 && duty <= 0.9)) {
					commands_printf("Duty must be between 0.02 and 0.9");
				}
				if (!(min_erpm > 0.0)) {
					commands_printf("ERPM must be greater than 0.0");
				}
				if (!(res >= 0.0)) {
					commands_printf("Resistance must be greater than 0.0");
				}
				commands_printf(" ");
			}
		} else {
			commands_printf("This command requires four arguments. [current duty min_erpm resistance]\n");
		}
	} else if (strcmp(argv[0], "measure_res_ind") == 0) {
		mc_configuration *mcconf = mempools_alloc_mcconf();
		*mcconf = *mc_interface_get_configuration();
		mc_configuration *mcconf_old = mempools_alloc_mcconf();
		*mcconf_old = *mc_interface_get_configuration();

		mcconf->motor_type = MOTOR_TYPE_FOC;
		mc_interface_set_configuration(mcconf);
		commands_printf("Measuring resistance and inductance...");
		float res = 0.0;
		float ind = 0.0;
		float ld_lq_diff = 0.0;
		int fault = mcpwm_foc_measure_res_ind(&res, &ind, &ld_lq_diff);
		if (fault == FAULT_CODE_NONE) {
			commands_printf("Resistance: %.6f ohm", (double)res);
			commands_printf("Inductance: %.2f uH (Lq-Ld: %.2f uH)\n", (double)ind, (double)ld_lq_diff);
		} else {
			commands_printf("Fault occured while measuring resistance and inductance: %s", mc_interface_fault_to_string(fault));
			commands_printf("For more info type \"faults\" to view all logged faults\n");
		}
		

		mc_interface_set_configuration(mcconf_old);

		mempools_free_mcconf(mcconf);
		mempools_free_mcconf(mcconf_old);
	} else if (strcmp(argv[0], "measure_linkage_foc") == 0) {
		if (argc == 2) {
			float duty = -1.0;
			int fault = FAULT_CODE_NONE;
			sscanf(argv[1], "%f", &duty);
			commands_printf("Measuring flux linkage foc...");
			if (duty > 0.0 && duty <= 0.9) {
				mc_configuration *mcconf = mempools_alloc_mcconf();
				*mcconf = *mc_interface_get_configuration();
				mc_configuration *mcconf_old = mempools_alloc_mcconf();
				*mcconf_old = *mc_interface_get_configuration();

				mcconf->motor_type = MOTOR_TYPE_FOC;
				mc_interface_set_configuration(mcconf);

				// Disable timeout
				systime_t tout = timeout_get_timeout_msec();
				float tout_c = timeout_get_brake_current();
				KILL_SW_MODE tout_ksw = timeout_get_kill_sw_mode();
				timeout_reset();
				timeout_configure(60000, 0.0, KILL_SW_MODE_DISABLED);

				for (int i = 0;i < 100;i++) {
					mc_interface_set_duty(((float)i / 100.0) * duty);
					fault = mc_interface_get_fault();
					if (fault != FAULT_CODE_NONE) {
						break;
					}
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
				mc_interface_wait_for_motor_release(1.0);
				mc_interface_set_configuration(mcconf_old);

				mempools_free_mcconf(mcconf);
				mempools_free_mcconf(mcconf_old);

				// Enable timeout
				timeout_configure(tout, tout_c, tout_ksw);

				vq_avg /= samples;
				rpm_avg /= samples;
				iq_avg /= samples;

				float linkage = (vq_avg - mcconf->foc_motor_r * iq_avg) / RPM2RADPS_f(rpm_avg);

				if (fault == FAULT_CODE_NONE) {
					commands_printf("Flux linkage: %.7f\n", (double)linkage);
				} else {
					commands_printf("Flux linkage detection failed with fault: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");

				}
			} else {
				commands_printf("Invalid argument. Duty must be between 0.0 and 0.9\n");
			}
		} else {
			commands_printf("This command requires one argument. [duty]\n");
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
			commands_printf("Measuring flux linkage openloop...");
			if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max &&
					erpm_per_sec > 0.0 && duty > 0.02 && duty <= 0.9 && res >= 0.0 && ind >= 0.0) {
				float linkage = 0.0, linkage_undriven = 0.0, undriven_samples = 0.0;
				bool result;

				int fault = conf_general_measure_flux_linkage_openloop(current, duty, erpm_per_sec, res, ind,
																	   &linkage, &linkage_undriven, &undriven_samples, &result);
				if (fault == FAULT_CODE_NONE) {
					if (result) {
						commands_printf(
									"Flux linkage            : %.7f\n"
									"Flux Linkage (undriven) : %.7f\n"
									"Undriven samples        : %.1f\n",
									(double)linkage, (double)linkage_undriven, (double)undriven_samples);
					} else {
						commands_printf("Failed to measure flux linkage");
					}

				} else {
					commands_printf("Fault occured while measuring flux linkage: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");
				}
			} else {
				commands_printf("Invalid argument(s).");
				if (!(current > 0.0 && current <= mc_interface_get_configuration()->l_current_max)) {
					commands_printf("Current must be between 0.0 and %.2f", (double)mc_interface_get_configuration()->l_current_max);
				}
				if (!(duty > 0.02 && duty <= 0.9)) {
					commands_printf("Duty must be between 0.02 and 0.9");
				}
				if (!(erpm_per_sec > 0.0)) {
					commands_printf("ERPM ramp rate must be greater than 0.0");
				}
				if (!(res >= 0.0)) {
					commands_printf("Resistance must be greater than 0.0");
				}
				if (!(ind >= 0.0)) {
					commands_printf("Inductance must be greater than 0.0");
				}
				commands_printf(" ");
			}
		} else {
			commands_printf("This command requires five arguments. [current duty erpm_ramp_per_sec resistance inductance]\n");
		}
	} else if (strcmp(argv[0], "foc_state") == 0) {
        commands_printf("FOC State:");
		mcpwm_foc_print_state();
		commands_printf(" ");
	} else if (strcmp(argv[0], "foc_dc_cal") == 0) {
		commands_printf("Performing DC offset calibration...");
		int res = mcpwm_foc_dc_cal(true);
		if (res >= 0) {
			conf_general_store_mc_configuration((mc_configuration*)mc_interface_get_configuration(),
					mc_interface_get_motor_thread() == 2);
			commands_printf("Done!\n");
		} else {
			commands_printf("DC Cal Failed: %d\n", res);
		}
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
#ifdef HW_HAS_PHASE_SHUNTS
		commands_printf("Phase Shunts: Yes");
#else
		commands_printf("Phase Shunts: No");
#endif

		commands_printf("Odometer : %llu m", mc_interface_get_odometer());
		commands_printf("Runtime  : %llu s", g_backup.runtime);

		float curr0_offset;
		float curr1_offset;
		float curr2_offset;

		mcpwm_foc_get_current_offsets(&curr0_offset, &curr1_offset, &curr2_offset,
				mc_interface_get_motor_thread() == 2);

		commands_printf("FOC Current Offsets: %.2f %.2f %.2f",
				(double)curr0_offset, (double)curr1_offset, (double)curr2_offset);

		float v0_offset;
		float v1_offset;
		float v2_offset;

		mcpwm_foc_get_voltage_offsets(&v0_offset, &v1_offset, &v2_offset,
				mc_interface_get_motor_thread() == 2);

		commands_printf("FOC Voltage Offsets: %.4f %.4f %.4f",
				(double)v0_offset, (double)v1_offset, (double)v2_offset);

		mcpwm_foc_get_voltage_offsets_undriven(&v0_offset, &v1_offset, &v2_offset,
				mc_interface_get_motor_thread() == 2);

		commands_printf("FOC Voltage Offsets Undriven: %.4f %.4f %.4f",
				(double)v0_offset, (double)v1_offset, (double)v2_offset);

#ifdef COMM_USE_USB
		commands_printf("USB config events: %d", comm_usb_serial_configured_cnt());
		commands_printf("USB write timeouts: %u", comm_usb_get_write_timeout_cnt());
#else
		commands_printf("USB not enabled on hardware.");
#endif

#if defined (V_REG) && defined (CURRENT_AMP_GAIN) && defined(CURRENT_SHUNT_RES)
		commands_printf("Current Measurement Range: %.1f A", (double)((V_REG / 2.0) / (CURRENT_AMP_GAIN * CURRENT_SHUNT_RES)));
#endif

#if defined (V_REG) && defined (VIN_R1) && defined(VIN_R2)
		commands_printf("Voltage Measurement Range: %.1f V", (double)((V_REG / 4095.0) * 4095.0 * ((VIN_R1 + VIN_R2) / VIN_R2)));
#endif

#ifdef HW_DEAD_TIME_NSEC
		commands_printf("Dead time: %.0f ns", (double)HW_DEAD_TIME_NSEC);
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
			commands_printf("Running FOC openloop...");
			if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max && erpm >= 0.0) {
				timeout_reset();
				mc_interface_set_openloop_current(current, erpm);
				int fault = mc_interface_get_fault();
				if (fault != FAULT_CODE_NONE) {
					commands_printf("Fault occured during openloop: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");
				}
			} else {
				commands_printf("Invalid argument(s).");
				if (!(current > 0.0 && current <= mc_interface_get_configuration()->l_current_max)) {
					commands_printf("Current must be between 0.0 and %.2f", (double)mc_interface_get_configuration()->l_current_max);
				}
				if (!(erpm >= 0.0)) {
					commands_printf("ERPM must be greater than 0.0");
				}
				commands_printf(" ");
			}
		} else {
			commands_printf("This command requires two arguments. [current erpm]\n");
		}
	} else if (strcmp(argv[0], "foc_openloop_duty") == 0) {
		if (argc == 3) {
			float duty = -1.0;
			float erpm = -1.0;
			sscanf(argv[1], "%f", &duty);
			sscanf(argv[2], "%f", &erpm);
			commands_printf("Running FOC openloop duty...");
			if (duty >= 0.0 && duty <= 0.9 && erpm >= 0.0) {
				timeout_reset();
				mc_interface_set_openloop_duty(duty, erpm);
				int fault = mc_interface_get_fault();
				if (fault != FAULT_CODE_NONE) {
					commands_printf("Fault occured during openloop: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");
				}
			} else {
				commands_printf("Invalid argument(s).");
				if (!(duty >= 0.0 && duty <= 0.9)) {
					commands_printf("Duty must be between 0.0 and 0.9");
				}
				if (!(erpm >= 0.0)) {
					commands_printf("ERPM must be greater than 0.0");
				}
				commands_printf(" ");
			}
		} else {
			commands_printf("This command requires two arguments. [duty erpm]\n");
		}
	} else if (strcmp(argv[0], "nrf_ext_set_enabled") == 0) {
		if (argc == 2) {
			int enabled = -1;
			sscanf(argv[1], "%d", &enabled);
			commands_printf("Sending COMM_EXT_NRF_SET_ENABLED...");
			if (enabled >= 0) {
				uint8_t buffer[2];
				buffer[0] = COMM_EXT_NRF_SET_ENABLED;
				buffer[1] = enabled;
				commands_send_packet_nrf(buffer, 2);
				commands_printf("Sent.\n");
			} else {
				commands_printf("Invalid argument. Enabled must be >= 0 \n");
			}
		} else {
			commands_printf("This command requires one argument. [enabled]\n");
		}
	} else if (strcmp(argv[0], "foc_sensors_detect_apply") == 0) {
		if (argc == 2) {
			float current = -1.0;
			sscanf(argv[1], "%f", &current);
			commands_printf("Detecting sensors for FOC...");
			if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max) {
				int res;
				int fault = conf_general_autodetect_apply_sensors_foc(current, true, true, &res);

				if (fault == FAULT_CODE_NONE) {
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
					commands_printf("Fault occured while detecting sensors: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");
				}
			} else {
				commands_printf("Invalid argument(s). Current must be between 0.0 and %.2f\n", (double)mc_interface_get_configuration()->l_current_max);
			}
		} else {
			commands_printf("This command requires one argument. [current]\n");
		}
	} else if (strcmp(argv[0], "rotor_lock_openloop") == 0) {
		if (argc == 4) {
			float current = -1.0;
			float time = -1.0;
			float angle = -1.0;
			int fault = FAULT_CODE_NONE;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &time);
			sscanf(argv[3], "%f", &angle);
			commands_printf("Locking rotor with openloop...");
			if (fabsf(current) <= mc_interface_get_configuration()->l_current_max &&
					angle >= 0.0 && angle <= 360.0) {
				if (time <= 1e-6) {
					timeout_reset();
					mc_interface_set_openloop_phase(current, angle);
					fault = mc_interface_get_fault();
					if (fault != FAULT_CODE_NONE) {
						commands_printf("Fault occured during openloop: %s", mc_interface_fault_to_string(fault));
						commands_printf("For more info type \"faults\" to view all logged faults\n");
						return;
					}
					commands_printf("OK\n");
				} else {
					int print_div = 0;
					for (float t = 0.0;t < time;t += 0.002) {
						timeout_reset();
						mc_interface_set_openloop_phase(current, angle);
						fault = mc_interface_get_fault();
						if (fault != FAULT_CODE_NONE) {
							commands_printf("Fault occured during openloop: %s", mc_interface_fault_to_string(fault));
							commands_printf("For more info type \"faults\" to view all logged faults\n");
							return;
						}
						chThdSleepMilliseconds(2);

						print_div++;
						if (print_div >= 200) {
							print_div = 0;
							commands_printf("T left: %.2f s", (double)(time - t));
						}
					}

					mc_interface_set_current(0.0);

					commands_printf("Done\n");
				}
			} else {
				commands_printf("Invalid argument(s).");
				if (!(fabsf(current) <= mc_interface_get_configuration()->l_current_max)) {
					commands_printf("Current must be less than %.2f", (double)mc_interface_get_configuration()->l_current_max);
				}
				if (!(angle >= 0.0 && angle <= 360.0)) {
					commands_printf("Angle must be between 0.0 and 360.0");
				}
				commands_printf(" ");
			}
		} else {
			commands_printf("This command requires three arguments. [current time angle]\n");
		}
	} else if (strcmp(argv[0], "foc_detect_apply_all") == 0) {
		if (argc == 2) {
			float max_power_loss = -1.0;
			sscanf(argv[1], "%f", &max_power_loss);
			commands_printf("Running detection...");
			if (max_power_loss > 0.0) {
				int motor_thread_old = mc_interface_get_motor_thread();

				int res = conf_general_detect_apply_all_foc(max_power_loss, true, true);

				commands_printf("Result: %d", res);
				mc_interface_select_motor_thread(1);

				if (res >= 0) {
					commands_printf("Detection finished and applied. Results:");
					const volatile mc_configuration *mcconf = mc_interface_get_configuration();
#ifdef HW_HAS_DUAL_MOTORS
					commands_printf("\nMOTOR 1\n");
#endif
					commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
					commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
					commands_printf("Motor L             : %.2f uH", (double)(mcconf->foc_motor_l * 1e6));
					commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
					commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
					if (mcconf->foc_temp_comp) {
						commands_printf("Temp Comp Base Temp : %.1f degC", (double)mcconf->foc_temp_comp_base_temp);
					}

					if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
						commands_printf("No sensors found, using sensorless mode.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
						commands_printf("Found hall sensors, using them.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
						commands_printf("Found AS5047 encoder, using it.\n");
					} else {
						commands_printf("Detection error: %d\n", res);
					}
#ifdef HW_HAS_DUAL_MOTORS
					mc_interface_select_motor_thread(2);
					mcconf = mc_interface_get_configuration();
					commands_printf("\nMOTOR 2\n");
					commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
					commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
					commands_printf("Motor L             : %.2f uH", (double)(mcconf->foc_motor_l * 1e6));
					commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
					commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
					if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
						commands_printf("No sensors found, using sensorless mode.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
						commands_printf("Found hall sensors, using them.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
						commands_printf("Found AS5047 encoder, using it.\n");
					} else {
						commands_printf("Detection error: %d\n", res);
					}
#endif
				} else {
					if (res == -10) {
						commands_printf("Could not measure flux linkage.");
					} else if (res == -11) {
						commands_printf("Persistent fault occurred during detection.");
					} else {
						commands_printf("Fault code occurred during detection: %s\n", mc_interface_fault_to_string(res+100)); // faults are offset by -100 here
						commands_printf("For more info type \"faults\" to view all logged faults\n");
					}


					commands_printf("Detection failed.\n");
				}

				mc_interface_select_motor_thread(motor_thread_old);
			} else {
				commands_printf("Invalid argument. Max power loss must be greater than 0\n");
			}
		} else {
			commands_printf("This command requires one argument. [Max_power_loss]\n");
		}
	} else if (strcmp(argv[0], "foc_detect_apply_all_can") == 0) {
		if (argc == 2) {
			float max_power_loss = -1.0;
			sscanf(argv[1], "%f", &max_power_loss);
			commands_printf("Running detection...");
			if (max_power_loss > 0.0) {

				int res = conf_general_detect_apply_all_foc_can(true, max_power_loss, 0.0, 0.0, 0.0, 0.0, NULL);

				commands_printf("Res: %d", res);

				if (res >= 0) {
					commands_printf("Detection finished and applied. Results:");
#ifdef HW_HAS_DUAL_MOTORS
					commands_printf("\nMOTOR 1\n");
#endif
					const volatile mc_configuration *mcconf = mc_interface_get_configuration();
					commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
					commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
					commands_printf("Motor L             : %.2f microH", (double)(mcconf->foc_motor_l * 1e6));
					commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
					commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
					if (mcconf->foc_temp_comp) {
						commands_printf("Temp Comp Base Temp : %.1f degC", (double)mcconf->foc_temp_comp_base_temp);
					}

					if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
						commands_printf("No sensors found, using sensorless mode.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
						commands_printf("Found hall sensors, using them.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
						commands_printf("Found AS5047 encoder, using it.\n");
					} else {
						commands_printf("Detection error: %d\n", res);
					}
#ifdef HW_HAS_DUAL_MOTORS
					mc_interface_select_motor_thread(2);
					mcconf = mc_interface_get_configuration();
					commands_printf("\nMOTOR 2\n");
					commands_printf("Motor Current       : %.1f A", (double)(mcconf->l_current_max));
					commands_printf("Motor R             : %.2f mOhm", (double)(mcconf->foc_motor_r * 1e3));
					commands_printf("Motor L             : %.2f microH", (double)(mcconf->foc_motor_l * 1e6));
					commands_printf("Motor Flux Linkage  : %.3f mWb", (double)(mcconf->foc_motor_flux_linkage * 1e3));
					commands_printf("Temp Comp           : %s", mcconf->foc_temp_comp ? "true" : "false");
					if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_SENSORLESS) {
						commands_printf("No sensors found, using sensorless mode.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_HALL) {
						commands_printf("Found hall sensors, using them.\n");
					} else if (mcconf->foc_sensor_mode == FOC_SENSOR_MODE_ENCODER) {
						commands_printf("Found AS5047 encoder, using it.\n");
					} else {
						commands_printf("Detection error: %d\n", res);
					}
					commands_printf("\nNote that this is only printing values of motors 1");
					commands_printf("and 2 of the currently connected unit, other motors");
					commands_printf("may have been detected, but won't be printed here");
#endif
				} else {
					if (res == -10) {
						commands_printf("Could not measure flux linkage.");
					} else if (res == -11) {
						commands_printf("Persistent fault occurred during detection.");
					} else {
						commands_printf("Fault code occurred during detection: %s\n", mc_interface_fault_to_string(res+100)); // faults are offset by -100 here
						commands_printf("For more info type \"faults\" to view all logged faults\n");
					}

					commands_printf("Detection failed.\n");
				}
			} else {
				commands_printf("Invalid argument. Max power loss must be greater than 0\n");
			}
		} else {
			commands_printf("This command requires one argument. [Max_power_loss]\n");
		}
	} else if (strcmp(argv[0], "uptime") == 0) {
		commands_printf("Uptime: %.2f s\n", (double)chVTGetSystemTimeX() / (double)CH_CFG_ST_FREQUENCY);
	} else if (strcmp(argv[0], "hall_analyze") == 0) {
		if (argc == 2) {
			float current = -1.0;
			int fault = FAULT_CODE_NONE;
			sscanf(argv[1], "%f", &current);
			commands_printf("Starting hall sensor analysis...\n");
			if (current > 0.0 && current <= mc_interface_get_configuration()->l_current_max) {
				mc_interface_lock();
				mc_configuration *mcconf = mempools_alloc_mcconf();
				*mcconf = *mc_interface_get_configuration();
				mc_motor_type motor_type_old = mcconf->motor_type;
				mcconf->motor_type = MOTOR_TYPE_FOC;
				mc_interface_set_configuration(mcconf);

				commands_init_plot("Angle", "Hall Sensor State");
				commands_plot_add_graph("Hall 1");
				commands_plot_add_graph("Hall 2");
				commands_plot_add_graph("Hall 3");
				commands_plot_add_graph("Combined");

				float phase = 0.0;

				for (int i = 0;i < 1000;i++) {
					timeout_reset();
					mc_interface_lock_override_once();
					mc_interface_set_openloop_phase((float)i * current / 1000.0, phase);
					fault = mc_interface_get_fault();
					if (fault != FAULT_CODE_NONE) {
						break;
					}
					chThdSleepMilliseconds(1);
				}

				bool is_second_motor = mc_interface_get_motor_thread() == 2;
				int hall_last = utils_read_hall(is_second_motor, mcconf->m_hall_extra_samples);
				float transitions[7] = {0.0};
				int states[8] = {-1, -1, -1, -1, -1, -1, -1, -1};
				int transition_index = 0;

				if (fault == FAULT_CODE_NONE) {

					for (int i = 0;i < 720;i++) {
						int hall = utils_read_hall(is_second_motor, mcconf->m_hall_extra_samples);
						if (hall_last != hall) {
							if (transition_index < 7) {
								transitions[transition_index++] = phase;
							}

							for (int j = 0;j < 8;j++) {
								if (states[j] == hall || states[j] == -1) {
									states[j] = hall;
									break;
								}
							}
						}
						hall_last = hall;

						// Notice that the plots are offset slightly in Y, to make it easier to see them.
						commands_plot_set_graph(0);
						commands_send_plot_points(phase, (float)(hall & 1) * 1.02);
						commands_plot_set_graph(1);
						commands_send_plot_points(phase, (float)((hall >> 1) & 1) * 1.04);
						commands_plot_set_graph(2);
						commands_send_plot_points(phase, (float)((hall >> 2) & 1) * 1.06);
						commands_plot_set_graph(3);
						commands_send_plot_points(phase, (float)hall);

						phase += 1.0;
						timeout_reset();
						mc_interface_lock_override_once();
						mc_interface_set_openloop_phase(current, phase);
						fault = mc_interface_get_fault();
						if (fault != FAULT_CODE_NONE) {
							break;
						}
						chThdSleepMilliseconds(20);
					}
				}
				mc_interface_lock_override_once();
				mc_interface_release_motor();
				mc_interface_wait_for_motor_release(1.0);
				mcconf->motor_type = motor_type_old;
				mc_interface_set_configuration(mcconf);
				mempools_free_mcconf(mcconf);
				mc_interface_unlock();

				if (fault != FAULT_CODE_NONE) {
					commands_printf("Fault occured while analyzing hall sensors: %s", mc_interface_fault_to_string(fault));
					commands_printf("For more info type \"faults\" to view all logged faults\n");
				} else {
					int state_num = 0;
					for (int i = 0;i < 8;i++) {
						if (states[i] != -1) {
							state_num++;
						}
					}

					if (state_num == 6) {
						commands_printf("Found 6 different states. This seems correct.\n");
					} else {
						commands_printf("Found %d different states. Something is most likely wrong...\n", state_num);
					}

					float min = 900.0;
					float max = 0.0;
					for (int i = 0;i < 6;i++) {
						float diff = fabsf(utils_angle_difference(transitions[i], transitions[i + 1]));
						commands_printf("Hall diff %d: %.1f degrees", i + 1, (double)diff);
						if (diff < min) {
							min = diff;
						}
						if (diff > max) {
							max = diff;
						}
					}

					float deviation = (max - min) / 2.0;
					if (deviation < 5) {
						commands_printf("Maximum deviation: %.2f degrees. This is good alignment.\n", (double)deviation);
					} else if ((max - min) < 10) {
						commands_printf("Maximum deviation: %.2f degrees. This is OK, but not great alignment.\n", (double)deviation);
					} else if ((max - min) < 15) {
						commands_printf("Maximum deviation: %.2f degrees. This is bad, but probably usable alignment.\n", (double)deviation);
					} else {
						commands_printf("Maximum deviation: %.2f degrees. The hall sensors are significantly misaligned. This has "
										"to be fixed for proper operation.\n", (double)(max - min));
					}

					commands_printf("Done. Go to the Realtime Data > Experiment page to see the plot.\n");
				}
			} else {
				commands_printf("Invalid argument(s). Current must be between 0.0 and %.2f\n", (double)mc_interface_get_configuration()->l_current_max);
			}
		} else {
			commands_printf("This command requires one argument. [current]\n");
		}
	} else if (strcmp(argv[0], "crc") == 0) {
		unsigned mc_crc0 = mc_interface_get_configuration()->crc;
		unsigned mc_crc1 = mc_interface_calc_crc(NULL, false);
		unsigned app_crc0 = app_get_configuration()->crc;
		unsigned app_crc1 = app_calc_crc(NULL);
		commands_printf("MC CFG crc: 0x%04X (stored)  0x%04X (recalc)", mc_crc0, mc_crc1);
		commands_printf("APP CFG crc: 0x%04X (stored)  0x%04X (recalc)", app_crc0, app_crc1);
		commands_printf("Discrepancy is expected due to run-time recalculation of config params.\n");
	} else if (strcmp(argv[0], "drv_reset_faults") == 0) {
		HW_RESET_DRV_FAULTS();
	} else if (strcmp(argv[0], "update_pid_pos_offset") == 0) {
		if (argc == 3) {
			float angle_now = -500.0;
			int store = false;

			sscanf(argv[1], "%f", &angle_now);
			sscanf(argv[2], "%d", &store);

			if (angle_now > -360.0 && angle_now < 360.0) {
				mc_interface_update_pid_pos_offset(angle_now, store);
				commands_printf("OK\n");
			} else {
				commands_printf("Invalid arguments\n");
			}
		}
	} else if (strcmp(argv[0], "fwinfo") == 0) {
		commands_printf("GIT Branch: %s", GIT_BRANCH_NAME);
		commands_printf("GIT Hash  : %s", GIT_COMMIT_HASH);
		commands_printf("Compiler  : %s\n", ARM_GCC_VERSION);
	} else if (strcmp(argv[0], "rebootwdt") == 0) {
		chSysLock();
		for (;;) {__NOP();}
	}

	// The help command
	else if (strcmp(argv[0], "help") == 0) {
		commands_printf("Valid commands are:");
		commands_printf("help");
		commands_printf("  Show this help");

		commands_printf("last_adc_duration");
		commands_printf("  The time the latest ADC interrupt consumed");

		commands_printf("kv");
		commands_printf("  The calculated kv of the motor (BLDC)");

		commands_printf("mem");
		commands_printf("  Show memory usage");

		commands_printf("threads");
		commands_printf("  List all threads");

		commands_printf("fault");
		commands_printf("  Prints the current fault code");

		commands_printf("faults");
		commands_printf("  Prints all stored fault codes and conditions when they arrived");

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

		commands_printf("measure_linkage_openloop [current] [duty] [erpm_per_sec] [motor_res] [motor_ind]");
		commands_printf("  Run the motor in openloop FOC and measure the flux linkage");
		commands_printf("  example measure_linkage_openloop 5 0.5 1000 0.076 0.000015");
		commands_printf("  tip: measure the resistance with measure_res first");

		commands_printf("foc_state");
		commands_printf("  Print some FOC state variables.");

		commands_printf("foc_dc_cal");
		commands_printf("  Calibrate current and voltage DC offsets.");

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

		commands_printf("foc_detect_apply_all_can [max_power_loss_W]");
		commands_printf("  Detect and apply all motor settings, based on maximum resistive motor power losses. Also");
		commands_printf("  initiates detection in all VESCs found on the CAN-bus.");

		commands_printf("uptime");
		commands_printf("  Prints how many seconds have passed since boot.");

		commands_printf("hall_analyze [current]");
		commands_printf("  Rotate motor in open loop and analyze hall sensors.");

		commands_printf("crc");
		commands_printf("  Print CRC values.");

		commands_printf("drv_reset_faults");
		commands_printf("  Reset gate driver faults (if possible).");

		commands_printf("update_pid_pos_offset [angle_now] [store]");
		commands_printf("  Update position PID offset.");

		commands_printf("fwinfo");
		commands_printf("  Print detailed firmware info.");

		commands_printf("rebootwdt");
		commands_printf("  Reboot using the watchdog timer.");

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

mc_fault_code terminal_get_first_fault(void) {
	if (fault_vec_write == 0) {
		return FAULT_CODE_NONE;
	} else {
		return fault_vec[0].fault;
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

