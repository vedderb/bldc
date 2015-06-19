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
 * terminal.c
 *
 *  Created on: 26 dec 2013
 *      Author: benjamin
 */

#include "ch.h"
#include "hal.h"
#include "terminal.h"
#include "commands.h"
#include "main.h"
#include "hw.h"
#include "comm_can.h"
#include "utils.h"

#include <string.h>
#include <stdio.h>

// Private variables
#define FAULT_VEC_LEN						30
static volatile fault_data fault_vec[FAULT_VEC_LEN];
static volatile int fault_vec_write = 0;

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

	if (strcmp(argv[0], "ping") == 0) {
		commands_printf("pong\n");
	} else if (strcmp(argv[0], "stop") == 0) {
		mcpwm_set_duty(0);
		commands_printf("Motor stopped\n");
	} else if (strcmp(argv[0], "last_adc_duration") == 0) {
		commands_printf("Latest ADC duration: %.4f ms", (double)(mcpwm_get_last_adc_isr_duration() * 1000.0));
		commands_printf("Latest injected ADC duration: %.4f ms", (double)(mcpwm_get_last_inj_adc_isr_duration() * 1000.0));
		commands_printf("Latest main ADC duration: %.4f ms\n", (double)(main_get_last_adc_isr_duration() * 1000.0));
	} else if (strcmp(argv[0], "kv") == 0) {
		commands_printf("Calculated KV: %.2f rpm/volt\n", (double)mcpwm_get_kv_filtered());
	} else if (strcmp(argv[0], "mem") == 0) {
		size_t n, size;
		n = chHeapStatus(NULL, &size);
		commands_printf("core free memory : %u bytes", chCoreStatus());
		commands_printf("heap fragments   : %u", n);
		commands_printf("heap free total  : %u bytes\n", size);
	} else if (strcmp(argv[0], "threads") == 0) {
		Thread *tp;
		static const char *states[] = {THD_STATE_NAMES};
		commands_printf("    addr    stack prio refs     state           name time    ");
		commands_printf("-------------------------------------------------------------");
		tp = chRegFirstThread();
		do {
			commands_printf("%.8lx %.8lx %4lu %4lu %9s %14s %lu",
					(uint32_t)tp, (uint32_t)tp->p_ctx.r13,
					(uint32_t)tp->p_prio, (uint32_t)(tp->p_refs - 1),
					states[tp->p_state], tp->p_name, (uint32_t)tp->p_time);
			tp = chRegNextThread(tp);
		} while (tp != NULL);
		commands_printf("");
	} else if (strcmp(argv[0], "fault") == 0) {
		commands_printf("%s\n", mcpwm_fault_to_string(mcpwm_get_fault()));
	} else if (strcmp(argv[0], "faults") == 0) {
		if (fault_vec_write == 0) {
			commands_printf("No faults registered since startup\n");
		} else {
			commands_printf("The following faults were registered since start:\n");
			for (int i = 0;i < fault_vec_write;i++) {
				commands_printf("Fault            : %s", mcpwm_fault_to_string(fault_vec[i].fault));
				commands_printf("Current          : %.1f", (double)fault_vec[i].current);
				commands_printf("Current filtered : %.1f", (double)fault_vec[i].current_filtered);
				commands_printf("Voltage          : %.2f", (double)fault_vec[i].voltage);
				commands_printf("Duty             : %.2f", (double)fault_vec[i].duty);
				commands_printf("RPM              : %.1f", (double)fault_vec[i].rpm);
				commands_printf("Tacho            : %d", fault_vec[i].tacho);
				commands_printf("TIM PWM CNT      : %d", fault_vec[i].tim_pwm_cnt);
				commands_printf("TIM Samp CNT     : %d", fault_vec[i].tim_samp_cnt);
				commands_printf("Comm step        : %d", fault_vec[i].comm_step);
				commands_printf("Temperature      : %.2f\n", (double)fault_vec[i].temperature);
			}
		}
	} else if (strcmp(argv[0], "rpm") == 0) {
		commands_printf("Electrical RPM: %.2f rpm\n", (double)mcpwm_get_rpm());
	} else if (strcmp(argv[0], "tacho") == 0) {
		commands_printf("Tachometer counts: %i\n", mcpwm_get_tachometer_value(0));
	} else if (strcmp(argv[0], "tim") == 0) {
		chSysLock();
		volatile int t1_cnt = TIM1->CNT;
		volatile int t8_cnt = TIM8->CNT;
		chSysUnlock();
		int duty = TIM1->CCR1;
		int top = TIM1->ARR;
		int voltage_samp = TIM8->CCR1;
		int current1_samp = TIM1->CCR4;
		int current2_samp = TIM8->CCR2;
		commands_printf("Tim1 CNT: %i", t1_cnt);
		commands_printf("Tim8 CNT: %u", t8_cnt);
		commands_printf("Duty cycle: %u", duty);
		commands_printf("Top: %u", top);
		commands_printf("Voltage sample: %u", voltage_samp);
		commands_printf("Current 1 sample: %u", current1_samp);
		commands_printf("Current 2 sample: %u\n", current2_samp);
	} else if (strcmp(argv[0], "volt") == 0) {
		commands_printf("Input voltage: %.2f\n", (double)GET_INPUT_VOLTAGE());
	} else if (strcmp(argv[0], "param_detect") == 0) {
		// Use COMM_MODE_DELAY and try to figure out the motor parameters.
		if (argc == 4) {
			float current = -1.0;
			float min_rpm = -1.0;
			float low_duty = -1.0;
			sscanf(argv[1], "%f", &current);
			sscanf(argv[2], "%f", &min_rpm);
			sscanf(argv[3], "%f", &low_duty);

			const volatile mc_configuration *mcconf = mcpwm_get_configuration();

			if (current > 0.0 && current < mcconf->l_current_max &&
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
		commands_printf("  Prints all CAN devices seen on the bus the past second\n");
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

