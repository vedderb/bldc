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
#include "comm.h"
#include "mcpwm.h"
#include "main.h"

#include <string.h>
#include <stdio.h>

void terminal_process_string(char *str) {
	enum { kMaxArgs = 64 };
	int argc = 0;
	char *argv[kMaxArgs];
	static char buffer[256];

	char *p2 = strtok(str, " ");
	while (p2 && argc < kMaxArgs) {
		argv[argc++] = p2;
		p2 = strtok(0, " ");
	}

	if (argc == 0) {
		comm_print("No command received\n");
		return;
	}

	if (strcmp(argv[0], "ping") == 0) {
		comm_print("pong\n");
	} else if (strcmp(argv[0], "stop") == 0) {
		mcpwm_use_pid(0);
		mcpwm_set_duty(0);
		comm_print("Motor stopped\n");
	} else if (strcmp(argv[0], "last_adc_duration") == 0) {
		sprintf(buffer, "Latest ADC duration: %.4f ms", (double)(mcpwm_get_last_adc_isr_duration() * 1000.0));
		comm_print(buffer);
		sprintf(buffer, "Latest injected ADC duration: %.4f ms", (double)(mcpwm_get_last_inj_adc_isr_duration() * 1000.0));
		comm_print(buffer);
		sprintf(buffer, "Latest main ADC duration: %.4f ms\n", (double)(main_get_last_adc_isr_duration() * 1000.0));
				comm_print(buffer);
	} else if (strcmp(argv[0], "kv") == 0) {
		sprintf(buffer, "Calculated KV: %.2f rpm/volt\n", (double)mcpwm_get_kv_filtered());
		comm_print(buffer);
	} else if (strcmp(argv[0], "mem") == 0) {
		size_t n, size;
		n = chHeapStatus(NULL, &size);
		sprintf(buffer, "core free memory : %u bytes", chCoreStatus());
		comm_print(buffer);
		sprintf(buffer, "heap fragments   : %u", n);
		comm_print(buffer);
		sprintf(buffer, "heap free total  : %u bytes\n", size);
		comm_print(buffer);
	} else if (strcmp(argv[0], "threads") == 0) {
		Thread *tp;
		static const char *states[] = {THD_STATE_NAMES};
		comm_print("    addr    stack prio refs     state           name time    ");
		comm_print("-------------------------------------------------------------");
		tp = chRegFirstThread();
		do {
			sprintf(buffer, "%.8lx %.8lx %4lu %4lu %9s %14s %lu",
					(uint32_t)tp, (uint32_t)tp->p_ctx.r13,
					(uint32_t)tp->p_prio, (uint32_t)(tp->p_refs - 1),
					states[tp->p_state], tp->p_name, (uint32_t)tp->p_time);
			comm_print(buffer);
			tp = chRegNextThread(tp);
		} while (tp != NULL);
		comm_print("");
	} else if (strcmp(argv[0], "fault") == 0) {
		switch (mcpwm_get_fault()) {
		case FAULT_CODE_NONE:
			comm_print("FAULT_CODE_NONE\n");
			break;

		case FAULT_CODE_OVER_VOLTAGE:
			comm_print("FAULT_CODE_OVER_VOLTAGE\n");
			break;

		case FAULT_CODE_UNDER_VOLTAGE:
			comm_print("FAULT_CODE_UNDER_VOLTAGE\n");
			break;

		case FAULT_CODE_DRV8302:
			comm_print("FAULT_CODE_DRV8302\n");
			break;

		default:
			break;
		}

	} else if (strcmp(argv[0], "help") == 0) {
		comm_print("Valid commands are:");
		comm_print("help");
		comm_print("  Show this help");

		comm_print("ping");
		comm_print("  Print pong here to see if the reply works");

		comm_print("stop");
		comm_print("  Stop the motor");

		comm_print("last_adc_duration");
		comm_print("  The time the latest ADC interrupt consumed");

		comm_print("kv");
		comm_print("  The calculated kv of the motor");

		comm_print("mem");
		comm_print("  Show memory usage");

		comm_print("threads");
		comm_print("  List all threads");

		comm_print("fault");
		comm_print("  Prints the current fault code\n");
	} else {
		sprintf(buffer, "Invalid command: %s\n"
				"type help to list all available commands\n", argv[0]);
		comm_print(buffer);
	}
}
