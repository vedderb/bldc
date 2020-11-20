/*
	Copyright 2019 Benjamin Vedder	benjamin@vedder.se

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

// Some useful includes
#include "mc_interface.h"
#include "utils.h"
#include "encoder.h"
#include "terminal.h"
#include "comm_can.h"
#include "hw.h"
#include "commands.h"
#include "timeout.h"

#include <math.h>
#include <string.h>
#include <stdio.h>

typedef enum tag_IO_PAD {
	TX_PAD = 0,
	RX_PAD = 1,
} IO_PAD;

// Threads
static THD_FUNCTION(io_thread, arg);
static THD_WORKING_AREA(io_thread_wa, 2048);

// Private functions
static void control_io(int argc, const char **argv);

// Private variables
static volatile bool stop_now = true;
static volatile bool is_running = false;
static bool b_isInit = false;

// Called when the io application is started. Start our
// threads here and set up callbacks.
void app_io_start(void) {

	stop_now = false;
	chThdCreateStatic(io_thread_wa, sizeof(io_thread_wa),
			NORMALPRIO, io_thread, NULL);

	// Terminal commands for the VESC Tool terminal can be registered.
	terminal_register_command_callback(
			"io",
			"Use tx(0) and rx(1) pins as digital output",
			"[pin] [state]",
			control_io);
}

// Called when the custom application is stopped. Stop our threads
// and release callbacks.
void app_io_stop(void) {
	terminal_unregister_callback(control_io);

	stop_now = true;
	while (is_running) {
		chThdSleepMilliseconds(1);
	}
}

void app_io_configure(app_configuration *conf) {
	(void)conf;
}

static THD_FUNCTION(io_thread, arg) {
	(void)arg;

	chRegSetThreadName("App IO");

	is_running = true;

	for(;;) {
		// Check if it is time to stop.
		if (stop_now) {
			is_running = false;
			return;
		}

		timeout_reset(); // Reset timeout if everything is OK.

		// Run your logic here. A lot of functionality is available in mc_interface.h.

		chThdSleepMilliseconds(10);
	}
}

// Callback function for the terminal command with arguments.
static void control_io(int argc, const char **argv) {

	if (b_isInit != true) {
		palSetPadMode(HW_UART_TX_PORT, HW_UART_TX_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palSetPadMode(HW_UART_RX_PORT, HW_UART_RX_PIN, PAL_MODE_OUTPUT_PUSHPULL);
		palClearPad(HW_UART_TX_PORT, HW_UART_TX_PIN);
		palClearPad(HW_UART_RX_PORT, HW_UART_RX_PIN);
		b_isInit = true;
	}

	if (argc == 3) {
		int pad = -1;
		int state = -1;
		sscanf(argv[1], "%d", &pad);
		sscanf(argv[2], "%d", &state);

		switch (pad) {
			case TX_PAD:
				if (state) {
					palSetPad(HW_UART_TX_PORT, HW_UART_TX_PIN);
				} else {
					palClearPad(HW_UART_TX_PORT, HW_UART_TX_PIN);
				}
				break;

			case RX_PAD:
				if (state) {
					palSetPad(HW_UART_RX_PORT, HW_UART_RX_PIN);
				} else {
					palClearPad(HW_UART_RX_PORT, HW_UART_RX_PIN);
				}
				break;

			default:
				commands_printf("Unsuported PAD index.\n");
				break;
		}
	}
	commands_printf("IO TX %d RX %d\n",
									palReadPad(HW_UART_TX_PORT, HW_UART_TX_PIN),
									palReadPad(HW_UART_RX_PORT, HW_UART_RX_PIN) );
}
