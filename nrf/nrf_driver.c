/*
	Copyright 2012-2015 Benjamin Vedder	benjamin@vedder.se

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
 * nrf_driver.c
 *
 *  Created on: 29 mar 2015
 *      Author: benjamin
 */

#include "nrf_driver.h"
#include "rf.h"
#include "rfhelp.h"
#include "conf_general.h"
#include "app.h"
#include "buffer.h"
#include "commands.h"

// Variables
static WORKING_AREA(rx_thread_wa, 1024);
static WORKING_AREA(tx_thread_wa, 512);
static mote_state mstate;

// Functions
static msg_t rx_thread(void *arg);
static msg_t tx_thread(void *arg);

void nrf_driver_init(void) {
	rf_init();
	rfhelp_init();

	// Set RF address
	const char addr[5] = {0xC4, 0xC5, 0xC6, 0xC7, app_get_configuration()->controller_id};
	rfhelp_set_rx_addr(0, addr, 5);
	rfhelp_set_tx_addr(addr, 5);

	chThdCreateStatic(rx_thread_wa, sizeof(rx_thread_wa), NORMALPRIO - 1, rx_thread, NULL);
	chThdCreateStatic(tx_thread_wa, sizeof(tx_thread_wa), NORMALPRIO - 1, tx_thread, NULL);
}

static msg_t tx_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("Nrf TX");

	for(;;) {
		// TODO! Send status

		chThdSleepMilliseconds(500);
	}

	return 0;
}

static msg_t rx_thread(void *arg) {
	(void)arg;

	chRegSetThreadName("RX");

	int reset_cnt = 0;

	for(;;) {
		uint8_t buf[32];
		int len;
		int pipe;

		for(;;) {
			int res = rfhelp_read_rx_data((char*)buf, &len, &pipe);
			chuck_data cdata;
			int32_t index = 0;
			int buttons;

			// If something was read
			if (res >= 0) {
				MOTE_PACKET packet = buf[0];

				switch (packet) {
				case MOTE_PACKET_BATT_LEVEL:
					// TODO!
					break;

				case MOTE_PACKET_BUTTONS:
					index = 1;
					mstate.js_x = buf[index++];
					mstate.js_y = buf[index++];
					buttons = buf[index++];
					mstate.bt_c = buttons & (1 << 0);
					mstate.bt_z = buttons & (1 << 1);
					mstate.bt_push = buttons & (1 << 2);
					mstate.vbat = (float)buffer_get_int16(buf, &index) / 1000.0;

					cdata.js_x = 255 - mstate.js_x;
					cdata.js_y = mstate.js_y;
					cdata.bt_c = mstate.bt_c;
					cdata.bt_z = mstate.bt_z;

					app_nunchuk_update_output(&cdata);
					break;

				default:
					break;
				}
			}

			// Stop when there is no more data to read.
			if (res <= 0) {
				break;
			} else {
				// Sleep a bit to prevent locking the other threads.
				chThdSleepMilliseconds(5);
			}
		}

		chThdSleepMilliseconds(5);

		// Restart the nrf every 100ms just in case
		reset_cnt++;
		if (reset_cnt > 20) {
			reset_cnt = 0;
			rfhelp_restart();
		}
	}

	return 0;
}
