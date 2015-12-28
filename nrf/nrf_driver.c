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

#include <string.h>
#include "nrf_driver.h"
#include "rf.h"
#include "rfhelp.h"
#include "conf_general.h"
#include "app.h"
#include "buffer.h"
#include "commands.h"
#include "crc.h"
#include "packet.h"

// Settings
#define MAX_PL_LEN		25
#define RX_BUFFER_SIZE	PACKET_MAX_PL_LEN

// Variables
static THD_WORKING_AREA(rx_thread_wa, 2048);
static THD_WORKING_AREA(tx_thread_wa, 512);
static mote_state mstate;
static uint8_t rx_buffer[RX_BUFFER_SIZE];

// Functions
static THD_FUNCTION(rx_thread, arg);
static THD_FUNCTION(tx_thread, arg);

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

static THD_FUNCTION(tx_thread, arg) {
	(void)arg;

	chRegSetThreadName("Nrf TX");

	for(;;) {
		uint8_t pl[6];
		int32_t index = 0;
		pl[index++] = MOTE_PACKET_ALIVE;
		rfhelp_send_data_crc((char*)pl, index);

		chThdSleepMilliseconds(50);
	}

}

static THD_FUNCTION(rx_thread, arg) {
	(void)arg;

	chRegSetThreadName("Nrf RX");

	int reset_cnt = 0;

	for(;;) {
		uint8_t buf[32];
		int len;
		int pipe;

		for(;;) {
			int res = rfhelp_read_rx_data_crc((char*)buf, &len, &pipe);
			chuck_data cdata;
			int32_t ind = 0;
			int buttons;

			// If something was read
			if (res >= 0) {
				MOTE_PACKET packet = buf[0];

				switch (packet) {
				case MOTE_PACKET_BATT_LEVEL:
					// TODO!
					break;

				case MOTE_PACKET_BUTTONS:
					ind = 1;
					mstate.js_x = buf[ind++];
					mstate.js_y = buf[ind++];
					buttons = buf[ind++];
					mstate.bt_c = buttons & (1 << 0);
					mstate.bt_z = buttons & (1 << 1);
					mstate.bt_push = buttons & (1 << 2);
					mstate.vbat = (float)buffer_get_int16(buf, &ind) / 1000.0;

					cdata.js_x = 255 - mstate.js_x;
					cdata.js_y = mstate.js_y;
					cdata.bt_c = mstate.bt_c;
					cdata.bt_z = mstate.bt_z;

					app_nunchuk_update_output(&cdata);
					break;

				case MOTE_PACKET_FILL_RX_BUFFER:
					memcpy(rx_buffer + buf[1], buf + 2, len - 2);
					break;

				case MOTE_PACKET_FILL_RX_BUFFER_LONG: {
					int rxbuf_ind = (unsigned int)buf[1] << 8;
					rxbuf_ind |= buf[2];
					if (rxbuf_ind < RX_BUFFER_SIZE) {
						memcpy(rx_buffer + rxbuf_ind, buf + 3, len - 3);
					}
				}
				break;

				case MOTE_PACKET_PROCESS_RX_BUFFER: {
					ind = 1;
					int rxbuf_len = (unsigned int)buf[ind++] << 8;
					rxbuf_len |= (unsigned int)buf[ind++];

					if (rxbuf_len > RX_BUFFER_SIZE) {
						break;
					}

					uint8_t crc_high = buf[ind++];
					uint8_t crc_low = buf[ind++];

					if (crc16(rx_buffer, rxbuf_len)
							== ((unsigned short) crc_high << 8
									| (unsigned short) crc_low)) {

						commands_set_send_func(nrf_driver_send_buffer);
						commands_process_packet(rx_buffer, rxbuf_len);
					}
				}
				break;

				case MOTE_PACKET_PROCESS_SHORT_BUFFER:
					commands_set_send_func(nrf_driver_send_buffer);
					commands_process_packet(buf + 1, len - 1);
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
				chThdSleepMilliseconds(1);
			}
		}

		chThdSleepMilliseconds(5);

		// Restart the nrf every 500ms just in case
		reset_cnt++;
		if (reset_cnt > 100) {
			reset_cnt = 0;
			rfhelp_restart();
		}
	}
}

void nrf_driver_send_buffer(unsigned char *data, unsigned int len) {
	uint8_t send_buffer[MAX_PL_LEN];

	if (len <= (MAX_PL_LEN - 1)) {
		uint32_t ind = 0;
		send_buffer[ind++] = MOTE_PACKET_PROCESS_SHORT_BUFFER;
		memcpy(send_buffer + ind, data, len);
		ind += len;
		rfhelp_send_data_crc((char*)send_buffer, ind);
	} else {
		unsigned int end_a = 0;
		for (unsigned int i = 0;i < len;i += (MAX_PL_LEN - 2)) {
			end_a = i;

			if (i > 255) {
				break;
			}

			uint8_t send_len = (MAX_PL_LEN - 2);
			send_buffer[0] = MOTE_PACKET_FILL_RX_BUFFER;
			send_buffer[1] = i;

			if ((i + (MAX_PL_LEN - 2)) <= len) {
				memcpy(send_buffer + 2, data + i, send_len);
			} else {
				send_len = len - i;
				memcpy(send_buffer + 2, data + i, send_len);
			}

			rfhelp_send_data_crc((char*)send_buffer, send_len + 2);
		}

		for (unsigned int i = end_a;i < len;i += (MAX_PL_LEN - 3)) {
			uint8_t send_len = (MAX_PL_LEN - 3);
			send_buffer[0] = MOTE_PACKET_FILL_RX_BUFFER_LONG;
			send_buffer[1] = i >> 8;
			send_buffer[2] = i & 0xFF;

			if ((i + (MAX_PL_LEN - 3)) <= len) {
				memcpy(send_buffer + 3, data + i, send_len);
			} else {
				send_len = len - i;
				memcpy(send_buffer + 3, data + i, send_len);
			}

			rfhelp_send_data_crc((char*)send_buffer, send_len + 3);
		}

		uint32_t ind = 0;
		send_buffer[ind++] = MOTE_PACKET_PROCESS_RX_BUFFER;
		send_buffer[ind++] = len >> 8;
		send_buffer[ind++] = len & 0xFF;
		unsigned short crc = crc16(data, len);
		send_buffer[ind++] = (uint8_t)(crc >> 8);
		send_buffer[ind++] = (uint8_t)(crc & 0xFF);

		rfhelp_send_data_crc((char*)send_buffer, ind);
	}
}
