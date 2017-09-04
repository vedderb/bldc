/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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
#define MAX_PL_LEN				25
#define RX_BUFFER_SIZE			PACKET_MAX_PL_LEN

#define ALIVE_INTERVAL			50  // Send alive packets at this rate
#define NRF_RESTART_TIMEOUT		500  // Restart the NRF if nothing has been received or acked for this time

// Variables
static THD_WORKING_AREA(rx_thread_wa, 2048);
static THD_WORKING_AREA(tx_thread_wa, 512);
static mote_state mstate;
static uint8_t rx_buffer[RX_BUFFER_SIZE];
static int nosend_cnt;
static int nrf_restart_rx_time;
static int nrf_restart_tx_time;

static systime_t pairing_time_end = 0;
static bool pairing_active = false;

static volatile bool tx_running = false;
static volatile bool tx_stop = true;
static volatile bool rx_running = false;
static volatile bool rx_stop = true;

// This is a hack to prevent race conditions when updating the appconf
// from the nrf thread
static volatile bool from_nrf = false;

// Functions
static THD_FUNCTION(rx_thread, arg);
static THD_FUNCTION(tx_thread, arg);
static int rf_tx_wrapper(char *data, int len);

bool nrf_driver_init(void) {
	if (from_nrf) {
		return true;
	}

	nrf_driver_stop();

	if (!rfhelp_init()) {
		return false;
	}

	nosend_cnt = 0;
	nrf_restart_rx_time = 0;
	nrf_restart_tx_time = 0;

	pairing_time_end = 0;
	pairing_active = false;

	rx_stop = false;
	tx_stop = false;
	chThdCreateStatic(rx_thread_wa, sizeof(rx_thread_wa), NORMALPRIO - 1, rx_thread, NULL);
	chThdCreateStatic(tx_thread_wa, sizeof(tx_thread_wa), NORMALPRIO - 1, tx_thread, NULL);
	rx_running = true;
	tx_running = true;

	return true;
}

void nrf_driver_stop(void) {
	if (from_nrf) {
		return;
	}

	tx_stop = true;
	rx_stop = true;

	if (rx_running || tx_running) {
		rfhelp_stop();
	}

	while (rx_running || tx_running) {
		chThdSleepMilliseconds(1);
	}
}

void nrf_driver_start_pairing(int ms) {
	if (!rx_running) {
		return;
	}

	pairing_time_end = chVTGetSystemTimeX() + MS2ST(ms);

	if (!pairing_active) {
		pairing_active = true;

		nrf_config conf = app_get_configuration()->app_nrf_conf;
		conf.address[0] = 0xC6;
		conf.address[1] = 0xC5;
		conf.address[2] = 0x0;
		conf.channel = 124;
		conf.crc_type = NRF_CRC_1B;
		conf.retries = 3;
		conf.retry_delay = NRF_RETR_DELAY_1000US;
		conf.send_crc_ack = true;
		conf.speed = NRF_SPEED_250K;

		rfhelp_update_conf(&conf);
	}
}

static int rf_tx_wrapper(char *data, int len) {
	int res = rfhelp_send_data_crc(data, len);

	if (res == 0) {
		nrf_restart_tx_time = NRF_RESTART_TIMEOUT;
	}

	return res;
}

static THD_FUNCTION(tx_thread, arg) {
	(void)arg;

	chRegSetThreadName("Nrf TX");
	tx_running = true;

	for(;;) {
		if (tx_stop) {
			tx_running = false;
			return;
		}

		nosend_cnt++;

		if (nosend_cnt >= ALIVE_INTERVAL && !pairing_active) {
			uint8_t pl[2];
			int32_t index = 0;
			pl[index++] = MOTE_PACKET_ALIVE;
			rf_tx_wrapper((char*)pl, index);
			nosend_cnt = 0;
		}

		chThdSleepMilliseconds(1);
	}

}

static THD_FUNCTION(rx_thread, arg) {
	(void)arg;

	chRegSetThreadName("Nrf RX");
	rx_running = true;

	for(;;) {
		if (rx_stop) {
			rx_running = false;
			return;
		}

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

				nrf_restart_rx_time = NRF_RESTART_TIMEOUT;

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

					memcpy(rx_buffer + rxbuf_len - (len - ind), buf + ind, len - ind);

					if (crc16(rx_buffer, rxbuf_len)
							== ((unsigned short) crc_high << 8
									| (unsigned short) crc_low)) {

						// Wait a bit in case retries are still made
						chThdSleepMilliseconds(2);

						commands_set_send_func(nrf_driver_send_buffer);
						from_nrf = true;
						commands_process_packet(rx_buffer, rxbuf_len);
						from_nrf = false;
					}
				}
				break;

				case MOTE_PACKET_PROCESS_SHORT_BUFFER:
					// Wait a bit in case retries are still made
					chThdSleepMilliseconds(2);

					commands_set_send_func(nrf_driver_send_buffer);
					from_nrf = true;
					commands_process_packet(buf + 1, len - 1);
					from_nrf = false;
					break;

				case MOTE_PACKET_PAIRING_INFO: {
					ind = 1;

					app_configuration appconf = *app_get_configuration();
					appconf.app_nrf_conf.address[0] = buf[ind++];
					appconf.app_nrf_conf.address[1] = buf[ind++];
					appconf.app_nrf_conf.address[2] = buf[ind++];
					appconf.app_nrf_conf.channel = buf[ind++];
					appconf.app_nrf_conf.crc_type = NRF_CRC_1B;
					appconf.app_nrf_conf.retries = 3;
					appconf.app_nrf_conf.retry_delay = NRF_RETR_DELAY_1000US;
					appconf.app_nrf_conf.send_crc_ack = true;
					appconf.app_nrf_conf.speed = NRF_SPEED_250K;

					pairing_active = false;

					from_nrf = true;
					conf_general_store_app_configuration(&appconf);
					app_set_configuration(&appconf);
					commands_send_appconf(COMM_GET_APPCONF, &appconf);

					unsigned char data[2];
					data[0] = COMM_NRF_START_PAIRING;
					data[1] = NRF_PAIR_OK;
					commands_send_packet(data, 2);

					from_nrf = false;
				} break;

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

		if (chVTGetSystemTimeX() > pairing_time_end && pairing_active) {
			pairing_active = false;
			nrf_config conf = app_get_configuration()->app_nrf_conf;
			rfhelp_update_conf(&conf);

			unsigned char data[2];
			data[0] = COMM_NRF_START_PAIRING;
			data[1] = NRF_PAIR_FAIL;
			commands_send_packet(data, 2);
		}

		chThdSleepMilliseconds(5);

		// Restart the nrf if nothing has been received for a while
		if (nrf_restart_rx_time > 0 && nrf_restart_tx_time > 0) {
			nrf_restart_rx_time -= 5;
			nrf_restart_tx_time -= 5;
		} else {
			rfhelp_power_up();
			rfhelp_restart();
			nrf_restart_rx_time = NRF_RESTART_TIMEOUT;
			nrf_restart_tx_time = NRF_RESTART_TIMEOUT;
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
		rf_tx_wrapper((char*)send_buffer, ind);
		nosend_cnt = 0;
	} else {
		unsigned int end_a = 0;
		unsigned int len2 = len - (MAX_PL_LEN - 5);

		for (unsigned int i = 0;i < len2;i += (MAX_PL_LEN - 2)) {
			if (i > 255) {
				break;
			}

			end_a = i + (MAX_PL_LEN - 2);

			uint8_t send_len = (MAX_PL_LEN - 2);
			send_buffer[0] = MOTE_PACKET_FILL_RX_BUFFER;
			send_buffer[1] = i;

			if ((i + (MAX_PL_LEN - 2)) <= len2) {
				memcpy(send_buffer + 2, data + i, send_len);
			} else {
				send_len = len2 - i;
				memcpy(send_buffer + 2, data + i, send_len);
			}

			rf_tx_wrapper((char*)send_buffer, send_len + 2);
			nosend_cnt = 0;
		}

		for (unsigned int i = end_a;i < len2;i += (MAX_PL_LEN - 3)) {
			uint8_t send_len = (MAX_PL_LEN - 3);
			send_buffer[0] = MOTE_PACKET_FILL_RX_BUFFER_LONG;
			send_buffer[1] = i >> 8;
			send_buffer[2] = i & 0xFF;

			if ((i + (MAX_PL_LEN - 3)) <= len2) {
				memcpy(send_buffer + 3, data + i, send_len);
			} else {
				send_len = len2 - i;
				memcpy(send_buffer + 3, data + i, send_len);
			}

			rf_tx_wrapper((char*)send_buffer, send_len + 3);
			nosend_cnt = 0;
		}

		uint32_t ind = 0;
		send_buffer[ind++] = MOTE_PACKET_PROCESS_RX_BUFFER;
		send_buffer[ind++] = len >> 8;
		send_buffer[ind++] = len & 0xFF;
		unsigned short crc = crc16(data, len);
		send_buffer[ind++] = (uint8_t)(crc >> 8);
		send_buffer[ind++] = (uint8_t)(crc & 0xFF);
		memcpy(send_buffer + 5, data + len2, len - len2);
		ind += len - len2;

		rf_tx_wrapper((char*)send_buffer, ind);
		nosend_cnt = 0;
	}
}
