/*
	Copyright 2015 Benjamin Vedder	benjamin@vedder.se

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

#include "rfhelp.h"
#include "rf.h"
#include "ch.h"
#include "hal.h"
#include "crc.h"
#include <string.h>

// Variables
static mutex_t rf_mutex;
static char rx_addr[6][5];
static char tx_addr[5];
static bool rx_addr_set[6];
static int address_length;
static bool tx_pipe0_addr_eq;

void rfhelp_init(void) {
	chMtxObjectInit(&rf_mutex);

//	address_length = rf_get_address_width();
	address_length = 3; // We assume length 3

	// This should not happen
	if (address_length > 5 || address_length < 3) {
		address_length = 5;
	}

	for (int i = 0;i < 6;i++) {
		rf_read_reg(NRF_REG_RX_ADDR_P0, rx_addr[i], address_length);
		rx_addr_set[i] = false;
	}
	rf_read_reg(NRF_REG_TX_ADDR, tx_addr, address_length);

	tx_pipe0_addr_eq = memcmp(rx_addr[0], tx_addr, address_length) == 0;
}

/**
 * Re-init the rf chip
 */
void rfhelp_restart(void) {
	chMtxLock(&rf_mutex);

	rf_init();
	rf_set_tx_addr(tx_addr, address_length);
	for (int i = 0;i < 6;i++) {
		if (rx_addr_set[i]) {
			rf_set_rx_addr(i, rx_addr[i], address_length);
		}
	}

	chMtxUnlock(&rf_mutex);
}

/**
 * Set TX mode, send data, wait for result, set RX mode.
 *
 * @param data
 * The data to be sent.
 *
 * @param len
 * Length of the data.
 *
 * @return
 * 0: Send OK.
 * -1: Max RT.
 * -2: Timeout
 */
int rfhelp_send_data(char *data, int len, bool ack) {
	int timeout = 60;
	int retval = -1;

	chMtxLock(&rf_mutex);

	rf_mode_tx();
	rf_clear_irq();
	rf_flush_all();

	// Pipe0-address and tx-address must be equal for ack to work.
	if (!tx_pipe0_addr_eq && ack) {
		rf_set_rx_addr(0, tx_addr, address_length);
	}

	if (ack) {
		rf_write_tx_payload(data, len);
	} else {
		rf_write_tx_payload_no_ack(data, len);
	}

	for(;;) {
		int s = rf_status();

		chThdSleepMilliseconds(1);
		timeout--;

		if (NRF_STATUS_GET_TX_DS(s)) {
			retval = 0;
			break;
		} else if (NRF_STATUS_GET_MAX_RT(s)) {
			rf_clear_maxrt_irq();
			retval = -1;
			break;
		} else if (timeout == 0) {
			retval = -2;
			break;
		}
	}

	// Restore pipe0 address
	if (!tx_pipe0_addr_eq && ack) {
		rf_set_rx_addr(0, rx_addr[0], address_length);
	}

	rf_mode_rx();

	chMtxUnlock(&rf_mutex);

	return retval;
}

/**
 * Same as rfhelp_send_data, but will add a crc checksum to the end. This is
 * useful for protecting against corruption between the NRF and the MCU in case
 * there are errors on the SPI bus.
 *
 * @param data
 * The data to be sent.
 *
 * @param len
 * Length of the data. Should be no more than 30 bytes.
 *
 * @return
 * 0: Send OK.
 * -1: Max RT.
 * -2: Timeout
 */
int rfhelp_send_data_crc(char *data, int len) {
	char buffer[len + 2];
	unsigned short crc = crc16((unsigned char*)data, len);

	memcpy(buffer, data, len);
	buffer[len] = (char)(crc >> 8);
	buffer[len + 1] = (char)(crc & 0xFF);

	return rfhelp_send_data(buffer, len + 2, true);
}

/**
 * Same as rfhelp_send_data_crc, but without ack. A counter is included for
 * duplicate packets and a number of resends can be specified.
 *
 * @param data
 * The data to be sent.
 *
 * @param len
 * Length of the data. Should be no more than 29 bytes.
 *
 * @param resends
 * The amount of resends for this packet. Should be at least 1.
 *
 * @return
 * 0: Send OK.
 * -1: Max RT. (Should not happen)
 * -2: Timeout
 */
int rfhelp_send_data_crc_noack(char *data, int len, int resends) {
	char buffer[len + 3];
	static unsigned char counter = 0;

	memcpy(buffer, data, len);
	buffer[len] = counter++;

	unsigned short crc = crc16((unsigned char*)buffer, len + 1);

	buffer[len + 1] = (char)(crc >> 8);
	buffer[len + 2] = (char)(crc & 0xFF);

	int res = 0;
	for (int i = 0;i < resends;i++) {
		res = rfhelp_send_data(buffer, len + 3, false);
		if (res != 0) {
			break;
		}
	}

	return res;
}

/**
 * Read data from the RX fifo
 *
 * @param data
 * Pointer to the array in which to store the data.
 *
 * @param len
 * Pointer to variable storing the data length.
 *
 * @param pipe
 * Pointer to the pipe on which the data was received. Can be 0.
 *
 * @return
 * 1: Read OK, more data to read.
 * 0: Read OK
 * -1: No RX data
 * -2: Wrong length read. Something is likely wrong.
 */
int rfhelp_read_rx_data(char *data, int *len, int *pipe) {
	int retval = -1;

	chMtxLock(&rf_mutex);

	int s = rf_status();
	int pipe_n = NRF_STATUS_GET_RX_P_NO(s);

	if (pipe_n != 7) {
		*len = rf_get_payload_width();
		if (pipe) {
			*pipe = pipe_n;
		}
		if (*len <= 32 && *len >= 0) {
			rf_read_rx_payload(data, *len);
			rf_clear_rx_irq();
//			rf_flush_rx();

			s = rf_status();
			if (NRF_STATUS_GET_RX_P_NO(s) == 7) {
				retval = 0;
			} else {
				retval = 1;
			}
		} else {
			*len = 0;
			retval = -2;
		}
	}

	chMtxUnlock(&rf_mutex);

	return retval;
}

/**
 * Same as rfhelp_read_rx_data, but will check if there is a valid CRC in the
 * end of the payload.
 *
 * @param data
 * Pointer to the array in which to store the data.
 *
 * @param len
 * Pointer to variable storing the data length.
 *
 * @param pipe
 * Pointer to the pipe on which the data was received. Can be 0.
 *
 * @return
 * 1: Read OK, more data to read.
 * 0: Read OK
 * -1: No RX data
 * -2: Wrong length read. Something is likely wrong.
 * -3: Data read, but CRC does not match.
 */
int rfhelp_read_rx_data_crc(char *data, int *len, int *pipe) {
	int res = rfhelp_read_rx_data(data, len, pipe);

	if (res >= 0 && *len > 2) {
		unsigned short crc = crc16((unsigned char*)data, *len - 2);

		if (crc	!= ((unsigned short) data[*len - 2] << 8 | (unsigned short) data[*len - 1])) {
			res = -3;
		}
	}

	*len -= 2;

	return res;
}

/**
 * Same as rfhelp_read_rx_data_crc, but for use with the corresponding
 * nocak send function.
 *
 * @param data
 * Pointer to the array in which to store the data.
 *
 * @param len
 * Pointer to variable storing the data length.
 *
 * @param pipe
 * Pointer to the pipe on which the data was received. Can be 0.
 *
 * @return
 * 1: Read OK, more data to read.
 * 0: Read OK
 * -1: No RX data
 * -2: Wrong length read. Something is likely wrong.
 * -3: Data read, but CRC does not match.
 * -4: Duplicate packet received.
 */
int rfhelp_read_rx_data_crc_noack(char *data, int *len, int *pipe) {
	int res = rfhelp_read_rx_data(data, len, pipe);
	static unsigned char counter = 0;

	if (res >= 0 && *len > 3) {
		unsigned short crc = crc16((unsigned char*)data, *len - 2);

		if (crc	!= ((unsigned short) data[*len - 2] << 8 | (unsigned short) data[*len - 1])) {
			res = -3;
		} else {
			unsigned char cnt = data[*len - 3];
			if (cnt == counter) {
				res = -4;
			}

			counter = cnt;
		}
	}

	*len -= 3;

	return res;
}

int rfhelp_rf_status(void) {
	chMtxLock(&rf_mutex);
	int s = rf_status();
	chMtxUnlock(&rf_mutex);

	return s;
}

void rfhelp_set_tx_addr(const char *addr, int addr_len) {
	chMtxLock(&rf_mutex);
	memcpy(tx_addr, addr, addr_len);
	address_length = addr_len;

	tx_pipe0_addr_eq = memcmp(rx_addr[0], tx_addr, address_length) == 0;

	rf_set_tx_addr(tx_addr, address_length);
	chMtxUnlock(&rf_mutex);
}

void rfhelp_set_rx_addr(int pipe, const char *addr, int addr_len) {
	chMtxLock(&rf_mutex);
	memcpy(rx_addr[pipe], addr, addr_len);
	address_length = addr_len;

	tx_pipe0_addr_eq = memcmp(rx_addr[0], tx_addr, address_length) == 0;
	rx_addr_set[pipe] = true;

	rf_set_rx_addr(pipe, addr, address_length);
	chMtxUnlock(&rf_mutex);
}

void rfhelp_power_down(void) {
	chMtxLock(&rf_mutex);
	rf_power_down();
	chMtxUnlock(&rf_mutex);
}

void rfhelp_power_up(void) {
	chMtxLock(&rf_mutex);
	rf_power_up();
	chMtxUnlock(&rf_mutex);
}
