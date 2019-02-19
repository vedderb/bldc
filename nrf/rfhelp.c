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
static nrf_config nrf_conf;
static bool init_done = false;

/**
 * Initialize the nrf24l01 driver
 *
 * @return
 * true: Writing an address and reading it back worked.
 * false: Writing an address and reading it failed. This means that something
 * is wrong with the SPI communication.
 */
bool rfhelp_init(void) {
	chMtxObjectInit(&rf_mutex);
	rf_init();

//	address_length = rf_get_address_width();
	address_length = 3; // We assume length 3

	// This should not happen
//	if (address_length > 5 || address_length < 3) {
//		address_length = 3;
//	}

	// Try a read and write to see if the SPI communication works
	char addr_old[3];
	rf_read_reg(NRF_REG_TX_ADDR, addr_old, 3);
	char addr_test[3] = {0x12, 0x41, 0xF3};
	rf_write_reg(NRF_REG_TX_ADDR, addr_test, 3);
	char addr_test_read[3];
	rf_read_reg(NRF_REG_TX_ADDR, addr_test_read, 3);
	rf_write_reg(NRF_REG_TX_ADDR, addr_old, 3);

	if (memcmp(addr_test, addr_test_read, 3) != 0) {
		rf_stop();
		return false;
	}

	for (int i = 0;i < 6;i++) {
		rf_read_reg(NRF_REG_RX_ADDR_P0 + i, rx_addr[i], address_length);
		rx_addr_set[i] = false;
	}

	rf_read_reg(NRF_REG_TX_ADDR, tx_addr, address_length);
	tx_pipe0_addr_eq = memcmp(rx_addr[0], tx_addr, address_length) == 0;

	// TODO: fill nrf_conf with values from the nrf chip. For now we assume
	// that nrf_conf is already set when rfhelp_restart is called.

	init_done = true;

	return true;
}

void rfhelp_stop(void) {
	rf_stop();
	init_done = false;
}

void rfhelp_update_conf(nrf_config *conf) {
	nrf_conf = *conf;

	if (init_done) {
		rfhelp_restart();
	}
}

/**
 * Re-init the rf chip
 */
void rfhelp_restart(void) {
	chMtxLock(&rf_mutex);

	rf_power_down();

	// Set default register values.
	// TODO: make this file consistent with multiple
	// rx_addr and tx_addr, and the rest in general.
	rf_write_reg_byte(NRF_REG_EN_RXADDR, 0);
	rf_write_reg_byte(NRF_REG_DYNPD, 0);

	rf_set_crc_type(nrf_conf.crc_type);
	rf_set_retr_retries(nrf_conf.retries);
	rf_set_retr_delay(nrf_conf.retry_delay);
	rf_set_power(nrf_conf.power);
	rf_set_speed(nrf_conf.speed);
	rf_set_address_width(NRF_AW_3); // Always use 3 byte address
	rf_set_frequency(2400 + (unsigned int)nrf_conf.channel);
	rf_enable_features(NRF_FEATURE_DPL | NRF_FEATURE_DYN_ACK);

	rf_enable_pipe_autoack(NRF_MASK_PIPE0);
	rf_enable_pipe_address(NRF_MASK_PIPE0);
	rf_enable_pipe_dlp(NRF_MASK_PIPE0);

	memcpy(tx_addr, nrf_conf.address, 3);
	memcpy(rx_addr[0], nrf_conf.address, 3);
	tx_pipe0_addr_eq = true;

	rf_set_tx_addr(tx_addr, address_length);
	rf_set_rx_addr(0, rx_addr[0], address_length);

	if (nrf_conf.power != NRF_POWER_OFF) {
		rf_power_up();
		rf_mode_rx();
	}
		rf_flush_all();
		rf_clear_irq();

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

	return rfhelp_send_data(buffer, len + 2, nrf_conf.send_crc_ack);
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
