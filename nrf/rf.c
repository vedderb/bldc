/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se
	Copyright 2015 Mikael Lovqvist 	devilholker@gmail.com

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

#include "rf.h"
#include "spi_sw.h"
#include "ch.h"

void rf_init(void) {
	spi_sw_init();
}

void rf_stop(void) {
	spi_sw_stop();
}

void rf_set_speed(NRF_SPEED speed) {
	char reg_old = rf_read_reg_byte(NRF_REG_RF_SETUP);
	char reg_new = reg_old;

	reg_new &= ~(NRF_RF_SETUP_RF_DR_LOW | NRF_RF_SETUP_RF_DR_HIGH);

	switch (speed) {
	case NRF_SPEED_250K:
		reg_new |= NRF_RF_SETUP_RF_DR_LOW;
		break;

	case NRF_SPEED_1M:
		break;

	case NRF_SPEED_2M:
		reg_new |= NRF_RF_SETUP_RF_DR_HIGH;
		break;

	default:
		break;
	}

	if (reg_old != reg_new) {
		rf_write_reg_byte(NRF_REG_RF_SETUP, reg_new);	// Update if we need
	}
}

void rf_set_power(NRF_POWER power) {
	if (power == NRF_POWER_OFF){
		return;
	}
	
	char reg_old = rf_read_reg_byte(NRF_REG_RF_SETUP);
	char reg_new = reg_old;

	reg_new &= ~(NRF_RF_SETUP_RF_PWR | 1);
	reg_new |= (char)power << 1;

	// In case this is a SI24R1 chip and the highest power is requested, set
	// the first bit to get 7dBm output.
	if (power == NRF_POWER_0DBM) {
		reg_new |= 1;
	}

	if (reg_old != reg_new) {
		rf_write_reg_byte(NRF_REG_RF_SETUP, reg_new);	// Update if we need
	}
}

void rf_set_address_width(NRF_AW aw) {
	rf_write_reg_byte(NRF_REG_SETUP_AW, (char)aw + 1);
}

void rf_set_crc_type(NRF_CRC crc_type) {
	char reg_old = rf_read_reg_byte(NRF_REG_CONFIG);
	char reg_new = reg_old;

	reg_new &= ~(NRF_CONFIG_CRCO | NRF_CONFIG_EN_CRC);

	switch (crc_type) {
	case NRF_CRC_DISABLED:
		break;

	case NRF_CRC_1B:
		reg_new |= NRF_CONFIG_EN_CRC;
		break;

	case NRF_CRC_2B:
		reg_new |= NRF_CONFIG_EN_CRC | NRF_CONFIG_CRCO;
		break;

	default:
		break;
	}

	if (reg_old != reg_new) {
		rf_write_reg_byte(NRF_REG_CONFIG, reg_new);	// Update if we need
	}
}

void rf_set_retr_retries(int retries) {
	char reg_old = rf_read_reg_byte(NRF_REG_SETUP_RETR);
	char reg_new = reg_old;

	reg_new &= ~NRF_SETUP_RETR_ARC;
	reg_new |= (char)retries & 0xF;

	if (reg_old != reg_new) {
		rf_write_reg_byte(NRF_REG_SETUP_RETR, reg_new);	// Update if we need
	}
}

void rf_set_retr_delay(NRF_RETR_DELAY delay) {
	char reg_old = rf_read_reg_byte(NRF_REG_SETUP_RETR);
	char reg_new = reg_old;

	reg_new &= ~NRF_SETUP_RETR_ARD;
	reg_new |= ((char)delay & 0xF) << 4;

	if (reg_old != reg_new) {
		rf_write_reg_byte(NRF_REG_SETUP_RETR, reg_new);	// Update if we need
	}
}

void rf_set_rx_addr(int pipe, const char *address, int addr_len) {
	rf_write_reg(NRF_REG_RX_ADDR_P0 + pipe, address, addr_len);
}

void rf_set_tx_addr(const char *address, int addr_len) {
	rf_write_reg(NRF_REG_TX_ADDR, address, addr_len);
}

void rf_write_tx_payload(const char *data, int length) {
	char cmd = NRF_CMD_WRITE_TX_PAYLOAD;
	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_transfer(0, data, length);
	spi_sw_end();

}

// Write payload for transmission without requesting acknowledge
void rf_write_tx_payload_no_ack(const char *data, int length) {
	char cmd = NRF_CMD_WRITE_TX_PAYLOAD_NO_ACK;
	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_transfer(0, data, length);
	spi_sw_end();

}

// Write payload for acknowledge
void rf_write_ack_payload(int pipe, const char *data, int length) {
	char cmd = NRF_CMD_WRITE_ACK_PAYLOAD | (pipe & 0x7);
	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_transfer(0, data, length);
	spi_sw_end();

}

// Read recieved payload
void rf_read_rx_payload(char *data, int length) {
	char cmd = NRF_CMD_READ_RX_PAYLOAD;
	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_transfer(data, 0, length);
	spi_sw_end();

}

// Set radio frequency in MHz (2400 to 2525 allowed)
void rf_set_frequency(int freq) {
	rf_write_reg_byte(NRF_REG_RF_CH, (freq - 2400) & 0x7F);

}

// Get radio frequency in MHz
int rf_get_frequency(void) {
	return rf_read_reg_byte(NRF_REG_RF_CH) + 2400;

}

int rf_get_address_width(void) {
	return rf_read_reg_byte(NRF_REG_SETUP_AW) + 2;
}

// Turn on radio
void rf_power_up(void) {
	int tmp = rf_read_reg_byte(NRF_REG_CONFIG);
	if ((tmp & (NRF_CONFIG_PWR_UP)) != (NRF_CONFIG_PWR_UP)) {
		tmp |= (NRF_CONFIG_PWR_UP);
		rf_write_reg_byte(NRF_REG_CONFIG, tmp);	//Update if we need
	}
}

// Turn off radio
void rf_power_down(void) {
	int tmp = rf_read_reg_byte(NRF_REG_CONFIG);
	if (tmp & (NRF_CONFIG_PWR_UP)) {
		tmp &= ~(NRF_CONFIG_PWR_UP);
		rf_write_reg_byte(NRF_REG_CONFIG, tmp);	//Update if we need
	}
}

// Set up radio for transmission
void rf_mode_tx(void) {
	int tmp = rf_read_reg_byte(NRF_REG_CONFIG);
	if (tmp & (NRF_CONFIG_PRIM_RX)) {
		tmp &= ~(NRF_CONFIG_PRIM_RX);
		rf_write_reg_byte(NRF_REG_CONFIG, tmp);	//Update if we need
	}
}

// Set up radio for reception
void rf_mode_rx(void) {
	int tmp = rf_read_reg_byte(NRF_REG_CONFIG);
	if ((tmp & (NRF_CONFIG_PRIM_RX)) != (NRF_CONFIG_PRIM_RX)) {
		tmp |= (NRF_CONFIG_PRIM_RX);
		rf_write_reg_byte(NRF_REG_CONFIG, tmp);	//Update if we need
	}
}

// Enable autoack on pipe
void rf_enable_pipe_autoack(int pipes) {
	int tmp = rf_read_reg_byte(NRF_REG_EN_AA);
	if ((tmp & (pipes)) != (pipes)) {
		tmp |= (pipes);
		rf_write_reg_byte(NRF_REG_EN_AA, tmp);	//Update if we need
	}
}

// Enable address on pipe
void rf_enable_pipe_address(int pipes) {
	int tmp = rf_read_reg_byte(NRF_REG_EN_RXADDR);
	if ((tmp & (pipes)) != (pipes)) {
		tmp |= (pipes);
		rf_write_reg_byte(NRF_REG_EN_RXADDR, tmp);	//Update if we need
	}
}

// Enable dynamic payload length
void rf_enable_pipe_dlp(int pipes) {
	int tmp = rf_read_reg_byte(NRF_REG_DYNPD);
	if ((tmp & (pipes)) != (pipes)) {
		tmp |= (pipes);
		rf_write_reg_byte(NRF_REG_DYNPD, tmp);	//Update if we need
	}
}

// Enabled various features
void rf_enable_features(int features) {
	int tmp = rf_read_reg_byte(NRF_REG_FEATURE);
	if ((tmp & (features)) != (features)) {
		tmp |= (features);
		rf_write_reg_byte(NRF_REG_FEATURE, tmp);	//Update if we need
	}
}

void rf_flush_tx(void) {
	char cmd = NRF_CMD_FLUSH_TX;
	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_end();
}

void rf_flush_rx(void) {
	char cmd = NRF_CMD_FLUSH_RX;
	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_end();
}

void rf_flush_all(void) {
	rf_flush_rx();
	rf_flush_tx();
}

void rf_clear_irq(void) {
	rf_write_reg_byte(NRF_REG_STATUS, NRF_STATUS_IRQ);
}

void rf_clear_rx_irq(void) {
	rf_write_reg_byte(NRF_REG_STATUS, NRF_STATUS_RX_DR);
}

void rf_clear_tx_irq(void) {
	rf_write_reg_byte(NRF_REG_STATUS, NRF_STATUS_TX_DS);
}

void rf_clear_maxrt_irq(void) {
	rf_write_reg_byte(NRF_REG_STATUS, NRF_STATUS_MAX_RT);
}

int rf_get_payload_width(void) {
	char w;
	char cmd = NRF_CMD_READ_RX_PAYLOAD_WIDTH;
	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_transfer(&w, 0, 1);
	spi_sw_end();
	return w;
}

int rf_status(void) {
	char w = NRF_CMD_NOP;
	spi_sw_begin();
	spi_sw_transfer(&w, &w, 1);
	spi_sw_end();
	return w;
}

int rf_fifo_status(void) {
	return rf_read_reg_byte(NRF_REG_FIFO_STATUS);
}

int rf_rx_power_detect(void) {
	return rf_read_reg_byte(NRF_REG_RPD) >> 1;
}

void rf_write_reg(int reg, const char *data, int len) {
	char cmd = NRF_CMD_WRITE_REGISTER | reg;

	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_transfer(0, data, len);
	spi_sw_end();
}

void rf_write_reg_byte(int reg, char data) {
	rf_write_reg(reg, &data, 1);
}

void rf_read_reg(int reg, char *data, int len) {
	char cmd = NRF_CMD_READ_REGISTER | reg;

	spi_sw_begin();
	spi_sw_transfer(0, &cmd, 1);
	spi_sw_transfer(data, 0, len);
	spi_sw_end();
}

char rf_read_reg_byte(int reg) {
	char result;
	rf_read_reg(reg, &result, 1);
	return result;
}
