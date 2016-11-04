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

#ifndef RF_H_
#define RF_H_

#include <stdint.h>
#include <datatypes.h>

// Status register masks
#define NRF_STATUS_TX_FULL					(1<<0)
#define NRF_STATUS_RX_P_NO					(1<<1 | 1<<2 | 1<<3)
#define NRF_STATUS_MAX_RT					(1<<4)
#define NRF_STATUS_TX_DS					(1<<5)
#define NRF_STATUS_RX_DR					(1<<6)
#define NRF_STATUS_IRQ 						(NRF_STATUS_MAX_RT | NRF_STATUS_TX_DS | NRF_STATUS_RX_DR)

// Status register read macros
#define NRF_STATUS_GET_TX_FULL(s)			(s & NRF_STATUS_TX_FULL)
#define NRF_STATUS_GET_RX_P_NO(s)			((s & NRF_STATUS_RX_P_NO) >> 1)
#define NRF_STATUS_GET_MAX_RT(s)			((s & NRF_STATUS_MAX_RT) >> 4)
#define NRF_STATUS_GET_TX_DS(s)				((s & NRF_STATUS_TX_DS) >> 5)
#define NRF_STATUS_GET_RX_DR(s)				((s & NRF_STATUS_RX_DR) >> 6)

// FIFO register masks
#define NRF_FIFO_RX_EMPTY					(1<<0)
#define NRF_FIFO_RX_FULL					(1<<1)
#define NRF_FIFO_TX_EMPTY					(1<<4)
#define NRF_FIFO_TX_FULL					(1<<5)

// Config register masks
#define NRF_CONFIG_PRIM_RX					(1<<0)
#define NRF_CONFIG_PWR_UP					(1<<1)
#define NRF_CONFIG_CRCO						(1<<2)
#define NRF_CONFIG_EN_CRC					(1<<3)
#define NRF_CONFIG_MASK_MAX_RT				(1<<4)
#define NRF_CONFIG_MASK_TX_DS				(1<<5)
#define NRF_CONFIG_MASK_RX_DR				(1<<6)

// Feature register masks
#define NRF_FEATURE_DYN_ACK					(1<<0)
#define NRF_FEATURE_ACK_PAYLOAD				(1<<1)
#define NRF_FEATURE_DPL						(1<<2)

// RF setup register masks
#define NRF_RF_SETUP_RF_DR_LOW				(1<<5)
#define NRF_RF_SETUP_RF_DR_HIGH				(1<<3)
#define NRF_RF_SETUP_RF_PWR					(1<<1 | 1<<2)

// RETR setup masks
#define NRF_SETUP_RETR_ARC					(1<<0 | 1<<1 | 1<<2 | 1<<3)
#define NRF_SETUP_RETR_ARD					(1<<4 | 1<<5 | 1<<6 | 1<<7)

#define NRF_MASK_PIPE0						(1<<0)
#define NRF_MASK_PIPE1						(1<<1)
#define NRF_MASK_PIPE2						(1<<2)
#define NRF_MASK_PIPE3						(1<<3)
#define NRF_MASK_PIPE4						(1<<4)
#define NRF_MASK_PIPE5						(1<<5)

// Commands
#define NRF_CMD_READ_REGISTER				0b00000000
#define NRF_CMD_WRITE_REGISTER				0b00100000
#define NRF_CMD_READ_RX_PAYLOAD				0b01100001 // Read RX-payload
#define NRF_CMD_WRITE_TX_PAYLOAD			0b10100000 // Write TX-payload
#define NRF_CMD_FLUSH_TX					0b11100001 // Flush TX FIFO
#define NRF_CMD_FLUSH_RX					0b11100010 // Flush RX FIFO
#define NRF_CMD_REUSE_TX					0b11100011 // Reuse last transmitted payload
#define NRF_CMD_ACTIVATE					0b01010000 // Activate features
#define NRF_CMD_READ_RX_PAYLOAD_WIDTH		0b01100000 // Read RX-payload width
#define NRF_CMD_WRITE_ACK_PAYLOAD			0b10101000 // Write payload for ACK packet
#define NRF_CMD_WRITE_TX_PAYLOAD_NO_ACK		0b10110000 // Disables AUTOACK on packet
#define NRF_CMD_NOP							0b11111111 // NOP

// Registers
#define NRF_REG_CONFIG						0x00 // Configuration Register
#define NRF_REG_EN_AA						0x01 // Auto Acknowledgment
#define NRF_REG_EN_RXADDR					0x02 // Enabled RX Addresses
#define NRF_REG_SETUP_AW					0x03 // Setup of Address Widths
#define	NRF_REG_SETUP_RETR					0x04 // Setup of Automatic Retransmission
#define NRF_REG_RF_CH						0x05 // RF Channel
#define NRF_REG_RF_SETUP					0x06 // RF Setup Register
#define NRF_REG_STATUS						0x07 // Status Register
#define NRF_REG_OBSERVE_TX					0x08 // Transmit observe register
#define NRF_REG_RPD							0x09 // Receive power detector
#define NRF_REG_RX_ADDR_P0					0x0A // Receive address data pipe 0
#define NRF_REG_RX_ADDR_P1					0x0B // Receive address data pipe 1
#define NRF_REG_RX_ADDR_P2					0x0C // Receive address data pipe 2
#define NRF_REG_RX_ADDR_P3					0x0D // Receive address data pipe 3
#define NRF_REG_RX_ADDR_P4					0x0E // Receive address data pipe 4
#define NRF_REG_RX_ADDR_P5					0x0F // Receive address data pipe 5
#define NRF_REG_TX_ADDR						0x10 // Transmit address
#define NRF_REG_RX_PW_P0					0x11 // Number of bytes in RX payload
#define NRF_REG_RX_PW_P1					0x12 // Number of bytes in RX payload
#define NRF_REG_RX_PW_P2					0x13 // Number of bytes in RX payload
#define NRF_REG_RX_PW_P3					0x14 // Number of bytes in RX payload
#define NRF_REG_RX_PW_P4					0x15 // Number of bytes in RX payload
#define NRF_REG_RX_PW_P5					0x16 // Number of bytes in RX payload
#define NRF_REG_FIFO_STATUS					0x17 // FIFO Status Register
#define NRF_REG_DYNPD						0x1C // Enable dynamic payload length
#define NRF_REG_FEATURE						0x1D // Feature Register

// Functions
void rf_init(void);
void rf_stop(void);
void rf_set_speed(NRF_SPEED speed);
void rf_set_power(NRF_POWER power);
void rf_set_address_width(NRF_AW aw);
void rf_set_crc_type(NRF_CRC crc_type);
void rf_set_retr_retries(int retries);
void rf_set_retr_delay(NRF_RETR_DELAY delay);
void rf_set_rx_addr(int pipe, const char *address, int addr_len);
void rf_set_tx_addr(const char *address, int addr_len);
void rf_write_tx_payload(const char *data, int length);
void rf_write_tx_payload_no_ack(const char *data, int length);
void rf_write_ack_payload(int pipe, const char *data, int length);
void rf_read_rx_payload(char *data, int length);
void rf_set_frequency(int freq);
int rf_get_frequency(void);
int rf_get_address_width(void);
void rf_power_up(void);
void rf_power_down(void);
void rf_mode_tx(void);
void rf_mode_rx(void);
void rf_enable_pipe_autoack(int pipes);
void rf_enable_pipe_address(int pipes);
void rf_enable_pipe_dlp(int pipes);
void rf_enable_features(int features);

void rf_flush_tx(void);
void rf_flush_rx(void);
void rf_flush_all(void);

void rf_clear_irq(void);
void rf_clear_rx_irq(void);
void rf_clear_tx_irq(void);
void rf_clear_maxrt_irq(void);
int rf_get_payload_width(void);
int rf_status(void);
int rf_fifo_status(void);
int rf_rx_power_detect(void);

void rf_write_reg(int reg, const char *data, int len);
void rf_write_reg_byte(int reg, char value);
void rf_read_reg(int reg, char *data, int len);
char rf_read_reg_byte(int reg);

#endif /* RF_H_ */
